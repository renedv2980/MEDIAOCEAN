*          DATA SET ACSCR00    AT LEVEL 101 AS OF 09/21/15                      
*PHASE T60C00A                                                                  
*&&ONLIN SET   Y                                                                
*INCLUDE ACJOBCOL                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 106 AS OF 9/26/14         *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'ACSCR00 - CONTROLLER OF REPORT GENERATOR'                       
SCR00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**SCR0**,RA,R9,R8,CLEAR=YES,RR=RE                    
*                                                                               
         USING WORKD,R7                                                         
         LR    R7,RC                                                            
         MVC   ASYSFACS,8(R1)                                                   
*                                                                               
         USING TWAD,R5                                                          
         L     R5,4(,R1)           R5=A(TWA) US                                 
*                                                                               
         USING COMFACSD,R6                                                      
         L     R6,16(,R1)          R6=A(COMFACS) US                             
*                                                                               
         ST    R1,ACPARMA          SET A(REG PLIST) IN ACPARMA                  
         ST    R1,ACFULL                                                        
         ST    RE,ACRELO                                                        
         ST    RB,ACBASE1                                                       
         ST    RA,ACBASE2                                                       
         ST    R9,ACBASE3                                                       
         ST    R8,ACBASE4                                                       
         ST    RD,ACWORKA                                                       
*                                                                               
         GOTO1 CCALLOV,ACPARM,(1,0),0,0           LOAD PHASE TABLE              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD CONTROLLER TABLES                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)                                                       
         LA    R0,((ACSELTAB-ACRECTAB)/4)+1      NUMBER OF TABLES               
         SR    RE,RE                                                            
         MVC   VBLDCUR,CBLDCUR                                                  
*                                                                               
SCR01    L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACRECTAB(RE)                                                  
         LA    RE,4(,RE)                                                        
         BCT   R0,SCR01                                                         
*                                                                               
         LA    R0,ACNUMTAB         NUMBER OF MY TABLES                          
         LA    R2,0                                                             
*                                                                               
SCR02    L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACCNTTAB(R2)                                                  
         LA    R2,4(,R2)                                                        
         LA    RE,4(,RE)                                                        
         BCT   R0,SCR02                                                         
*                                                                               
         MVI   ACINDS,ACI24PFK     ACTIVATE FOR 24 PFKEYS                       
         MVI   ACFSTIND,ACHKBEF                                                 
         MVI   ACRECIND,ACHKAFT                                                 
         MVI   ACACTIND,0                                                       
         MVI   ACKEYIND,ACHKBEF+ACNOVAL                                         
         MVI   ACLSTIND,ACHKBEF                                                 
         MVI   ACLFMIND,ACHKBEF+ACLFMIRV                                        
         MVI   ACLSMIND,ACLSMISK+ACHKBEF                                        
*                                                                               
         SR    RE,RE               SET HOOK ADDRESSES                           
         LA    R0,CONADDRN         NUMBER OF ADDRESS TO LOAD                    
         L     R1,=A(CONADDRS)     POINT TO TABLE                               
         A     R1,ACRELO                                                        
SCR03    L     RF,0(,R1)           LOAD A(VALUE) FROM TABLE                     
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         A     RF,ACRELO           ADD RELOCATION FACTOR                        
         ST    RF,ACPHSLST(RE)     STORE IN LIST                                
         LA    RE,4(,RE)           BUMP TO NEXT ENTRY                           
         LA    R1,4(,R1)                                                        
         BCT   R0,SCR03                                                         
*                                                                               
         MVI   ELEMSEQ,0           CLEAR OUT SEQUENCE                           
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   DATADISP,=H'49'                                                  
         SPACE 1                                                                
***********************************************************************         
*        RELOCATE INCLUDE MODULES FOR GLOBAL ADDRESSABILITY           *         
***********************************************************************         
         SPACE 1                                                                
         L     RF,=V(ACJOBCOL)     FOR PRODUCTION ESTIMATES                     
         A     RF,ACRELO                                                        
         ST    RF,AJOBCOL                                                       
         MVC   ASOFDAT,CSOFDAT                                                  
         SPACE 1                                                                
***********************************************************************         
*        RELOCATE INDEPENDENT ROUTINES FOR GLOBAL ADDRESSABILITY      *         
***********************************************************************         
         SPACE 1                                                                
         L     R1,=A(R_ADDMSG)                                                  
         A     R1,ACRELO                                                        
         ST    R1,ADDMSG                                                        
*                                                                               
         L     R1,=A(R_VOFFIC)                                                  
         A     R1,ACRELO                                                        
         ST    R1,VOFFICE                                                       
*                                                                               
         L     R1,=A(RMAKLIST)                                                  
         A     R1,ACRELO                                                        
         ST    R1,MAKELIST                                                      
*                                                                               
         L     R1,=A(VALPARMS)                                                  
         A     R1,ACRELO                                                        
         ST    R1,VALPARM                                                       
*                                                                               
         L     R1,=A(IOHOOK)                                                    
         A     R1,ACRELO                                                        
         ST    R1,VIO                                                           
*                                                                               
         LA    R1,WORKD            SET I/O VARIABLES                            
         AH    R1,=Y(IOAREA1-WORKD)                                             
         ST    R1,ACIOADD                                                       
         LH    R1,=Y(IOAREA2-IOAREA1)                                           
         STH   R1,ACIOLEN                                                       
         MVI   ACIONUM,3                                                        
*                                                                               
         MVC   ACSYSPGM,=X'060C'                                                
         MVI   ACHLPSCR,X'FE'      SET TWA VARIABLES                            
         MVI   ACTWAREC,1                                                       
         MVI   ACTWAACT,2                                                       
         MVI   ACTWAKEY,0                                                       
         MVI   ACTWAOPT,5                                                       
*                                                                               
         LH    R1,=Y(SCROVLYH-TWAD)                                             
         STCM  R1,3,ACENDTWA                                                    
         LH    R1,=Y(IOAREA1-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA1                                                      
         LH    R1,=Y(IOAREA2-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA2                                                      
         LH    R1,=Y(IOAREA3-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA3                                                      
         LH    R1,=Y(IOAREA0-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA0                                                      
         LH    R1,=Y(APLOCAL-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,APALOCAL                                                      
         LH    R1,=Y(RFPAREA-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,ARFPBLK                                                       
         LH    R1,=Y(MINIO-WORKD)                                               
         LA    R1,WORKD(R1)                                                     
         ST    R1,AMINIO                                                        
         LH    R1,=Y(MINIOTAB-WORKD)                                            
         LA    R1,WORKD(R1)                                                     
         ST    R1,AMINIOTB                                                      
         LH    R1,=Y(OFFBLK-WORKD)                                              
         LA    R1,WORKD(R1)                                                     
         ST    R1,AOFFBLK                                                       
*                                                                               
         LH    R1,=Y(REQOFFL-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AREQOFFL                                                      
*                                                                               
         USING TIOBD,RF                                                         
         L     R1,ACFULL           ORIGINAL PARAMETER LIST                      
         L     RF,0(,R1)           A(TIOB)                                      
         SR    R2,R2                                                            
         ICM   R2,3,TIOBCURD       DISPLACEMENT TO CURSOR                       
         A     R2,4(,R1)           A(TWA), ADD START OF TWA                     
         ST    R2,ACURSOR                                                       
         DROP  RF                                                               
*                                                                               
         L     R1,=A(SAVEIO-TWAD)                                               
         LA    R1,TWAD(R1)                                                      
         ST    R1,ASAVEIO                                                       
*                                                                               
         OC    TWASAGN,TWASAGN     NEW SECURITY?                                
         BZ    SCR10               NO                                           
         LH    R1,=Y(SECAREA-TWAD)                                              
         LA    R1,TWAD(R1)         POINT TO SECAREA                             
         ST    R1,ACASEC           SAVE ADDRESS                                 
*                                                                               
         TM    SECINDS-SECD(R1),SECIINIT                                        
         BO    SCR10                                                            
         LR    RE,R1                                                            
         LA    RF,SECAREAL                                                      
         XCEFL                                                                  
         GOTO1 CSECRET,ACPARM,('SECPINIT',ACASEC),SECAREAL                      
         BE    *+6                                                              
         DC    H'00'                                                            
         SR    R1,R1                                                            
*                                                                               
         USING SECD,R2                                                          
         L     R2,ACASEC           LOAD SECRECT BLOCK                           
         MVC   TWAPERSN,SECPID                                                  
*                                                                               
SCR10    DS    0H                                                               
         GOTO1 CGENERAL,WORKD      CALL GENERAL                                 
         B     EXIT                FOR PATCHING PURPOSES                        
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
EXITR1   XIT1  REGS=(R1)                                                        
*                                                                               
EXITRF   XIT1  REGS=(RF)                                                        
*                                                                               
         DROP  R2,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*  LOAD CORE RESIDENT PHASES                                          *         
***********************************************************************         
         SPACE 1                                                                
PHASES   DS    0F                                                               
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QCENTER)                                                     
         DC    AL1(QUNDRLIN)                                                    
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QRFPIO)                                                      
         DC    AL1(QOFFAL)                                                      
         DC    AL1(QTSAR)                                                       
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  HOOK ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1  BASE=ACBASE1,WORK=(RC,RWRKX-RWRKD),LABEL=NO                      
*                                                                               
         USING RWRKD,RC                                                         
         USING TWAD,R5                                                          
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         L     R5,ATWA                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SR    RF,RF                                                            
         IC    RF,ACMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
         SPACE 2                                                                
         B     HKFRST              ACMFRST                                      
         B     HKREC               ACMRECR                                      
         B     HOOKX               ACMACTR                                      
         B     HKCKKEY             ACMKEYR                                      
         B     HOOKX               ACMOPTR                                      
         B     HKLFMR              ACMLFMR                                      
         B     HKLSMR              ACMLSMR                                      
         B     HOOKX               ACMREPR                                      
         B     HOOKX               ACMOTHR                                      
         B     HOOKX               ACMRECA                                      
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HKBLDPF             ACMLSTR                                      
*                                                                               
HOOKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
HOOKOK   SR    RE,RE               SET CC = OK                                  
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  HOOK FIRST THINGS FIRST                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING COMFACSD,R1                                                      
         USING XTRAINFD,RF                                                      
HKFRST   NI    GENIND,GENRSTEL+GENPSWD     ONLY THING TO LEAVE ON               
         L     R1,ACOM                     A(COMFACS)                           
         L     RF,CXTRAINF                 A(XTRAINFO) IN COMFACS               
         TM    XIFLAG1,XIROMODE+XIROSYS+XIWRONGF                                
         BZ    *+8                                                              
         OI    GENIND,GENREADO     SET READ ONLY SWITCH                         
         DROP  R1                                                               
                                                                                
         USING FACTSD,R1                                                        
*        GOTO1 VGETFACT,RPARM,(X'80',RBYTE),F#SEIND                             
*        TM    RBYTE,X'14'         IS SYSTEM READ ONLY ?                        
*        BZ    HKFRST10                                                         
*        OI    GENIND,GENREADO     SET READ ONLY SWITCH                         
*                                                                               
HKFRST10 GOTO1 VGETFACT,RPARM,(2,0)                                             
         L     R1,RPARM                                                         
*                                  ************************************         
*                                  * FATSTAT6 IS INITIALIZED FROM     *         
*                                  * TSTAT6. TSTAT6 IS IN DSECT UTLD  *         
*                                  * DEFIND IN MODULE FAUTL,          *         
*                                  * X'80' = STERIO EMULATOR (S=Y)    *         
*                                  * X'08' = STERIO FULL     (S=*Y)   *         
*                                  * X'01' = STERIO SPECIAL  (S=**Y)  *         
*                                  ************************************         
         TM    FATSTAT6,X'08'      FULL STEREO MODE ?                           
         BZ    *+8                                                              
         OI    GENIND,GENSTRO                                                   
         SR    RF,RF                                                            
         IC    RF,CULANG                                                        
         CLI   CULANG,15           TOO HIGH A NUMBER ?                          
         BNH   *+6                 NO                                           
         DC    H'00'               YEP, SOMETHING WRONG                         
         MH    RF,=H'16'                                                        
         A     RF,FAXLATES                                                      
         L     RE,8(,RF)           POINT TO UPPER CASE TABLE                    
         ST    RE,AUPCASE          CURRENT LANGUAGE TRANSLATE TABLES            
         DROP  R1                                                               
*                                                                               
         MVC   SCNP3NEQ(2),=C',='  SPECIAL 3RD PARAMETER TO SCANNER             
         MVC   SCNP3NEQ+2(1),SCCOMMA       LANGUAGE SOFT COMMA                  
         MVI   SCNP3NEQ+3,X'30'            IGNORE   EQUAL SIGN                  
*        MVC   WIDEL,=C'WIDE'      OUTPUT TYPE LANDSCAPE                        
*        MVC   WIDEP,=C'WIDEPC'    OUTPUT TYPE PROTRAIT                         
*                                                                               
         XC    AIO,VIO             SWAP ROUTINE CALL TO HOOK                    
         XC    VIO,AIO                                                          
         XC    AIO,VIO                                                          
*                                                                               
         TM    TWAMODE,TWAMNXT     FIRST TIME THROUGH                           
         BZ    HKF150              YES                                          
         CLC   CULANG,SAVELANG                                                  
         BE    HKF150              NO CHANGE OF LANGUAGE                        
         OI    GENIND,GENLANG                                                   
*                                                                               
         USING RECTABD,R2                                                       
         SR    R1,R1                                                            
         L     R2,ACRECTAB                                                      
HKF102   ICM   R1,1,RECELEN        ENTRY LENGTH                                 
         BZ    HKF104                                                           
         CLC   RECNUMB,TWALREC     LAST RECORD                                  
         BE    HKF103              FOUND MATCH                                  
         AR    R2,R1                                                            
         B     HKF102                                                           
         DROP  R2                                                               
*                                                                               
HKF103   GOTO1 AEXPREC,(R2)                                                     
         MVC   SCRREC,SPACES                                                    
         MVC   SCRREC,SCRECNAM                                                  
         OI    SCRRECH+6,FVOXMT                                                 
*                                                                               
         USING ACTTABD,R2                                                       
HKF104   L     R2,ACACTTAB                                                      
*                                                                               
HKF105   CLI   ACTTABD,EOT         END OF TABLE                                 
         BE    HKF110                                                           
         TM    TWAMODE,TWAMLSM     LIST SELECT MODE ?                           
         BZ    HKF106              NO                                           
         OI    GENIND,GENNSCR      YES, ACSCR00 RE-LOADS SCREEN                 
         CLI   ACTNUMB,ACTSEL      LOOK FOR "SELECT" ACTION                     
         BE    HKF108              FOUND IT                                     
         B     HKF107                                                           
*                                                                               
HKF106   CLC   ACTNUMB,TWALACT     LAST ACTION                                  
         BE    HKF108                                                           
*                                                                               
HKF107   LA    R2,ACTTABL(,R2)                                                  
         B     HKF105                                                           
         DROP  R2                                                               
*                                                                               
HKF108   GOTO1 AEXPACT,(R2)                                                     
         MVC   SCRACT,SPACES                                                    
         MVC   SCRACT,SCACTNAM                                                  
         OI    SCRACTH+6,FVOXMT                                                 
*                                                                               
HKF110   MVC   SCRTYP,SPACES       UNKNOWN TYPE, SO CLEAR                       
*                                                                               
         USING REPTABD,R3                                                       
         L     R3,ACTYPTAB                                                      
HKF112   CLI   REPCODE,EOT         END OF TABLE?                                
         BE    HKF150                                                           
         CLC   REPTYNO,LASTTYNO                                                 
         BE    HKF115                                                           
         LA    R3,REPLNQ(,R3)      BUMP TO NEXT TYPE                            
         B     HKF112                                                           
*                                                                               
HKF115   GOTO1 EXPRPTY,(R3)                                                     
         MVC   SCRTYP(L'REPCODE),RREPCDE                                        
         OI    SCRTYPH+6,FVOXMT                                                 
         DROP  R3                                                               
*                                                                               
HKF150   L     RF,=A(DELMTAB)                                                   
         A     RF,ACRELO                                                        
*                                                                               
HKF152   CLI   0(RF),X'FF'                                                      
         BE    HKF155              CAN'T FIND, ASSUME ENG                       
         CLC   CULANG,0(RF)                                                     
         BE    HKF160                                                           
         LA    RF,DELMTLN(,RF)                                                  
         B     HKF152                                                           
*                                                                               
HKF155   L     RF,=A(DELMTAB)                                                   
         A     RF,ACRELO                                                        
*                                                                               
HKF160   MVC   APLNGSET,1(RF)                                                   
         TM    TWASWPST,TWAFIRST   FIRST TIME THROUGH ?                         
         BO    HKFRST20            NO,   SKIP                                   
         OI    TWASWPST,TWAFIRST   SAY   NOT  FIRST TIME THROUGH                
         GOTO1 AFVAL,SCRCODEH      GET   INITIAL FORMAT CODE                    
         MVC   TWAIFORM,FVIFLD     SAVE  INITIAL FORMAT CODE                    
         OC    TWASAGN,TWASAGN     OLD OR NEW SECURITY                          
         BNZ   HKFRST04            NEW                                          
         MVC   TWAPERSN,SPACES                                                  
         MVC   TWAPERSN(3),=C'DDS'                                              
*        B     HKFRST04                                                         
*                                                                               
*&&DO                                                                           
         USING SA0REC,R2                                                        
HKFRST01 LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUAGYSEC    ALPHA SECURITY agency CODE                   
         CLC   CUAGYSEC,SPACES     WASN'T SURE IF ALWAYS SET                    
         BH    *+10                                                             
         MVC   SA0KAGY,CUAALF      ALPHA AGENCY CODE                            
         MVC   SA0KNUM,CUPASS      CURRENT PASSWORD #                           
         MVC   FVMSGNO,=AL2(ACESEC)                                             
         GOTO1 AIO,IO3+IORD+IOCTFILE                                            
         BNE   HKFRST04                                                         
         L     R2,AIOAREA3                                                      
         LA    R2,SA0DATA          POINT TO ELEMENTS                            
*                                                                               
         USING SAPALD,R2                                                        
         SR    R1,R1                                                            
HKFRST02 CLI   SAPALEL,0          END OF RECORD?                                
         BE    HKFRST04                                                         
         CLI   SAPALEL,SAPALELQ   FIND ELEMENT                                  
         BE    HKFRST03                                                         
         IC    R1,SAPALLN                                                       
         AR    R2,R1                                                            
         B     HKFRST02           LOOP                                          
*                                                                               
HKFRST03 MVC   TWAPERSN,SAPALPID  SAVE PERSONS ID                               
*&&                                                                             
         USING ACCRECD,R2                                                       
HKFRST04 XC    SVOFFAL,SVOFFAL                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
HKFRST20 LA    R2,IOKEY                                                         
         MVC   ACCKEY,SPACES                                                    
         MVC   ACCKEY(1),CUABIN    COMPANY CODE                                 
         GOTO1 AIO,IO3+IORD+IOACCFIL                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CPYELD,R2                                                        
         L     R2,AIOAREA3                                                      
         AH    R2,=Y(ACCORFST)                                                  
         SR    R1,R1                                                            
*                                                                               
HKFRST25 CLI   CPYEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         CLI   CPYEL,CPYELQ        COMPANY ELEMENT X'10'                        
         BE    HKFRST30                                                         
         IC    R1,CPYLN                                                         
         BNZ   *+6                                                              
         DC    H'0'                ELEMENT LENGTH OF ZERO                       
         AR    R2,R1                                                            
         B     HKFRST25                                                         
*                                                                               
HKFRST30 ICM   R1,1,CPYSFST        COMPANY FISCAL START MONTH                   
         BNZ   *+8                                                              
         LA    R1,C'1'             IF NOT SET DEAFAULT TO JANUARY               
         STC   R1,FISCALMO                                                      
         CLI   FISCALMO,C'1'                                                    
         BNL   HKFRST31            MUST BE X'F1' - X'F9'                        
         AH    R1,=H'09'           MAKE X'C1'-X'C3' ==> X'CA'-X'CC'             
         STC   R1,FISCALMO                                                      
*                                                                               
HKFRST31 NI    FISCALMO,TURNOFF-X'F0'     MAKE BINARY 1-12                      
*&&UK                                                                           
         CLI   CPYLN,CPYLN3Q                                                    
         BL    HKFRS31A                                                         
         OC    CPYCURRS,CPYCURRS   TEST OF 2ND CURRENCY                         
         BZ    HKFRS31A                                                         
         OI    GENIND,GENEURO                                                   
*&&                                                                             
HKFRS31A MVC   SVCMPST1,CPYSTAT1                                                
         MVC   SVCMPST2,CPYSTAT2                                                
         MVC   SVCMPST3,CPYSTAT3                                                
         MVC   SVCMPST4,CPYSTAT4                                                
         MVC   SVCMPST5,CPYSTAT5                                                
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFACST1,CPYSTAT1                                                
         MVC   OFFACST2,CPYSTAT2                                                
         MVC   OFFACST3,CPYSTAT3                                                
         MVC   OFFACST4,CPYSTAT4                                                
         CLI   CPYLN,CPYLN2Q                                                    
         BNE   HKFRST32                                                         
         MVC   OFFACST5,CPYSTAT5                                                
         MVC   OFFACST6,CPYSTAT6                                                
         MVC   OFFACST7,CPYSTAT7                                                
         MVC   OFFACST8,CPYSTAT8                                                
*                                                                               
HKFRST32 MVI   OFFAACT,OFFAINI                                                  
         OC    OFFASAV(OFFASAVL),SVOFFAL                                        
         BZ    *+8                                                              
         MVI   OFFAACT,OFFARES                                                  
         MVC   OFFACOMF,ACOM                                                    
         MVC   OFFATSAR,VTSAR                                                   
         MVC   OFFAALPH,CUAALF                                                  
         MVC   OFFACPY,CUABIN                                                   
         MVC   OFFAREQL,AREQOFFL                                                
*        MVC   OFFACST1(OFFAOPOS-OFFACST1),SVCMPSTA                             
         MVC   OFFALIMA,CUACCS                                                  
         MVC   OFFAAUTH,CUAUTH                                                  
*        MVI   OFFAINDS,OFFAIOFF                                                
         MVC   OFFASAV(OFFASAVL),SVOFFAL                                        
         NI    OFFAINDS,TURNOFF-OFFAIUOL-OFFAIXOL                               
         GOTO1 VOFFAL                                                           
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(0266)   INVALID OFFICE FOR ID                       
         MVC   SVOFFAL,OFFASAV                                                  
*                                                                               
HKFRSTX  B     HOOKX                                                            
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  HOOK RECORD                                                        *         
***********************************************************************         
         SPACE 1                                                                
HKREC    MVI   REPMODE,0                                                        
         CLC   TWALREC,INREC       SAME RECORD AS BEFORE?                       
         BE    *+10                                                             
         XC    APCURSOR,APCURSOR                                                
*                                                                               
         USING ACTTABD,R2                                                       
*        TM    SCRACTH+4,FVITHIS   INPUT THIS TIME ?                            
*        BZ    HKREC20                                                          
         CLI   INREC,RECRPT        ONLY IF RECORD = "REPORT"                    
         BNE   HKREC20                                                          
         CLI   TWASCRN,SCRRLDS     X'E8' REQUEST/DISPLAY REQUEST SCREEN         
         BNE   HKREC20                                                          
         GOTO1 AFVAL,SCRACTH                                                    
         SR    R4,R4                                                            
         IC    R4,FVXLEN           EX LENGTH                                    
         EX    R4,*+8                                                           
         BE    HKREC20             DON'T BOTHER CHECKING                        
         CLC   SCRACT(0),AC@SELCT                                               
         L     R2,ACACTTAB                                                      
*                                                                               
HKREC10  CLI   ACTTABD,EOT         END OF TABLE                                 
         BE    HKREC12             I DON'T KNOW AND DON'T CARE                  
         GOTO1 AEXPACT,ACTTABD                                                  
         EX    R4,*+8                                                           
         BE    HKREC16                                                          
         CLC   SCACTNAM(0),FVIFLD  ENTERED INPUT ACTION                         
         LA    R2,ACTTABL(,R2)                                                  
         B     HKREC10                                                          
*                                                                               
HKREC12  L     R2,ACACTTAB                                                      
         CLI   FVILEN,3            HOW LONG WAS THE DATA INPUT ?                
         BNH   HKREC20             DON'T BOTHER IF  ONLY 3 OR LESS              
*                                                                               
HKREC15  CLI   ACTTABD,EOT         END OF TABLE                                 
         BE    HKREC20             I DON'T KNOW AND DON'T CARE                  
         GOTO1 AEXPACT,ACTTABD                                                  
         CLC   SCACTNAM(3),FVIFLD  CHOPPED SHORT INPUT ACTION                   
         BE    HKREC16                                                          
         LA    R2,ACTTABL(,R2)                                                  
         B     HKREC15                                                          
*                                                                               
HKREC16  CLI   ACTNUMB,ACTSOON     SOON      REQUEST                            
         BE    HKREC18                                                          
         CLI   ACTNUMB,ACTOVER     OVERNIGHT REQUEST                            
         BE    HKREC18                                                          
         CLI   ACTNUMB,ACTCNCL     CANCEL    REQUEST                            
         BE    HKREC18                                                          
         CLI   ACTNUMB,ACTRES      RESTORE   REQUEST                            
         BNE   HKREC20                                                          
*                                                                               
HKREC18  MVC   FVMSGNO,=AL2(FVFMIXI)  NO,    INVALID REC/ACT COMBO              
*                                                                               
HKREC20  TM    TWAMODE,TWAMNXT     FIRST TIME THROUGH                           
         BZ    HKREC30             YES                                          
         TM    GENIND,GENLANG                                                   
         BZ    HKREC40                                                          
         TM    GENIND,GENNSCR      HAVE ACSCR00 RE-LOAD SCREEN INSTEAD          
         BO    HKREC30             YES                                          
         MVI   TWASCRN,0           FORCE RELOAD OF SCREEN                       
*                                                                               
HKREC30  MVC   SAVELANG,CULANG     SAVE CURRENT LANGUAGE                        
         GOTO1 VDICTAT,RPARM,C'LU  ',ACDICTAB,DICLIST                           
         TM    TWAMODE,TWAMNXT     FIRST TIME THROUGH                           
         BO    HKREC40             NO, SO EXIT                                  
         MVC   SRLCOL,=C'0001'                                                  
         MVC   SRLHLP,AC@PAGE                                                   
         MVC   SRLPRVW,AC@HALF                                                  
         OI    TWASWPST,TWAFIRST                                                
         MVI   PRVWROW,1           PREVIEW ROW SET                              
         MVI   PRVWCOL,1                                                        
         MVC   SAVFORM,TWAIFORM    FORMAT CODE IF ANY                           
         MVC   FLTFORM,TWAIFORM    FORMAT CODE TO FILTER ON                     
         MVC   SAVEREP,TWAIFORM    FORMAT CODE                                  
         XC    AFMTADR,AFMTADR                                                  
*                                                                               
HKREC40  DS    0H                                                               
HKRECX   B     HOOKX                                                            
         EJECT ,                                                                
***********************************************************************         
*  HOOK KEY / VALIDATE REPORT TYPE                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING REPTABD,R3                                                       
HKCKKEY  TM    GENIND,GENNSCR      LOAD SCREEN (SELECT MODE ONLY)               
         BZ    HKCKKY05                                                         
         L     R1,ATWAEND                                                       
         GOTO1 AOVSCR              RE-LOAD SCREEN FOR LANGUAGE CHANGE           
*                                                                               
HKCKKY05 L     R3,ACTYPTAB                                                      
         GOTO1 AFVAL,SCRTYPH                                                    
         BE    HKCKKY10            REPORT TYPE INPUT                            
         CLI   INACT,ACTADD        ARE WE ADDING A RECORD?                      
         BNE   HKCKKEYX                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)   NOT VALID INPUT                          
         B     HKCKKEYX                                                         
*                                                                               
HKCKKY10 CLI   REPNUM,EOT          END OF TABLE?                                
         BNE   HKCKKY20                                                         
         MVC   FVMSGNO,=AL2(ACEIVTY)   INVALID TYPE                             
         B     HKCKKEYX                                                         
*                                                                               
HKCKKY20 GOTO1 EXPRPTY,(R3)                                                     
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXCLC RF,RREPCDE,FVIFLD                                                
         BNE   HKCKKY25                                                         
         CLI   INREC,RECAPG                                                     
         BE    HKCKKY22                                                         
         CLI   INREC,RECRPT        DON'T BOTHER CHECKING                        
         BE    HKCKKY24                                                         
         TM    REPFLAG,REPSJCL     SPECIAL APG JCL?                             
         BZ    HKCKKY24                                                         
         B     HKCKKY25                                                         
*                                                                               
HKCKKY22 TM    REPFLAG,REPSJCL                                                  
         BZ    HKCKKY25                                                         
*                                                                               
HKCKKY24 TM    REPFLAG,REPDDS      DDS ONLY?                                    
         BZ    HKCKKY30            NO, SO OK                                    
         TM    CUSTAT,CUSDDS       YES, SO ARE WE AT DDS?                       
         BNZ   HKCKKY30            YES, SO OK                                   
*                                                                               
HKCKKY25 LA    R3,REPLNQ(,R3)      BUMP TO NEXT TYPE                            
         B     HKCKKY10                                                         
*                                                                               
HKCKKY30 MVC   SCRTYP(L'REPCODE),RREPCDE                                        
         OI    SCRTYPH+6,FVOXMT                                                 
         OC    TWASAGN,TWASAGN     NEW SECURITY?                                
         BZ    HKCKKY35            NO                                           
         GOTO1 VSECRET,ACPARM,('SECPFLDP',ACASEC),REPJCLID                      
         BNL   HKCKKY35            WRITE/READ BIT ON                            
         MVC   FVMSGNO,=AL2(1445)                                               
         B     HKCKKEYX            NO ACCESS TO REPORT TYPE                     
*                                                                               
HKCKKY35 MVC   APREPCDE,RREPCDE    SAVE REPCODE                                 
         MVC   APREPNUM,REPNUM                                                  
         MVC   APREPTYP,RREPTYP                                                 
         MVC   APREPIND,REPIND                                                  
         MVC   APREPUL,REPUL       UNIT/LEDGER                                  
         MVC   APREPCUL,REPCUL     CONTRA UNIT/LEDGER                           
         MVC   APREPTY#,REPTYNO    RECORD OVERIDE TYPE                          
         MVC   APREPJCL,REPJCLID   ID TO IDENTIFY JCL TO USE                    
         MVC   APREPLD#,REPLDNO    ID FOR $REQ (LANDSCAPE VERSION)              
         MVC   APREPPT#,REPPTNO    ID FOR $REQ (PORTRIAT VERSION)               
         MVC   PREVREP,APREPCDE                                                 
         MVC   LASTTYNO,REPTYNO                                                 
         MVI   APMLDGR,NO                                                       
         TM    REPFLAG,REPMLDGR                                                 
         BZ    *+8                                                              
         MVI   APMLDGR,YES                                                      
         MVI   SOONVLUE,0                                                       
         TM    REPFLAG,REPXSOON                                                 
         BO    HKCKKY37            NOT ALLOWED TO SOON                          
         MVI   SOONVLUE,1          LEVEL ONE                                    
         TM    REPFLAG,REPASOON    RESTRICTIONS ON ACCOUNT TO SOON              
         BZ    HKCKKY37            YES                                          
         MVI   SOONVLUE,2          LEVEL TWO                                    
*                                                                               
HKCKKY37 MVI   APGFLAG,NO                                                       
         TM    REPFLAG,REPSJCL     APG JCL TYPE                                 
         BZ    *+8                                                              
         MVI   APGFLAG,YES                                                      
         CLI   INKEYN,0            ANY KEY COMPONENTS                           
         BE    HKCKKEYX            NO                                           
         L     R1,ATWAEND                                                       
         SR    RE,RE                                                            
         LA    RF,TWAD                                                          
         AH    RF,=Y(SAVAREA-TWAD)                                              
         TM    1(R1),FVAPROT       PROTECTED FIELD                              
         BZ    *+16                                                             
         IC    RE,0(,R1)                                                        
         BXLE  R1,RE,*-12                                                       
         B     HKCKKEYX                                                         
         ST    R1,AKEYHDR                                                       
*                                                                               
HKCKKEYX B     HKCKPFK                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET RECORD, ACTION AND PFKEY MATCH FROM PFKTABLE                   *         
***********************************************************************         
         SPACE 1                                                                
HKCKPFK  XC    APFKENTR,APFKENTR                                                
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         MVC   TWASWPRE,SCRECN     INITIALIZE SWAP RECORD                       
         MVC   TWASWPAC,SCACTN     INITIALIZE SWAP ACTION                       
         CLI   APPFKEY,0                                                        
         BE    HKCKPFKX                                                         
         CLI   INACT,ACTSEL        MAKE SURE IT IS SELECT MODE                  
         BE    *+12                                                             
         NI    TWAMODE,TURNOFF-TWAMSEL                                          
         MVI   SVSELACT,0                                                       
         L     R1,AACTHDR                                                       
         ST    R1,FVADDR                                                        
*                                                                               
         USING PFKTABD,R3                                                       
         L     R3,ACPFKTAB         FIND REC/ACTION/PFK IN PFK TABLE             
HKCKPF30 CLI   PFKSEL,EOT                                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(ACEPFK)      INVALID PFKEY                          
         B     HKCKPFKX                                                         
         CLC   PFKSEL,APPFKEY      MATCH ON PFKEY                               
         BNE   HKCKPF32                                                         
         GOTO1 =A(VALPFK),RPARM,(RC),(R3),RR=ACRELO                             
         BE    HKCKPF40                                                         
*                                                                               
HKCKPF32 LA    R3,PFKTABL(,R3)                                                  
         B     HKCKPF30                                                         
*                                                                               
         USING RECTABD,R2                                                       
*                                                                               
HKCKPF40 TM    PFKIND,PFKIRST      RESTORE SAVED REC/ACT?                       
         BZ    HKCKPF45                                                         
         MVI   TWASWPAC,ACTDIS     DEFAULT TO DISPLAY                           
         CLI   SVRECORD,0                                                       
         BE    HKCKPF43                                                         
         MVC   TWASWPRE,SVRECORD                                                
         MVC   TWASWPAC,SVACTION                                                
*                                                                               
HKCKPF43 OI    TWASWPST,TWASWAP                                                 
         CLI   PFKNEWRE,NULL       RESTORE RECORD ONLY?                         
         BE    *+10                YES, SO FAR                                  
         MVC   TWASWPRE,PFKNEWRE   NO, ALWAYS USE PFK RECORD                    
         CLI   PFKNEWAC,NULL       RESTORE ACTION ONLY?                         
         BE    *+10                YES                                          
         MVC   TWASWPAC,PFKNEWAC   NO, ALWAYS USE PFK ACTION                    
         B     HKCKPF90                                                         
*                                                                               
HKCKPF45 CLC   PFKNEWRE,INREC                                                   
         BE    *+16                                                             
         XC    SVRECORD,SVRECORD                                                
         XC    SVACTION,SVACTION                                                
         TM    PFKIND,PFKISAV      SHOULD WE SAVE CURRENT REC/ACT               
         BZ    HKCKPF50            NO                                           
         MVC   SVRECORD,INREC      CURRENT RECORD                               
         MVC   SVACTION,INACT      CURRENT ACTION                               
         TM    TWAMODE,TWAMLSM     LIST SELECT MODE?                            
         BZ    HKCKPF50            NO                                           
         MVC   SVRECORD,SCRECN     CURRENT RECORD                               
         MVC   SVACTION,SCACTN     CURRENT ACTION                               
*                                                                               
HKCKPF50 CLI   SVACTION,ACTADD     WAS ACTION SAVED ADD ?                       
         BNE   *+8                                                              
         MVI   SVACTION,ACTCHA     SWITCH ADD TO CHANGE                         
         CLC   PFKREC,PFKNEWRE     IS THERE A NEW RECORD TYPE?                  
         BE    HKCKPF60                                                         
         MVC   TWASWPRE,PFKNEWRE   SWAP RECORD                                  
         OI    TWASWPST,TWASWAP    TURN ON SWAP                                 
*                                                                               
HKCKPF60 CLI   PFKNEWAC,NULL       NO NEW ACTION?                               
         BE    HKCKPF90                                                         
         OI    TWASWPST,TWASWAP    TURN ON SWAP                                 
         MVC   TWASWPAC,PFKNEWAC   SWAP ACTION                                  
*                                                                               
HKCKPF90 ST    R3,APFKENTR         SAVE ENTRY OF PFKEY TABLE                    
*                                                                               
HKCKPFKX B     HOOKX                                                            
         EJECT ,                                                                
***********************************************************************         
*  CHANGE RECORD FORMAT TO RECORD HEADER IF ACTION IS ADD             *         
***********************************************************************         
         SPACE 1                                                                
         USING PFKTABD,R3                                                       
         SPACE 1                                                                
HKLFMR   TM    ACLFMIND,ACHKBEF    HOOK BEFORE?                                 
         BZ    HKLFM50             NO                                           
         TM    GENIND,GENNSCR      DID WE RE-LOAD SCREEN ?                      
         BO    HKLFM30             YES, SO FORCE DISREC MODE                    
*                                                                               
HKLFM10  CLI   INACT,ACTADD        IS IT ACTION ADD?                            
         BNE   HKLFM20             NO                                           
*                                  SET TO DO AFTER NOW                          
         MVI   ACLFMIND,ACHKAFT+ACLFMIRV                                        
         CLI   INREC,RECFRM        FORMAT RECORD?                               
         BE    HOOKOK                                                           
         SPACE 1                                                                
***********************************************************************         
*  RESET SAVRECI/APRECID TO REDISPLAY RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
HKLFM20  TM    SCROPTH+4,FVITHIS   INPUT OCCURED?                               
         BNZ   HKLFM25                                                          
         ICM   R3,15,APFKENTR      GET PFKEY ENTRY                              
         BZ    HKLFMX              NO VALID PFKEY PRESSED                       
*                                                                               
HKLFM25  MVC   TWALSACT,INACT                                                   
         CLI   APMODE,APMVALK      ONLY FOR KEY VALIDATION                      
         BNE   HKLFMX                                                           
         CLI   INACT,ACTCHA        ACTION CHANGE?                               
         BNE   HKLFMX                                                           
         TM    SCROPTH+4,FVITHIS   INPUT OCCURED?                               
         BZ    HKLFMX                                                           
*                                                                               
HKLFM30  OI    ACLSMIND,ACLSMICI   RE-CHECK APRECID                             
*        IC    RF,APRECID                                                       
*        LA    RF,1(,RF)                                                        
*        STC   RF,SAVRECI                                                       
         B     HKLFMX                                                           
         EJECT ,                                                                
***********************************************************************         
*  CHANGE ACTION ADD TO ACTION CHANGE IF RECORD ADD OK                *         
***********************************************************************         
         SPACE 1                                                                
HKLFM50  MVI   ACLFMIND,ACHKBEF+ACLFMIRV                                        
         CLI   TWASWPAC,ACTADD                                                  
         BNE   HKLFM55                                                          
         MVC   SCRACT,AC@CHG       RECORD ADD SO SET ACTION TO CHANGE           
         MVI   INACT,ACTCHA        CHANGE ADD TO CHANGE FOR PFK'S               
         OI    SCRACTH+6,FVOXMT                                                 
*                                                                               
HKLFM55  MVC   FVADDR,AINKHDR                                                   
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(INFADD1)                                            
         B     HOOKOK                                                           
*                                                                               
HKLFMX   B     HOOKX                                                            
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  DEACTIVE RETURN PFKEY (LSM ONLY)                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING PFKTABD,R3                                                       
HKLSMR   TM    ACLSMIND,ACHKBEF    HOOK BEFORE?                                 
         BZ    HKLSMX                                                           
         MVI   ACPFXIT,0           USE DEFAULT                                  
         CLI   APPFKEY,0           PFKEY USED?                                  
         BE    HKLSMX              NO                                           
         CLI   APPFKEY,PFK12       AS IT RETURN                                 
         BNE   HKLSMX              NO, SO DON'T WORRY                           
         ICM   R3,15,APFKENTR                                                   
         BZ    HKLSMX                                                           
         TM    PFKIND,PFKINRTN     NO RETRUN                                    
         BZ    *+8                                                              
         MVI   ACPFXIT,X'FF'       DE-ACTIVATE DEFAULT RETURN PFKEY             
*                                                                               
HKLSMX   B     HOOKX                                                            
         EJECT ,                                                                
***********************************************************************         
*        BUILD AND DISPLAY PFKEYS IN EXTEND FIELD HEADERS             *         
*        #253 (X'FD') SELECT FIELD LINE                               *         
*        #254 (X'FE') PFKEY FIELD LINE                                *         
***********************************************************************         
         SPACE 1                                                                
         USING PFKTABD,R3                                                       
HKBLDPF  DS    0H                                                               
*                                                                               
         L     R2,AACTHDR                                                       
         SR    R1,R1                                                            
         XC    APFKHDR1,APFKHDR1                                                
         XC    APFKHDR2,APFKHDR2                                                
         XC    ASELHDR1,ASELHDR1                                                
         XC    ASELHDR2,ASELHDR2                                                
*                                                                               
HKBLD05  CLI   0(R2),0             AT END OF SCREEN?                            
         BE    HKBLD10             YES GET OUT FINISHED FOR NOW                 
         IC    R1,0(,R2)           LENGTH OF FIELD                              
         TM    1(R2),FVAXTND       EXTENDED FIELD HEADER?                       
         BZ    HKBLD09             NO, SO BUMP TO NEXT                          
*                                                                               
HKBLD06  SHI   R1,8                SUBTRACT LENGTH OF EXTEND FIELD              
         LR    RF,R2               USE RF FOR CALCULATIONS                      
         AR    RF,R1               BUMP TO EXTENDED FIELD HEADER                
         LA    R3,APFKHDR1         PFKEY LINE ADDRESS                           
         OC    APFKHDR1,APFKHDR1   DID WE ALREADY SAVE AN ADDRESS?              
         BZ    *+8                 NO, SO SAVE IN HDR1                          
         LA    R3,APFKHDR2         YES, SO SAVE 2ND LINE IN HDR2                
         CLI   0(RF),X'FE'         254 (PFK HELP)                               
         BE    HKBLD07             FOUND PFKEY LINE                             
*                                                                               
         LA    R3,ASELHDR1         SELECT HELP LINE ADDRESS                     
         OC    ASELHDR1,ASELHDR1   DID WE ALREADY SAVE AN ADDRESS?              
         BZ    *+8                 NO, SO SAVE IN HDR1                          
         LA    R3,ASELHDR2         YES, SO SAVE 2ND LINE IN HDR2                
         CLI   0(RF),X'FD'         253 (SELECT HELP)                            
         BNE   HKBLD08             NOT AN EXTENDED HEADER WE WANT               
*                                                                               
HKBLD07  SHI   R1,8                R1 = SIZE OF FIELD                           
         STH   R1,4(,R3)           SIZE OF FIELD                                
         ST    R2,0(,R3)           SAVE ADDRESS OF SCREEN                       
*                                                                               
HKBLD08  IC    R1,0(,R2)           RESTORE BUMPING LENGTH                       
*                                                                               
HKBLD09  AR    R2,R1               BUMP (IN THE NIGHT)                          
         B     HKBLD05             TRY AGAIN                                    
*                                                                               
HKBLD10  OC    APFKHDR1,APFKHDR1   ANY PFKEY LINE?                              
         BZ    HKBLD70             NO, SO CHECK SELECT HELP                     
         MVI   RBYTE,0             FIRST PASS, I.E. USING FULL LENGTH           
*                                                   FOR PFK NAMES               
*                                                                               
HKBLD12  MVI   RPFKAREA,C' '       CLEAR AREA                                   
         MVC   RPFKAREA+1(L'RPFKAREA-1),RPFKAREA                                
         ICM   R2,15,APFKHDR2      CLEAR 2ND PFKEY HELP LINE IF ANY             
         BZ    HKBLD15             NONE                                         
         TWAXC (R2),(R2),PROT=Y                                                 
*                                                                               
HKBLD15  L     R2,APFKHDR1         CLEAR 1ST PFKEY LINE                         
         TWAXC (R2),(R2),PROT=Y                                                 
*                                                                               
         USING PFKTABD,R3                                                       
         L     R3,ACPFKTAB                                                      
         LA    R4,RPFKAREA+2                                                    
HKBLD20  CLI   PFKREC,EOT          END OF TABLE?                                
         BE    HKBLD60             YES, SO CLEAN UP                             
         GOTO1 =A(VALPFK),RPARM,(RC),(R3),RR=ACRELO                             
         BNE   HKBLD50                                                          
         TM    PFKIND,PFKIRST      RESTORE TYPE PFKEY?                          
         BZ    HKBLD30             NO, SO OK                                    
         TM    TWAMODE,TWAMSEL+TWAMLSM                                          
         BNZ   HKBLD30                                                          
         CLI   PFKNEWRE,NULL                                                    
         BNE   HKBLD30             NEXT                                         
         CLI   SVRECORD,0          ANY THING TO RESTORE?                        
         BE    HKBLD50             NO, SO NOT VALID PFKEY                       
*                                                                               
HKBLD30  MVC   RPFKAREA(2),AC@PFK     MOVE IN PF                                
         SR    R1,R1                                                            
         CLI   PFKNAME,C' '        IGNORE PFKNAME IF SPACES                     
         BE    HKBLD50             LOOP FOR NEXT PFK                            
         IC    R1,PFKSEL           GET NUMBER                                   
         CVD   R1,RDUB             CONVERT TO CHARACTER                         
         OI    RDUB+7,X'0F'        ADJUST SIGN FOR CHARACTER FORMAT             
         UNPK  RWORK(2),RDUB+6(2)                                               
         MVC   RWORK+2(1),SCEQUAL  PUT THE NUMBER EQUALS "="                    
         LA    R0,2                SAVE LENGTH OF TWO                           
         MVC   0(2,R4),RWORK+1     R4=PFKEY AREA                                
         CLI   RWORK,C'0'          IF 1-9 ONLY 2 CHARACTERS NEEDED              
         BE    *+14                                                             
         LA    R0,3                SAVE LENGTH OF THREE                         
         MVC   0(3,R4),RWORK       IF >9 THEN 3 CHARACTERS                      
         AR    R4,R0               BUMP UP IN PFKEY AREA                        
*                                                                               
         MVC   RPFKNAM,PFKNAME                                                  
         CLI   RPFKNAM,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNL   HKBLD32                                                          
         CLI   RBYTE,0             USING FULL  LEN   FOR  PFK  NAMES ?          
         BE    HKBLD31             YES,  CONTINUE                               
         TM    PFKIND2,PFKSHRTL    ANY   SHORT LEN   NAME AVAILABLE ?           
         BZ    HKBLD31             NO,   SKIP                                   
         MVI   RPFKNAM+3,PFKNAMSQ  USE   SHORT LENGTH    NAME                   
*                                                                               
HKBLD31  GOTO1 VDICTAT,RPARM,C'SL  ',RPFKNAM,0                                  
*                                                                               
HKBLD32  CLI   RBYTE,0             USING FULL  LEN  FOR  PFK  NAMES ?           
         BE    HKBLD33             YES,  USE   FULL LENGTH                      
         TM    PFKIND2,PFKSHRTL    ANY   SHORT LEN   NAME AVAILABLE ?           
         BO    HKBLD34             YES,  USE   SHORT     LENGTH                 
*                                                                               
HKBLD33  MVC   0(PFKNAMEQ,R4),RPFKNAM    PFKEY NAME WITH LONG NAME              
         LA    R4,PFKNAMEQ-1(R4)         LAST  BYTE OF   NAME                   
         B     HKBLD35                   CONTINUE                               
*                                                                               
HKBLD34  MVC   0(PFKNAMSQ,R4),RPFKNAM    PFKEY NAME WITH SHORT NAME             
         LA    R4,PFKNAMSQ-1(R4)         LAST  BYTE OF   NAME                   
*                                                                               
HKBLD35  CLI   0(R4),C' '          FIND  NONE  SPACE     CHARACTER              
         BNE   *+8                                                              
         BCT   R4,HKBLD35                                                       
         LA    R4,2(,R4)           BUMP  UP    BY   ONE                         
*                                  FIND  NUM   OF   BYTES     USED              
         LA    R2,RPFKAREA         START OF    AREA                             
         LR    R6,R4               END   OF    AREA SO   FAR                    
         SR    R6,R2               LENGTH      USED                             
         CH    R6,=Y(MAXPFLEN)     DOES  THIS  PFKEY     FIT ?                  
         BL    HKBLD50             YES,  LOOP  FOR  NEXT PFKEY                  
         CLI   RBYTE,0             USING FULL  LEN  FOR  PFK  NAMES ?           
         BNE   HKBLD60             NO,   NO    MORE WILL FIT,                   
         MVI   RBYTE,1             USE   SHORT PFK  NAMES                       
         B     HKBLD12             REDO  WITH  SHORT     PFK  NAMES             
*                                                                               
HKBLD50  LA    R3,PFKTABL(,R3)                                                  
         B     HKBLD20                                                          
*                                                                               
HKBLD60  LA    R2,RPFKAREA         START OF PFKEY LINE                          
         LR    R3,R2                                                            
         LH    R1,PFKLEN1          LENGTH OF PFKEY FIELD                        
         AR    R3,R1               POINT INTO PFKAREA                           
         CLI   0(R3),C' '          FIND FIRST BLANK                             
         BE    HKBLD62                                                          
         BCTR  R3,0                                                             
         BCT   R1,*-10                                                          
         LH    R1,PFKLEN1                                                       
*                                                                               
HKBLD62  BCTR  R1,0                                                             
         L     R2,APFKHDR1                                                      
         OI    6(R2),FVOXMT        TRANSMIT                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RPFKAREA                                                 
         LH    R1,PFKLEN2                                                       
         BCTR  R1,0                                                             
         ICM   R2,15,APFKHDR2      GET 2ND PFK SCREEN ADDRESS                   
         BZ    HKBLD70             NONE, SO GET OUT                             
         OI    6(R2),FVOXMT        TRANSMIT                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)      MOVE REMAINDER TO SCREEN, 2ND LINE            
         EJECT ,                                                                
***********************************************************************         
*  BUILD SELECT DISPLAY LINE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SELTABD,R2                                                       
HKBLD70  OC    ASELHDR1,ASELHDR1     WAS THERE AN EXTEND HEADER X'FD'           
         BZ    HKBLDPFX                                                         
         MVC   RHLPAREA,SPACES                                                  
         L     R2,ACSELTAB                                                      
         LA    R4,RHLPAREA                                                      
*                                                                               
HKBLD72  CLI   SELCODE,EOT         END OF TABLE?                                
         BE    HKBLD90             YES SO DISPLAY                               
         CLC   SELRECB,INREC       MATCH RECORD                                 
         BNE   HKBLD88             LOOP TO NEXT                                 
         CLC   SELACTB,INACT       MATCH ACTION                                 
         BNE   HKBLD88             LOOP TO NEXT                                 
*                                                                               
*                                  ** SPECIAL CASE **                           
         CLI   SELRECB,RECRPT      RECORD = REPORT ?                            
         BNE   HKBLD74                                                          
         CLI   SELACTN,ACTDIS      DISPLAY REQUEST ?                            
         BE    HKBLD73                                                          
         CLI   SELACTN,ACTCNCL     CANCEL  REQUEST ?                            
         BE    HKBLD73                                                          
         CLI   SELACTN,ACTRES      RESTORE REQUEST ?                            
         BNE   HKBLD74                                                          
*                                                                               
HKBLD73  TM    INOPTA,INOAEXPN                                                  
         BO    HKBLD88             IGNORE THIS  WHEN OPTION ON                  
         TM    INOPTA,INOAEXPY+INOAEXPO                                         
         BZ    HKBLD88             IGNORE THESE WHEN OPTION NOT ON              
*                                                                               
HKBLD74  TM    ACLSMIND,ACLSMISK   EXTENDED TABLE?                              
         BZ    HKBLD75             NO SO OK SELECTION                           
         CLI   SELLANG,0           WHICH LANGUAGE (NONE?)                       
         BE    HKBLD75             NO LANG SO OK SELECTION                      
         CLC   SELLANG,CULANG      MATCH ON LANGUAGE CODE                       
         BNE   HKBLD88             LOOP TO NEXT                                 
*                                                                               
HKBLD75  OC    TWASAGN,TWASAGN     NEW SECURITY?                                
         BZ    HKBLD76             NO                                           
         MVC   RHALF(1),SELRECN    NEW SELECTABLE RECORD                        
         MVC   RHALF+1(1),SELACTN  NEW SELECTABLE ACTION                        
         LA    RF,RHALF                                                         
         GOTO1 VSECRET,RPARM,('SECPRACT',ACASEC),(0(RF),1(RF))                  
         BNE   HKBLD88             NO GOOD, GET NEXT                            
*                                                                               
         USING ACTTABD,R1                                                       
HKBLD76  L     R1,ACACTTAB         ACTION TABLE                                 
*                                                                               
HKBLD78  CLI   ACTTABD,EOT         END OF TABLE?                                
         BE    HKBLD88             INVALID ACTION, SO GET NEXT SELECT           
         CLC   SELACTN,ACTNUMB     MATCH ACTION                                 
         BE    HKBLD80                                                          
         LA    R1,ACTTABL(,R1)     BUMP TO NEXT ACTION                          
         B     HKBLD78                                                          
*                                                                               
HKBLD80  GOTO1 AEXPACT                                                          
         MVC   0(L'SELCODE,R4),SELCODE                                          
         MVC   1(L'SCEQUAL,R4),SCEQUAL                                          
         MVC   2(L'SCACTNAM,R4),SCACTNAM                                        
         LA    R4,L'SCACTNAM+2(,R4)          BUMP TO END                        
         LA    RE,RHLPAREA+L'RHLPAREA                                           
         CR    R4,RE                                                            
         BH    HKBLD90                                                          
         CLI   0(R4),C' '          ELIMINATE SPACES                             
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   1(L'SCCOMMA,R4),SCCOMMA                                          
         LA    R4,2(,R4)           BUMP UP TWO                                  
*                                                                               
HKBLD88  LA    R0,SELTABL          LENGTH OF TABLE NONEXTENDED                  
         TM    ACLSMIND,ACLSMISK   EXTENDED TABLE?                              
         BZ    *+8                 NO                                           
         LA    R0,SELTAB2L         LENGTH OF TABLE EXTENDED                     
         AR    R2,R0                                                            
         B     HKBLD72                                                          
*                                                                               
HKBLD90  BCTR  R4,0                                                             
         MVI   0(R4),C' '          BLANK OUT LAST COMMA                         
         L     R2,ASELHDR1                                                      
         LH    R1,SELLEN1                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RHLPAREA                                                 
         OI    6(R2),FVOXMT                                                     
*                                                                               
HKBLDPFX B     HOOKX                                                            
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  COMMON ROUTINES                                                    *         
*        NTRY - R1 = A(PARAMETER LIST)                                *         
*               RF = ROUNTINE NUMBER (HIGH ORDER BYTE)                *         
***********************************************************************         
         SPACE 1                                                                
         USING RWRKD,RC                                                         
         USING TWAD,R5                                                          
COMMON   NTR1  BASE=ACBASE1,WORK=(RC,RWRKX-RWRKD),LABEL=NO                      
         L     R5,ATWA                                                          
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         STCM  RF,8,ROUT#                                                       
         CLC   IOAREA,RIOSAVE      ALREADY SAVED?                               
         BE    *+10                YES                                          
         MVC   RIOSAVE,IOAREA      SAVE IO AREA                                 
         CLI   ROUT#,23                                                         
         BH    COMN10                                                           
         CLI   ROUT#,20                                                         
         BL    COMN10                                                           
         ST    R1,RSVR1            ADDRESS TO PARAMETER FOR ROUTINE             
         MVC   RPARM+20(2),=C'L='                                               
         MVC   RPARM+22(2),=Y(L'RFPLOAD)                                        
         GOTO1 VDMGR,RPARM,DMWRT,TEMPSTR,(4,0),ARFPBLK     SAVE IO              
         GOTO1 VDMGR,RPARM,DMREAD,TEMPSTR,(3,0),ARFPBLK    RESTORE RFP          
         L     R1,RSVR1                                                         
*                                                                               
COMN10   SR    RF,RF                                                            
         IC    RF,ROUT#                                                         
         SLL   RF,2                                                             
         B     COMMONRT(RF)                                                     
*                                                                               
COMMONRT B     R_GETNME            0  GET NAME ELEMENT X'20'                    
         B     R_GETEL             1  GET ELEMENT IN APELCODE                   
         B     R_GETPER            2  GET PERSON?                               
         B     R_GETLDG            3  GET LEDGER RECORD                         
         B     R_GETACT            4  GET ACTIVITY?                             
         B     R_ADDNME            5  ADD NAME ELEMENT X'20'                    
         B     R_ADDEL             6  ADD AN ELEMENT IN APELEM                  
         B     R_ADDID             7  ADD ID INFORMATION -ACTION CHANGE         
         B     R_FRSTEL            8  GET FIRST ELEMENT                         
         B     R_NEXTEL            9  GET NEXT  ELEMENT                         
         B     R_DELEL             10 DELETE ELEMENT                            
         B     R_VALDEF            11 VALIDATE KEYWORD                          
         B     R_VALLST            12 VALIDATE +/- LIST                         
         B     R_GETTYP            13 GET REPORT TYPE X'25'                     
         B     R_ADRPTY            14 ADD REPORT TYPE X'25'                     
         B     R_GETPID            15 Get PId code from PId#                    
         B     R_VALLDG            16 VALIDATE LEDGER                           
         B     R_TXTGET            17 SPECIAL TEXT GET                          
         B     R_VALFCR            18 VALIDATE FOREIGN CURRENCY                 
         B     R_DTSTMP            19 STAMP DATE/TIME ON REC - REQUEST          
         B     R_RFPINT            20 RFP INITIALIZATION                        
         B     R_RFPGRP            21 RFP GROUP                                 
         B     R_RFPSYM            22 RFP SYMBOL                                
         B     R_RFPADD            23 ADD RFP REQUEST                           
         B     EXIT                24 N/D                                       
         B     R_CNVTTY            25 CONVERT TRANACTION TYPE                   
         B     EXIT                26 N/D                                       
         B     R_MKHEAD            27 MAKE COLUMN HEADING                       
         B     R_VALSPL            28 SPECIAL KEYWORD VALIDATION?               
         B     EXIT                29 N/D                                       
         B     R_FMTSEC            30 FORMAT SECURITY                           
         B     EXIT                31 N/D                                       
*                                                                               
COMMONX  CLI   ROUT#,23                                                         
         BH    COMN90                                                           
         CLI   ROUT#,20                                                         
         BL    COMN90                                                           
         MVC   RPARM+20(2),=C'L='                                               
         MVC   RPARM+22(2),=Y(L'RFPLOAD)                                        
         GOTO1 VDMGR,RPARM,DMWRT,TEMPSTR,(3,0),ARFPBLK     SAVE RFP             
         GOTO1 VDMGR,RPARM,DMREAD,TEMPSTR,(4,0),ARFPBLK    RESTORE IO           
*                                                                               
COMN90   CLC   RIOSAVE,IOAREA      TEST ANY IO EXECUTED                         
         BE    *+14                                                             
         OI    APINDS,APILRERD     YES - SET APPLICATION FLAG                   
         MVC   IOAREA(L'RIOSAVE),RIOSAVE                                        
         CLC   FVMSGNO,=AL2(FVFOK) SET CONCODE FOR CALLER                       
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*        GET NAME FROM RECORD                                         *         
*        P1 = A(RECORD TO GET NAME FROM)                              *         
*        P2 = A(SCREEN FIELD HEADER)                                  *         
***********************************************************************         
         SPACE 1                                                                
R_GETNME ICM   R4,15,0(R1)                                                      
         BZ    GETNAMEN                                                         
         ICM   R2,7,5(R1)         SCREEN FIELD TO FILL IN                       
         BZ    GETNAMEN                                                         
*                                                                               
         USING NAMELD,R4                                                        
         AH    R4,DATADISP                                                      
         SR    R3,R3                                                            
*                                                                               
GETNAME1 CLI   0(R4),0             END OF RECORD?                               
         BE    GETNAMEN                                                         
         CLI   0(R4),NAMELQ        IS IT A NAME ELEMENT                         
         BE    GETNAME2                                                         
         IC    R3,1(,R4)           LENGTH OF ELEMENT                            
         AR    R4,R3               BUMP TO NEXT ELEMENT                         
         B     GETNAME1                                                         
*                                                                               
GETNAME2 ICM   R3,1,4(R1)          GET FIELD SIZE IF NOT HEADER                 
         BNZ   GETNAME4            NOT A SCREEN FIELD                           
         IC    R3,0(R2)            LENGTH OF HEADER + MAX DATA                  
         SH    R3,=H'8'            FIELD HEADER LENGTH W/O EXTENSION            
         TM    1(R2),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SH    R3,=H'8'            FIELD HEADER LENGTH WITH EXTENSION           
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         LA    R2,8(,R2)           BUMP FOR FIELD HEADER                        
*                                                                               
GETNAME4 BCTR  R3,0                SUBTRACT ONE FOR EXECUTE                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,NAMLN                                                         
         SH    R0,=Y(NAMLN1Q+1)    LENGTH OF NAME IN ELEMENT                    
         BM    GETNAMEN            BAD NAME ELEMENT                             
         CH    R3,=Y(L'NAMEREC)                                                 
         BNL   *+8                                                              
         LA    R3,L'NAMEREC-1      MAX LENGTH OF NAME FIELD                     
         CR    R0,R3               NAME LENGTH VS. FIELD LENGTH                 
         BH    *+6                                                              
         LR    R3,R0               USE SMALLER LENGTH OF THE TWO                
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NAMEREC                                                  
         SR    RE,RE                                                            
         B     GETNAMEX                                                         
*                                                                               
GETNAMEN LTR   RE,RE                                                            
*                                                                               
GETNAMEX B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        ADD NAME TO RECORD                                           *         
*        P1 = A(RECORD TO ADD NAME TO)                                *         
*        P2 = A(SCREEN FIELD HEADER)                                  *         
***********************************************************************         
         SPACE 1                                                                
R_ADDNME ICM   R4,15,0(R1)                                                      
         BZ    ADDNAMEX                                                         
         ICM   R2,15,4(R1)         SCREEN FIELD TO RETRIEVE FROM                
         BZ    ADDNAMEX                                                         
*                                                                               
         XC    RELEMENT,RELEMENT                                                
         MVC   RELEMENT,APELEM    SAVE APPLICATION ELEMENT                      
         XC    APELEM,APELEM                                                    
         MVI   APELCODE,NAMELQ                                                  
         GOTO1 LDELEL,(R4)                                                      
         SR    R3,R3                                                            
         ICM   R3,1,5(R2)          INPUT LENGTH                                 
         BZ    ADDNAMEX            NO NAME ADDED                                
         CH    R3,=Y(L'NAMEREC)    MAX LENGTH CHECK                             
         BNH   *+8                                                              
         LA    R3,L'NAMEREC        MAX LENGTH OF NAME FIELD                     
*                                                                               
         USING NAMELD,R6                                                        
         LA    R6,APELEM                                                        
         LA    R3,2(,R3)           ELEMENT LENGTH                               
         MVI   NAMEL,NAMELQ        X'20' ELEMENT                                
         STC   R3,NAMLN            LENGTH OF ELEMENT                            
         SH    R3,=Y(NAMLN1Q+1)    LENGTH OF NAME IN ELEMENT                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),8(R2)                                                 
         GOTO1 LADDEL,(R4)                                                      
         MVC   APELEM,RELEMENT     RESTORE APPLICATION ELEMENT                  
*                                                                               
ADDNAMEX B     COMMONX                                                          
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*     GET ID FROM RECORD                                              *         
*        P1(1)     0 IF "R" Then get last requested date instead      *         
*          (3)     1-3  A(Record to get activity date from)           *         
*        P2(1)     0    Length of field or zero if header             *         
*          (3)     1-3  Area or  A(Screen field header)               *         
*     RETURN                                                          *         
*        CC SET                                                       *         
*        P2(1)     0 IF OK, 1 IF NOT                                  *         
*        P2(3)     1-3 PACKED DATE YYMMD                              *         
***********************************************************************         
         SPACE 1                                                                
R_GETACT ST    R1,RSVR1                                                         
         ICM   R4,15,0(R1)                                                      
         BZ    GETACTN                                                          
         ICM   R2,7,5(R1)          SCREEN FIELD TO FILL IN                      
         BZ    GETACTN                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,4(R1)          GET FIELD SIZE IF NOT A HEADER               
         BNZ   GETACT2             NOT A SCREEN FIELD                           
         IC    RF,0(,R2)           LENGTH OF HEADER + MAX DATA                  
         SH    RF,=H'8'            FIELD HEADER LENGTH W/O EXTENSION            
         BNP   GETACTN                                                          
         TM    1(R2),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+12                                                             
         SHI   RF,8                FIELD HEADER LENGTH WITH EXTENSION           
         BNP   GETACTN                                                          
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         LA    R2,8(,R2)           BUMP UP FOR FIELD HEADER                     
*                                                                               
GETACT2  BCTR  RF,0                SUBTRACT ONE FOR EXECUTE                     
         EXMVC RF,0(R2),SPACES                                                  
                                                                                
         AH    R4,DATADISP                                                      
GETACT4  CLI   0(R4),EOR           End of record?                               
         BE    GETACTN                                                          
         CLI   0(R4),PACELQ        Personal activity                            
         BE    GETACT5                                                          
         CLI   0(R4),DTSELQ        Format or request details                    
         BE    GETACT6                                                          
GETACT4A IC    RF,1(,R4)           Length of element                            
         AR    R4,RF               Bump to next element                         
         B     GETACT4                                                          
*                                                                               
         USING PACELD,R4                                                        
GETACT5  CLI   0(R1),C'R'          Request date or last changed ?               
         BE    GETACT4A            Request date, don't want this now            
         OC    PACDATE,PACDATE                                                  
         BZ    GETACTN                                                          
         LA    R3,PACDATE                                                       
         ICM   R3,8,=AL1(1)        DATECON TYPE YYMMDD (EBCIDC)                 
         B     GETACT8                                                          
*                                                                               
         USING DTSELD,R4                                                        
GETACT6  OC    DTSDATE,DTSDATE                                                  
         BZ    GETACTN                                                          
         MVI   RBYTE,DTSTREQ       Last requested date                          
         CLI   0(R1),C'R'          Request date or last changed ?               
         BE    *+8                                                              
         MVI   RBYTE,DTSTSCR       Format last changed date                     
         CLC   DTSTYPE,RBYTE       Right type ?                                 
         BNE   GETACT4A            No, get next element                         
         LA    R3,DTSDATE                                                       
         ICM   R3,8,=AL1(2)        DATECON TYPE COMPRESSED                      
*                                                                               
GETACT8  GOTO1 VDATCON,RPARM,(R3),(8,(R2))                                      
         L     R2,RSVR1                                                         
         LA    R2,5(,R2)           P2+1                                         
         GOTO1 VDATCON,RPARM,,(1,(R2))                                          
         MVI   4(R2),0                                                          
         B     GETACTX                                                          
*                                                                               
GETACTN  L     R1,RSVR1            RELOAD A(PARAMETER LIST)                     
         MVI   4(R1),1             SET TO NO GOOD                               
*                                                                               
GETACTX  L     R1,RSVR1                                                         
         CLI   4(R1),0                                                          
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        GET ID FROM RECORD                                           *         
*        P1 = A(RECORD TO GET ID FROM)                                *         
*        P2 = A(SCREEN FIELD HEADER)                                  *         
***********************************************************************         
         SPACE 1                                                                
R_GETPER ICM   R4,15,0(R1)                                                      
         BZ    GETPERN                                                          
         ICM   R2,7,5(R1)          SCREEN FIELD TO FILL IN                      
         BZ    GETPERN                                                          
*                                                                               
         ICM   R3,1,4(R1)          Get field size if not a header               
         BNZ   GETPER10            Not a screen field                           
         IC    R3,0(,R2)           Lenght of header + max data                  
         SHI   R3,8                Field header lenght w/o extention            
         TM    1(R2),X'02'         Extened field header ?                       
         BZ    *+8                 No                                           
         SHI   R3,8                Yes so less length of extention              
         OI    6(R2),X'80'         Transmit field                               
         AHI   R2,8                Bump to data portion of field                
                                                                                
GETPER10 SHI   R3,1                                                             
         BM    GETPERN             No length supplied                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
                                                                                
         AH    R4,DATADISP                                                      
         SR    RF,RF                                                            
GETPER12 CLI   0(R4),EOR           End of record?                               
         BE    GETPERN                                                          
         CLI   0(R4),PACELQ        Personal activity element                    
         BE    GETPER20                                                         
         CLI   0(R4),DTSELQ        Date/Time stamp of format                    
         BE    GETPER30                                                         
GETPER18 IC    RF,1(,R4)           Length of element                            
         AR    R4,RF               Bump to next element                         
         B     GETPER12                                                         
*                                                                               
         USING PACELD,R4                                                        
GETPER20 LA    RE,PACPERS                                                       
         B     GETPERY                                                          
*                                                                               
         USING DTSELD,R4                                                        
GETPER30 CLI   DTSTYPE,DTSTSCR     Last changed                                 
         BNE   GETPER18            No try again                                 
         GOTOR GETPID,RPARM,DTSPID#,WORK                                        
         LA    RE,WORK                                                          
                                                                                
GETPERY  CHI   R3,L'PACPERS-1      Max length less 1 for EX                     
         BNH   *+8                 R3 is smaller than L'PACPERS                 
         LA    R3,L'PACPERS-1      Max size                                     
         EXMVC R3,0(R2),0(RE)                                                   
         SR    RE,RE                                                            
                                                                                
GETPERN  LTR   RE,RE                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        Get PId Code from PId#                                       *         
*        P1 = A(PId#)                                                 *         
*        P2 = A(Return area) Minimum lenght is 8                      *         
***********************************************************************         
         USING SA0REC,R2                                                        
R_GETPID L     R3,0(,R1)               A(pId #)                                 
         L     R4,4(,R1)               A(return area)                           
         MVC   0(8,R4),SPACES                                                   
         OC    0(L'SA0KNUM,R3),0(R3)   Is the value zero ?                      
         BNZ   GETPID10                                                         
         MVC   0(3,R4),=CL3'DDS'   DDS user                                     
         B     GETPIDX                                                          
                                                                                
GETPID10 MVC   RIOSVKEY,IOKEY                                                   
         LA    R2,IOKEY                                                         
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ    C'0'                                         
         MVC   SA0KAGY,CUAGYSEC                                                 
         MVC   SA0KNUM,0(R3)                                                    
         GOTO1 VIO,IO3+IORD+IOCTFILE                                            
         BNE   GETPID40                                                         
                                                                                
         USING SAPALD,RE                                                        
GETPID20 L     R2,AIOAREA3                                                      
         LA    RE,SA0DATA          First element                                
GETPID22 CLI   0(RE),EOR           End of record ?                              
         BE    GETPID40            Couldn't find pId code                       
         CLI   0(RE),SAPALELQ      X'C3' PERSON ID                              
         BE    GETPID25                                                         
         SR    RF,RF                                                            
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     GETPID22                                                         
                                                                                
GETPID25 MVC   0(8,R4),SAPALPID                                                 
         B     GETPID90                                                         
         DROP  RE                                                               
                                                                                
GETPID40 MVC   0(8,R4),=CL8'*UNKNOWN'                                           
                                                                                
GETPID90 MVC   IOKEY,RIOSVKEY                                                   
                                                                                
GETPIDX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*        RE-EVALUATE TYPE AND ADJUST APREP VALUES                     *         
*        P1 = A(AIOAREA OF FORMAT RECORD)                             *         
***********************************************************************         
         SPACE 1                                                                
R_GETTYP LTR   R2,R1               IO AREA WITH FORAMT RECORD                   
         BZ    GETTYP99            NONE                                         
         CLI   0(R2),X'2D'                                                      
         BNE   GETTYPEX            NO GOOD                                      
         MVI   FORMTYPE,SCRIBE                                                  
         CLI   1(R2),RESKSUBQ      SCRIBE FORMATS?                              
         BE    GETTYP08                                                         
         MVI   FORMTYPE,APG                                                     
         CLI   1(R2),APGKSUBQ      APG FORMATS?                                 
         BNE   GETTYPEX                                                         
*        CLI   INREC,RECAPG        MAKE SURE TYPE IS COMPATABLE                 
*        BE    *+8                                                              
*        CLI   INREC,RECRPT        MAKE SURE TYPE IS COMPATABLE                 
*        BNE   GETTYPEX                                                         
*                                                                               
GETTYP08 AH    R2,DATADISP                                                      
         MVI   APREPNUM,C'R'       DEFAULT                                      
         MVCDD APREPCDE,AC#RSRCV   DEFAULT TO RECEIVABLES                       
         GOTO1 VDICTAT,RPARM,C'SU  ',APREPCDE,0                                 
*                                                                               
         USING STYELD,R2                                                        
         SR    R1,R1                                                            
GETTYP10 CLI   0(R2),0             END OF RECORD?                               
         BE    GETTYP18                                                         
         CLI   0(R2),STYELQ        X'25'                                        
         BNE   GETTYP15            FOUND FREE FORM NUMBER                       
         MVC   APREPNUM,STYCODE                                                 
         MVC   APREPCDE,STYNAME                                                 
         MVC   APSECKYW,STYSEC#1                                                
         MVC   APSECPRF,STYSEC#5                                                
         B     GETTYP18                                                         
*                                                                               
GETTYP15 IC    R1,1(,R2)           BUMP TO NEXT                                 
         AR    R2,R1                                                            
         B     GETTYP10                                                         
         DROP  R2                                                               
*                                                                               
         USING REPTABD,R3                                                       
GETTYP18 L     R3,ACTYPTAB                                                      
*                                                                               
GETTYP20 CLI   REPCODE,EOT         END OF TABLE?                                
         BE    GETTYP99                                                         
         TM    REPFLAG,REPDDS      DDS ONLY OPTION?                             
         BZ    *+12                NO, SO OK                                    
         TM    CUSTAT,CUSDDS       YES, SO ARE WE AT DDS?                       
         BZ    GETTYP24            NO, SO GET NEXT                              
         GOTO1 EXPRPTY,(R3)                                                     
         CLC   APREPCDE,RREPCDE                                                 
         BNE   GETTYP24            NO, SO GET NEXT                              
         CLI   FORMTYPE,SCRIBE                                                  
         BNE   GETTYP23                                                         
         TM    REPFLAG,REPSJCL                                                  
         BO    GETTYPEX                                                         
*                                                                               
GETTYP23 CLC   APREPNUM,REPNUM     MATCH ON REPORT NUMBER                       
         BE    GETTYP25            FOUND MATCH                                  
*                                                                               
GETTYP24 LA    R3,REPLNQ(,R3)      BUMP TO NEXT                                 
         B     GETTYP20                                                         
*                                                                               
GETTYP25 OC    TWASAGN,TWASAGN     NEW SECURITY?                                
         BZ    GETTYP30            NO                                           
         GOTO1 VSECRET,ACPARM,('SECPFLDP',ACASEC),REPJCLID                      
         BNL   GETTYP30            WRITE/READ BIT ON                            
         MVC   FVMSGNO,=AL2(1445)                                               
*                                                                               
GETTYP30 MVC   APREPTYP,RREPTYP                                                 
         MVC   APREPIND,REPIND                                                  
         MVC   APREPCUL,REPCUL                                                  
         MVC   APREPUL,REPUL                                                    
         MVC   APREPTY#,REPTYNO                                                 
         MVC   APREPJCL,REPJCLID   ID TO IDENTIFY JCL TO USE                    
         MVC   APREPLD#,REPLDNO    ID FOR $REQ (LANDSCAPE VERSION)              
         MVC   APREPPT#,REPPTNO    ID FOR $REQ (PORTRIAT  VERSION)              
         MVI   APMLDGR,NO                                                       
         TM    REPFLAG,REPMLDGR                                                 
         BZ    *+8                                                              
         MVI   APMLDGR,YES                                                      
         MVI   APLDGR#,1                                                        
         XC    APLDGRS,APLDGRS                                                  
         MVC   APLDGRS(2),APREPUL  DEFAULT                                      
         CLI   APMLDGR,YES                                                      
         BNE   GETTYP40                                                         
*                                                                               
         USING RFLELD,R2                                                        
GETTYP32 CLI   0(R2),0                                                          
         BE    GETTYP40                                                         
         CLI   0(R2),RFLELQ        X'C5' FILTER ELEMENT                         
         BNE   GETTYP34                                                         
         CLI   RFLSEQ,0                                                         
         BNE   GETTYP40            PAST IT ALREADY                              
         CLI   RFLTYPE,RFLLDG      LEDGER                                       
         BE    GETTYP35                                                         
GETTYP34 SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     GETTYP32            LOOP                                         
*                                                                               
GETTYP35 IC    R1,RFLLN                                                         
         SHI   R1,RFLLNQ                                                        
         LA    R2,RFLDATA                POINT TO U/L LIST                      
         SR    RF,RF                                                            
         LA    RE,APLDGRS                                                       
*                                                                               
GETTYP36 CLM   RF,1,=AL1(L'APLDGRS/2)    MAX LEDGERS ALLOWED                    
         BNH   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   0(2,RE),0(R2)                                                    
         LA    RF,1(,RF)           COUNT NUMBER OF U/L'S                        
         LA    RE,2(,RE)           NEXT SAVE LOCATION                           
         LA    R2,3(,R2)           BUMP TO NEXT POSSIBLE U/L                    
         SHI   R1,3                                                             
         BP    GETTYP36                                                         
*                                                                               
         LA    RE,APLDGRS                                                       
         STC   RF,APLDGR#          SAVE NUMBER OF LEDGERS                       
GETTYP38 CLC   APREPUL,0(RE)       CHECK TO SEE FORMAT HAS DEFAULT U/L          
         BE    GETTYP40                                                         
         LA    RE,2(,RE)                                                        
         BCT   RF,GETTYP38                                                      
         MVC   APREPUL,APLDGRS     USE FIRST LEDGER ON FORMAT INSTEAD           
         DROP  R2                                                               
*                                                                               
GETTYP40 MVC   LASTTYNO,REPTYNO                                                 
         MVI   SOONVLUE,0                                                       
         TM    REPFLAG,REPXSOON                                                 
         BO    GETTYP50            NOT ALLOWED TO SOON                          
         MVI   SOONVLUE,1          LEVEL ONE                                    
         TM    REPFLAG,REPASOON    RESTRICTIONS ON ACCOUNT TO SOON              
         BZ    GETTYP50            YES                                          
         MVI   SOONVLUE,2          LEVEL TWO                                    
*                                                                               
GETTYP50 MVI   APGFLAG,NO                                                       
         TM    REPFLAG,REPSJCL     APG JCL TYPE                                 
         BZ    *+8                                                              
         MVI   APGFLAG,YES                                                      
         B     GETTYP99                                                         
*                                                                               
GETTYPEX MVC   FVMSGNO,=AL2(ACERPTYP)                                           
*                                                                               
GETTYP99 B     COMMONX                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*        ADD PRESONAL ACTIVITY ELEMENT                                *         
*        P1 = A(RECORD TO ADD ACTIVITY PERSON/DATE                    *         
***********************************************************************         
         SPACE 1                                                                
         USING PACELD,R6                                                        
R_ADDID  ICM   R4,15,0(R1)                                                      
         BZ    ADDIDX                                                           
*                                                                               
         XC    RELEMENT,RELEMENT                                                
         MVC   RELEMENT,APELEM     SAVE APPLICATION ELEMENT                     
         XC    APELEM,APELEM                                                    
         MVI   APELCODE,PACELQ     REMOVE CURRENT X'A1'                         
         GOTO1 LDELEL,(R4)                                                      
                                                                                
         USING DTSELD,R6                                                        
         MVI   APELCODE,DTSELQ     REMOVE CURRENT X'FB'                         
         MVI   ELEMSEQ,DTSTSCR                                                  
         GOTO1 LDELEL,(R4)                                                      
                                                                                
         LA    R6,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   DTSEL,DTSELQ        X'FB' DATE/TIME STAMP ELEMENT                
         MVI   DTSLN,DTSLN2Q                                                    
         MVI   DTSTYPE,DTSTSCR     Scribe DATE/TIME                             
         MVC   DTSDATE,ASCDAT      Compressed                                   
         GOTO1 VGETFACT,RPARM,(2,0)                                             
         L     R1,RPARM                                                         
         MVC   DTSTIME,1(R1)                                                    
         MVC   DTSPID#,CUPASS                                                   
         GOTO1 LADDEL,(R4)                                                      
*                                                                               
ADDID10  MVC   APELEM,RELEMENT     RESTORE APPLICATION ELEMENT                  
*                                                                               
ADDIDX   B     COMMONX                                                          
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*        DATE AND TIME STAMP ELEMENT - ACTION REQUEST                 *         
*        P1 = A(RECORD TO ADD DATE AND TIME)                          *         
*        P2 = ADDRESS OF OVERNIGHT REQUEST                            *         
***********************************************************************         
         SPACE 1                                                                
R_DTSTMP ICM   R4,15,0(R1)                                                      
         BZ    DTSTMPX                                                          
         L     R3,4(,R1)           2ND PARM FOR OVERNIGHT REQUESTS              
         MVI   RBYTE,C'Y'          DELETE X'FA' ELEMENTS                        
*                                                                               
         USING DTSELD,R1                                                        
         XC    RELEMENT,RELEMENT                                                
         MVC   RELEMENT,APELEM     SAVE APPLICATION ELEMENT                     
         XC    APELEM,APELEM                                                    
         MVI   APELCODE,DTSELQ     REMOVE OLD ELEMENTS X'FB'                    
         LR    R1,R4                                                            
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
DTSTMP05 CLI   0(R1),EOR           End of record ?                              
         BE    DTSTMP10            Not found                                    
         CLI   DTSEL,DTSELQ        X'FB'                                        
         BNE   *+8                                                              
         CLI   DTSTYPE,DTSTREQ     Request type ?                               
         BE    DTSTMP08            Yes                                          
         IC    RF,DTSLN                                                         
         AR    R1,RF                                                            
         B     DTSTMP05                                                         
*                                                                               
DTSTMP08 CLC   DTSDATE,ASCDAT      DID WE DO ONE TODAY?                         
         BNE   *+8                                                              
         MVI   RBYTE,C'N'                                                       
         MVI   ELEMSEQ,DTSTREQ                                                  
         GOTO1 LDELEL,(R4)         DELETE OLD ONES ONLY                         
         DROP  R1                                                               
*                                                                               
         USING DTSELD,R6                                                        
DTSTMP10 LA    R6,APELEM                                                        
         MVI   DTSEL,DTSELQ        X'FB' DATE/TIME STAMP ELEMENT                
         MVI   DTSLN,DTSLN2Q                                                    
         MVI   DTSTYPE,DTSTREQ     REQUESTED DATE/TIME                          
         MVC   DTSDATE,ASCDAT      COMPRESSED                                   
         GOTO1 VGETFACT,RPARM,(2,0)                                             
         L     R1,RPARM                                                         
         MVC   DTSTIME,1(R1)                                                    
         MVC   DTSPID#,CUPASS                                                   
         GOTO1 LADDEL,(R4)                                                      
*                                  ALL ADDEL ERRORS WILL SHOW UP WHEN           
*                                  WE  DO THE ADDEL FOR THE X'FA' EL            
*                                                                               
         USING PTRELD,R6                                                        
         XC    APELEM,APELEM                                                    
         CLI   RBYTE,C'N'          DELETE OLD X'FA' ELEMENTS?                   
         BE    DTSTMP15            NO, BUILD ELEMENT                            
         MVI   APELCODE,PTRELQ     REMOVE OLD ELEMENTS X'FA'                    
         GOTO1 LDELEL,(R4)                                                      
*                                                                               
DTSTMP15 LTR   R3,R3               ARE WE DOING OVERNIGHT REQUEST?              
         BZ    DTSTMP90            NO, DONE                                     
         CLI   RBYTE,C'N'          DELETED X'FA' ELEMENTS?                      
         BNE   DTSTMP30            NO, KEEP AND APPEND                          
*                                                                               
DTSTMP20 MVI   APELCODE,PTRELQ                                                  
         SR    RF,RF                                                            
         LR    R1,R4                                                            
         AH    R1,DATADISP                                                      
*                                                                               
DTSTMP22 CLI   0(R1),0             END OF RECORD ?                              
         BE    DTSTMP30            YES                                          
         CLI   0(R1),PTRELQ        X'FA' ELEMENTS                               
         BE    DTSTMP25                                                         
DTSTMP23 IC    RF,1(,R1)           GET LENGTH                                   
         AR    R1,RF                                                            
         B     DTSTMP22                                                         
*                                                                               
DTSTMP25 DS    0H                                                               
*MN                                                                             
         CLI   1(R1),X'FF'         IS THIS ADDRESS ELEMENT FULL                 
         BE    DTSTMP23                                                         
         IC    RF,1(,R1)           GET LENGTH                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R1)     COPY OLD ELEMENT                             
         MVI   APELCODE,X'FC'                                                   
         MVI   0(R1),X'FC'                                                      
         GOTO1 LDELEL,(R4)                                                      
         MVI   APELCODE,PTRELQ                                                  
         B     DTSTMP40                                                         
*                                                                               
DTSTMP30 MVI   PTREL,PTRELQ                                                     
         MVI   PTRTYPE,PTRTREQ     OVERNIGHT REQUEST                            
         MVI   PTRLN,PTRLN1Q                                                    
*                                                                               
DTSTMP40 LR    RE,R6               ADDRESS OF ELEMENT                           
         ZIC   RF,PTRLN                                                         
         AR    RE,RF                                                            
         CHI   RF,255                                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         STCM  R3,15,0(RE)         SAVE THE NEW ADDRESS                         
         LA    RF,4(,RF)           CALC NEW LENGTH                              
         STC   RF,PTRLN                                                         
         GOTO1 LADDEL,(R4)                                                      
*                                                                               
DTSTMP90 MVC   APELEM,RELEMENT     RESTORE APPLICATION ELEMENT                  
*                                                                               
DTSTMPX  B     COMMONX                                                          
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  INITIALIZE RFP                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFPBLK,R4                                                        
         SPACE 1                                                                
R_RFPINT L     R0,ARFPBLK          RFP AREA                                     
         LH    R1,=Y(RFPBLKLN)     LENGTH OF AREA                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,ARFPBLK          RFP AREA                                     
         MVI   RFPINIT,0           INITALIZE BLOCK                              
         MVC   RFPACOMF,ACOM       A(COMFACS)                                   
         MVC   RFPAMIN,AMINIO      A(MINIO IO BUFFER)                           
         MVC   RFPAMINR,AMINIOTB   A(MINIO RECORD TABLE)                        
         MVC   RFPMINRL,=Y(L'MINIOTAB)                                          
         MVC   RFPFUID,TWAUSRID    USER ID                                      
         MVC   RFPFAGY,TWAAGY      AGENCY                                       
         MVI   RFPFSYS,C'A'        ACCOUNTING                                   
         GOTO1 VRFP,RPARM,(R4)                                                  
         B     COMMONX                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*              VALIDATE RFP GROUP                                     *         
*              R1 = A(FIELD HEADER)                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RFPBLK,R4                                                        
R_RFPGRP L     R4,ARFPBLK                                                       
         GOTO1 AFVAL                                                            
         BE    RFPGRP10                                                         
         MVC   FVMSGNO,=AL2(1)     MISSING INPUT                                
         B     RFPGRPX                                                          
*                                                                               
RFPGRP10 MVI   RFPMODE,RFPVALGP    VALIDATE GROUP                               
         MVC   RFPFGRP,FVIFLD      GROUP NAME                                   
         OI    RFPFFLAG,RFPFSYMS   RETURN ALL SYMOBLIC EQUATES                  
         GOTO1 VRFP,RPARM,(R4)                                                  
         CLI   RFPERROR,RFPNOERR                                                
         BE    RFPGRPX                                                          
         MVC   FVMSGNO,=AL2(1815)  INVALID GROUP NAME                           
*                                                                               
RFPGRPX  B     COMMONX                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*              RFP SYMBOL VALIDATION                                  *         
*              INPUT    P1 = AL1(FIELD NUMBER VALIDATING)             *         
*                          = AL3(VALID FIELD HEADER)                  *         
*                       P2 = AL1(REQUEST CARD FIELD LENGTH)           *         
*                            AL3(REQUEST CARD AREA)                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RFPBLK,R4                                                        
R_RFPSYM L     R4,ARFPBLK                                                       
         L     R2,4(,R1)           REQUEST CARD AREA                            
         LA    R2,0(,R2)           CLEAR HOB                                    
         XC    RHALF,RHALF                                                      
         MVC   RCHAR,0(R1)         SAVE OFF FIELD NUMBER                        
         MVC   RBYTE,4(R1)         STORE LENGTH OF REQUEST CARD FIELD           
         L     R3,0(,R1)           FIELD HEADER WITH RFP INPUT                  
         LA    R3,0(,R3)           CLEAR HOB                                    
         GOTO1 AFVAL,(R3)                                                       
         BNE   RFPSYMX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       NUMBER OF SYMBOLS IN TABLE                   
         BZ    RFPSYMX             NONE SET UP                                  
         CLI   FVXLEN,L'RFPVSYMB-1                                              
         BH    RFPSYM18                                                         
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
*                                                                               
RFPSYM15 EX    RF,*+8                                                           
         BE    RFPSYM20                                                         
         CLC   RFPVSYMB(0),FVIFLD  COMPARE (LENGTH=24)                          
         LA    R4,RFPVSYML(,R4)    POINT TO AND BUMP UP                         
         BCT   R0,RFPSYM15         LOOP                                         
*                                                                               
RFPSYM18 MVC   FVMSGNO,=AL2(1814)  INVALID SYMBOL                               
         B     RFPSYMX                                                          
*                                                                               
RFPSYM20 L     RE,=A(FLDTAB)                                                    
         A     RE,ACRELO                                                        
*                                                                               
RFPSYM22 CLI   0(RE),EOT           END OF TABLE?                                
         BNE   RFPSYM24                                                         
         MVC   FVMSGNO,=AL2(1816)  SYMBOL NOT VALID FOR THIS FIELD              
         B     RFPSYMX                                                          
*                                                                               
RFPSYM24 CLC   RCHAR,0(RE)         MATCH FIELD NUMBER                           
         BE    RFPSYM30                                                         
         SR    RF,RF                                                            
         IC    RF,1(,RE)           NUMBER OF ENTRIES                            
         SLL   RF,1                MULTIPLY BY 2                                
         LA    RE,2(RF,RE)         BUMP TO NEXT ENTRY                           
         B     RFPSYM22                                                         
*                                                                               
RFPSYM30 SR    RF,RF                                                            
         IC    RF,1(,RE)           NUMBER OF ENTRIES                            
*                                                                               
RFPSYM32 LA    RE,2(,RE)           BUMP TO MSG# VALUES                          
         CLC   0(2,RE),RFPVSYME+1  MATCH MSG NUMBER                             
         BE    RFPSYM35                                                         
         BCT   RF,RFPSYM32                                                      
         MVC   FVMSGNO,=AL2(1816)  SYMBOL NOT VALID FOR THIS FIELD              
         B     RFPSYMX                                                          
*                                                                               
RFPSYM35 LA    RF,L'RFPVSYMB       FIND LENGTH OF SYSMBOL                       
         LA    RE,RFPVSYMB+L'RFPVSYMB-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(,R3)           LENGTH OF FIELD                              
         TM    1(R3),FVAXTND       EXTENDED FIELD HEADER?                       
         BZ    *+8                 NO, SO BUMP TO NEXT                          
         SH    R1,=H'08'           SUBTRACT LENGTH OF EXTEND FIELD              
         CR    RF,R1               IF RF=L'SYMBOL > R1=L'FIELD THEN             
         BL    *+6                                                              
         LR    RF,R1               USE R1=L'FIELD                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),RFPVSYMB    FILL IN FULL SYMBOL ON SCREEN                
         OI    6(R3),FVOXMT                                                     
         LTR   R2,R2                                                            
         BZ    RFPSYMX             DON'T SET REQUEST CARD                       
         MVC   0(4,R2),RFPVSYME    PUT ESC SEQUENCE IN CARD                     
         MVC   3(1,R2),RBYTE       OVER RIDE LENGTH IN CARD                     
*                                                                               
RFPSYMX  B     COMMONX                                                          
         EJECT ,                                                                
***********************************************************************         
*              ADD A RPF REQUEST                                      *         
*              P1 = REQUEST CARD AREA                                 *         
*              P2 = REQUEST HEADER IN REQUEST CARD AREA               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACQD,R6                                                          
         USING RFPBLK,R4                                                        
R_RFPADD L     R4,ARFPBLK          RFP AREA                                     
         L     R2,0(,R1)           REQUEST RECORD                               
         L     R3,4(,R1)           REQUEST HDR IN REQUEST RECORD                
         LA    R6,L'RFPVREQC(,R2)  POINT TO FIRST DATA CARD                     
         MVC   RFPFRQID,ACQPROG    REQUEST #                                    
         ZIC   R1,15(,R3)          REQUEST HDR+15                               
         SRL   R1,4                GET NUMBER OF REQUEST CARDS                  
         LA    R1,2(,R1)           ADD ONE FOR HEADER + ONE FOR BEFORE          
         STC   R1,RFPFNUMR                                                      
         MVI   RFPMODE,RFPADDRQ    ADD REQUEST                                  
         LR    R0,R2               R0 = REQUEST CARD AREA                       
         MH    R1,=Y(L'RFPVREQC)   NUMBER OF 80 BYTE CARDS TO MOVE              
         LR    RF,R1                                                            
         LA    RE,RFPVREQH         RFP REQUEST DATA AREA                        
         MVCL  RE,R0               MOVE IN REQUEST DATA FOR RFP                 
         GOTO1 VRFP,RPARM,(R4)                                                  
         CLI   RFPERROR,RFPNOERR                                                
         BE    RFPADD10                                                         
         MVC   FVMSGNO,=AL2(1424)  INVALID REQUEST                              
         B     RFPADDX                                                          
*                                                                               
RFPADD10 MVC   FVADDR,AACTHDR                                                   
         MVC   FVMSGNO,=AL2(2068)  ADD REQUEST TO &T                            
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'RFPFGRP),RFPFGRP                                        
*        MVC   FVXTRA+L'RFPFGRP+1(1),SCCOMMA        PUT A COMMA                 
*        CURED (B1,RFPFSEQN),(3,FVXTRA+L'RFPFGRP+2),0,DMCB=RPARM                
*        GOTO1 VSQUASH,RPARM,FVXTRA,(0,L'FVXTRA)                                
         SR    R1,R1                                                            
         B     EXIT                DO NOT SET CC BASED ON FVMSGNO               
*                                                                               
RFPADDX  B     COMMONX                                                          
         DROP  R4,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*              ADD REPORT TYPE ELEMENT                                *         
*              R1 = A(RECORD TO ADD REPORT TYPE TO                    *         
***********************************************************************         
         SPACE 1                                                                
         USING STYELD,R6                                                        
R_ADRPTY ICM   R4,15,0(R1)                                                      
         BZ    ADDRPTYX                                                         
         LR    R6,R4                                                            
         AH    R6,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
ADDRPT05 CLI   0(R6),0             END OF RECORD                                
         BE    ADDRPT20                                                         
         CLI   0(R6),STYELQ        X'25' GET ELEMENT                            
         BE    ADDRPT10                                                         
         IC    RF,1(,R6)                                                        
         AR    R6,RF                                                            
         B     ADDRPT05                                                         
*                                                                               
ADDRPT10 MVC   STYCODE,APREPNUM                                                 
         MVC   STYNAME,APREPCDE                                                 
         TM    STYSTAT,STYSSTO     WAS THIS AMENDED IN STEREO SCRIBE            
         BZ    ADDRPTYX            NO, SO DON'T BOTHER WITH IT                  
         TM    GENIND,GENSTRO      ARE WE IN FULL STEREO MODE ?                 
         BO    ADDRPTYX                   YES                                   
         NI    STYSTAT,TURNOFF-STYSSTO    TURN OFF STEREO AMENDED BIT           
         B     ADDRPTYX                                                         
*                                                                               
ADDRPT20 XC    RELEMENT,RELEMENT                                                
         MVC   RELEMENT,APELEM     SAVE APPLICATION ELEMENT                     
         XC    APELEM,APELEM                                                    
         LA    R6,APELEM                                                        
         MVI   STYEL,STYELQ        ADD FREE FORM ELEMENT X'25'                  
         MVI   STYLN,STYLNQ2       SIZE OF ELEMENT                              
         MVC   STYCODE,APREPNUM                                                 
         MVC   STYNAME,APREPCDE                                                 
         GOTO1 LADDEL,(R4)                                                      
         MVC   APELEM,RELEMENT     RESTORE APPLICATION ELEMENT                  
*                                                                               
ADDRPTYX B     COMMONX                                                          
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*        GET LEDGER LEVEL LENGTHS AND NAMES                           *         
* NTRY   R1 = A(TWO BYTE UNIT LEDGER VARIABLE)                        *         
* EXIT   ACLEDGER, ACLEV1, ACLEVNM1, ETC. HAVE LEDGER INFO            *         
*        CC = NO IF LEDGER NOT FOUND                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGRECD,R2                                                       
LGETLDGR NTR1                                                                   
         MVI   RLOCAL,YES                                                       
         B     *+8                                                              
*                                                                               
R_GETLDG MVI   RLOCAL,NO                                                        
         LTR   R1,R1                                                            
         BZ    GETLDGN             INVALID CALL                                 
         CLC   ACLEDGER,0(R1)      DO WE ALREADY HAVE LEDGER                    
         BE    GETLDGX             YES, SO EXIT                                 
         MVC   RIOSVKEY,IOKEY                                                   
         LA    R2,IOKEY                                                         
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(2),0(R1)                                                 
         GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   GETLDGN                       NO LEDGER FOUND                    
         XC    ACLEDGER(ACLDGLEN),ACLEDGER   CLEAR LEDGER RECORD INFO           
         XC    ACLE#TRX,ACLE#TRX                                                
         MVC   ACLEDGER,LDGKUNT                                                 
         MVC   ACULNAME,SPACES                                                  
         XC    AEQULVL,AEQULVL                                                  
         L     R2,AIOAREA3                                                      
         AH    R2,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
GETLDG05 CLI   0(R2),0             END OF RECORD?                               
         BE    GETLDGX             FINISHED                                     
         CLI   0(R2),ACLELQ        X'16' ACCOUNT LEVEL ELEMENT?                 
         BE    GETLDG10                                                         
         CLI   0(R2),NAMELQ        X'20' LEDGER NAME?                           
         BE    GETLDG20                                                         
         CLI   0(R2),RSTELQ        X'30' STATUS ELEMENT                         
         BE    GETLDG30                                                         
         CLI   0(R2),APRELQ        X'CA' ACCOUNT EQUIVALENT ELEMENT             
         BE    GETLDG40                                                         
         CLI   0(R2),NUMELQ        X'21' TRANSACTION COUNT ELEMENT              
         BE    GETLDG50                                                         
*                                                                               
GETLDG08 IC    RF,1(,R2)                                                        
         AR    R2,RF                                                            
         B     GETLDG05                                                         
*                                                                               
         USING ACLELD,R2                                                        
GETLDG10 IC    RF,ACLLN                                                         
         SH    RF,=Y(ACLLN1Q+1)                                                 
         BM    GETLDG08            NO INFO SO LOOP                              
         EXMVC RF,ACLEV1,ACLVLEN                                                
         B     GETLDG08                                                         
*                                                                               
         USING NAMELD,R2                                                        
GETLDG20 IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         BM    GETLDG08            NO INFO SO LOOP                              
         EXMVC RF,ACULNAME,NAMEREC                                              
         B     GETLDG08                                                         
*                                                                               
         USING RSTELD,R2                                                        
GETLDG30 MVC   ACLEDSEC,RSTSECY    SECURITY NUMBER                              
         B     GETLDG08                                                         
*                                                                               
         USING APRELD,R2                                                        
GETLDG40 DS    0H                                                               
         SR    RE,RE                                                            
         IC    RE,APRSEQ                                                        
         MHI   RE,AEQUGRPA                                                      
         LA    RE,AEQULVL(RE)                                                   
         MVC   0(3,RE),APRSEQ                                                   
         B     GETLDG08                                                         
*                                                                               
         USING NUMELD,R2                                                        
GETLDG50 CLI   NUMLN,NUMLN2Q                                                    
         BNE   GETLDG08                                                         
         CLI   NUMTYPE,NUMTYLEQ                                                 
         BNE   GETLDG08                                                         
         MVC   ACLE#TRX,NUM#TRX   N'TRANSACTIONS ON LEDGER                      
         B     GETLDG08                                                         
*                                                                               
GETLDGN  MVC   FVMSGNO,=AL2(ACELEDG)           INVALID LEDGER                   
*                                                                               
GETLDGX  MVC   IOKEY,RIOSVKEY                                                   
         CLI   RLOCAL,NO          LOCAL CALL                                    
         BE    COMMONX                                                          
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT  ,                                                               
***********************************************************************         
*        VALIDATE LEDGER AGAINST REPORT TYPE                          *         
* NTRY   R1 = A(TWO BYTE UNIT LEDGER VARIABLE)                        *         
* EXIT   ACLEDGER, ACLEV1, ACLEVNM1, ETC. HAVE LEDGER INFO            *         
*        CC = NO IF LEDGER NOT VALID                                  *         
***********************************************************************         
         SPACE 1                                                                
R_VALLDG LTR   R1,R1                                                            
         BZ    VALLDGN             INVALID CALL                                 
         GOTO1 LGETLDGR                                                         
         BNE   VALLDGN                                                          
*                                                                               
         USING REPTABD,R3                                                       
         L     R3,ACTYPTAB         ADDRESS OF TYPE TABLE IN 01 PHASE            
VALLDG10 CLI   0(R3),EOT           END OF TABLE                                 
         BE    VALLDGN             NO NOT VALID                                 
         TM    REPFLAG,REPSJCL                                                  
         BO    VALLDG15                                                         
         CLC   ACLEDGER,REPUL      COMPARE LEDGERS                              
         BNE   VALLDG20                                                         
*                                                                               
VALLDG15 CLC   APREPJCL,REPJCLID   VALID RUN FOR JOB?                           
         BE    VALLDGX                                                          
*                                                                               
VALLDG20 LA    R3,REPLNQ(,R3)      LOOP TO NEXT REPORT                          
         B     VALLDG10                                                         
*                                                                               
VALLDGN  MVC   FVMSGNO,=AL2(ACELEDG)           INVALID LEDGER                   
*                                                                               
VALLDGX  B     COMMONX                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*        VALIDATE CURRENCY CODE                                       *         
* NTRY   R1 = A(THREE BYTE CODE VARIABLE)                             *         
*        CC = NO IF NOT VALID                                         *         
***********************************************************************         
         SPACE 1                                                                
R_VALFCR MVC   RIOSVKEY,IOKEY                                                   
         LTR   R1,R1                                                            
         BZ    VALFCURX            INVALID CALL                                 
         LR    RF,R1                                                            
         GOTO1 VBLDCUR,RPARM,(RF),(0,RFCURY),ACOM                               
         CLI   RPARM,0                                                          
         BE    VALFCURX                                                         
         MVC   FVMSGNO,=AL2(1403)                                               
*                                                                               
VALFCURX MVC   IOKEY,RIOSVKEY                                                   
         B     COMMONX                                                          
         EJECT ,                                                                
***********************************************************************         
*  ADD AN ELEMENT TO RECORD, R1 = A(RECORD TO ADD ELEMENT TO)         *         
***********************************************************************         
         SPACE 1                                                                
LADDEL   NTR1                                                                   
         MVI   RLOCAL,YES                                                       
         B     *+8                                                              
*                                                                               
         USING RESRECD,R4                                                       
R_ADDEL  MVI   RLOCAL,NO                                                        
         LR    R4,R1                                                            
         LA    R6,=CL8'ACCBIG'     2K RECORD TYPES                              
         MVI   R4KREC,NO                                                        
         CLC   =X'2D02',RESKTYP    SCRIBE RECORD ?                              
         BNE   ADDEL10             NO                                           
         CLI   RESKSEQ,RESKSREG    X'40' REGULAR FORMAT                         
         BNE   ADDEL10                                                          
         MVI   R4KREC,YES                                                       
         LA    R6,=CL8'ACCVBIG'    4K SIMULATED RECORD TYPES                    
*                                                                               
ADDEL10  GOTO1 VHELLO,RPARM,(C'P',(R6)),(R4),APELEM,0                           
         CLI   RPARM+12,0          ELEMENT ADD OK SO FAR                        
         BE    ADDEL12                                                          
         MVC   FVMSGNO,=AL2(58)    ELEMENT NOT ON FILE                          
         CLI   RPARM+12,X'05'      RECORD TOO BIG                               
         BNE   ADDELX              OTHER ERROR                                  
         MVC   FVMSGNO,=AL2(ACREC2BG)                                           
         B     ADDELX                                                           
*                                                                               
ADDEL12  CLI   R4KREC,YES          4K SIMIULATED RECORD TYPE ?                  
         BNE   ADDELX              NO                                           
         MVI   R4KREC,NO               RESET TO NO FOR SAFETY                   
         CLC   RESRLEN,=AL2(MAXRECQ)   IS 4K TYPE OR 2K ?                       
         BNH   ADDELX                  DON'T CARE, UNDER 2K                     
         LH    R2,RESRLEN          GET RECORD LENGTH                            
         LR    R6,R4               LOAD START OF RECORD                         
         AH    R6,DATADISP         BUMP TO ELEMENTS                             
         LA    R1,ACCORFST+1       R1 = TOTAL LEN OF RECORD                     
         DROP  R4                                                               
*                                  MAKE SURE WE CAN SPLIT RECORD IN TWO         
         SR    RF,RF                                                            
         MVC   FVMSGNO,=AL2(ACREC2BG)  SET POTENTIAL ERROR                      
ADDEL20  IC    RF,1(,R6)               GET LENGTH OF ELEMENT                    
         AR    R1,RF               ADD UNTIL REACH SIZE OF FIRST REC            
         SR    R2,RF               DECREASE TOTAL LENGTH BY LEN OF ELEM         
         AR    R6,RF               BUMP UP IN RECORD                            
         CLM   R1,3,=AL2(MAXRECQ)  HAVE WE REACH THE LIMIT ?                    
         BL    ADDEL20             NO, KEEP GOING UNTIL MAX OUT                 
         BE    *+6                                                              
         AR    R2,RF               BACK OUT LAST SR INTSR.                      
*                                                                               
         CLM   R2,3,=AL2(MAXRECQ)  WOULD 2ND RECORD BE OK ?                     
         BH    ADDELX              STILL TOO BIG                                
         MVC   FVMSGNO,=AL2(FVFOK) RESET TO OK, RECORD IS SPLITABLE             
*                                                                               
ADDELX   CLI   RLOCAL,NO           LOCAL CALL ?                                 
         BE    COMMONX             NO                                           
         CLC   FVMSGNO,=AL2(FVFOK) SET CONCODE FOR CALLER                       
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*        GET AN ELEMENT FROM RECORD                                   *         
* NTRY - R1 = A(RECORD TO FIND ELEMENT)                               *         
*        ELEMSEQ = 0-256 FIND ELEMENT WITH SPECIFIC SEQUENCE NUMBER   *         
* EXIT - R1 = A(ELEMENT FOUND)                                        *         
*        CC = SET                                                     *         
***********************************************************************         
         SPACE 1                                                                
R_GETEL  AH    R1,DATADISP                                                      
*                                                                               
R_FRSTEL CLI   0(R1),0             END OF RECORD?                               
         BNE   *+12                                                             
         CLI   0(R1),1             SET CONDITION CODE FLAG                      
         B     ELEXIT                                                           
         CLI   APELCODE,0          INVALID ELEMENT CODE                         
         BE    ELEXIT                                                           
         CLC   APELCODE,0(R1)                                                   
         BNE   R_NEXTEL            NO GET NEXT ELEMENT                          
         CLI   ELEMSEQ,0           DO WE CHECK SEQUENCE NUMBER?                 
         BE    *+14                NO, SO FOUND                                 
         CLC   ELEMSEQ,2(R1)       DOES SEQUENCE NUMBER MATCH?                  
         BNE   R_NEXTEL            NO GET NEXT ELEMENT                          
         MVI   ELEMSEQ,0           CLEAR OUT SEQUENCE                           
         B     EXITR1              YES SO FOUND                                 
*                                                                               
R_NEXTEL SR    RF,RF                                                            
         ICM   RF,1,1(R1)          GET ELEMENT LENGTH                           
         BNZ   *+12                                                             
         CLI   1(R1),1             SET CONDITION CODE FLAG                      
         B     EXIT                                                             
         AR    R1,RF               BUMP TO NEXT ELEMENT                         
         B     R_FRSTEL                                                         
ELEXIT   MVI   ELEMSEQ,0           CLEAR OUT SEQUENCE                           
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*        DEL AN ELEMENT FROM RECORD                                   *         
*        APELCODE HAS ELEMENT CODE TO DELETE                          *         
*        ELEMSEQ HAS SPECIFIC SEQUENCE NUMBER TO DELETE               *         
*        R1 = AL4(RECORD TO DELETE ALL ELEMENT)                       *         
*        ELEMSEQ = 0                                                  *         
*                        <OR>                                         *         
*        R1 = AL4(RECORD TO DELETE AN ELEMENT)                        *         
*        ELEMSEQ = 1-256 DELETE ELEMENT WITH SEQUENCE NUMBER          *         
***********************************************************************         
         SPACE 1                                                                
LDELEL   NTR1                                                                   
         MVI   RLOCAL,YES                                                       
         B     *+8                                                              
*                                                                               
R_DELEL  MVI   RLOCAL,NO           SAVE ADDRESS OF RECORD                       
         LR    R4,R1               SAVE ADDRESS OF RECORD                       
         AH    R1,DATADISP                                                      
         SR    R5,R5                                                            
*                                                                               
DELEL10  CLI   0(R1),0             END OF RECORD?                               
         BE    DELEL20                                                          
         CLC   APELCODE,0(R1)                                                   
         BNE   DELEL15                                                          
         CLI   ELEMSEQ,0           DO WE COMPARE SEQUENCE NUMBER?               
         BE    DELEL14             NO                                           
         CLC   ELEMSEQ,2(R1)                                                    
         BNE   DELEL15                                                          
*                                                                               
DELEL14  MVI   0(R1),X'FF'                                                      
*                                                                               
DELEL15  IC    R5,1(,R1)           BUMP TO NEXT ELEMENT                         
         AR    R1,R5                                                            
         B     DELEL10                                                          
*                                                                               
DELEL20  LR    R6,R1               LOAD R6 WITH END OF RECORD LOCATION          
         GOTO1 VHELLO,RPARM,(C'D',=CL8'ACCVBIG'),(X'FF',(R4)),0                 
         CLI   RPARM+12,0          ELEMENT ADD OK                               
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(58)                                                 
         MVI   ELEMSEQ,0           CLEAR OUT SEQUENCE                           
         AH    R4,DATADISP                                                      
*                                                                               
DELEL30  CLI   0(R4),0             FIND END OF NEW RECORD                       
         BE    DELEL50                                                          
         IC    R5,1(,R4)           BUMP TO NEXT ELEMENT                         
         AR    R4,R5                                                            
         B     DELEL30                                                          
*                                                                               
DELEL50  SR    R6,R4               CLEAR OUT REST OF RECORD                     
         BCTR  R6,0                                                             
         EXXC  R6,0(R4),0(R4)                                                   
*                                                                               
DELELX   CLI   RLOCAL,NO           LOCAL CALL ?                                 
         BE    COMMONX             NO                                           
         CLC   FVMSGNO,=AL2(FVFOK) SET CONCODE FOR CALLER                       
         B     EXIT                RETURN FROM LOCAL CALL                       
         EJECT ,                                                                
***********************************************************************         
*        STANDARDIZE GETTEXT                                          *         
*        PARM1   AL1 TYPE (DEFAULT "S" SCREEN TYPE)                   *         
*                AL3 TEXT NUMBER                                      *         
*        PARM2   AL1 LENGTH  OF OUTPUT AREA                           *         
*                    OR ZERO IF HEADER FIELD                          *         
*                AL3 ADDRESS OF OUTPUT AREA                           *         
*        PARM3   AL1 LENGTH  OF EXTRA OUTPUT DATA                     *         
*                    OR ZERO IF NO EXTRA DATA                         *         
*                AL3 ADDRESS OF EXTRA OUTPUT DATA                     *         
***********************************************************************         
         SPACE 1                                                                
         USING GETTXTD,R2                                                       
LTXTGET  NTR1                                                                   
*                                                                               
R_TXTGET LA    R2,RPARM                                                         
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,2(R1)                                                    
         MVI   GTMTYP,GTMSCR       SCREEN TYPE (DEFAULT)                        
         CLI   0(R1),C' '                                                       
         BNH   *+10                                                             
         MVC   GTMTYP,0(R1)                                                     
         MVC   GTMSYS,ASSYSO       ACCOUNT SYSTEM                               
         MVC   GTMAXL(4),4(R1)                                                  
         CLI   8(R1),0                                                          
         BE    TXTGET05                                                         
         MVC   GTLTXT,8(R1)        LENGTH OF EXTRA OUTPUT                       
         MVC   GTATXT,9(R1)        A(OF EXTRA OUTPUT DATA)                      
*                                                                               
TXTGET05 SR    RF,RF                                                            
         IC    RF,4(,R1)                                                        
         L     RE,4(,R1)                                                        
         CLI   4(R1),0             HEADER FIELD ?                               
         BNE   TXTGET10                                                         
         IC    RF,0(,RE)                                                        
         SH    RF,=H'08'                                                        
         TM    1(RE),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SH    RF,=H'08'                                                        
         LA    RE,8(,RE)           BUMP TO ACTUAL FIELD                         
         STCM  RE,7,GTAOUT                                                      
         STC   RF,GTMAXL                                                        
*                                                                               
TXTGET10 OI    GT1INDS,GT1OWRK+GT1NOREF                                         
         SH    RF,=H'01'                                                        
         BM    TXTGET90                                                         
         EXMVC RF,0(RE),SPACES     CLEAR FIELD                                  
         GOTO1 VGETTXT,GETTXTD                                                  
         OI    APINDS,APILRERD     RE-READ RECORD ON RETURN                     
*                                                                               
TXTGET90 B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE FOR A POSSIBLE +/- LIST                                   *         
*                                                                     *         
*  ON INPUT:                                                          *         
*     PARM LIST:                                                      *         
*        P1  BYTE  0   = NUMBER OF LEDGERS PASSED -                   *         
*                          IF THIS FIELD IS ZERO, THEN 1 IS ASSUMED   *         
*        P1  BYTES 1-3 = ADDRESS OF SPECIFIC LEDGER(S) REQUESTED -    *         
*                          IF THE ADDRESS POINTS TO C'  ' THEN USE    *         
*                             THIS MODULE'S CONTRA ACCOUNT TABLE      *         
*                          THE FORMAT OF A LEDGER LIST IS:            *         
*                             ULULUL... (A PACKED LIST W/O COMMAS)    *         
*        P2  BYTE  0   = LENGTH  OF POSSIBLE LIST NAME                *         
*        P2  BYTES 1-3 = ADDRESS OF POSSIBLE LIST NAME                *         
*        P3  BYTE  3   = 0       - VALIDATE FOR ACCOUNT LIST          *         
*                      = 1       - VALIDATE FOR ACCOUNT OR LEDGER LIST*         
*                      = 255     - DO NOT VALIDATE THE ACCOUNT LIST   *         
*                      = LITTACT - VALIDATE FOR ACCOUNT LIST          *         
*                      = LITTLDG - VALIDATE FOR LEDGER LIST           *         
*                      = LITTMED - VALIDATE FOR BILLING SOURCE (MEDIA)*         
*                      = LITTWRK - VALIDATE FOR WORK-CODE LIST        *         
*        P4  BYTE  3   = C'#' - RETURN TRX COUNT FOR ACC LIST MEMBERS *         
*                                                                     *         
*     APELEM   = THE CURRENT FILTER ELEMENT                           *         
*                NOTE: FIRST BYTE IS ZERO IF NO FILTER ELEMENT EXISTS.*         
*                                                                     *         
*     APREPNUM = THE CURRENT REPORT TYPE, USED WHEN THE LEDGER        *         
*                REQUESTED IS SPACES (A CONTRA ACCOUNT REQUEST)       *         
*                                                                     *         
*  USES:                                                              *         
*     CNTRTAB  = THE CONTRA TABLE                                     *         
*     AIO      = READS THE LIST RECORD                                *         
*                                                                     *         
*  ON OUTPUT:                                                         *         
*     PARM LIST:                                                      *         
*        P1  BYTES 0-3 = ADDRESS OF THE LIST RECORD (IF RF = 0)       *         
*                                                                     *         
*     RF = RETURN CODE                                                *         
*           0 = VALID LIST                                            *         
*           4 = INVALID LIST                                          *         
*          -1 = NOT A LIST                                            *         
*                                                                     *         
*     IOAREA3  = THE LIST RECORD (IF RF = 0)                          *         
*                                                                     *         
*  NOTES:                                                             *         
*     WHEN MAKING CHANGES TO THIS ROUTINE, PLEASE REVIEW THE VALLST   *         
*     ROUTINE IN ACSCR14 FOR SIMILAR CHANGES.                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
R_VALLST DS    0H                                                               
         L     R3,4(,R1)           POINT   TO   POSSIBLE LIST                   
         MVC   RBYTE,11(R1)        SAVE    LIST TYPE                            
         CLI   11(R1),0            DEFAULT LIST TYPE REQUESTED ?                
         BNE   *+8                 NO,     SKIP                                 
         MVI   RBYTE,LITTACT       YES,    USE  DEFAULT OF LITTACT              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   0(R3),C'+'          INCLUDE TYPE LIST ?                          
         BE    *+8                                                              
         CLI   0(R3),C'-'          EXCLUDE TYPE LIST ?                          
         BNE   VALLSTNO            NOT     A    LIST                            
*                                                                               
         USING RFLELD,RE           SET     ADDRESSABILITY                       
         LA    RE,APELEM           GET     CURRENT FILTER DATA ELEMENT          
         CLI   RFLEL,RFLELQ        DO      WE   HAVE A    FILTER EL ?           
         BNE   VALLST05            NO,     SKIP THE  TEST                       
         TM    RFLIND,RFLXCLD      EXCLUDE FILTER ALREADY ON ?                  
         BO    VALLSTER            YES,    ERROR - ENTERED '*+' OR '*-'         
         DROP  RE                                                               
*                                                                               
         USING LSTRECD,R2                                                       
VALLST05 DS    0H                                                               
         CLI   4(R1),LLIST+1       IS      THE  LENGTH OF LIST > 5 ?            
         BH    VALLSTER            YES,    INVALID   LIST                       
         CLI   4(R1),2             IS      THE  LENGTH OF LIST < 2 ?            
         BL    VALLSTER            YES,    INVALID   LIST                       
         LR    R5,R1               SAVE    THE  PARM LIST ADDRESS               
*                                                                               
         LA    R2,IOKEY                                                         
         MVC   LSTKEY,SPACES                                                    
         MVI   LSTKTYP,LSTKTYPQ    X'1D'                                        
         MVC   LSTKCPY,CUABIN      COMPANY CODE                                 
         MVC   LSTKLST,1(R3)       MOVE    IN   LIST                            
         GOTO1 AIO,IORD+IOACCFIL+IO3                                            
         BNE   VALLSTER            INVALID LIST                                 
*                                                                               
         USING LITELD,R1           LIST    TYPE ELEMENT                         
         L     R1,AIOAREA3         FIGURE  OUT  WHICH     LIST                  
         AH    R1,DATADISP         X'1E'   LIST INFO DATA                       
*                                                                               
VALLST10 DS    0H                                                               
         CLI   0(R1),0             END     OF   RECORD    ?                     
         BE    VALLSTER            YES,    INVALID   LIST                       
         CLI   0(R1),LITELQ        X'1E'   LIST ELEMENT   TYPE                  
         BE    VALLST15            YES,    FOUND     ELEMENT                    
         SR    RF,RF               ADD     SIZE OF   THIS ELEMENT               
         IC    RF,1(,R1)                   TO   THE  START     OF               
         AR    R1,RF                       THIS ELEMENT                         
         B     VALLST10            LOOP    TO  PROCESS    NEXT ELEMENT          
*                                                                               
VALLST15 DS    0H                                                               
         CLC   RBYTE,LITTYPE       MATCHING     TYPE ?                          
         BE    VALLST20            YES,    VALID     LIST TYPE                  
         CLI   11(R5),255          SKIP    VALIDATION     REQUESTED ?           
         BE    VALLST20            YES,    ACCEPT    LIST TYPE                  
         CLI   11(R5),1            ACCEPT  ONLY ACCOUNT   OR   LEDGER ?         
         BNE   VALLSTER            NO,     INVALID   LIST                       
         CLI   LITTYPE,LITTACT     ACCOUNT LIST ?                               
         BE    VALLST20            YES,    VALID     LIST TYPE                  
         CLI   LITTYPE,LITTLDG     LEDGER  LIST ?                               
         BNE   VALLSTER            NO,     INVALID   LIST                       
*                                                                               
VALLST20 DS    0H                  VALID   LIST TYPE                            
         CLI   LITTYPE,LITTWRK     WORK-CODE    TYPE ?                          
         BE    VALLSTOK            YES,    SET  GOOD RETURN    CODE             
         MVC   RBYTE,LITTYPE       SAVE    LIST TYPE                            
*                                                                               
         USING LIDELD,R2                                                        
         L     R2,AIOAREA3                                                      
         AH    R2,DATADISP                                                      
*                                                                               
VALLST30 DS    0H                                                               
         CLI   0(R2),0             END     OF   RECORD    ?                     
         BE    VALLSTER            YES,    INVALID   LIST                       
         CLI   0(R2),LIDELQ        X'1F'   LIST ELEMENT   TYPE                  
         BNE   VALLST75            NO,     FIND NEXT ELEMENT                    
*                                                                               
         L     R4,0(,R5)           POINT   TO   LEDGER    LIST                  
         CLC   0(2,R4),SPACES      ANY     LEDGER    PASSED    ?                
         BE    VALLST50            NO,     USE  CONTRA    ACCOUNT TABLE         
*                                                                               
         LA    RE,LIDDLEDG-LIDELD  INIT    WITH LNG  UP   TO   THE DATA         
         LA    RF,LIDDLEDG         ->      LEDGER    FIELD     IN  REC          
*                                                                               
VALLST35 DS    0H                                                               
         L     R4,0(,R5)           POINT   TO   LEDGER    LIST                  
         SR    R0,R0               CLEAR   REGISTER                             
         ICM   R0,1,0(R5)          GET     NUMBER    OF   LEDGERS               
         BNZ   VALLST40            FOUND,  SKIP                                 
         LA    R0,1                DEAFAULT     TO   ONE                        
*                                                                               
VALLST40 DS    0H                                                               
         CLC   0(LUNLG,RF),0(R4)   MATCH   ON   LEDGER    ?                     
         BE    VALLST80            YES,    GOOD LIST                            
         LA    R4,LUNLG(,R4)       ->      NEXT LEDGER                          
         BCT   R0,VALLST40         MORE,   COMPARE   WITH NEXT LEDGER           
*                                                                               
         CLI   RBYTE,LITTLDG       IS      THIS A    LEDGER    LIST ?           
         BNE   VALLST75            NO,     FIND NEXT ELEMENT                    
         LA    RE,L'LIDDLEDG(,RE)  YES,    BUMP TO   NEXT UNIT/LEDGER           
         LA    RF,L'LIDDLEDG(,RF)          BUMP TO   NEXT UNIT/LEDGER           
         CLM   RE,1,LIDLN          CHECKED ALL  UNIT/LEDGERS   ?                
         BL    VALLST35            NO,     CHECK     NEXT UNIT/LEDGER           
         B     VALLST75            YES,    FIND NEXT ELEMENT                    
*                                                                               
         USING CNTRTBLD,RF         CONRA   ACCOUNT   TABLE                      
VALLST50 DS    0H                                                               
         L     RF,ACCNTTAB         ADDR    OF   CONTRA    TABLE                 
         CLI   APREPJCL,REPJCLV    IS      IT   PRODUCT.  TYPE ?                
         BE    VALLSTOK            YES,    SO   DON'T     VALIDATE              
         CLI   APREPJCL,REPJCLX    IS      IT   EXPENSE   TYPE ?                
         BE    VALLSTOK            YES,    SO   DON'T     VALIDATE              
         CLI   APREPJCL,REPJCLP    IS      IT   PAYABLES  TYPE ?                
         BE    VALLSTOK            YES,    SO   DON'T     VALIDATE              
         CLI   APREPJCL,REPJCLB    IS      IT   CASH      TYPE ?                
         BE    VALLSTOK            YES,    SO   DON'T     VALIDATE              
         CLI   APREPJCL,REPJCLG    IS      IT   G/L       TYPE ?                
         BE    VALLSTOK            YES,    SO   DON'T     VALIDATE              
*                                                                               
VALLST60 DS    0H                                                               
         CLC   APREPNUM,CNTRRTYP   MATCHING     REPORT    TYPE ?                
         BNE   VALLST70            NO,     TRY  NEXT ENTRY                      
         CLC   LIDDLEDG,CNTRUNLG   MATCHING     UNIT/LEDGER    ?                
         BE    VALLSTOK            YES,    GOOD LIST                            
*                                                                               
VALLST70 DS    0H                                                               
         LA    RF,CNTRLNQ(,RF)     NEXT    TABLE     ENTRY                      
         CLI   0(RF),EOT           END     OF   CONTRA    TABLE     ?           
         BNE   VALLST60            NO,     TRY  AGAIN                           
         DROP  RF                                                               
*                                                                               
VALLST75 DS    0H                  FIND    NEXT ELEMENT                         
         SR    RF,RF               ADD     SIZE OF   THIS ELEMENT               
         IC    RF,1(,R2)                   TO   THE  START     OF               
         AR    R2,RF                       THIS ELEMENT                         
         B     VALLST30            LOOP    TO   PROCESS   NEXT ELEMENT          
*                                                                               
VALLST80 DS    0H                                                               
         CLI   RBYTE,LITTACT       TEST ACCOUNT LIST                            
         BNE   VALLSTOK                                                         
         CLI   15(R5),TX#          TEST WANT TRX COUNT RETURNED                 
         BNE   VALLSTOK                                                         
         USING ACTRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,SPACES       READ ACCOUNT RECS IN LIST                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),LIDDLEDG                                              
         LA    R8,LIDDACCS         R8=A(CURRENT LIST ITEM)                      
VALLST82 SR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         AHI   RF,-1                                                            
         MVC   ACTKACT(0),0(R8)                                                 
         EX    RF,*-6                                                           
         DROP  R6                                                               
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   VALLST94                                                         
*                                                                               
         USING ABLELD,R1                                                        
         L     R1,AIOAREA2                                                      
         AH    R1,DATADISP                                                      
         SR    R0,R0                                                            
VALLST84 CLI   ABLEL,0                                                          
         BE    VALLST94                                                         
         CLI   ABLEL,ABLELQ        FIND BALANCE EL (=LOW-LVL A/C)               
         BNE   VALLST86                                                         
         CLI   ABLLN,ABLLN3Q                                                    
         BL    VALLST94            SAFETY:SHOULDN'T HAPPEN                      
         ICM   RE,15,ABLTXS                                                     
         B     VALLST90                                                         
         USING NUMELD,R1                                                        
VALLST86 CLI   NUMEL,NUMELQ        ELSE FIND NUMEL (=HI-LVL A/C)                
         BNE   VALLST88                                                         
         CLI   NUMTYPE,NUMTYHIQ                                                 
         BNE   VALLST88                                                         
         ICM   RE,15,NUM#TRX                                                    
         B     VALLST90                                                         
*                                                                               
VALLST88 IC    R0,NUMLN                                                         
         AR    R1,R0                                                            
         B     VALLST84                                                         
*                                                                               
VALLST90 ICM   RF,15,ACAC#TRX                                                   
         AR    RF,RE                                                            
         STCM  RF,15,ACAC#TRX      ACCUMULATE TRANSACTION COUNT                 
*                                                                               
VALLST94 SR    RF,RF               BUMP TO NEXT LIDEL ITEM                      
         IC    RF,LIDITLN                                                       
         AR    R8,RF               R8=NEXT ACCOUNT IN LIDEL                     
         IC    RF,LIDLN            CALC/TEST END OF ELEMENT                     
         AR    RF,R2               RF=END OF LIDEL                              
         CR    R8,RF                                                            
         BL    VALLST82            READ NEXT ACCOUNT, THIS LIDEL                
VALLST96 IC    R0,LIDLN            ELSE READ NEXT LIDEL                         
         AR    R2,R0                                                            
         CLI   LIDEL,0                                                          
         BE    VALLSTOK            EOR - OK                                     
         CLI   LIDEL,LIDELQ                                                     
         BNE   VALLST96                                                         
         LA    R8,LIDDACCS         REFRESH LIDEL ITEM POINTER                   
         B     VALLST82                                                         
*                                                                               
VALLSTER DS    0H                  INVALID LIST                                 
         MVC   FVMSGNO,=AL2(ACEIVLT)                                            
         L     RF,=F'4'            SET     ERROR     RETURN    CODE             
         B     VALLSTEX            EXIT                                         
*                                                                               
VALLSTNO DS    0H                                                               
         L     RF,=F'-1'           SET     NOT  A   VALLIST RETURN CODE         
         B     VALLSTEX            EXIT                                         
*                                                                               
VALLSTOK DS    0H                                                               
         L     RE,AIOAREA3         GET     ADDRESS   OF   THE  RECORD           
         LA    RE,0(,RE)           CLEAR   THE  HIGH ORDER     BYTE             
         ST    RE,0(,R5)           RETURN  THE  ADDR OF   THE  RECORD           
         SR    RF,RF               SET     GOOD RETURN    CODE                  
*                                                                               
VALLSTEX DS    0H                                                               
         OI    APINDS,APILRERD     RE-READ RECORD ON RETURN                     
         B     EXITRF                                                           
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*        VALIDATE DEFINED KEYTYPE                                     *         
*        R1 = HEADER FIELD TO VALIDATE                                *         
***********************************************************************         
         SPACE 1                                                                
LVALDEF  NTR1                                                                   
*                                                                               
R_VALDEF MVC   APKEYWRD,SPACES     CLEAR CURRENT KEYWORD                        
         MVC   APKEYHD,SPACES                                                   
         XC    RKYWHDR,RKYWHDR                                                  
         MVI   RXTRALN,0                                                        
         MVC   RXTRA,SPACES                                                     
         MVI   RXMODE,RXM1ST       FIRST TIME THROUGH                           
         MVC   RKEYHD,SPACES                                                    
         MVI   APUSRKYW,NO                                                      
         MVI   LEVLAST,NO                                                       
         MVI   AC00PASS,0                                                       
         LA    R6,1                SET R6 TO A BAD ADDRESS                      
         LTR   R4,R1                                                            
         BZ    VALDEFX             NOT VALID CALL                               
*                                                                               
         USING DEFTABD,R6                                                       
         CLM   R4,8,=CL1'E'        ENTRY ALREADY FOUND?                         
         BNE   VALDEF05                                                         
         LA    R6,0(,R4)           CLEAR HOB R6=TABLE ENTRY                     
         GOTO1 EXPKEYW,(R6)        MATCH IN TABLE                               
         B     VALDEF50            VALIDATE ONLY                                
*                                                                               
VALDEF05 SR    R3,R3                                                            
         ICM   R3,1,5(R4)          ANY INPUT?                                   
         BZ    VALDEFX             NO                                           
         ST    R4,RKYWHDR          SAVE OF FIELD HDR ADDRESS                    
         MVC   RCARD,SPACES                                                     
         BCTR  R3,0                LENGTH OF SCREEN FIELD - 1                   
         EXMVC R3,RCARD,8(R4)                                                   
         MVC   RPARM+8(2),=C',='                                                
         MVC   RPARM+10(1),SCCOMMA        LANGUAGE SOFT C','                    
         MVC   RPARM+11(1),APOPNPRN       LANGUAGE SOFT C'('                    
         GOTO1 VSCANNER,RPARM,(C'C',RCARD),(2,RBLOCK)                           
         CLI   RPARM+4,1           NUMBER OF PARAMETERS                         
         BL    VALDEFX                                                          
         CLC   RBLOCK+12(10),SPACES       ANY  KEYWORD ?                        
         BE    VALDEFX                    NO,  INVALID                          
         SR    R3,R3                                                            
         IC    R3,RBLOCK           LENGTH OF PARAMETER                          
         LA    R4,RBLOCK+12                                                     
         CLI   0(R4),C'&&'         MACRO TYPE?                                  
         BNE   VALDEF10                                                         
         CLI   REPMODE,REPHEAD     VALID HEADING KEYWORD?                       
         BE    *+8                 YES, SO CONTINUE                             
         CLI   INREC,RECHEAD       VALID HEADING KEYWORD?                       
         BNE   VALDEFX             NO, MACRO NOT ALLOWED                        
         CLI   RPARM+4,1           NUMBER OF PARAMETERS                         
         BH    VALDEFX             ONLY ONE MACRO ALLOWED FOR NOW               
         SH    R3,=H'01'           SUBTRACT ONE FROM PARAMETER LENGTH           
         BNP   VALDEFX             NO KEYWORD SUPPLIED                          
         LA    R4,1(,R4)           BUMP PAST C'&'                               
*                                                                               
VALDEF10 MVC   APKEYWRD,0(R4)                                                   
         OC    APKEYWRD,SPACES     MAKE UPPER CASE                              
         CH    R3,=Y(KEYWDLN)      CAN'T BE LONGER THAN KEYWDLN                 
         BH    VALDEFX             TO LONG                                      
*                                                                               
         USING DEFTABD,R6                                                       
         L     R6,ACDEFTAB                                                      
         CLI   RXMODE,RXM2ND       SECOND PASS ?                                
         BNE   *+8                 NO,  SKIP                                    
         L     R6,ACDEFTB2         YES, ONLY SCAN FOR KEYWORDS WITH             
*                                       EMBEDDED '#" SIGNS                      
         SH    R3,=H'01'           ONE LESS FOR EXECUTE                         
         SR    RF,RF                                                            
*                                                                               
VALDEF32 CLI   0(R6),EOT           END OF TABLE                                 
         BNE   VALDEF45            YES                                          
         OC    RKYWHDR,RKYWHDR                                                  
         BZ    VALDEF34                                                         
         XC    KYWFLDH,KYWFLDH                                                  
         MVC   KYWFLD,SPACES                                                    
         MVI   KYWFLDH,20                                                       
         MVI   KYWFLDH+5,6         MOVE IN LENGTH FROM SCAN BLOCK               
         MVC   KYWFLD(6),APKEYWRD  MOVE IN KEYWORD ALONE                        
         GOTO1 AJOBCOL,RPARM,(1,KYWFLDH),ESTWRK,ACOM                            
         CLI   4(R1),0             WAS IT VALID ESTIMATE KEYWORD?               
         BE    VALDEF33            NOT FOUND,   NOT A ESTIMATE TYPE             
*                                                                               
         CLI   RXMODE,RXM1ST       IS IT FIRST TIME THROUGH                     
         BNE   VALDEFX             VALID EST KEYWORD BUT NOT SUPPORTED          
         MVI   RXMODE,RXM2ND       SET TO SAY 2ND PASS                          
         BAS   RE,ESTIMATE                                                      
*        GOTO1 =A(ESTIMATE),RPARM,(RC),RR=ACRELO                                
         BE    VALDEF10            VALIDATE   #    SIGN KEYWORDS                
         B     VALDEF34            NOT   A    #    SIGN KEYWORD                 
*                                                                               
VALDEF33 CLI   RXMODE,RXM1ST       IS IT 1ST  TIME THROUGH ?                    
*                                  NOT   VALID     EMBEDDED  #    SIGN          
         BNE   VALDEF34                  KEYWORD                                
         MVI   RXMODE,RXM2ND       SET   TO   SAY  2ND  PASS                    
*                                  CHECK FOR  EMBEDDED  NUMBERS                 
         BAS   RE,ESTIMAT2                                                      
*        GOTO1 =A(ESTIMAT2),RPARM,(RC),RR=ACRELO                                
         BE    VALDEF10            VALIDATE   #    SIGN KEYWORDS                
*                                  NOT   A    #    SIGN KEYWORD                 
         USING KWDRECD,R2                                                       
VALDEF34 CLI   REPMODE,REPKYWD     USER DEFINED ONLY FROM TABLE                 
         BE    VALDEFX             NOT VALID FOR CALL                           
         CLI   INREC,RECKWD        USER DEFINED ONLY FROM TABLE                 
         BE    VALDEFX             NOT VALID FOR CALL                           
         LA    R2,IOKEY                                                         
         MVC   RIOSVKEY,IOKEY      SAVE CURRENT KEY                             
         MVC   KWDKEY,SPACES       INITALIZE                                    
         MVI   KWDKTYP,KWDKTYPQ    X'2D'                                        
         MVI   KWDKSUB,KWDKSUBQ    X'06'                                        
         MVC   KWDKCPY,CUABIN      COMPANY CODE                                 
         MVC   KWDKCODE,APKEYWRD   USER DEFINED KEYWORD                         
         CLI   AC00PASS,0          WAS  KEYWORD MODIFIED ?                      
         BE    *+10                NO,  SKIP                                    
         MVC   KWDKCODE,RSVKYWRD   YES, USE ORIGINAL KEYWORD                    
         GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         MVC   IOKEY,RIOSVKEY                                                   
         BNE   VALDEFX                                                          
         L     R2,AIOAREA3                                                      
         AH    R2,DATADISP                                                      
         LR    RF,R2               USE FOR LATER                                
         SR    R1,R1                                                            
*                                                                               
VALDF34A CLI   0(R2),0             END OF RECORD?                               
         BE    VALDEFX                                                          
         CLI   0(R2),STYELQ        X'25'  HAVE TO CHECK IF TYPE MATCHED         
         BE    VALDF34B                                                         
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     VALDF34A                                                         
*                                                                               
         USING STYELD,R2                                                        
VALDF34B CLC   APREPCDE,STYNAME    DO   TYPES MATCH ?                           
         BNE   VALDEFIV            NO,  KEYWORD IS NOT VALID                    
*                                                                               
         LR    R2,RF                                                            
VALDF34L CLI   0(R2),0             END OF RECORD?                               
         BE    VALDEFX                                                          
         CLI   0(R2),LIDELQ        X'1F'                                        
         BE    VALDEF35                                                         
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     VALDF34L                                                         
*                                                                               
         USING LIDELD,R2                                                        
VALDEF35 MVI   APUSRKYW,YES                                                     
         MVC   APKEYWRD,LIDDACCS                                                
         L     R6,ACDEFTAB                                                      
         MVI   RXMODE,RXM1ST       SEARCH THE WHOLE KEYWORD TABLE               
         DROP  R2                                                               
*                                                                               
VALDEF45 GOTO1 EXPKEYW,(R6)        MATCH IN TABLE                               
         CLC   RKEYWRD,APKEYWRD                                                 
         BE    VALDEF47                                                         
         CLC   =AL2(AC#RSBLK),DEFDDNUM   IS THIS BLANK LINE KEYWORD?            
         BNE   VALDEF46            NO                                           
         GOTO1 FLEN2,RPARM,(L'APKEYWRD,APKEYWRD)                                
         SH    RF,=H'02'           ONE FOR "EX" & ONE FOR # IN BLK#             
         BM    VALDEF46                                                         
         EXCLC RF,RKEYWRD,APKEYWRD COMPARE LESS THE NUMBER                      
         BNE   VALDEF46                                                         
         LA    RE,APKEYWRD+1(RF)                                                
         CLI   0(RE),C'0'          IS IT A NUMBER ?                             
         BH    VALDEF47            YES,  LOOKS OK                               
*                                                                               
VALDEF46 SR    RF,RF                                                            
         IC    RF,DEFLEN           BUMP TO NEXT KEYWORD                         
         AR    R6,RF                                                            
         B     VALDEF32                                                         
*                                                                               
VALDEF47 TM    DEFCLIND,DEFCLJBR   IS IT A JOBBER COLUMN                        
         BZ    VALDEF48            NO, SO DON'T CHECK VALIDITY                  
         LA    R2,ESTWRK           SEE IF VALID FOR THIS COUNTRY                
         OC    RKYWHDR,RKYWHDR                                                  
         BZ    VALDEFX             ELSE  NOT  VALID KEYWORD THEN                
         CLI   RXMODE,RXM2ND       1ST OR 2ND PASS                              
         BE    VALDEF48            IF  2ND PASS THEN SKIP                       
         XC    KYWFLDH,KYWFLDH                                                  
         MVC   KYWFLD,SPACES                                                    
         MVI   KYWFLDH+5,6         MOVE IN LENGTH FROM SCAN BLOCK               
         MVI   KYWFLDH,20                                                       
         MVC   KYWFLD(6),APKEYWRD  MOVE IN KEYWORD ALONE                        
         GOTO1 AJOBCOL,RPARM,(1,KYWFLDH),(R2),ACOM                              
         CLI   4(R1),0             WAS IT     VALID ESTIMATE KEYWORD ?          
         BE    VALDEFX             NOT FOUND, NOT VALID JOBBER KEYWORD          
*                                                                               
VALDEF48 CLI   DEFSPCL,0           SPECIAL KEYWORD?                             
         BE    VALDEF50            NO                                           
         CLI   DEFSPCL,DEFLVL9     ACCOUNT LEVEL KEYWORD?                       
         BH    VALDF48A            NO                                           
         CLI   DEFSPCL,DEFLVL1     ACCOUNT LEVEL KEYWORD?                       
         BNL   VALDEF50            YES, SO CONTINUE AS NORMAL                   
*                                                                               
VALDF48A DS    0H                                                               
         CLI   DEFSPCL,DEFCTRL     EMBEDDED #, JOBBER KEYWORD                   
         BE    VALDF48C            YES, CHECK WHEN MATCHED                      
         CLI   DEFSPCL,DEFCTR#     EMBEDDED #, NON-JOBBER KEYWORD               
         BNE   VALDEF49            NO,  CONTINUE                                
*                                                                               
VALDF48C DS    0H                                                               
         CLI   RXMODE,RXM1ST       FIRST TIME THROUGH MATCHED?                  
         BNE   VALDEF50            NO, 2ND TIME THROUGHT THEN OK                
         B     VALDEFX             ELSE  NOT  VALID KEYWORD THEN                
*                                                                               
VALDEF49 CLI   DEFSPCL,DEFDRKY     PDAY                                         
         BE    VALDEF50            YES, COUNTINUE AS USUAL                      
         CLI   DEFSPCL,DEFEXRT     EXRT()                                       
         BE    VALDF49A                                                         
         CLI   DEFSPCL,DEFSUM      CUME()                                       
         BE    VALDF49A                                                         
*&&UK*&& CLI   DEFSPCL,DEFMAP      TYMAP()                                      
*&&UK*&& BE    VALDF49A                                                         
         CLI   DEFSPCL,DEFSIGN     SIGN()                                       
         BNE   VALDEFX             DON'T  KNOW WHAT IT IS                       
*                                                                               
VALDF49A CLI   RBLOCK+1,2          SHOULD HAVE AT LEAST 2 CHARS                 
         BL    VALDEFX                                                          
         ZIC   R1,RBLOCK+1                                                      
         LA    RE,RBLOCK+22                                                     
         AR    RE,R1                                                            
         BCTR  RE,0                                                             
         CLC   0(1,RE),APCLSPRN    HAS TO HAVE ')'                              
         BNE   VALDEFX                                                          
*                                                                               
VALDEF50 LA    RF,COL+ROW+HDL                                                   
         CLI   REPMODE,REPEVRY     MODE ROW/COL/HEADING                         
         BE    VALDEF55            EVERY KEYWORD REGARDLESS                     
         MVC   RFULL,APREPIND                                                   
         NC    RFULL,DEFREPS                                                    
         BNZ   VALDEF52                                                         
         TM    DEFIND,DEF2NDK      IS THERE AN IDENTICAL KEYWORD                
         BO    VALDEF46            YES, BUMP UP TO NEXT KEYWORD                 
         B     VALDEFIV            NOT  VALID LEDGER GROUP FOR KEYWORD          
*                                                                               
VALDEF52 LA    RF,HDL                                                           
         CLI   REPMODE,REPHEAD     VALID HEADING KEYWORD?                       
         BE    VALDEF55                                                         
         LA    RF,ROW                                                           
         CLI   REPMODE,REPROW      VALID ROW KEYWORD?                           
         BE    VALDEF55                                                         
         LA    RF,COL                                                           
         CLI   REPMODE,REPCOL      VALID COLUMN KEYWORD?                        
         BE    VALDEF55                                                         
         LA    RF,HDL                                                           
         CLI   INREC,RECHEAD       VALID HEADING KEYWORD?                       
         BE    VALDEF55                                                         
         LA    RF,ROW                                                           
         CLI   INREC,RECROW        VALID ROW KEYWORD?                           
         BE    VALDEF55                                                         
         LA    RF,COL                                                           
         CLI   INREC,RECCOL        VALID COLUMN KEYWORD ?                       
         BE    VALDEF55                                                         
         CLI   REPMODE,REPKYWD     VALID USER DEFINED KEYWORDS ?                
         BE    VALDEF55                                                         
         CLI   INREC,RECKWD        VALID USER DEFINED KEYWORDS ?                
         BNE   VALDEFIV            NO,  KEYWORD IS NOT VALID                    
*                                                                               
VALDEF55 EX    RF,*+8                                                           
         B     *+8                                                              
         TM    DEFIND,0            VALID USAGE ?                                
         BZ    VALDEFIV            NO,  KEYWORD IS NOT VALID                    
*&&UK                                                                           
         CLI   DEFCTRY,0           TEST COUNTRY-RESTRICTED KEYWORD              
         BE    VALDEF58            NO                                           
         LA    RE,X'70'            BNZ                                          
         TM    DEFCTRY,DEFCNOT     TEST 'ALL BUT' SPECIFIED COUNTRY             
         BNZ   *+8                 YES                                          
         LA    RE,X'80'            BZ                                           
                                                                                
         LA    RF,DEFCGBR                                                       
         CLI   CUCTRY,CTRYGBR                                                   
         BE    *+12                                                             
         LA    RF,DEFCGER                                                       
         CLI   CUCTRY,CTRYGER                                                   
         BNE   VALDEF58            SAFETY - NO COUNTRY SET...                   
                                                                                
         EX    RF,*+8              TEST COUNTRY CODE SET                        
         EX    RE,*+8              BRANCH TO ERROR IF SET/NOT SET               
         TM    DEFCTRY,0                                                        
         NOP   VALDEFX                                                          
*&&                                                                             
VALDEF58 TM    DEFIND,DEFDDS       DDS ONLY KEYWORD ?                           
         BZ    VALDEF60            NO                                           
         TM    CUSTAT,CUSDDS       YES, SO ARE WE AT DDS ?                      
         BZ    VALDEFX             NO,  KEYWORD IS NOT RECOGNIZED               
*                                                                               
VALDEF60 CLI   APUSRKYW,YES        IS IT USER DEFINED KEYWORD ?                 
         BE    VALDEF90            YES, SO DON'T BOTHER GETTING LEVEL           
         SR    R2,R2               CLEAR R2 FOR VLEV ROUTINES                   
         SR    RF,RF                                                            
         ICM   RF,1,DEFROUT                                                     
         BZ    VALDEF90                                                         
         CLI   DEFROUT,(VALRTNX-VALRTN)/4                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
VALRTN   B     VLEVA               LEVEL 1                                      
         B     VLEVB               LEVEL 2                                      
         B     VLEVC               LEVEL 3                                      
         B     VLEVD               LEVEL 4                                      
         B     VLEVL               LAST LEVEL (COULD BE 4 TO 1)                 
         B     VACUL               UNIT/LEDGER                                  
         B     VAACEQU             ACCOUNT EQUIVALENT LEVEL                     
         B     VAPREQU             ACCOUNT EQUIVALENT LEVEL                     
         B     VFLTN               FILTER NAME                                  
*                                                                               
VALRTNX  EQU   *                                                                
*                                                                               
VALDEF90 TM    DEFIND,DEFGTHD      USE HEADING ANYWAY                           
         BO    VALDEF92                                                         
         CLI   APREPJCL,REPJCLV    PRODUCTION?                                  
         BE    VALDEF91            PRODUCTION FILLS IN AMOUNT HEADING           
         CLI   INACT,ACTHLP        CALLED FROM HELP OVERLAY ?                   
         BE    VALDEF91            YES, GENERATE HEADINGS                       
*&&UK*&& TM    DEFCLOTH,DEFCLHED   ALWAYS SHOW HEADINGS ?                       
*&&UK*&& BO    VALDEF91            YES, GENERATE HEADINGS                       
         TM    DEFCLOTH,DEFCLAMT   IF IT IS   ACCUMLATED                        
         BO    VALDEF95                                                         
*                                                                               
VALDEF91 CLI   APUSRKYW,YES        SKIP THE HEADING ?                           
         BE    VALDEF95            YES                                          
*                                                                               
VALDEF92 MVC   APKEYHD,RKEYHD                                                   
*                                                                               
VALDEF95 SR    R1,R1                                                            
         LR    R1,R6                                                            
         B     EXITR1                                                           
*                                                                               
VALDEFX  LTR   R6,R6               SET  CC TO NOT                               
         LA    R1,0                SET  R1 TO 0                                 
         B     EXITR1                                                           
*                                                                               
VALDEFIV LTR   R1,R6               SET  CC TO NOT & SET  R1 TO KEYWORD          
         B     EXITR1                                                           
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE LEDGER LEVEL FOR KEYWORD                                  *         
***********************************************************************         
         SPACE 1                                                                
VLEVL    MVI   LEVLAST,YES                                                      
VLEVD    LA    R2,1(,R2)                                                        
VLEVC    LA    R2,1(,R2)                                                        
VLEVB    LA    R2,1(,R2)                                                        
         MHI   R2,(ACLDGLEN-L'ACLEDGER)/4                                       
*                                                                               
VLEVA    LA    R2,ACLEV1(R2)                                                    
         B     *+8                                                              
VACUL    LA    R2,ACULNAME                                                      
         LA    R1,APREPUL          ACCOUNT U/L                                  
         CLI   DEFROUTX,DEFRAUL    USE     ACCOUNT U/L ?                        
         BE    VALLEV40            YES,    SKIP                                 
         LA    R1,APREPCUL         CONTRA  U/L                                  
         CLI   DEFROUTX,DEFRCUL    USE     CONTRA  U/L ?                        
         BE    VALLEV40            YES, SKIP                                    
         CLI   DEFROUTX,0          ANY     EXTRA   DATA ?                       
         BE    VALDEFIV            NO,     SET     CC   TO   ERROR              
*                                  CHECK   ROUTINE #                            
         CLI   DEFROUTX,DEFRLAST   ROUTINE BEYOUND LAST ?                       
         BNH   *+6                 NO,     CONTINUE                             
         DC    H'00'               YES,    BAD     TABLE     ENTRY              
*                                                                               
         ZIC   R1,DEFROUTX         GET     ROUTINE #                            
         AHI   R1,-DEFRUL13        THESE   ARE  ACTUALLY TABLE ENTRY #S         
         SLL   R1,1                MULTIPLY        BY   2                       
         L     RF,=A(DEFULTBL)     ADD     BASE    OF   TABLE                   
         A     RF,ACRELO                                                        
         AR    R1,RF               ->      U/L     ENTRY                        
         CLI   DEFROUT,VALUL                                                    
         BNE   VALLEV40            JUST SEE IF LEDGER EXISTS                    
         GOTO1 LGETLDGR                                                         
         BNE   VALDEFIV            KEYWORD IS NOT VALID                         
         B     VALDEF90            PRESS ONWARD                                 
*                                                                               
VALLEV40 CLC   0(L'ACLEDGER,R1),SPACES                                          
         BH    VALLEV43                                                         
         CLI   APREPJCL,REPJCL1    COST ?                                       
         BE    VALDEF90                                                         
         CLI   APREPJCL,REPJCLV    PRODUCTION ?                                 
         BE    VALDEF90                                                         
         CLC   0(L'ACLEDGER,R1),SPACES                                          
         BE    VALDEF90                                                         
*                                                                               
VALLEV43 GOTO1 LGETLDGR                                                         
         BNE   VALDEFIV            KEYWORD IS NOT VALID                         
         CLI   LEVLAST,YES                                                      
         BNE   VALLEV50                                                         
         LA    RF,4                                                             
*                                                                               
VALLEV45 CLI   0(R2),0                                                          
         BNE   VALLEV50                                                         
         AHI   R2,-(ACLDGLEN-L'ACLEDGER)/4                                      
         BCT   RF,VALLEV45                                                      
         B     VALDEFIV            KEYWORD IS NOT VALID                         
*                                                                               
VALLEV50 CLI   0(R2),0                                                          
         BE    VALDEFIV            KEYWORD IS NOT VALID                         
         MVC   RKEYHD,SPACES       CLEAR THE KEY HEADING LINE(S)                
         CLI   DEFROUT,VALUL       UNIT/LEDGER ?                                
         BE    VALLEV52                                                         
         MVC   RKEYHD(L'ACLEVNM1),L'ACLEV1(R2)                                  
         B     VALDEF90                                                         
*                                                                               
VALLEV52 MVC   RKEYHD,0(R2)                                                     
         B     VALDEF90                                                         
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
* GET LEDGER RECORD - GET EQUIVALENT RULES ELEMENT -                  *         
* VALIDATE LEVEL REQUESTED FOR ACCOUNT EQUIVALENT                     *         
*                                                                     *         
***********************************************************************         
*MN                                                                             
         USING DEFTABD,R6                                                       
VAPREQU  LA    R1,=C'SJ'                                                        
         B     *+8                                                              
VAACEQU  LA    R1,APREPUL                                                       
         GOTO1 LGETLDGR                                                         
         BNE   VALDEFIV            KEYWORD IS NOT VALID                         
*                                                                               
VACEQU20 DS    0H                                                               
         SR    RF,RF                                                            
         IC    RF,DEFACEQU                                                      
         MHI   RF,AEQUGRPA                                                      
         LA    RF,AEQULVL(RF)                                                   
         OC    0(AEQUGRPA,RF),0(RF)                                             
         BZ    VALDEFIV                                                         
                                                                                
         CLI   DEFSPCL,0                                                        
         BE    VALDEF90                                                         
                                                                                
         MVI   DUB,0                                                            
         MVN   DUB(1),DEFSPCL                                                   
         CLC   DUB(1),2(RF)                                                     
         BH    VALDEFIV                                                         
         B     VALDEF90                                                         
         DROP  R6                                                               
         EJECT ,                                                                
*MN                                                                             
***********************************************************************         
* NO HEADINGS FOR FILTER KEYWORDS,                                    *         
* THEN THE HEADINGS WILL BE READ FROM FILTER NAME RECORDS             *         
*                                                                     *         
***********************************************************************         
VFLTN    MVC   RKEYHD,SPACES                                                    
         B     VALDEF90                                                         
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO EXPAND REPORT TYPE                                       *         
*                                                                     *         
* NTRY - R1=A(REP TABLE ENTRY)                                        *         
* EXIT - RREPTYP CONTAINS EXPANDED REPORT CODE AND TYPE               *         
***********************************************************************         
         SPACE 1                                                                
         USING REPTABD,R1                                                       
EXPRPTY  MVC   RREPCDE,REPCODE                                                  
         MVC   RREPTYP,REPSTYPE                                                 
         CLI   RREPCDE,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNLR  RE                                                               
*                                                                               
EXPRPTYN STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,RPARM,C'TU  ',('RREPLNQ',RREPCDE),0                      
*                                                                               
EXPRPTYX LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO EXPAND KEYWORDS                                          *         
*                                                                     *         
* NTRY - R1=A(KEYWORD ENTRY)                                          *         
* EXIT - RKEYWRD CONTAINS EXPANDED KEYWORD                            *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R1                                                       
EXPKEYW  MVC   RKEYWRD,DEFCODE                                                  
         MVC   RKEYHD,SPACES                                                    
         MVC   RKEYHD(L'DEFCODE),DEFCODE                                        
         MVI   RKEYHD+3,L'RKEYHD   CHANGE LENGTH FOR DICTATE                    
         CLI   RKEYWRD,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BL    EXPKYWN                                                          
         MVC   RKEYHD,SPACES                                                    
         TM    DEFIND,DEFGTXT                                                   
         BZ    EXPKYW1                                                          
         STM   RE,R1,SVREGS                                                     
         SR    RF,RF                                                            
         ICM   RF,3,DEFHELP#                                                    
         B     EXPKYW5                                                          
*                                                                               
EXPKYW1  SR    RF,RF                                                            
         IC    RF,DEFLEN                                                        
         AHI   RF,-(DEFDSC-DEFTABD+1)                                           
         BMR   RE                                                               
         CHI   RF,L'RKEYHD-1                                                    
         BL    *+8                                                              
         LA    RF,L'RKEYHD-1                                                    
         EXMVC RF,RKEYHD,DEFDSC                                                 
         BR    RE                                                               
*                                                                               
EXPKYWN  STM   RE,R1,SVREGS                                                     
         GOTO1 VDICTAT,RPARM,C'SU  ',RKEYWRD,0                                  
         LM    RE,R1,SVREGS        NEED TO RESTORE R1                           
         CLI   DEFSPCL,DEFLVL1     ACCOUNT LEVEL TYPE KEYWORD ?                 
         BL    EXPKYWN2            NO                                           
         CLI   DEFSPCL,DEFLVL9     ACCOUNT LEVEL TYPE KEYWORD ?                 
         BH    EXPKYWN2            NO                                           
         LA    RE,RKEYWRD                                                       
         LA    RF,L'RKEYWRD                                                     
         CLI   0(RE),C'#'          FIND NUMBER SIGN                             
         BE    EXPKYWN1                                                         
         LA    RE,1(,RE)           NEXT CHARACTER                               
         BCT   RF,*-12                                                          
         DC    H'00'                                                            
*                                                                               
EXPKYWN1 MVC   0(1,RE),DEFSPCL     SUBSTITUTE LEVEL #                           
*                                                                               
EXPKYWN2 SR    RF,RF                                                            
         ICM   RF,3,DEFHEAD#       OVER-RIDE DDICT KEYWORD HEADING ?            
         BNZ   EXPKYW5             YES                                          
         GOTO1 VDICTAT,RPARM,C'SL  ',RKEYHD,0                                   
         B     EXPKYWX                                                          
*                                                                               
EXPKYW5  GOTO1 LTXTGET,RPARM2,(C'S',(RF)),(L'RKEYHD,RKEYHD),           X        
               (RXTRALN,RXTRA)                                                  
*                                                                               
EXPKYWX  LM    RE,R1,SVREGS                                                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* R_CNVTTY - CONVERTS TRANSACTION TYPES TO BINARY OR STRINGS          *         
*     PARM1, BYTE 0    = C'N' = CONVERT TO BINARY NUMBERS             *         
*                      = C'S' = CONVERT TO STRINGS                    *         
*            BYTES 1-3 = ADDRESS OF SCANBLOCK, FOR C'N' ONLY          *         
*                      = ADDRESS OF RFLEL, FOR C'S' ONLY              *         
*     PARM2, BYTE 0    = C'N' NUMBER OF PARAMETERS IN SCAN BLOCK      *         
*                      = C'S' LENGTH OF OUTPUT FIELD                  *         
*            BYTES 1-3 = ADDRESS OF OUTPUT                            *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2                                                        
R_CNVTTY DS    0H                                                               
         L     R2,0(,R1)           ADDRESS OF INPUT                             
         LA    R2,0(,R2)           STRIP OFF HOB                                
         L     R6,4(,R1)           ADDRESS OF OUTPUT                            
         LA    R6,0(,R6)           STRIP OFF HOB                                
         ST    R6,RADDRS                                                        
         MVC   RLEN,4(R1)          SAVE INPUT LENGTH                            
         CLI   0(R1),C'N'                                                       
         BE    CNVTP300                                                         
*                                                                               
         ZIC   R3,RFLLN                                                         
         SHI   R3,RFLLNQ                                                        
*                                                                               
         LA    R6,RBLOCK                                                        
         MVC   RBLOCK,SPACES                                                    
         LA    R4,RFLDATA                                                       
         TM    RFLIND,RFLXCLD      EXCLUSION?                                   
         BZ    CNVTP010                                                         
         MVI   0(R6),C'*'                                                       
         LA    R6,1(,R6)                                                        
*                                                                               
CNVTP010 CLI   0(R4),TYFIRST       IS SPECIAL TYPE?                             
         BL    CNVTP050                                                         
         CLI   0(R4),TYLAST        IS SPECIAL TYPE 30?                          
         BH    CNVTP050                                                         
*                                                                               
         L     R1,=A(TTYPETAB)                                                  
         A     R1,ACRELO                                                        
*                                                                               
CNVTP020 CLI   0(R1),0             EOT?                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID TYPE                                 
         CLC   8(1,R1),0(R4)                                                    
         BE    CNVTP030                                                         
         LA    R1,13(,R1)                                                       
         B     CNVTP020                                                         
*                                                                               
CNVTP030 MVC   TYPECDE,9(R1)                                                    
         CLI   TYPECDE,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNL   CNVTP032                                                         
         GOTO1 VDICTAT,RPARM,SCDICONE,TYPECDE,0                                 
*                                                                               
CNVTP032 MVC   0(3,R6),TYPECDE                                                  
         LA    R0,3                                                             
         LA    RE,2(,R6)                                                        
*                                                                               
CNVTP040 CLI   0(RE),C' '          ADJUST SPACING                               
         BNE   CNVTP070                                                         
         BCTR  RE,0                                                             
         BCT   R0,CNVTP040                                                      
         B     CNVTP070                                                         
*                                                                               
CNVTP050 EDIT  (B1,(R4)),(3,(R6)),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
CNVTP070 AR    R6,R0                                                            
         LA    R4,1(,R4)                                                        
         CHI   R3,1                                                             
         BE    *+14                                                             
         MVC   0(1,R6),SCCOMMA                                                  
         LA    R6,1(,R6)                                                        
         BCT   R3,CNVTP010                                                      
         LA    RF,RBLOCK                                                        
         SR    R6,RF                                                            
         SR    RF,RF                                                            
         ICM   RF,1,RLEN                                                        
         BZ    CNVTP072                                                         
         CR    R6,RF                                                            
         BNH   *+6                                                              
         DC    H'00'                                                            
*                                                                               
CNVTP072 SHI   R6,1                                                             
         BM    EXIT                                                             
         L     RF,RADDRS                                                        
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),RBLOCK                                                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
CNVTP300 SR    R5,R5                                                            
         IC    R5,4(,R1)           GET NUMBER OF PARMS IN BLOCK                 
*                                                                               
CNVTP305 TM    2(R2),X'80'         NUMBER?                                      
         BZ    CNVTP310            CHECK SPECIAL TRANS TYPES                    
         L     R1,4(,R2)           IF NUMBER THEN LOAD INTO R1                  
         CHI   R1,255              CAN'T BE > 255                               
         BNH   CNVTP380            SAVE IT AND GO FOR NEXT ONE                  
         MVC   FVMSGNO,=AL2(ACENO255)                                           
         B     CNVTP399                                                         
*                                                                               
CNVTP310 SR    R4,R4                                                            
         ICM   R4,1,0(R2)          LENGTH OF INPUT                              
         BNZ   CNVTP311                                                         
         MVC   FVMSGNO,=AL2(ACEIVTY)                                            
         B     CNVTP399            NOT OK                                       
*                                                                               
CNVTP311 BCTR  R4,0                                                             
*                                                                               
         L     R3,=A(TTYPETAB)                                                  
         A     R3,ACRELO                                                        
*                                                                               
CNVTP312 SR    R1,R1                                                            
         IC    R1,8(,R3)           GET TRANS TYPE FROM TABLE                    
         CLI   0(R3),0                                                          
         BNE   CNVTP315                                                         
         MVC   FVMSGNO,=AL2(ACEPRM)                                             
         EXMVC R4,FVXTRA,12(R2)                                                 
         B     CNVTP399            NOT OK                                       
*                                                                               
CNVTP315 MVC   TYPECDE,9(R3)       TRANSACTION TYPE CODE                        
         CLI   TYPECDE,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNL   CNVTP320                                                         
         GOTO1 VDICTAT,APPARM,C'SU  ',TYPECDE,0                                 
*                                                                               
CNVTP320 EXCLC R4,TYPECDE,12(R2)                                                
         BE    *+12                OK                                           
         LA    R3,13(,R3)          BUMP TO NEXT TRANS TYPE                      
         B     CNVTP312                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,8(,R3)           GET TRANS TYPE NUMBER FROM TABLE             
CNVTP380 STC   R1,0(,R6)           SAVE TRANS TYPE IN APPARM+0                  
         LA    R6,1(,R6)                                                        
         LA    R2,32(,R2)          NEXT SCANNER ENTRY                           
*                                                                               
CNVTP390 BCT   R5,CNVTP305                                                      
*                                                                               
CNVTP395 SR    RE,RE               SET CC = OK                                  
*                                                                               
CNVTP399 LTR   RE,RE                                                            
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO MAKE HEADLINES IN RCLEL, 0(R1) POINTS TO RCLEL           *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R6                                                        
R_MKHEAD DS    0H                                                               
         L     R6,0(,R1)           POINTS TO RCLEL                              
         XC    TEMPWORK,TEMPWORK                                                
         MVI   RCLHD1LN,0                                                       
         MVI   RCLHD2LN,0                                                       
         LA    RE,RCLNDATA         POINT TO DATA                                
         SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH OF DATA FIELD                         
         AR    RE,RF               POINT TO END OF DATA FIELD                   
         CLC   APKEYHD,SPACES                                                   
         BNH   MKHD90              NO HEADS FOR THIS ONE                        
         CLI   RCLSPCL,RCLSPKY1    OR                                           
         BE    MKHD90              THIS ONE                                     
         CLI   RCLSPCL,RCLSPDTE    OR                                           
         BE    MKHD90              THIS ONE                                     
         MVC   TEMPWORK(L'APKEYHD),APKEYHD                                      
         GOTO1 VSQUASH,APPARM,TEMPWORK,(0,L'APKEYHD)                            
*                                                                               
         SR    R5,R5                                                            
         SR    R1,R1                                                            
         L     RF,APPARM+4         LENGTH OF SQUASHED TEXT                      
         LA    RF,1(,RF)           ADD ONE TO GET TO BLANK                      
         LA    RE,TEMPWORK                                                      
         MVI   WORDNUM,0                                                        
         MVI   WORDONE,0                                                        
         MVI   WORDBRK,0                                                        
         MVI   WORDEND,0                                                        
*                                                                               
MKHD10   CLI   0(RE),C' '                                                       
         BH    MKHD20                                                           
         LA    R5,1(,R5)           WORD COUNT                                   
         CLI   WORDONE,0                                                        
         BNE   *+8                                                              
         STC   R1,WORDONE          END OF FIRST WORD                            
         CHI   R1,L'RCLNHDL1                                                    
         BH    *+8                                                              
         STC   R1,WORDBRK          WORDS UNDER 12 CHARACTERS                    
         STC   R1,WORDEND                                                       
*                                                                               
MKHD20   LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,MKHD10                                                        
         STC   R5,WORDNUM          NUMBER OF WORDS                              
*                                                                               
         CLI   WORDNUM,2                                                        
         BH    MKHD40                                                           
         MVC   WORDBRK,WORDONE                                                  
         B     MKHD60              ONLY ONE WORD                                
*                                                                               
MKHD40   SR    R1,R1                                                            
         CLI   WORDBRK,0           MORE THAN 2 WORDS BUT < 12 CHAR?             
         BNE   MKHD60                                                           
         SR    RF,RF                                                            
         IC    RF,WORDNUM                                                       
         TM    WORDNUM,X'01'       IS IT ODD OR EVEN                            
         BZ    *+8                                                              
         AHI   RF,1                MAKE EVEN                                    
         SRL   RF,1                DIVIDE BY TWO                                
         LA    RE,TEMPWORK                                                      
         IC    R1,WORDEND          LENGTH OF ALL WORD(S)                        
*                                                                               
MKHD45   CLI   0(RE),C' '          FIND THE NTH WORD (RF)                       
         BNH   MKHD48                                                           
         LA    RE,1(,RE)           BUMP UP IN TEMPWORK                          
         BCT   R1,MKHD45           ONE LESS IN TOTAL LENGTH OF INPUT            
         DC    H'00'                                                            
*                                                                               
MKHD48   BCT   RF,MKHD45           LESS ONE WORD OF 1/2 TOTAL WORDS             
*                                                                               
         LA    RF,TEMPWORK                                                      
         SR    RE,RF                                                            
         STC   RE,WORDBRK          LENGTH TO MOVE TO FIRST HEAD                 
*                                                                               
MKHD60   LA    RE,RCLNDATA         POINT TO   DATA FIELD   FIRST                
         SR    R1,R1                                                            
         IC    R1,RCLDATLN         GET   LEN  OF   DATA                         
         AR    RE,R1               POINT TO   HEADING  LOCATION                 
         IC    R1,WORDBRK                                                       
         CLI   WORDBRK,L'RCLNHDL1  IS    THE  DATA TOO BIG FOR HDR 1 ?          
         BNH   *+8                 NO,   SKIP                                   
         LA    R1,L'RCLNHDL1       USE   MAX  LEN  FOR HDR 1                    
         STC   R1,RCLHD1LN         SAVE  LEN  OF   HEADING 1                    
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),TEMPWORK                                                
         LA    RE,1(R1,RE)         POINT TO   HEAD 2                            
         CLI   WORDNUM,1           ONLY  ONE  WORD ?                            
         BNE   MKHD70              NO,   CONTINUE                               
         CLI   WORDBRK,L'RCLNHDL1  WAS   THE  DATA TOO BIG FOR HDR 1 ?          
         BNH   MKHD90              NO,   DONE                                   
         LA    R5,TEMPWORK+1(R1)   ->    NEXT CHAR IN  WORD                     
         B     MKHD75              OUTPUT     THE  REST    IN  HDR 2            
*                                                                               
MKHD70   LA    R5,TEMPWORK+2(R1)   POINT TO   NEXT WORD(S)                      
*                                                                               
MKHD75   IC    R1,WORDEND                                                       
         LA    R1,TEMPWORK(R1)                                                  
         SR    R1,R5                                                            
         BNP   MKHD90              NO MORE DATA                                 
         CHI   R1,L'RCLNHDL2       WILL  THE  DATA FIT ?                        
         BNH   *+8                 YES,  SKIP                                   
         LA    R1,L'RCLNHDL2       USE   MAX  LEN  FOR HDR 2                    
         STC   R1,RCLHD2LN         SAVE  LEN  OF   HEADING 2                    
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(R5)                                                   
         LA    RE,1(R1,RE)         POINT TO END                                 
*                                                                               
MKHD90   LA    RF,RCLEL            RF = BEGINING OF ELEMENT                     
         SR    RE,RF               RE = END      OF ELEMENT                     
         STC   RE,RCLLN            SAVE NEW LENGTH                              
*                                                                               
MKHDXIT  B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO CHECK ON SECURITY OF FIELDS (1-32) OR (33-64)            *         
*         P1 = BIT SETTINGS 1-32                                      *         
*         P2 = START REFERANCE FOR FIELD NUMBER                       *         
***********************************************************************         
         SPACE 1                                                                
R_FMTSEC DS    0H                                                               
         LR    R6,R1               SAVE ADDRESS OF PARAMETER LIST               
         ICM   R3,15,0(R1)         GET SECURITY BITS                            
         BZ    FMTSECOK                                                         
         LA    R0,32               UP TO 32 LEVELS                              
         LA    R2,1                BIT SHIFT REGISTER (POWER OF 2)              
         MVC   RBYTE,7(R1)         GET STARTING POINT                           
*                                                                               
FMTSEC10 LR    RF,R2                                                            
         NR    RF,R3                                                            
         BZ    FMTSEC20            NOT USED                                     
         GOTO1 VSECRET,RPARM,('SECPFLDP',ACASEC),RBYTE                          
         BL    FMTSECNO                                                         
*                                                                               
FMTSEC20 SLL   R2,1                TRY NEXT LEVEL                               
         IC    RF,RBYTE            COUNT UP FROM START TO (START + 32)          
         LA    RF,1(,RF)                                                        
         STC   RF,RBYTE            NEXT NUMBER                                  
         BCT   R0,FMTSEC10                                                      
*                                                                               
FMTSECOK XC    0(4,R6),0(R6)                                                    
         B     COMMONX                                                          
*                                                                               
FMTSECNO MVC   0(1,R6),RBYTE       SHOW WHICH ONE FAILED                        
         B     COMMONX                                                          
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO VALIDATE PARAMETERS FOR SPECIAL KEYWORDS                 *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R6                                                       
R_VALSPL DS    0H                                                               
         L     R6,0(,R1)           GET ADDRESS OF DEFTABLE ENTRY                
         MVC   TEMPWORK(1),4(R1)   GET MAX NUMBER OF COLUMNS                    
         L     R2,4(,R1)           GET ADDRESS OF BLOCK                         
         MVC   TEMPWORK+1(1),8(R1) GET CURRENT COLUMN #                         
         L     R5,8(,R1)           GET ADDRESS OF COLUMN ARRAY                  
         CLI   DEFSPCL,DEFEXRT     EXCHANGE RATE ?                              
         BE    VALSP010                 OR                                      
         CLI   DEFSPCL,DEFSUM      CUME() OR SUM() TYPE COLUMN ?                
         BE    VALSP010                 OR                                      
*&&UK*&& CLI   DEFSPCL,DEFMAP      TYMAP()                                      
*&&UK*&& BE    VALSP010                 OR                                      
         CLI   DEFSPCL,DEFSIGN     SIGN() ?                                     
         BNE   VALSPOK             NO,  SKIP                                    
                                                                                
VALSP010 CLI   DEFSPCLI,0          NO PARAMETERS?                               
         BE    VALSPOK                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(ACEPRM)  INVALID PARAMETER                          
         ZIC   R4,1(,R2)             LENGTH OF PARAMETER                        
         BCTR  R4,0                  MINUS ONE FOR C')'                         
         LA    R3,22(,R2)            POINT TO 2ND PART                          
         TM    DEFSPCLI,DEFCOL#      COLUMN PARAMETER?                          
         BZ    VALSP100                                                         
*                                                                               
*                                    HARD CODE UNTIL CHANGED ALL OVER -         
*                                    THIS REQUIRES A DATA BASE UPDATE           
*        CLC   0(1,R3),APCOLCHR      IS IT A COLUMN CHAR, C'C' US               
         CLI   0(R3),C'C'            IS IT A COLUMN CHAR, C'C' US               
         BNE   VALSP100              ASSUME CONSTANT                            
         MVC   FVMSGNO,=AL2(ACEIVCN) INVALID COLUMN NUMBER                      
         BCTR  R4,0                  MINUS ONE FOR COLUMN INDICATOR             
         LA    R3,1(,R3)                                                        
         GOTO1 VCASHVAL,APPARM,(C'N',(R3)),(R4)                                 
         CLI   APPARM,0            VALID NUMBER?                                
         BNZ   VALSP200                                                         
         CLI   APPARM+7,0          NO COLUMN 0'S                                
         BE    VALSP200                                                         
         CLC   APPARM+7(1),TEMPWORK  MAX # OF COLUMNS EXCEEDED                  
         BH    VALSP200                                                         
         SR    RE,RE                                                            
         ICM   RE,1,APPARM+7         COLUMN NUMBER                              
         LA    RF,0(RE,R5)                                                      
         MVC   0(1,RF),TEMPWORK+1                                               
         B     VALSPOK                                                          
*                                                                               
VALSP100 TM    DEFSPCLI,DEFCNST    CONSTANT PARAMETER?                          
         BZ    VALSP200                                                         
         MVC   FVMSGNO,=AL2(ACECNST)  INVALID CONSTANT NUMBER                   
         GOTO1 VCASHVAL,APPARM,(C'N',(R3)),(R4)                                 
         CLI   APPARM,0                                                         
         BE    VALSPOK                                                          
*                                                                               
VALSP200 BCTR  R4,0                SHOW WHAT IS WRONG                           
         EX    R4,*+8                                                           
         B     VALSPNO                                                          
         MVC   FVXTRA(0),0(R3)                                                  
*                                                                               
VALSPOK  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSPNO  B     COMMONX                                                          
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  SEE IF REVISION OR PLANNING ESTIMATE AND SET TO REFIND IN KEYWORDS *         
***********************************************************************         
         SPACE 1                                                                
         USING JBCLD,R2                                                         
ESTIMATE NTR1                                                                   
         LA    R2,ESTWRK                                                        
         CLI   JBCLTYP,JBCLCOL     SINGLE TYPE ONLY                             
         BNE   ESTMATNO                                                         
         CLI   JBCLCN1E,C'R'       REVISION TYPE?                               
         BE    ESTMAT10                                                         
         CLI   JBCLCN1E,C'P'       PLANNING TYPE?                               
         BNE   ESTMATNO                                                         
*                                                                               
ESTMAT10 MVC   AC00PASS,JBCLCN1V   SAVE TO PASS TO CALLER                       
         GOTO1 FLEN2,RPARM,(L'APKEYWRD,APKEYWRD)                                
         SR    R0,R0                                                            
         LA    R3,APKEYWRD                                                      
         LA    R4,RXKEYWRD                                                      
         MVC   RXKEYWRD,SPACES                                                  
*                                                                               
ESTMAT20 CLI   0(R3),C'0'          IS  IT A NUMBER?                             
         BNL   ESTMAT25            YES                                          
         MVC   0(1,R4),0(R3)       NO, SO MOVE TO RWORK                         
         LA    R4,1(,R4)           NEW SPACE TO BUILD IN                        
         B     ESTMAT28                                                         
*                                                                               
ESTMAT25 LTR   R0,R0               DID  WE DO THIS CODE ONCE ALREADY ?          
         BNZ   ESTMAT28            YES, SO JUST MOVE ON                         
         LA    R0,1                SET TO SAY SET DONE ONCE                     
         MVI   0(R4),C'#'          SYMBOLIZE NUMBER IN KEYWORD                  
         LA    R4,1(,R4)           NEW SPACE TO BUILD IN                        
*                                                                               
ESTMAT28 LA    R3,1(,R3)           NEXT CHARACTER                               
         BCT   RF,ESTMAT20                                                      
*                                                                               
         SR    R1,R1                                                            
         IC    R1,JBCLCN1V         GET REV OR PLAN #                            
         CVD   R1,RDUB                                                          
         LA    RF,3                                                             
         LA    RE,RWORK                                                         
         OI    RDUB+7,X'0F'                                                     
         UNPK  RWORK(3),RDUB                                                    
*                                                                               
ESTMAT30 CLI   0(RE),C'0'          REMOVE LEADING ZEROS                         
         BNE   ESTMAT35                                                         
         LA    RE,1(,RE)                                                        
         BCT   RF,ESTMAT30                                                      
         DC    H'00'               SHOULDN'T ALL BE ZERO                        
*                                                                               
ESTMAT35 STC   RF,RXTRALN          SIZE OF NUMBER IN CHAR FORM                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RXTRA(0),0(RE)      SAVE NUMBER IN CHAR FORM                     
*                                                                               
ESTMATOK SR    RF,RF                                                            
         LA    R4,RXKEYWRD                                                      
*                                                                               
ESTMATNO LTR   RF,RF                                                            
         XIT1  REGS=(R4)                                                        
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  SEE IF WE MAY HAVE A NON-JOBBER KEYWORD WITH AN EMBEDDED # SIGN    *         
*  THE NUMBER MUST BE IN THE RANGE OF 1-99                            *         
***********************************************************************         
         SPACE 1                                                                
ESTIMAT2 NTR1                                                                   
         MVC   RSVKYWRD,APKEYWRD   SAVE INPUT     KEYWORD                       
         GOTO1 FLEN2,RPARM,(L'APKEYWRD,APKEYWRD)                                
         LA    R3,APKEYWRD         ->   INPUT     KEYWORD                       
         LA    R4,RXKEYWRD         ->   OUTPUT    KEYWORD                       
         SR    R6,R6               LENGTH    OF   NUMBER                        
         LA    RE,RWORK            ->   NUMERIC   AREA                          
         MVC   RXKEYWRD,SPACES                                                  
         MVC   RWORK,SPACES                                                     
*                                                                               
EST2010  DS    0H                  PROCESS   INPUT     CHARACTERS               
         CLI   0(R3),C'0'          IS   IT   A    NUMBER ?                      
         BL    EST2030             NO,  SAVE CHARACTER                          
*                                                                               
*                                  FOUND     NUMERIC                            
         LTR   R6,R6               1ST  NUMERIC   CHARACTER ?                   
         BNZ   EST2030             NO,  SO   JUST MOVE IT                       
         MVI   0(R4),C'#'          SYMBOLIZE NUMBER    IN   KEYWORD             
         LA    R4,1(,R4)           NEXT OUTPUT    CHARACTER                     
*                                                                               
EST2020  DS    0H                  SAVE NUMERIC                                 
         MVC   0(1,RE),0(R3)       SAVE NUMERIC   CHARACTER                     
         LA    R6,1(,R6)           BUMP LENGTH    OF   NUMERIC                  
         LA    RE,1(,RE)           NEXT NUMERIC   CHARACTER                     
         LA    R3,1(,R3)           NEXT INPUT     CHARACTER                     
         SH    RF,=H'01'           NO   MORE INPUT     CHARACTERS ?             
         BZ    EST2040             YES, DONE                                    
         CLI   0(R3),C'0'          IS   IT   A    NUMBER ?                      
         BNL   EST2020             YES, SAVE THE  NUMERIC                       
*                                                                               
EST2030  DS    0H                  SAVE INPUT     CHARACTER                     
         MVC   0(1,R4),0(R3)       MOVE TO   RXKEYWRD                           
         LA    R4,1(,R4)           NEXT OUTPUT    CHARACTER                     
         LA    R3,1(,R3)           NEXT INPUT     CHARACTER                     
         BCT   RF,EST2010                                                       
*                                                                               
EST2040  DS    0H                  END  OF   KEYWORD                            
         SH    R6,=H'01'           MINUS     ONE  FOR  EXECUTE                  
         BM    EST2NO              NO   NUMERICS, INVALID                       
         CH    R6,=H'01'           MORE THAN TWO  NUMERIC   CHARACTERS?         
         BH    EST2NO              YES, INVALID                                 
         EX    R6,EST2PACK         PACK THE  NUM  INTO RDUB                     
         CVB   R1,RDUB             CONVERT   TO   BINARY                        
         LTR   R1,R1               IS   IT   ZERO ?                             
         BZ    EST2NO              YES, INVALID                                 
         STC   R1,AC00PASS         SAVE TO   PASS TO   CALLER                   
*                                                                               
*                                  FOUND     NUMERIC   1-99                     
         CLI   RWORK,C'0'          WAS  1ST  CHAR A    C'0' ?                   
         BE    EST2060             YES, USE  2ND  BYTE                          
         MVC   RXTRA(2),RWORK      SAVE NUMERIC   DATA                          
*                                      (MAY  HAVE MOVED    EXTRA SPACE)         
         LA    R6,1(,R6)           RESTORE   LENGTH                             
         STC   R6,RXTRALN          SIZE OF   NUMBER                             
         B     EST2OK                                                           
*                                                                               
EST2060  DS    0H                                                               
         MVC   RXTRA(1),RWORK+1    SAVE NUMERIC   DATA                          
         MVI   RXTRALN,1           ONE  BYTE OF   DATA                          
*                                                                               
EST2OK   DS    0H                                                               
         SR    RE,RE                                                            
         LA    R4,RXKEYWRD         ->   NEW  KEYWORD                            
*                                                                               
EST2NO   DS    0H                                                               
         LTR   RE,RE                                                            
         XIT1  REGS=(R4)                                                        
*                                                                               
EST2PACK PACK  RDUB,RWORK(0)       PACK RWORK     INTO RDUB                     
         EJECT ,                                                                
***********************************************************************         
*  FIND LENGTH OF DATA                                                *         
*        INPUT  P1 = BYTE 1   LENGTH OF FIELD                         *         
*                  = BYTE 2-4 A(FIELD)                                *         
*                                                                     *         
*        OUTPUT RF = LENGTH                                           *         
***********************************************************************         
         SPACE 1                                                                
FLEN2    SR    RF,RF               RF   WILL HAVE LENGTH                        
         ICM   RF,1,0(R1)          GET  LENGTH    TO   CHECK                    
         BZR   RE                  NONE,     SO   LEN=0    (RF=0)               
         L     R1,0(,R1)           ->   FIELD                                   
         AR    R1,RF               ->   LAST CHARACTER                          
         BCTR  R1,0                                                             
*                                                                               
FLEN210  CLI   0(R1),C' '          IS   CHARACTER BLANK                         
         BH    FLEN2EX             RF=  LENGTH                                  
         BCTR  R1,0                BUMP DOWN IN   AREA                          
         BCT   RF,FLEN210          TRY  NEXT CHARACTER                          
*                                  NONE LEFT,     RF=0                          
FLEN2EX  BR    RE                  RETURN,   RF   HAS  LENGTH                   
         EJECT ,                                                                
***********************************************************************         
*        OPTION ROUTINES                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RWRKD,RC                                                         
         USING TWAD,R5                                                          
OPTIONS  NTR1  BASE=ACBASE1,WORK=(RC,RWRKX-RWRKD),LABEL=NO                      
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         L     R5,ATWA                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         CH    RF,=AL2(OPTMAX)                                                  
         BNH   *+2(RF)                                                          
         DC    H'0'                NO SUCH ROUTINE                              
         SPACE 2                                                                
OPTRTNS  B     OPTRRQD             REQUEST DETAILS                              
         B     OPTROPI             SET INOPT 1-4 TO INOPTI                      
         B     OPTRFLT             SET FILTER VALUE                             
         B     OPTRDTE             DATE RANGE IN PACKED FORM                    
         B     OPTRVER             VERSION NUMBER                               
*                                                                               
OPTMAX   EQU   *-OPTRTNS                                                        
         SPACE 2                                                                
OPTIONX  B     EXIT                                                             
         EJECT ,                                                                
OPTRRQD  SR    RF,RF                                                            
         CLC   FVIFLD(1),APYES        C'Y' FOR ALL FORMATS AND REQUEST          
         BNE   *+8                                                              
         LA    RF,INOAEXPY                                                      
         CLC   FVIFLD(1),APONLY       C'O' FOR ONLY ONES WITH REQUEST           
         BNE   *+8                                                              
         LA    RF,INOAEXPO                                                      
         CLC   FVIFLD(1),APNO         C'N' FOR ONLY ONES W/O REQUEST            
         BNE   *+8                                                              
         LA    RF,INOAEXPN                                                      
         STC   RF,SCWORK                                                        
         LTR   RF,RF                                                            
         BNZ   OPTIONX                                                          
         MVC   FVMSGNO,=AL2(FVFDINV)                                            
         CLI   FVILEN,0            ANY INPUT?                                   
         BNZ   OPTIONX                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    SCWORK,INOAEXPY                                                  
         B     OPTIONX                                                          
         EJECT ,                                                                
OPTROPI  MVC   SCWORK(L'INOPTNS),INOPTI                                         
         MVC   INOPTNS,INOPTI      SET INOPT 1-4 TO INOPTI                      
         TM    INOPT3,INPSWD       Password option ?                            
         BZ    OPTIONX                                                          
*&&US*&& CLC   =C'##%%',FVIFLD                                                
*&&UK*&& CLC   =C'ACCENT',FVIFLD                                                
         BNE   OPTIONX                                                          
         OI    GENIND,GENPSWD      Set on                                       
         MVC   SCROPT,SPACES                                                    
         OI    SCROPTH+6,FVOXMT                                                 
         B     OPTIONX                                                          
         EJECT ,                                                                
OPTRFLT  MVC   SCWORK,SPACES                                                    
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
         EXMVC R1,SCWORK,FVIFLD                                                 
         MVC   INOPTNS,INOPTI      SET INOPT 1-4 TO INOPTI                      
         B     OPTIONX                                                          
         EJECT ,                                                                
OPTRDTE  LA    R1,RELEMENT                                                      
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD             DTE1  DTE2                  
         MVI   SOFITYPE,SOFITYMD                                                
         MVI   SOFIINDS,SOFIIANY+SOFIIF1O+SOFIIF2O+SOFIISLH+SOFIIRES            
         MVI   SOFOTYPE,SOFOTSD5                                                
         LA    R0,FVIHDR                                                        
         ST    R0,SOFAINP                                                       
         LA    R0,SCWORK                                                        
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOM                                                     
         MVI   SOFSYSN,6                                                        
         MVC   SOFLANG,CULANG                                                   
         GOTO1 ASOFDAT,(R1)                                                     
         BZ    *+14                                                             
         MVC   FVMSGNO,SOFERROR                                                 
         B     *+8                                                              
         OI    INOPTA,INOADTES     INDICATE DATE WAS INPUT                      
*                                                                               
OPTRDT90 MVC   INOPTNS,INOPTI                                                   
         B     OPTIONX                                                          
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  VERSION.MODIFICATION NUMBER (OPTION IS VER=VVVVV.MMM)              *         
***********************************************************************         
         SPACE 1                                                                
OPTRVER  SR    RF,RF                                                            
         IC    RF,FVILEN           LENGTH OF RHS OF OPTION                      
         LA    R3,FVIFLD           RHS    INPUT                                 
OPTRVR10 CLI   0(R3),C'.'          FIND PLACE HOLDER                            
         BE    OPTRVR15                                                         
         LA    R3,1(,R3)           BUMP UP INTIL WE FIND C'.'                   
         BCT   RF,OPTRVR10                                                      
         B     OPTRVR20                                                         
*                                                                               
OPTRVR15 LA    R3,1(,R3)           POINT PAST C'.'                              
         BCTR  RF,0                LESS ONE FOR MODIFICATION NUMBER LEN         
         GOTO1 VCASHVAL,RPARM,(C'N',(R3)),(RF)                                  
         CLI   0(R1),0                                                          
         BNE   OPTRVR80                                                         
         CLC   4(4,R1),=F'255'     MODIFY MUST BE LESS THEN 256                 
         BH    OPTRVR80                                                         
         MVC   SCWORK+2(1),7(R1)   MOVE IN BINARY VALUE                         
*                                                                               
OPTRVR20 LA    RE,FVIFLD           VERSION NUMBER                               
         SR    R3,RE               FIND LENGTH OF VERSION NUMBER                
         BCTR  R3,0                                                             
         GOTO1 VCASHVAL,RPARM,(C'N',FVIFLD),(R3)                                
         CLI   0(R1),0                                                          
         BNE   OPTRVR80                                                         
         MVC   SCWORK(2),6(R1)     GET BINARY VALUE                             
         B     OPTRVR90                                                         
*                                                                               
OPTRVR80 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
OPTRVR90 MVC   INOPTNS,INOPTI                                                   
         B     OPTIONX                                                          
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
         SPACE 1                                                                
ACCDIR   DC    CL8'ACCDIR'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
         EJECT ,                                                                
         LTORG                                                                  
         DROP  R5,R7,R8,R9,RA,RB,RC                                             
         EJECT ,                                                                
***********************************************************************         
*  TRANSLATE LANGUAGE SOFT FORMAT RECORD (READ OR WRITE)              *         
*                                                                     *         
*        INPUT  P1 = BYTE 1   "R",READREC,  TRANSLATE RECORD          *         
*                             "W",WRITEREC, TRANSLATE RECORD          *         
*                             "E" READELM,  TRANSLATE ELEMENT         *         
*                             "F" WRITEELM, TRANSLATE ELEMENT         *         
*                             "G" READELS,  TRANSLATE ELEMENTS        *         
*                             "H" WRITEELS, TRANSLATE ELEMENTS        *         
*                    BYTE 2-4  A(FORMAT RECORD)                       *         
*               P2 = BYTE 1-4  A(0)       ACTION= READREC, WRITEREC   *         
*                              A(ELEMENT) ACTION= READELM, WRITEREC   *         
*                                                 READELS, WRITEELS   *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R4                                                       
         USING WORKD,R7                                                         
         USING TRLWRKD,RC                                                       
TRNSLATE NMOD1 TRLWRKX-TRLWRKD,**TRLT**,RA,R9,CLEAR=YES                         
         L     R4,0(,R1)                                                        
         LA    R4,0(,R4)           CLEAR HOB                                    
         ST    R4,TRLAIO           BEGINING OF RECORD                           
         LR    R6,R4                                                            
         L     R3,ASAVEIO          INITIALIZE IN CASE WE DELETE AND ADD         
         XC    0(80,R3),0(R3)      ELEMENTS                                     
         MVC   0(42,R3),0(R4)                                                   
         LH    RF,DATADISP                                                      
         LA    RF,1(,RF)                                                        
         STH   RF,RESRLEN-RESRECD(R3)                                           
         MVI   TRLNEWEL,NO         NO ELEMENTS TO DELETE AND ADD                
         XC    RELEMCDE,RELEMCDE                                                
         XC    TRLSTYEL,TRLSTYEL                                                
         MVI   RSINGLE,NO          PROCESS ONLY THESE ELEMENTS                  
         MVC   RACTION,0(R1)                                                    
         CLI   0(R1),READREC       TRANSLATE ENTIRE REC TO READABLE             
         BE    TRNS015             CODES                                        
         CLI   0(R1),WRITEREC      TRANSLATE ENTIRE REC TO DICTONARY            
         BE    TRNS015             CODES                                        
*                                                                               
         L     R6,4(,R1)           A(ELEMENT)                                   
         LTR   R6,R6                                                            
         BZ    TRNSLTX             BAD PARAMETER                                
         MVC   RELEMCDE,0(R6)      SAVE OF ELEMENT CODE                         
         MVI   RACTION,READ        SET ACTION                                   
         MVI   RSINGLE,YES         PROCESS ONLY ONE ELEMENT                     
         CLI   0(R1),READELM       TRANSLATE ELEMENT  TO READABLE CODE          
         BE    TRNS018                                                          
         MVI   RSINGLE,NO          PROCESS ONLY THESE ELEMENTS                  
         CLI   0(R1),READELS       TRANSLATE ELEMENTS TO READABLE CODE          
         BE    TRNS018                                                          
*                                                                               
         MVI   RACTION,WRITE                                                    
         MVI   RSINGLE,YES         PROCESS ONLY ONE ELEMENT                     
         CLI   0(R1),WRITEELM      TRANSLATE ELEMENT  TO DICT# CODE             
         BE    TRNS018                                                          
         MVI   RSINGLE,NO          PROCESS ONLY THESE ELEMENTS                  
         CLI   0(R1),WRITEELS      TRANSLATE ELEMENTS TO DICT# CODE             
         BE    TRNS018                                                          
         DC    H'00'               NO SUCH ACTION                               
*                                                                               
TRNS015  CLI   RESKTYP,RESKTYPQ    X'2D' TYPE RECORD?                           
         BNE   TRNSLTX                                                          
         CLI   RESKSUB,RESKSUBQ    SCRIBE FORMATS?                              
         BNE   TRNSLTX                                                          
         AH    R6,DATADISP                                                      
*                                                                               
TRNS018  MVC   RSVMODE,REPMODE     SAVE CURRENT REPMODE                         
         CLI   RACTION,WRITE       WRITING A RECORD                             
         BNE   TRNS500                                                          
         MVI   TRLSEC#1,0          INITIALIZE KEYWORD SECURITY BIT              
         LA    RE,TRLTAB                                                        
         SR    RF,RF                                                            
         IC    RF,SCCOMMA          GET LANGUAGE SOFT VALUE OF COMMA             
         AR    RE,RF               BUILD TRANSLATE TABLE                        
         MVI   0(RE),X'FF'         REPLACE COMMAS WITH X'01'S                   
         DROP  R4                                                               
*                                                                               
TRNS020  CLI   0(R6),0             END OF RECORD                                
         BE    TRNS950                                                          
         CLI   RELEMCDE,0          IGNORE THIS CODE                             
         BE    TRNS022                                                          
         CLI   RSINGLE,YES         PROCESS ONE ELEMENT?                         
         BE    TRNS022             YES, SO SKIP CODE                            
         CLC   RELEMCDE,0(R6)      DOES IT MATCH                                
         BNE   TRNS025             NO                                           
*                                                                               
TRNS022  CLI   0(R6),STYELQ        X'25' REPORT TYPE                            
         BE    TRNS050                                                          
         CLI   0(R6),RHDELQ        X'C1' REPORT HEADING                         
         BE    TRNS100                                                          
         CLI   0(R6),RRWELQ        X'C2' REPORT ROW                             
         BE    TRNS200                                                          
         CLI   0(R6),RCLELQ        X'C3' REPORT COLUMN                          
         BE    TRNS300                                                          
         CLI   0(R6),RFLELQ        X'C5' REPORT FILTER                          
         BE    TRNS400                                                          
*                                                                               
TRNS025  CLI   RSINGLE,YES         PROCESS ONLY ONE                             
         BE    TRNS950                                                          
         SR    R1,R1                                                            
         IC    R1,1(,R6)           NEXT ELEMENT                                 
         AR    R6,R1                                                            
         B     TRNS020                                                          
         EJECT ,                                                                
         USING STYELD,R6                                                        
         USING REPTABD,R3                                                       
TRNS050  ST    R6,TRLSTYEL                                                      
         CLI   STYNAME,ESCHIGHQ    IS IT TRANSLATED ALREADY?                    
         BL    TRNS025             YES, SO DON'T RE-TRANSLATE                   
*                                                                               
         L     R3,ACTYPTAB                                                      
TRNS052  CLI   0(R3),EOT           END  OF TABLE ?                              
         BE    TRNS025             YES, DON'T TRANSLATE                         
         MVC   TRLRCDE,REPCODE                                                  
         MVC   TRLRTYP,REPSTYPE                                                 
         CLI   TRLRCDE,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNL   TRNS054                                                          
         GOTO1 VDICTAT,TRLPRMS,C'TU  ',('TRLRLNQ',TRLRCDE),0                    
*                                                                               
TRNS054  CLC   STYNAME,TRLRCDE                                                  
         BNE   *+10                NO, SO GET NEXT                              
         CLC   STYCODE,REPNUM      MATCH ON REPORT NUMBER                       
         BE    TRNS055             FOUND MATCH                                  
         LA    R3,REPLNQ(,R3)      BUMP TO NEXT                                 
         B     TRNS052                                                          
*                                                                               
TRNS055  MVC   STYNAME,REPCODE     LANG SOFT TRANSLATION                        
         B     TRNS025                                                          
         DROP  R3,R6                                                            
         EJECT ,                                                                
         USING RHDELD,R6                                                        
TRNS100  TM    RHDFRM,RHDDEF       MUST BE DEFINED KEYWORD                      
         BZ    TRNS025                                                          
         TM    RHDFRM,RHDDICT      IS IT TRANSLATED ALREADY?                    
         BO    TRNS025             DON'T TRANSLATE AGAIN                        
         MVI   REPMODE,REPHEAD     PROCESS HEADING                              
         XC    TRLFLDH,TRLFLDH                                                  
         MVC   TRLFLD,SPACES                                                    
         SR    R2,R2                                                            
         IC    R2,RHDLN            ELEMENT LENGTH                               
         SHI   R2,RHDLNQ+1                                                      
         BM    TRNS025             THIS DON'T MAKE SENSE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   TRLFLD(0),RHDDATA                                                
         MVI   TRLFLDH+5,12                                                     
*                                                                               
         USING DEFTABD,R1                                                       
NEW      USING RHDELD,TRLELEM                                                   
*                                                                               
         GOTO1 VALDEF,TRLFLDH                                                   
         BNE   TRNS025             SKIP THIS ONE (SOMETHING WRONG)              
         CLI   DEFCODE,ESCHIGHQ    IS IT A ESCAPE SEQUENCE?                     
         BNL   TRNS025             NO SO SKIP                                   
         CLI   APUSRKYW,YES                                                     
         BE    TRNS025             USER DEFINED KEYWORD, SKIP                   
         OI    RHDFRM,RHDDICT                                                   
         XC    TRLELEM,TRLELEM                                                  
         MVC   NEW.RHDEL(RHDLNQ),RHDEL     SAVE OFF ELEMENT                     
         CLC   DEFDDNUM,=AL2(AC#RSBLK)                                          
         BNE   TRNS150                                                          
         LA    RF,RHDDATA                                                       
         AR    RF,R2                   POINT TO LAST CHARACTER (NUMBER)         
         MVC   NEW.RHDXDATA,0(RF)      SAVE OFF CHARACTER NUMBER                
         CLI   NEW.RHDXDATA,C'0'       WAS IT A VALID NUMBER  ?                 
         BH    *+8                     YES                                      
         MVI   NEW.RHDXDATA,C'1'              DEFAULT TO ONE                    
         NI    NEW.RHDXDATA,TURNOFF-X'F0'     MAKE IT BINARY                    
*                                                                               
TRNS150  MVC   NEW.RHDDATA(2),DEFDDNUM        MOVE IN DICT#                     
         MVI   NEW.RHDLN,RHDLNQ+2                                               
*                                                                               
TRNS160  MVI   RHDEL,X'FF'         DELETE                                       
         MVI   TRLNEWEL,YES                                                     
         L     R1,ASAVEIO                                                       
         GOTO1 TRLADDEL            ADD TRANSLATED ELEMENT TO TEMP IO            
         B     TRNS025                                                          
         DROP  R1,R6                                                            
         DROP  NEW                                                              
         EJECT ,                                                                
NEW      USING RRWELD,TRLELEM                                                   
         USING RRWELD,R6                                                        
*                                                                               
TRNS200  TM    RRWOPT2,RRWDICT     TRANSLATED ALREADY?                          
         BO    TRNS025             YES, SO DON'T RE-TRANSLATE                   
         MVI   REPMODE,REPROW      PROCESS ROWS                                 
         XC    TRLFLDH,TRLFLDH                                                  
         SR    RF,RF                                                            
         IC    RF,RRWDATLN         LENGTH OF DATA                               
         STC   RF,TRLFLDH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TRLFLD(0),RRWNDATA                                               
         GOTO1 CHR2NUM,(R6)                                                     
         BNE   TRNS225                                                          
         MVC   NEW.RRWEL(RRWNLNQ),RRWEL                                         
         SR    RF,RF                                                            
         IC    RF,TRL#PRMS         GET NUMBER OF PARAMETER + KEYWORD            
         SLL   RF,1                MULTIPLY BY 2, LENGTH OF DATA                
         STC   RF,NEW.RRWDATLN                                                  
         LA    RE,NEW.RRWNDATA(RF)         POINT TO END OF ELEMENT              
         BCTR  RF,0                                                             
         EX    RF,*+8                      SAVE OFF DATA                        
         B     *+10                                                             
         MVC   NEW.RRWNDATA(0),RKEYDATA    SAVE GROUP OF DICTIONARY #'S         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,RRWDATLN                                                      
         LA    R1,RRWNDATA(R1)     POINT TO ROWNPRFX                            
         ICM   RF,1,RRWPFXLN       GET LENGTH OF PREFIX                         
         BZ    TRNS222                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)       MOVE IN PREFIX                               
         LA    RE,1(RF,RE)         POINT TO END OF ELEMENT                      
*                                                                               
TRNS222  OI    NEW.RRWOPT2,RRWDICT         DICTIONARY CONVERSION                
         CLI   RFIXCTL,0                   SPECIAL KEYWORD ?                    
         BE    TRNS224                     FINISHED WITH THIS ELEMENT           
         MVC   NEW.RRWXDATA,AC00PASS                                            
         MVC   NEW.RRWSPCL,RFIXCTL         USE  SPECIAL TYPE                    
*                                                                               
TRNS224  LA    RF,NEW.RRWEL        RF = START OF ELEMENT                        
         SR    RE,RF               RE = END   OF ELEMENT                        
         STC   RE,NEW.RRWLN                                                     
         MVI   RRWEL,X'FF'         DELETE ELEMENT PROCESSED                     
         MVI   TRLNEWEL,YES        SET FLAG TO ADD AT END                       
         L     R1,ASAVEIO                                                       
         GOTO1 TRLADDEL            ADD TRANSLATED ELEMENT TO TEMP IO            
         B     TRNS025             NEXT ELEMENT                                 
*                                                                               
TRNS225  SR    RF,RF                                                            
         IC    RF,RRWDATLN                                                      
*                                                                               
TRNS230  EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   RRWNDATA(0),TRLTAB  FIND COMMAS (DESTROYS R1 AND R2)             
         BZ    TRNS025             FINISHED                                     
         MVI   0(R1),X'01'         REPLACE COMMAS WITH X'01'S                   
         B     TRNS230                                                          
         DROP  R6                                                               
         DROP  NEW                                                              
         EJECT ,                                                                
         USING RCLELD,R6                                                        
NEW      USING RCLELD,TRLELEM                                                   
*                                                                               
TRNS300  TM    RCLOPT2,RCLDICT     TRANSLATED ALREADY?                          
         BO    TRNS025             YES, SO DON'T RE-TRANSLATE                   
         MVI   REPMODE,REPCOL      PROCESS COLUMNS                              
         XC    TRLFLDH,TRLFLDH                                                  
         CLI   RCLSPCL,RCLSPDTE    DATE1-DATE2 COLUMN ?                         
         BNE   TRNS320                                                          
         SR    RF,RF                                                            
         IC    RF,RCLDATLN                                                      
*                                                                               
         LR    R1,RF               SAVE OFF LENGTH OF DATA                      
         LA    RE,RCLNDATA                                                      
TRNS305  CLI   0(RE),C'-'          FIND DASH                                    
         BE    TRNS306                                                          
         LA    RE,1(,RE)                                                        
         BCT   RF,TRNS305                                                       
         DC    H'00'               SHOULD HAVE FOUND IT                         
*                                                                               
TRNS306  LA    RE,1(,RE)           BUMP PAST DASH                               
         ST    RE,TRLFULL          SAVE OFF ADDRESS                             
         SR    R1,RF               LENGTH OF KEYWORD                            
         BCTR  RF,0                                                             
         STC   RF,TRLBYTE          SAVE OFF LENGTH OF 2ND KEYWORD               
         STC   R1,TRLFLDH+5        SET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRLFLD(0),RCLNDATA                                               
*                                                                               
         USING DEFTABD,R1                                                       
         GOTO1 VALDEF,TRLFLDH      FIND DICTIONARY ENTRY                        
         BNE   TRNS325             DON'T TRANSLATE                              
         MVC   NEW.RCLEL(RCLNLNQ),RCLEL                                         
         MVI   NEW.RCLDATLN,4            SET TO 2 KEYWORDS                      
         MVC   NEW.RCLNDATA(2),DEFDDNUM  SAVE DICTIONARY # OF KEYWORD           
         L     RE,TRLFULL                POINT TO 2ND KEYWORD IN RCLELD         
         SR    RF,RF                                                            
         IC    RF,TRLBYTE                GET LENGTH OF 2ND KEYWORD              
         MVC   TRLFLD,SPACES                                                    
         STC   RF,TRLFLDH+5              SAVE OFF LENGTH                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TRLFLD(0),0(RE)                                                  
         GOTO1 VALDEF,TRLFLDH              FIND DICTIONARY ENTRY                
         BNE   TRNS325                     DON'T TRANSLATE                      
         MVC   NEW.RCLNDATA+2(2),DEFDDNUM  SAVE DICTIONARY # OF KEYWORD         
         LA    RE,NEW.RCLNDATA+4                                                
         SR    RF,RF                                                            
         IC    RF,RCLDATLN                                                      
         LA    R5,RCLNDATA(RF)     POINT TO END OF RCLNDATA                     
         B     TRNS321                                                          
         DROP  R1                                                               
*                                                                               
TRNS320  MVC   TRLFLDH+5(1),RCLDATLN                                            
         SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH OF DATA                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TRLFLD(0),RCLNDATA                                               
         LA    R5,RCLNDATA+1(RF)   POINT TO END OF RCLNDATA                     
         GOTO1 CHR2NUM,(R6)                                                     
         BNE   TRNS325                                                          
         MVC   NEW.RCLEL(RCLNLNQ),RCLEL                                         
         SR    RF,RF                                                            
         IC    RF,TRL#PRMS         GET NUMBER OF PARAMETER + KEYWORD            
         SLL   RF,1                MULTIPLY BY 2, LENGTH OF DATA                
         STC   RF,NEW.RCLDATLN                                                  
         LA    RE,NEW.RCLNDATA(RF)                                              
         BCTR  RF,0                                                             
         EX    RF,*+8                      SAVE OFF DATA                        
         B     *+10                                                             
         MVC   NEW.RCLNDATA(0),RKEYDATA    SAVE GROUP OF DICTIONARY #'S         
*                                                                               
TRNS321  IC    RF,RCLHD1LN         GET LENGTH OF HEADING 1                      
         SR    R1,R1                                                            
         IC    R1,RCLHD2LN         GET LENGTH OF HEADING 2                      
         AR    RF,R1                                                            
         AHI   RF,-1                                                            
         BM    TRNS322                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R5)       MOVE IN HEADINGS                             
         LA    RE,1(RF,RE)         POINT TO END OF ELEMENT                      
*                                                                               
TRNS322  OI    NEW.RCLOPT2,RCLDICT     DICTIONARY CONVERSION                    
         CLI   RFIXCTL,0               SPECIAL KEYWORD ?                        
         BE    TRNS324                 NO,  FINISHED WITH THIS ELEMENT          
         MVC   NEW.RCLXDATA,AC00PASS                                            
         MVC   NEW.RCLSPCL,RFIXCTL     USE  SPECIAL TYPE                        
*                                                                               
TRNS324  LA    RF,NEW.RCLEL        RF = START OF ELEMENT                        
         SR    RE,RF               RE = END   OF ELEMENT                        
         STC   RE,NEW.RCLLN                                                     
         MVI   RCLEL,X'FF'         DELETE ELEMENT PROCESSED                     
         MVI   TRLNEWEL,YES        SET FLAG TO ADD AT END                       
         L     R1,ASAVEIO                                                       
         GOTO1 TRLADDEL            ADD TRANSLATED ELEMENT TO TEMP IO            
         B     TRNS025             FINISHED WITH THIS ELEMENT                   
*                                                                               
TRNS325  SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH OF DATA TO SCAN                       
         BCTR  RF,0                                                             
*                                                                               
TRNS330  EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   RCLNDATA(0),TRLTAB  FIND COMMAS (DESTROYS R1 AND R2)             
         BZ    TRNS025             FINISHED                                     
         MVI   0(R1),X'01'         REPLACE COMMAS WITH X'01'S                   
         B     TRNS330                                                          
         DROP  NEW                                                              
         DROP  R6                                                               
         EJECT ,                                                                
         USING RFLELD,R6                                                        
TRNS400  CLI   RFLTYPE,RFLTTYPE                                                 
         BE    TRNS025             DON'T PROCESS THIS ONE                       
         CLI   RFLTYPE,RFLBUDGT                                                 
         BE    TRNS025             DON'T PROCESS THIS ONE                       
         CLI   RFLTYPE,RFLPCDE                                                  
         BE    TRNS025             DON'T PROCESS THIS ONE                       
*&&US*&& CLI   RFLTYPE,RFLAPMT                                                  
*&&US*&& BE    TRNS025             Don't process for approval method            
         CLI   RFLTYPE,RFLAMFR                                                  
         BE    TRNS025             DON'T PROCESS THIS ONE                       
         CLI   RFLTYPE,RFLAMTO                                                  
         BE    TRNS025             DON'T PROCESS THIS ONE                       
         SR    RF,RF                                                            
         IC    RF,RFLLN                                                         
         SHI   RF,RFLLNQ+1                                                      
*                                                                               
TRNS425  EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   RFLDATA(0),TRLTAB   DESTROYS R1 AND R2                           
         BZ    TRNS025             FINISHED                                     
         MVI   0(R1),X'01'         REPLACE COMMA WITH X'01'                     
         B     TRNS425                                                          
         DROP  R6                                                               
         EJECT ,                                                                
TRNS500  CLI   RACTION,READREC     C'R' READING RECORD                          
         BE    *+6                                                              
         DC    H'00'               NOT A VALID ACTION                           
*                                  SETUP TO REPLACE X'01' WITH LANGUAGE         
*                                  SOFT COMMAS                                  
         MVC   TRLTAB+X'01'(1),SCCOMMA                                          
*                                                                               
TRNS520  CLI   0(R6),0             END OF RECORD                                
         BE    TRNS950                                                          
         CLI   RELEMCDE,0          IGNORE THIS CODE                             
         BE    TRNS522                                                          
         CLI   RSINGLE,YES         PROCESS ONE ELEMENT?                         
         BE    TRNS522             YES, SO SKIP CODE                            
         CLC   RELEMCDE,0(R6)      DOES IT MATCH                                
         BNE   TRNS525             NO                                           
*                                                                               
TRNS522  CLI   0(R6),STYELQ        X'25' REPORT TYPE                            
         BE    TRNS550                                                          
         CLI   0(R6),PACELQ        X'A1' ACTIVITY                               
         BE    TRNS530                                                          
         CLI   0(R6),RHDELQ        X'C1' REPORT HEADING                         
         BE    TRNS600                                                          
         CLI   0(R6),RRWELQ        X'C2' REPORT ROW                             
         BE    TRNS700                                                          
         CLI   0(R6),RCLELQ        X'C3' REPORT COLUMN                          
         BE    TRNS800                                                          
         CLI   0(R6),RFLELQ        X'C5' REPORT FILTERS                         
         BE    TRNS900                                                          
*                                                                               
TRNS525  CLI   RSINGLE,YES                                                      
         BE    TRNS950                                                          
         SR    R1,R1                                                            
         IC    R1,1(,R6)           NEXT ELEMENT                                 
         AR    R6,R1                                                            
         B     TRNS520                                                          
         EJECT ,                                                                
***********************************************************************         
* New date stamp with date/time/PId                                   *         
***********************************************************************         
NEW      USING DTSELD,TRLELEM                                                   
         USING PACELD,R6                                                        
TRNS530  MVC   TRLIOKEY,IOKEY      Save off IOKEY                               
         XC    TRLELEM,TRLELEM                                                  
         MVI   NEW.DTSEL,DTSELQ                                                 
         MVI   NEW.DTSLN,DTSLN2Q       New length                               
         MVI   NEW.DTSTYPE,DTSTSCR     Scribe format info                       
         GOTOR VDATCON,TRLPRMS,(1,PACDATE),(2,NEW.DTSDATE)                      
*                                                                               
* Read security record based on PID code to get pid # ?                         
*                                                                               
         USING SAPEREC,R5                                                       
         CLC   PACPERS,=CL8'DDS'                                                
         BE    TRNS538             Delete PACELQ element X'A1'                  
         LA    R5,IOKEY                                                         
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ    C'F'                                         
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,CUAGYSEC    Agency security                              
         MVC   SAPEPID,PACPERS     Person code                                  
         GOTO1 VIO,IO3+IOHI+IOCTFILE                                            
                                                                                
         L     R5,AIOAREA3                                                      
         CLC   IOKEY(SAPEDEF-SAPEKEY),0(R5)                                     
         BNE   TRNS540                                                          
                                                                                
         USING SAPWDD,R1                                                        
         SR    RF,RF                                                            
         LA    R1,SAPEDATA                                                      
TRNS534  CLI   0(R1),EOR                                                        
         BE    TRNS540                                                          
         CLI   0(R1),SAPWDELQ      Password and code                            
         BE    TRNS536                                                          
         IC    RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     TRNS534                                                          
                                                                                
TRNS536  MVC   NEW.DTSPID#,SAPWDNUM                                             
         DROP  R1,R5                                                            
                                                                                
TRNS538  MVI   TRLNEWEL,YES                                                     
         L     R1,ASAVEIO                                                       
         GOTO1 TRLADDEL            Add translated element to temp io            
                                                                                
         MVI   PACEL,X'FF'         Mark for deletion                            
                                                                                
TRNS540  MVC   IOKEY,TRLIOKEY      Restore IOKEY                                
         J     TRNS525                                                          
         DROP  R6                                                               
         DROP  NEW                                                              
         EJECT ,                                                                
***********************************************************************         
* Convert SCRIBE type element                                                   
***********************************************************************         
         USING STYELD,R6                                                        
TRNS550  XC    RRECDIC#,RRECDIC#   CLEAR DICTIONARY NUMBER FOR RECORD           
         CLI   STYNAME,ESCHIGHQ    ESCAPE SEQUENCE?                             
         BNL   TRNS525             NEXT ELEMENT                                 
         MVC   RRECDIC#,STYNAME+1  SAVE DICTIONARY NUMBER FOR RECORD            
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',STYNAME,0                                
         B     TRNS525                                                          
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
NEW      USING RHDELD,TRLELEM                                                   
         USING RHDELD,R6                                                        
TRNS600  TM    RHDFRM,RHDDEF                                                    
         BZ    TRNS525             ONLY TRANSLATE DEFINED HEADINGS              
         TM    RHDFRM,RHDDICT                                                   
         BZ    TRNS525             ONLY TRANSLATE TRANSLATED ITEMS              
         NI    RHDFRM,TURNOFF-RHDDICT                                           
         XC    TRLELEM,TRLELEM                                                  
         MVC   NEW.RHDEL(RHDLNQ),RHDEL  SAVE OFF ELEMENT                        
         MVC   RKEYWORD,SPACES                                                  
         MVI   RKEYWORD,ESCLFJTQ   LEFT JUSTIFIED                               
         MVC   RKEYWORD+1(2),RHDDATA                                            
         MVI   RKEYWORD+3,KEYWDLN                                               
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RKEYWORD,0                               
         MVI   NEW.RHDDATA,C'&&'                                                
*                                                                               
         GOTO1 FLEN,TRLPRMS,(L'RKEYWORD,RKEYWORD)                               
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NEW.RHDDATA+1(0),RKEYWORD                                        
         LA    RF,2+RHDLNQ(,RF)           INCREASE FOR EX AND C'&'              
         CLC   =AL2(AC#RSBLK),RHDDATA     IS IT KEYWORD BLK#                    
         BNE   TRNS560                    NO                                    
         LA    RE,NEW.RHDEL(RF)    POINT TO END OF ELEMENT                      
         MVC   0(1,RE),RHDXDATA    MOVE IN NUMBER                               
         OI    0(RE),X'F0'         CONVERT BINARY # TO CHARACTER #              
         LA    RF,1(,RF)           ADD ONE FOR NUMBER                           
*                                                                               
TRNS560  STC   RF,NEW.RHDLN        SAVE NEW LENGTH                              
         MVI   RHDEL,X'FF'         DELETE                                       
         MVI   TRLNEWEL,YES                                                     
         L     R1,ASAVEIO                                                       
         GOTO1 TRLADDEL            ADD TRANSLATED ELEMENT TO TEMP IO            
         B     TRNS525                                                          
         DROP  R6                                                               
         DROP  NEW                                                              
         EJECT ,                                                                
         USING RRWELD,R6                                                        
TRNS700  SR    RF,RF                                                            
         IC    RF,RRWDATLN         LENGTH OF RRWNDATA                           
         LR    R0,RF                                                            
         SRL   R0,1                DIVIDE BY 2                                  
         BCTR  R0,0                LESS ONE FOR KEYWORD                         
         STC   R0,TRL#PRMS         NUMBER OF POTENTIAL PARAMETERS               
         BCTR  RF,0                ONE LESS OF EXECUTE INSTR.                   
         TM    RRWOPT2,RRWDICT     NEED TO CONVERT?                             
         BZ    TRNS725             NO                                           
         MVI   REPMODE,REPROW                                                   
         NI    RRWOPT2,TURNOFF-RRWDICT                                          
         XC    RELMDATA,RELMDATA                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RELMDATA(0),RRWNDATA                                             
         MVC   RKEYDATA,SPACES                                                  
         MVC   RKEYWORD,SPACES                                                  
         MVI   RKEYWORD,ESCLFJTQ        LEFT JUSTIFY                            
         MVC   RKEYWORD+1(2),RELMDATA   KEYWORD DICT#                           
         MVI   RKEYWORD+3,KEYWDLN                                               
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RKEYWORD,0                               
         CLI   RKEYWORD,C'('       COULD IT FIND DICTIONARY VALUE ?             
         BE    TRNS720             NO, SO WERE IN TROUBLE BUT TRY               
         CLI   RRWXDATA,0                                                       
         BE    TRNS720                                                          
*                                                                               
TRNS710  MVC   RNUM2CHR,RRWXDATA                                                
         BAS   RE,KYW2FIX               REPLACE C'#' WITH NUMBER                
*                                                                               
NEW      USING RRWELD,TRLELEM                                                   
*                                                                               
TRNS720  BAS   RE,NUM2CHR                                                       
         XC    TRLELEM,TRLELEM                                                  
         OC    RKEYDATA,SPACES                                                  
         MVC   NEW.RRWEL(RRWNLNQ),RRWEL                                         
         GOTO1 FLEN,TRLPRMS,(L'RKEYDATA,RKEYDATA)                               
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         STC   RF,NEW.RRWDATLN     SAVE LENGTH OF DATA                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NEW.RRWNDATA(0),RKEYDATA                                         
         LA    RE,NEW.RRWNDATA+1(RF)    POINT TO END OF ELEMENT                 
         LA    R5,RRWNDATA              ORIGINAL DATA                           
         IC    RF,RRWDATLN         LENGTH OF ORIGINAL DATA                      
         AR    R5,RF               POINT TO WHERE PREFIX WOULD BE               
         SR    R1,R1                                                            
         ICM   R1,1,RRWPFXLN       LENGTH OF PREFIX                             
         BZ    TRNS724             NO                                           
         BCTR  R1,0                YES, SO RE-ATTACH                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R5)       ATTACH PREFIX                                
         LA    RE,1(R1,RE)         POINT TO NEW END OF ELEMENT                  
*                                                                               
TRNS724  LA    RF,NEW.RRWEL        RF = START OF NEW ELEMENT                    
         SR    RE,RF               RE = END OF ELEMENT                          
         STC   RE,NEW.RRWLN        SAVE NEW LENGTH OF ELEMENT                   
         MVI   RRWEL,X'FF'         MARK DELETED                                 
         L     R1,ASAVEIO          ADD NEW ELEMENT TO SAVED AREA                
         GOTO1 TRLADDEL                                                         
         MVI   TRLNEWEL,YES                                                     
         B     TRNS525                                                          
*                                                                               
TRNS725  EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   RRWNDATA(0),TRLTAB  FIND X'01' DESTROYS R1 AND R2                
         BZ    TRNS525             FINISHED                                     
         MVC   0(1,R1),SCCOMMA     REPLACE X'01'S WITH COMMAS                   
         B     TRNS725                                                          
         EJECT ,                                                                
         USING RCLELD,R6                                                        
TRNS800  SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH OF DATA                               
         LR    R0,RF                                                            
         SRL   R0,1                DIVIDE BY 2                                  
         BCTR  R0,0                LESS ONE FOR KEYWORD                         
         STC   R0,TRL#PRMS         NUMBER OF POTENTIAL PARAMETERS               
         BCTR  RF,0                ONE LESS OF EXECUTE INSTR.                   
*                                                                               
         TM    RCLOPT2,RCLDICT     NEED TO CONVERT?                             
         BZ    TRNS830             NO                                           
         MVI   REPMODE,REPCOL                                                   
         NI    RCLOPT2,TURNOFF-RCLDICT                                          
         XC    RELMDATA,RELMDATA                                                
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RELMDATA(0),RCLNDATA                                             
         MVC   RKEYDATA,SPACES                                                  
         MVC   RKEYWORD,SPACES                                                  
         MVI   RKEYWORD,ESCLFJTQ        LEFT JUSTIFY                            
         MVC   RKEYWORD+1(2),RELMDATA   KEYWORD DICT#                           
         MVI   RKEYWORD+3,KEYWDLN                                               
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RKEYWORD,0                               
         CLI   RKEYWORD,C'('          COULD IT FIND DICTIONARY VALUE ?          
         BE    TRNS820                NO, SO WERE IN TROUBLE BUT TRY            
         CLI   RCLXDATA,0                                                       
         BE    TRNS820                                                          
         MVC   RNUM2CHR,RCLXDATA      REPLACE C'#' WITH NUMBER                  
         BAS   RE,KYW2FIX                                                       
*                                                                               
NEW      USING RCLELD,TRLELEM                                                   
*                                                                               
TRNS820  CLI   RCLSPCL,RCLSPDTE    TRANSLATE DATE1-DATE2 ?                      
         BNE   TRNS823             NO                                           
         GOTO1 FLEN,TRLPRMS,(L'RKEYWORD,RKEYWORD)                               
         XC    TRLELEM,TRLELEM                                                  
         MVC   NEW.RCLEL(RCLNLNQ),RCLEL                                         
         MVC   NEW.RCLHD1LN,RCLHD1LN                                            
         MVC   NEW.RCLHD2LN,RCLHD2LN                                            
         STC   RF,TRLBYTE          SAVE OFF LENGTH                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NEW.RCLNDATA(0),RKEYWORD                                         
         LA    RE,NEW.RCLNDATA+1(RF)    POINT PAST 1ST KEYWORD                  
         MVI   0(RE),C'-'               INSERT THE MINUS SIGN                   
         LA    RE,1(,RE)                                                        
         ST    RE,TRLFULL                                                       
         MVI   RKEYWORD,ESCLFJTQ          LEFT JUSTIFY                          
         MVC   RKEYWORD+1(2),RELMDATA+2   2ND KEYWORD DICT#                     
         MVI   RKEYWORD+3,KEYWDLN                                               
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RKEYWORD,0                               
         GOTO1 FLEN,TRLPRMS,(L'RKEYWORD,RKEYWORD)                               
         L     RE,TRLFULL          RELOAD END OF RCLNDATA                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),RKEYWORD    MOVE IN 2ND KEYWORD                          
         SR    R1,R1                                                            
         IC    R1,TRLBYTE          GET LENGTH OF 1ST KEYWORD                    
         LA    RF,2(R1,RF)         FIGURE OUT FULL LENGTH OF RCLNDATA           
         STC   RF,NEW.RCLDATLN                                                  
         LA    RE,NEW.RCLNDATA(RF) POINT TO END OF RCLNDATA                     
         B     TRNS824                                                          
*                                                                               
TRNS823  MVC   RNUM2CHR,RCLENDT+1                                               
         BAS   RE,NUM2CHR                                                       
         OC    RKEYDATA,SPACES                                                  
         XC    TRLELEM,TRLELEM                                                  
         MVC   NEW.RCLEL(RCLNLNQ),RCLEL                                         
         MVC   NEW.RCLHD1LN,RCLHD1LN                                            
         MVC   NEW.RCLHD2LN,RCLHD2LN                                            
         GOTO1 FLEN,TRLPRMS,(L'RKEYDATA,RKEYDATA)                               
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'00'                                                            
         STC   RF,NEW.RCLDATLN     SAVE LENGTH OF DATA                          
         BCTR  RF,0                                                             
         LA    RE,NEW.RCLEL+RCLNLNQ                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),RKEYDATA    SAVE TRANSLATED DATA                         
         LA    RE,1(RE,RF)         BUMP TO WHERE 1ST HEADING GOES               
*                                                                               
TRNS824  LA    R5,RCLNDATA                                                      
         IC    RF,RCLDATLN                                                      
         AR    R5,RF               POINT TO 1ST HEADING OF ORIGINAL ELM         
         IC    RF,RCLHD1LN         LENGTH OF HEADING 1                          
         SR    R1,R1                                                            
         IC    R1,RCLHD2LN         LENGTH OF HEADING 2                          
         AR    RF,R1               LENGTH OF BOTH HEADINGS                      
         SHI   RF,1                ONE FOR EX INSTR.                            
         BM    TRNS825             NO HEADINGS PRESENT                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R5)       MOVE IN HEADINGS                             
         LA    RE,1(RF,RE)         POINT TO END OF ELEMENT                      
*                                                                               
TRNS825  LA    RF,NEW.RCLEL        RF = START OF ELEMENT                        
         SR    RE,RF               RE = END   OF ELEMENT                        
         STC   RE,NEW.RCLLN                                                     
         MVI   RCLEL,X'FF'         MARK DELETED                                 
         L     R1,ASAVEIO                                                       
         GOTO1 TRLADDEL                                                         
         MVI   TRLNEWEL,YES                                                     
         B     TRNS525                                                          
         DROP  NEW                                                              
*                                                                               
TRNS830  EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   RCLNDATA(0),TRLTAB  FIND    X'01' DESTROYS R1 AND R2             
         BZ    TRNS525             FINISHED                                     
         MVC   0(1,R1),SCCOMMA     REPLACE X'01'S WITH COMMAS                   
         B     TRNS830                                                          
         EJECT ,                                                                
         USING RFLELD,R6                                                        
TRNS900  CLI   RFLTYPE,RFLTTYPE    IGNORE THIS TYPE                             
         BE    TRNS525                                                          
         CLI   RFLTYPE,RFLBUDGT    IGNORE THIS TYPE                             
         BE    TRNS525                                                          
         CLI   RFLTYPE,RFLPCDE     IGNORE THIS TYPE                             
         BE    TRNS525                                                          
*&&US*&& CLI   RFLTYPE,RFLAPMT                                                  
*&&US*&& BE    TRNS525             Don't process for approval method            
         CLI   RFLTYPE,RFLAMFR                                                  
         BE    TRNS525             DON'T PROCESS THIS ONE                       
         CLI   RFLTYPE,RFLAMTO                                                  
         BE    TRNS525             DON'T PROCESS THIS ONE                       
         SR    RF,RF                                                            
         IC    RF,RFLLN                                                         
         SHI   RF,RFLLNQ+1                                                      
         BM    TRNS525                                                          
*                                                                               
TRNS925  EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   RFLDATA(0),TRLTAB   DESTROYS R1 AND R2                           
         BZ    TRNS525             FINISHED                                     
         MVC   0(1,R1),SCCOMMA     REPLACE X'01' WITH COMMAS                    
         B     TRNS925                                                          
         DROP  R6                                                               
         EJECT ,                                                                
         USING STYELD,R6                                                        
TRNS950  CLI   RACTION,WRITE       WRITING A RECORD                             
         BNE   TRNS954                                                          
         ICM   R6,15,TRLSTYEL      LOAD X'25' ELEMENT STYELD                    
         BZ    TRNS954                                                          
         CLI   0(R6),STYELQ        IS IT STILL THE ELEMENT ?                    
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   STYSEC#1,TRLSEC#1                                                
         MVC   APSECKYW,STYSEC#1                                                
         DROP  R6                                                               
*                                                                               
TRNS954  CLI   TRLNEWEL,YES                                                     
         BNE   TRNSLTX             NO ELEMENTS TO ADD                           
         L     R3,ASAVEIO                                                       
         L     R4,TRLAIO                                                        
         GOTO1 VHELLO,TRLPRMS,(C'D',=CL8'ACCVBIG'),(X'FF',(R4)),0               
         CLI   TRLPRMS+12,0        ELEMENT ADD OK                               
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(58)                                                 
*                                  ADD NEW TRANSLATED ELEMENTS TO REC           
         AH    R3,DATADISP         POINT TO FIRST ELEMENT                       
TRNS955  CLI   0(R3),0             END OF RECORD                                
         BE    TRNSLTX             NO MORE TO ADD                               
         LR    RE,R3               SAVE A(CURRENT ELEMENT)                      
         SR    RF,RF                                                            
         IC    RF,1(,R3)           NEXT ELEMENT                                 
         AR    R3,RF                                                            
         BCTR  RF,0                MOVE ELEMENT INTO TRLELEM                    
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TRLELEM(0),0(RE)                                                 
         GOTO1 TRLADDEL,(R4)                                                    
         B     TRNS955                                                          
*                                                                               
TRNSLTX  MVC   REPMODE,RSVMODE     RESTORE                                      
*                                                                               
TRLXIT   XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  CONVERT STRING CHARACTER FOR ROWS OR COLS TO DICTIONARY VALUES     *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R6                                                       
CHR2NUM  NTR1                                                                   
         LR    R3,R1               SAVE ELEMENT ADDRESS                         
         MVI   RFIXCTL,0                                                        
         MVI   TRL#PRMS,0                                                       
         GOTO1 VALDEF,TRLFLDH      FIND DICTIONARY ENTRY                        
         BNE   CHR2NO              NO GOOD SO SKIP                              
         CLI   APUSRKYW,YES                                                     
         BE    CHR2NO              USER DEFINED KEYWORD, SKIP                   
         LR    R6,R1                                                            
         OC    TRLSEC#1,DEFSEC#                                                 
         CLI   DEFCODE,ESCHIGHQ    IS IT A ESCAPE SEQUENCE?                     
         BNL   CHR2NO              NO SO SKIP                                   
         MVI   TRL#PRMS,1                                                       
         XC    RKEYDATA,RKEYDATA                                                
         MVC   RKEYDATA(2),DEFDDNUM      SAVE DICTIONARY # OF KEYWORD           
*                                                                               
         LA    R2,ESTWRK           ESTIMATE WORK HAS EST# IN BINARY             
         CLI   DEFSPCL,DEFCTRL     EMBEDED NUMBER, JOBBER KEYWORD ?             
         BNE   CHR2N01             NO,  TRY ANOTHER TYPE                        
*        MVC   RKEYDATA+11(1),AC00PASS      PASS REV OR PLAN # FROM             
         MVI   RFIXCTL,RCLSPCTL             SET SPECIAL SWITCH                  
         B     CHR2N08                                                          
*                                                                               
CHR2N01  CLI   DEFSPCL,DEFCTR#     EMBEDED NUMBER, NON-JOBBER KEYWORD ?         
         BNE   CHR2N02             NO,  TRY ANOTHER TYPE                        
*        MVC   RKEYDATA+11(1),AC00PASS      PASS REV OR PLAN # FROM             
         MVI   RFIXCTL,RCLSPCT#             SET SPECIAL SWITCH                  
         B     CHR2N08                                                          
*                                                                               
CHR2N02  CLI   DEFSPCL,DEFLVL1     ACCOUNT LEVEL TYPE                           
         BL    CHR2N03                                                          
         CLI   DEFSPCL,DEFLVL9     ACCOUNT LEVEL TYPE                           
         BH    CHR2N03                                                          
         MVC   AC00PASS,DEFSPCL                                                 
         MVC   RFIXCTL,DEFSPCL             SET SPECIAL SWITCH                   
         B     CHR2N08                                                          
*                                                                               
CHR2N03  CLI   DEFSPCL,DEFDRKY     PDAY KEYWORD                                 
         BE    CHR2N08                                                          
*                                                                               
CHR2N04  CLI   DEFSPCL,0                                                        
         BE    CHR2N08                                                          
         B     CHR2NO              FOR NOW DON'T TRANSLATE SPECIALS             
*                                                                               
*        CLI   DEFSPCL,DEFEXRT     EXCHANGE RATE ?                              
*        BE    CHR2NO                                                           
*        CLI   DEFSPCL,DEFSUM      RUNNING TOTAL ?                              
*        BNE   CHR2NO                                                           
*                                                                               
CHR2N08  GOTO1 VSCANNER,TRLPRMS,TRLFLDH,(5,TRLBLOCK),SCNP3NEQ                   
         SR    R0,R0                                                            
         IC    R0,TRLPRMS+4        NUMBER OF PARAMETERS                         
         STC   R0,TRL#PRMS         SAVE NUMBER OF PARMS                         
         AHI   R0,-1               LESS ONE FOR KEYWORD                         
         BZ    CHR2YES             FINISHED                                     
         BP    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING PARMD,R2                                                         
         LA    R4,TRLBLOCK         POINT TO SCAN BLOCK                          
         LA    R5,RKEYDATA         START OF STORAGE OF DICT VALUES              
         LA    R4,32(,R4)          FIRST PARAMETER IN SCAN BLOCK                
*MN                                                                             
         CLC   RKEYDATA(2),=AL2(AC#RSUSC)       USERFIELD (SPECIAL)             
         BE    CHR2N10                                                          
         CLC   RKEYDATA(2),=AL2(AC#RSUSF)       USERFIELD (SPECIAL)             
         BNE   CHR2N11                                                          
*                                                                               
CHR2N10  LA    R5,2(,R5)           NEXT SPOT                                    
         CLI   0(R4),2                                                          
         BH    CHR2NO              USER FIELDS MUST BE LENGTH 1 OR 2            
         MVC   0(2,R5),12(R4)                                                   
         LA    R4,32(,R4)          NEXT PARAMETER IN SCAN BLOCK                 
         BCT   R0,CHR2N10          NEXT PARAMETER IF ANY                        
         B     CHR2YES                                                          
*                                                                               
CHR2N11  DS    0H                  VALIDATE THE PARAMETERS                      
         GOTO1 VALPARM,TRLPRMS,(R3),((R0),TRLBLOCK),(R6),RPARMDEF               
         BNE   CHR2NO              TEMPORARIY (DON'T TRANSLATE)                 
*&&DO                                                                           
*        WARNING MESSAGE WILL BE PLACED ON RECORD IN THE FUTURE                 
         BE    CHR2N18                                                          
         LA    RF,=AL2(ACEPRM)     INVALID PARAMETER                            
         ST    RF,TRLPRMS          SET UP  PARM 1                               
         MVI   TRLPRMS,MNOSTCOL    COLUMN  TYPE                                 
         CLI   REPMODE,REPCOL                                                   
         BE    CHR2N12                                                          
         MVI   TRLPRMS,MNOSTROW    ROW TYPE                                     
         CLI   REPMODE,REPROW                                                   
         BE    CHR2N12                                                          
         DC    H'00'               NOT SUPPORTED                                
*                                                                               
CHR2N12  GOTO1 ADDMSG,TRLPRMS,,(RSEQNUM,(R6)),('MNOMTERR',0),0                  
         MVI   TRLNEWEL,YES        ADD ERRORMSG ELEMENT                         
         B     CHR2NO                                                           
         DROP  R1                                                               
*&&                                                                             
CHR2N18  DS    0H                                                               
         LA    R3,RPARMDEF         ->   TABLE OF PARAMETER DEFINITIONS          
*                                                                               
CHR2N20  DS    0H                                                               
         LA    R5,2(,R5)           NEXT SPOT                                    
         L     R2,0(,R3)           ->   PARAMETER DEFINITION ENTRY              
         CLI   PARDDESC,ESCHIGHQ   MAKE SURE IT IS A DICT#                      
         BNL   CHR2NO              SKIP, NOT CONVERTABLE                        
         CLC   PARDDNUM(2),=X'3FFF'  LARGEST NUMBER SUPPORTED                   
         BH    CHR2NO              TOO HIGH TO HANDLE                           
         ZIC   RF,0(,R4)           LENGTH OF PARAMETER                          
         BCTR  RF,0                MINUS ONE FOR EXECUTE                        
         SLL   RF,6                SHIFT EX LEN TO HIGH ORDER BITS              
         STC   RF,TRLBYTE                                                       
         MVC   0(2,R5),PARDDNUM    SAVE DICT#                                   
*                                  ATTACH EXECUTE LENGTH HIGH ORDER             
         OC    0(1,R5),TRLBYTE     TWO BITS (NUMBERS 0 - 3)                     
         LA    R3,4(,R3)           NEXT PARMETER DEFINITION ENTRY               
         LA    R4,32(,R4)          NEXT PARAMETER IN SCAN BLOCK                 
         BCT   R0,CHR2N20          NEXT PARAMETER IF ANY                        
*                                  ATTACH EXECUTE LENGTH HIGH ORDER             
CHR2YES  SR    RE,RE                                                            
*                                                                               
CHR2NO   LTR   RE,RE                                                            
         B     TRLXIT                                                           
         DROP  R2,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*        CONVERT PARAMETERS TO CHARATER FORMAT                        *         
***********************************************************************         
         SPACE 1                                                                
         USING RPRMD,R4                                                         
NUM2CHR  NTR1                                                                   
         ST    R6,R@ELEMNT         SAVE ELEMENT ADDRESS                         
         SR    R3,R3                                                            
         XC    TRLHALF,TRLHALF     SET TO ZERO                                  
         XC    TRLFULL,TRLFULL     SET TO ZERO                                  
         XC    TRLWORK,TRLWORK                                                  
         ICM   R3,1,TRL#PRMS       NUMBER OF PARAMETERS                         
         BZ    NUM2C50             NONE                                         
         CLI   TRL#PRMS,3          MAX OF 3 PARAMETERS                          
         BNH   *+8                                                              
         MVI   TRL#PRMS,3          ONLY ALLOW A MAX OF 3, LOSE REST             
         LA    R4,TRLWORK          PARAMETERS 1 - 4                             
         LA    R5,RELMDATA+2       BUMP PAST KEYWORD                            
*MN                                                                             
NUM2C10  DS    0H                                                               
         CLC   RELMDATA(2),=AL2(AC#RSUSC)    USER FIELD (SPECIAL)               
         BE    NUM2C11                                                          
         CLC   RELMDATA(2),=AL2(AC#RSUSF)    USER FIELD (SPECIAL)               
         BNE   NUM2C14                                                          
NUM2C11  MVI   RPRMOEXL,1                                                       
         XC    RPRMDIC,RPRMDIC                                                  
         MVC   RPRMKEY,SPACES                                                   
         MVC   RPRMKEY(2),0(R5)                                                 
         B     NUM2C40                                                          
*                                                                               
NUM2C14  SR    R1,R1                                                            
         IC    R1,0(,R5)           GET ORIGINAL EXECUTE LENGTH                  
         SRL   R1,6                SHIFT TWO BYTES TO RESTORE EX LENGTH         
         STC   R1,RPRMOEXL         ORIGINAL EXECUTE LENGTH                      
         NI    0(R5),B'00111111'   REMOVE EX LENGTH                             
         MVI   RPRMKEY,ESCLFJTQ    LEFT JUSTIFY                                 
         MVC   RPRMKEY+1(2),0(R5)  MOVE IN DICT# OF PARAMETER                   
         MVC   RPRMDIC,0(R5)       SAVE DICT# VALUE                             
         MVI   RPRMKEY+3,4         LENGTH 4 MAX                                 
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RPRMKEY,0                                
*                                                                               
         CLI   REPMODE,REPCOL      PROCESSING  COLUMNS ?                        
         BNE   NUM2C40             NO, PROCESS ROWS                             
*                                                                               
         USING PARMD,R2                                                         
         L     R2,ACPRMTAB         LOAD TABLE                                   
NUM2C20  CLI   PARMD,EOT           END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'00'               HUGH?                                        
         CLC   RPRMDIC,PARDDNUM    MATCH DICT NUMBERS                           
         BE    NUM2C22                                                          
         LA    R2,PARMLNQ(,R2)     NEXT PARAMETER                               
         B     NUM2C20                                                          
*                                                                               
NUM2C22  MVC   RPRMGRP,PARMUSED    SAVE PARM GRP TYPE                           
         TM    PARMCIND,RCLNROLL   MUST BE A NON ROLLING DATE TYPE              
         BZ    NUM2C40             NOT SPECIAL,  PROCESS AS NORMAL              
         SR    R1,R1                                                            
         IC    R1,RNUM2CHR                                                      
         CVD   R1,TRLDUB                                                        
         OI    TRLDUB+7,X'0F'      ADJUST SIGN FOR CHARACTER FORMAT             
         UNPK  TRLWORK2(3),TRLDUB                                               
*                                                                               
*                                  ************************************         
*                                  * THIS FINDS THE NUMBER TO APPEND  *         
*                                  ************************************         
         LA    R2,3                MAX LENGTH OF 3                              
         LA    RE,TRLWORK2         POINT AT START OF EBCDIC NUMBER              
NUM2C24  CLI   0(RE),C'0'                                                       
         BNE   NUM2C25                                                          
         LA    RE,1(,RE)           NEXT CHARACTER                               
         BCT   R2,NUM2C24                                                       
         DC    H'00'               CAN'T ALL BE ZERO                            
*                                                                               
*                                  ************************************         
*                                  * THIS FINDS NON-"?" PART          *         
*                                  ************************************         
NUM2C25  SR    R1,R1                                                            
         LA    RF,L'RPRMKEY                                                     
         LA    R6,RPRMKEY                                                       
*                                                                               
NUM2C26  CLI   0(R6),C'?'          FIND FIRST QUESTION MARK                     
         BE    NUM2C30                                                          
         LA    R1,1(,R1)           COUNT LENGTH                                 
         LA    R6,1(,R6)           LOOK AT NEXT CHARACTER                       
         BCT   RF,NUM2C26                                                       
         DC    H'00'               HAS TO HAVE A QUESTION MARK                  
*                                                                               
*                                  ************************************         
*                                  * ACCOUNT FOR PARAMETER LENGTH     *         
*                                  ************************************         
NUM2C30  AR    R1,R2               FULL LENGTH OF PARAMETER                     
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RE)       ATTACH NUMBER TO PARAMETER                   
         LR    RF,R1                                                            
         B     NUM2C42                                                          
         DROP  R2                                                               
*                                                                               
NUM2C40  GOTO1 FLEN,TRLPRMS,(L'RPRMKEY,RPRMKEY)                                 
*                                                                               
NUM2C42  STC   RF,RPRMLEN          ACTUAL LENGTH OF PARAMETER                   
         LR    R1,RF                                                            
         A     R1,TRLFULL                                                       
         ST    R1,TRLFULL          SAVE FULL LENGTH                             
         IC    R1,RPRMOEXL         GET  ORIGINAL  EXECUTE LENGTH                
         LA    R1,2(,R1)           GET  ORIGINAL  LENGTH  WITH COMMA            
         AH    R1,TRLHALF          ADD  SUM  OF   PARM LENGTHS                  
         STH   R1,TRLHALF          SAVE SUM  OF   PARM LENGTHS                  
         LTR   RF,RF               PARM LEN  =    0 ?                           
         BZ    *+6                 YES, SKIP                                    
         BCTR  RF,0                                                             
         STC   RF,RPRMFEXL         EX LENGTH OF ACTUAL                          
         LA    R4,RPRMLNQ(,R4)     NEXT PARAMETER AREA                          
         LA    R5,2(,R5)           NEXT POSSIBLE PARAMETER DICT #               
         BCT   R3,NUM2C10                                                       
*                                                                               
*                                  ************************************         
*                                  * ACCOUNT FOR KEYWORD              *         
*                                  ************************************         
NUM2C50  GOTO1 FLEN,TRLPRMS,(L'RKEYWORD,RKEYWORD)                               
         STC   RF,RKEYWRDL         SAVE KEYWORD   LENGTH                        
         SH    RF,=H'01'                                                        
         BNM   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RKEYDATA(0),RKEYWORD                                             
         LA    RF,1(,RF)           GET  FULL KEYWORD   LENGTH                   
         AH    RF,TRLHALF          GET  MINIMUM   DATA LENGTH                   
         STH   RF,TRLHALF          SAVE MINIMUM   DATA LENGTH                   
         LA    R1,MXRDATLN-1       MAX ROW DATA LEN. (CHARATER FORMAT)          
         CLI   REPMODE,REPCOL      PROCESSING COLUMNS ?                         
         BNE   *+8                                                              
         LA    R1,MXCDATLN-1       MAX COL DATA LEN. (CHARACTER FORMAT)         
         CR    RF,R1               ANY  MORE PARMS     FIT ?                    
         BNL   NUM2C80             NO,  SKIP                                    
*                                                                               
RW       USING RRWELD,R6           MAP  ROW  ELEMENT                            
CL       USING RCLELD,R6           MAP  COL  ELEMENT                            
*                                                                               
         L     R6,R@ELEMNT         ROW OR COL ELEMENT                           
*                                                                               
*                                  ************************************         
*                                  * GROUP 3 DEFAULTS                 *         
*                                  ************************************         
         LA    R2,CL.RCLOPT2       COL  OPTIONS   FOR  CODE+NAME                
         CLI   REPMODE,REPCOL      COLUMNS   ELEMENT ?                          
         BE    *+8                 YES, SKIP                                    
         LA    R2,RW.RRWOPT        ROW  OPTIONS   FOR  CODE+NAME                
*                                                                               
         TM    0(R2),NME+CDE       BOTH NAME AND  CODE ON ?                     
         BNO   NUM2C56             NO,  SKIP                                    
         MVC   RADDPRM#,=AL2(AC#RSCAN)  PARM =   'BOTH'                         
*                                                                               
         BAS   RE,GETDPRMQ              GET  MIN  DEFAULT   PARM LENGTH         
         CLC   =AL2(AC#RSWC),RELMDATA   KEYWORD  "WC"  ?                        
         BE    NUM2C52                  YES, SPECIAL   CASE                     
         CLC   =AL2(AC#RSWCN),RELMDATA  KEYWORD  "WCN" ?                        
         BNE   NUM2C54                  YES, CONTINUE                           
*                                                                               
NUM2C52  CLI   CULANG,LANGFRE           FRENCH ?                                
         BNE   NUM2C54                  NO,  SKIP                               
         MVI   RADDPRML,3               MIN  PARM LENGTH                        
*                                                                               
NUM2C54  MVI   TRLBYTE,PARMGRP3         PARM GROUP     3                        
         BAS   RE,DEFAULTS              ADD  PARM IF   POSSIBLE                 
         B     NUM2C80                  DONE ADDING    PARAMETERS               
*                                                                               
NUM2C56  DS    0H                                                               
         TM    0(R2),NME+CDE            NAME OR   CODE ON ?                     
         BNZ   NUM2C80                  YES, SKIP                               
*                                                                               
*                                  ************************************         
*                                  * GROUP 2 DEFAULTS                 *         
*                                  ************************************         
         LA    R2,CL.RCLDATES      DATE BASIS     FOR  COL  ELEMNT              
         CLI   REPMODE,REPCOL      COLUMNS   ELEMENT ?                          
         BE    *+8                 YES, SKIP                                    
         LA    R2,RW.RRWDATES      DATE BASIS     FOR  ROW  ELEMNT              
         CLI   0(R2),0             ANY  DATE BASIS ?                            
         BE    NUM2C70             NO,  SKIP GRP  2                             
         OC    RRECDIC#,RRECDIC#   ANY  DICT NUM  FOR  RECORD ?                 
         BZ    NUM2C70             YES, SKIP GRP  2                             
*                                                                               
         USING REPTABD,R3          MAP  REPORT    TYPE DEFINITIONS              
         L     R3,ACTYPTAB         ->   REPORT    TYPE DEFINITIONS              
NUM2C60  CLI   0(R3),EOT           END  OF   TABLE ?                            
         BNE   *+6                 NO,  CONTINUE                                
         DC    H'00'               YES, DUMP                                    
         CLC   RRECDIC#,REPCODE+1  SAME DICTIONARY # ?                          
         BE    NUM2C62             YES, CONTINUE                                
         LA    R3,REPLNQ(,R3)      GET  NEXT TABLE     ENTRY                    
         B     NUM2C60             TRY  NEXT TABLE     ENTRY                    
*                                                                               
NUM2C62  MVC   TRLJCLID,REPJCLID   SAVE JCL  ID   CHARACTER                     
*                                                                               
         USING PARMD,R3            MAP  PARAMETER TABLE     ELEMENT             
         L     R3,ACPRMTAB         ->   PARAMETER TABLE                         
NUM2C64  CLI   PARMD,EOT           END  OF   TABLE                              
         BE    NUM2C70             YES, NO   GRP  2    DEFAULT                  
*                                                                               
         CLI   PARMUSED,PARMGRP2   GROUP     2    PARM ?                        
         BNE   NUM2C68             NO,  GET  NEXT PARAMETER                     
         CLC   PARMRIND,0(R2)      SAME DATE BASIS ?                            
         BNE   NUM2C68             NO,  GET  NEXT PARAMETER                     
*                                                                               
         LA    RE,PARMREPS         MATCH     REPORT    JCL  IDS                 
         LA    RF,L'PARMREPS       UP   TO   8    JCL  IDS                      
         CLC   =C'ALL',0(RE)       ALL  JCL  IDS  ARE  VALID ?                  
         BE    NUM2C69             YES, FOUND     MATCH                         
*                                                                               
NUM2C66  CLI   0(RE),C' '          END  OF   JCL  IDS ?                         
         BE    NUM2C68             YES, GET  NEXT PARAMETER                     
         CLC   TRLJCLID,0(RE)      RECORD'S  JCL  ID   MATCH ?                  
         BE    NUM2C69             YES, USE  PARAMETER                          
         LA    RE,1(,RE)           TRY  NEXT JCL  ID   TYPE                     
         BCT   RF,NUM2C66          TEST NEXT JCL  ID   TYPE                     
*                                                                               
NUM2C68  LA    R3,PARMLNQ(,R3)     ->   NEXT PARAMETER                          
         B     NUM2C64             TRY  NEXT PARAMETER                          
*                                                                               
NUM2C69  MVC   RADDPRM#,PARDDNUM   GET  DATA DICTIONARY     NUMBER              
         DROP  R3                                                               
*                                                                               
         BAS   RE,GETDPRMQ         GET  MIN  DEFAULT   PARM LENGTH              
         MVI   TRLBYTE,PARMGRP2    PARM GROUP     2                             
         BAS   RE,DEFAULTS         ADD  PARM IF   POSSIBLE                      
*                                                                               
*                                  ************************************         
*                                  * GROUP 4 DEFAULTS                 *         
*                                  ************************************         
NUM2C70  CLI   REPMODE,REPCOL      COLUMNS   ELEMENT ?                          
         BNE   NUM2C80             NO,  SKIP GRP  4                             
         CLI   CL.RCLDATES,0       ANY  DATE BASIS ?                            
         BE    NUM2C80             NO,  SKIP GRP  4                             
*                                  'PER'     BASIS ?                            
         TM    CL.RCLDTEFG,TURNOFF-RCLTODTE                                     
         BNZ   NUM2C80             NO,  SKIP GRP  4                             
         CLC   =AL2(AC#RSMTH),RELMDATA  KW   =    MONTH ?                       
         BE    NUM2C80                  YES, SKIP GRP  4                        
         CLC   =AL2(AC#RSRY1),RELMDATA  KW   =    YEAR ?                        
         BE    NUM2C80                  YES, SKIP GRP  4                        
         MVC   RADDPRM#,=AL2(AC#RSRPE)  PARM =   'PER'                          
*                                       GET  MIN  DEFAULT   PARM LENGTH         
         BAS   RE,GETDPRMQ                                                      
         MVI   TRLBYTE,PARMGRP2         PARM GROUP     4                        
         BAS   RE,DEFAULTS              ADD  PARM IF   POSSIBLE                 
         DROP  RW,CL                                                            
*                                                                               
*                                  ************************************         
*                                  * BUILD DATA FIELD                 *         
*                                  ************************************         
NUM2C80  SR    R3,R3                                                            
         ICM   R3,1,TRL#PRMS       GET  NUM  OF   PARMS     TO   TAG ON         
         BZ    NUM2CHRX            NONE,     RETURN                             
         LA    R4,TRLWORK          ->   PARAMETERS                              
         CLI   TRL#PRMS,2          EXACTLY   TWO  PARMS ?                       
         BNE   NUM2C85             NO,  CONTINUE                                
         CLI   RPRMGRP,PARMGRP4    1ST  PARM IN   GRP  4 ?                      
         BNE   NUM2C85             NO,  CONTINUE                                
*                                  2ND  PARM IN   GRP  2 ?                      
         CLI   RPRMGRP+RPRMLNQ,PARMGRP2                                         
         BNE   NUM2C85             NO,  CONTINUE                                
*                                                                               
*  SWAP THE  PARMS, I.E. MAKE GRP 2  PRECEED GRP 4                              
*                                                                               
         XC    RPRMD(RPRMLNQ),RPRMD+RPRMLNQ                                     
         XC    RPRMD+RPRMLNQ(RPRMLNQ),RPRMD                                     
         XC    RPRMD(RPRMLNQ),RPRMD+RPRMLNQ                                     
*                                                                               
NUM2C85  ZIC   RF,RKEYWRDL         GET  KEYWORD   LENGTH                        
         LA    R1,MXRDATLN         ROW DATA LENGTH                              
         CLI   REPMODE,REPCOL      COLUMNS ?                                    
         BNE   *+8                 NO ROWS                                      
         LA    R1,MXCDATLN         COLUMN DATA LENGTH                           
         SR    R1,RF               LENGTH TO BUILD PARAMETERS                   
         SR    R1,R3               R3 = NUMBER OF COMMAS NEEDED                 
         LA    R5,RKEYDATA(RF)     POINT TO START OF PARM BUILD                 
*                                                                               
         LA    R2,RPRMFEXL         SET FLAG TO USE LENGTH AS IS                 
         C     R1,TRLFULL          SEE IF THIS WILL FIT AS IS                   
         BNL   *+8                                                              
         LA    R2,RPRMOEXL         SET FLAG TO USE ORIGINAL EX LENGTHS          
*                                                                               
NUM2C90  MVC   0(1,R5),SCCOMMA                                                  
         LA    R5,1(,R5)           BUMP PAST COMMA                              
         SR    RF,RF                                                            
         IC    RF,0(,R2)                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),RPRMKEY                                                  
         LA    R5,1(RF,R5)         POINT TO NEW LOCATION                        
         LA    R2,RPRMLNQ(,R2)     NEXT VALUE IN PARAMETER                      
         LA    R4,RPRMLNQ(,R4)     NEXT PARAMETER                               
         BCT   R3,NUM2C90                                                       
*                                                                               
NUM2CHRX B     TRLXIT                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  FIND LENGTH OF DATA                                                *         
*        INPUT  P1 = BYTE 1   LENGTH OF FIELD                         *         
*                  = BYTE 2-4 A(FIELD)                                *         
*                                                                     *         
*        OUTPUT  RF=LENGTH                                            *         
*                                                                     *         
*        NOTE:   R1,RE,RF ARE DESTROYED                               *         
***********************************************************************         
         SPACE 1                                                                
FLEN     SR    RF,RF               RF WILL HAVE LENGTH                          
         ICM   RF,1,0(R1)                                                       
         BZR   RE                  NO LENGTH TO CHECK, SO LEN=0 (RF=0)          
         L     R1,0(,R1)                                                        
         AR    R1,RF                                                            
         BCTR  R1,0                POINT TO END OF AREA LESS ONE                
*                                                                               
FLEN10   CLI   0(R1),C' '                                                       
         BHR   RE                  RF=LENGTH                                    
         BCTR  R1,0                BUMP DOWN IN AREA                            
         BCT   RF,FLEN10                                                        
         BR    RE                  IF HERE RF=0                                 
         EJECT ,                                                                
         USING RESRECD,R4                                                       
TRLADDEL NTR1                                                                   
         LR    R4,R1                                                            
         MVI   TRL4KREC,NO                                                      
         LA    R6,=C'ACCBIG'       2K RECORD                                    
         CLC   =X'2D02',RESKTYP                                                 
         BNE   TRLAEL10                                                         
         CLI   RESKSEQ,RESKSREG    X'40' REGULAR FORMAT                         
         BNE   TRLAEL10                                                         
         MVI   TRL4KREC,YES                                                     
         LA    R6,=C'ACCVBIG'      4K SIMULATED RECORD                          
*                                                                               
TRLAEL10 GOTO1 VHELLO,TRLPRMS,(C'P',(R6)),(R4),TRLELEM,0                        
         CLI   TRLPRMS+12,0        RECORD SEEMS OK ?                            
         BE    TRLAEL12                                                         
         MVC   FVMSGNO,=AL2(58)    ELEMENT NOT ON FILE                          
         CLI   TRLPRMS+12,5        RECORD TOO BIG                               
         BNE   TRLAELX             NO, OTHER ERROR                              
         MVC   FVMSGNO,=AL2(ACREC2BG)                                           
         B     TRLAELX                                                          
*                                                                               
TRLAEL12 CLI   TRL4KREC,YES        SPLITABLE RECORD ?                           
         BNE   TRLAELX             NO, SO RECORD IS OK                          
         MVI   TRL4KREC,NO             RESET FOR SAFETY                         
         CLC   RESRLEN,=AL2(MAXRECQ)   WOULD WE ACTUALLY SPLIT RECORD ?         
         BNH   TRLAELX                 NO, SO DON'T CARE AT THIS POINT          
         LH    R2,RESRLEN          GET RECORD LENGTH                            
         LR    R6,R4               LOAD START OF RECORD                         
         AH    R6,DATADISP         BUMP TO ELEMENTS                             
         LA    R1,ACCORFST+1       R1 = TOTAL LEN OF RECORD                     
         DROP  R4                                                               
*                                  MAKE SURE WE CAN SPLIT THIS RECORD           
         SR    RF,RF                                                            
         MVC   FVMSGNO,=AL2(ACREC2BG)  SET POTENTIAL ERROR                      
TRLAEL20 IC    RF,1(,R6)               GET LENGTH OF ELEMENT                    
         AR    R1,RF               ADD UNTIL REACH SIZE OF FIRST REC            
         SR    R2,RF               DECREASE TOTAL LENGTH BY LEN OF ELEM         
         AR    R6,RF               BUMP UP IN RECORD                            
         CLM   R1,3,=AL2(MAXRECQ)  HAVE WE REACHED THE LIMIT ?                  
         BL    TRLAEL20            NO, KEEP GOING UNTIL MAX OUT                 
         BE    *+6                                                              
         AR    R2,RF               BACK OUT LAST SR INTSR.                      
*                                                                               
         CLM   R2,3,=AL2(MAXRECQ)  WOULD 2ND RECORD BE OK ?                     
         BH    TRLAELX             STILL TOO BIG                                
         MVC   FVMSGNO,=AL2(FVFOK) RESET TO OK, RECORD IS SPLITABLE             
*                                                                               
TRLAELX  CLC   FVMSGNO,=AL2(FVFOK) SET CONCODE FOR CALLER                       
         B     TRLXIT                                                           
         EJECT ,                                                                
***********************************************************************         
*  REPLACE C"#" IN KEYWORD WITH A NUMBER...   R#C --> R18C            *         
***********************************************************************         
         SPACE 1                                                                
         USING WORKD,R7                                                         
KYW2FIX  NTR1                                                                   
         LA    R6,6                MAX TRANSLATED KEYWORD LENGTH                
         LA    R3,RKEYWORD         LOOK FOR # WITH IN KEYWORD                   
         LA    R4,TRLWORK          RE-BUILT KEYWORD AREA                        
         MVC   TRLWORK,SPACES                                                   
*                                                                               
KYW2F10  CLI   0(R3),C'#'                                                       
         BE    KYW2F20                                                          
         MVC   0(1,R4),0(R3)       MOVE IN NON-C'#'                             
         LA    R3,1(,R3)           NEXT CHARACTER  IN    KEYWORD                
         LA    R4,1(,R4)           NEXT SPOT IN RE-BUILT KEYWORD                
         BCT   R6,KYW2F10                                                       
         DC    H'00'               MUST HAVE BOOBOO                             
*                                                                               
KYW2F20  SR    R1,R1                                                            
         ICM   R1,1,RNUM2CHR       NUMBER TO CHANGE TO CHARACTER FORM           
         BNZ   *+8                                                              
         LA    R1,1                ERROR, SO FORCE TO R1 OR P1                  
         LA    RF,1                INITIALZE TO LENGTH 1                        
         LA    RE,RNUM2CHR         NO NEED TO CONVERT TO NUMBER IF              
         CLI   RNUM2CHR,C'0'       RNUM2CHR = X'F0' TO X'F9'                    
         BNL   KYW2F30                                                          
         CVD   R1,TRLDUB                                                        
         OI    TRLDUB+7,X'0F'      ADJUST SIGN FOR CHARACTER FORMAT             
         UNPK  TRLWORK2(3),TRLDUB                                               
*                                                                               
         LA    RF,3                MAX LENGTH OF 3                              
         LA    RE,TRLWORK2         POINT AT START OF EBCDIC NUMBER              
KYW2F25  CLI   0(RE),C'0'          FIND START OF NUMBER (IF NOT ZERO)           
         BNE   KYW2F30                                                          
         LA    RE,1(,RE)           NEXT CHARACTER NUMBER                        
         BCT   RF,KYW2F25                                                       
         DC    H'00'               SHOULDN'T BE ALL ZERO                        
*                                                                               
KYW2F30  BCTR  RF,0                LESS ONE FOR EXECUTE                         
         EX    RF,*+8              REPLACE C'#' WITH NUMBER                     
         B     *+10                                                             
         MVC   0(0,R4),0(RE)       MOVE IN NUMBER FROM WORK TO TRLWORK          
         LA    R4,1(RF,R4)         NEXT SPOT IN RE-BUILT KEYWORD                
*                                                                               
         SH    R6,=H'01'           MOVE REST OF KEYWORD, LESS 1 FOR #           
         BNP   KYW2F90             FINISHED, NO MORE TO MOVE IN                 
         LA    R3,1(,R3)           BUMP PAST C'#'                               
KYW2F50  MVC   0(1,R4),0(R3)       MOVE IN REST OF KEYWORD                      
         LA    R3,1(,R3)           NEXT CHARACTER  IN    KEYWORD                
         LA    R4,1(,R4)           NEXT SPOT IN RE-BUILT KEYWORD                
         BCT   R6,KYW2F50                                                       
*                                                                               
KYW2F90  MVC   RKEYWORD,TRLWORK    REPLACE OLD KEYWORD WITH NEW                 
         B     TRLXIT                                                           
***********************************************************************         
*  FIND  LENGTH OF DEFAULT PARAMETER                                  *         
*                                                                     *         
*        INPUT:  RADDPRM# = DEFAULT PARAMETER NUMBER                  *         
*                                                                     *         
*        OUTPUT: RADDPRML = DEFAULT PARAMETER LENGTH                  *         
*                                                                     *         
*        CALLS:  VDICTAT                                              *         
***********************************************************************         
         SPACE 1                                                                
GETDPRMQ NTR1                                                                   
         MVI   RDFLTPLQ,ESCLEN2Q   GET  TWO  BYTE PARM LENGTH                   
         MVC   RDFLTPLQ+1(2),RADDPRM#   DEFAULT   PARM NUMBER                   
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RDFLTPLQ,0                               
         CLI   RDFLTPLQ,C'0'       IS   1ST  CHAR C'0' ?                        
         BNE   GETDP10             NO,  CALC MAX  LENGTH                        
         CLI   RDFLTPLQ+1,C'0'     IS   2ND  CHAR >    C'0'                     
         BNH   GETDP10             NO,  CALC MAX  LENGTH                        
*                                  IS   2ND  CHAR >    C'4'                     
         CLI   RDFLTPLQ+1,C'0'+L'RPRMKEY                                        
         BH    GETDP10             YES, CALC MAX  LENGTH                        
         MVC   RADDPRML,RDFLTPLQ+1 GET  NUMBER                                  
*                                  CONVERT   TO   BINARY                        
         NI    RADDPRML,TURNOFF-C'0'                                            
         B     GETDPEX             RETURN                                       
*                                                                               
GETDP10  MVI   RDFLTPLQ,ESCLFJTQ   LEFT JUSTIFY                                 
         MVC   RDFLTPLQ+1(2),RADDPRM#   DEFAULT   PARM NUMBER                   
         MVI   RDFLTPLQ+3,L'RPRMKEY     LEN  4                                  
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RDFLTPLQ,0                               
*                                  GET  SIZE OF  DEFAULT    PARM                
         GOTO1 FLEN,TRLPRMS,(L'RPRMKEY,RDFLTPLQ)                                
         LTR   RF,RF               IS   DEFAULT  ALL   BLANK ?                  
         BNZ   *+6                 NO,  CONTINUE                                
         DC    H'00'               YES, DUMP -   SHOULD     NOT  OCCUR          
*                                                                               
GETDP30  STC   RF,RADDPRML         SAVE FULL LENGTH                             
*                                                                               
GETDPEX  B     TRLXIT              RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  ADD   DEFAULT PARAMETER                                            *         
*                                                                     *         
*        INPUT:  RADDPRM# = DEFAULT PARAMETER NUMBER                  *         
*                RADDPRML = DEFAULT PARAMETER LENGTH                  *         
*                RELMDATA = INPUT   DATA                              *         
*                TRLWORK  = PARAMETERS 1-4                            *         
*                TRLHALF  = MINIMUM DATA LENGTH                       *         
*                TRLBYTE  = DEFAULT PARAMETER GROUP TYPE              *         
*                TRL#PRMS = NUMBER  OF PARAMETERS (SO FAR)            *         
*                                                                     *         
*        USES:   TRLDUB   = DOUBLE WORD WORK AREA                     *         
*                                                                     *         
*        OUTPUT: TRLWORK  = PARAMETERS 1-4                            *         
*                TRLHALF  = MINIMUM DATA LENGTH                       *         
*                TRL#PRMS = NUMBER  OF PARAMETERS (SO FAR)            *         
*                                                                     *         
*                CONDITION CODE:                                      *         
*                          EQ = DEFAULT ADDED OR PREVIOUSLY ADDED     *         
*                          NE = DEFAULT PARM DOES NOT FIT             *         
*                                                                     *         
*        CALLS:  VDICTAT                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RPRMD,R4                                                         
DEFAULTS NTR1                                                                   
         LA    R1,RELMDATA+2       ->   1ST  PARM                               
         SR    R2,R2               CLEAR     REGISTER                           
         ICM   R2,1,TRL#PRMS       NUM  OF   PARMS                              
         BZ    DFAULT20            NONE,     ADD  PARM                          
*                                                                               
*                                  ************************************         
*                                  * SEARCH  FOR  MATCHING  PARM      *         
*                                  ************************************         
DFAULT10 MVC   TRLDUB(2),0(R1)     GET  PARM LIST ELEMENT                       
         NI    TRLDUB,X'3F'        TURN OFF  LEN  BITS                          
         CLC   TRLDUB(2),RADDPRM#  IS THE PARAMETER THERE ALREADY ?             
         BE    DFAULTOK            YES,   ONE OF THE  PARAMS, SO OK             
         LA    R1,2(,R1)           ->     TRY ANOTHER PARM                      
         BCT   R2,DFAULT10         TEST NEXT PARM                               
*                                                                               
*                                  ************************************         
*                                  * ADJUST  MINI DATA LENGTH         *         
*                                  ************************************         
DFAULT20 LH    R1,TRLHALF          GET  MINI DATA LEN (SO   FAR)                
         LA    R1,1(,R1)           ADD  ONE  FOR  COMMA                         
         ZIC   R3,RADDPRML         MIN  DFLT PARM LENGTH                        
         AR    R1,R3               NEW  MIN  PARM LENGTH                        
         LA    RE,=AL2(MXRDATLN)                                                
         CLI   REPMODE,REPCOL                                                   
         BNE   *+8                                                              
         LA    RE,=AL2(MXCDATLN)                                                
         CLM   R1,3,0(RE)          WILL IT   FIT  ?                             
         BH    DFAULTNO            NO,  EXIT WILL NOT  FIT                      
         STH   R1,TRLHALF          NEW  MIN  DATA LEN (SO   FAR)                
*                                                                               
*                                  ************************************         
*                                  * INSERT  DEFAULT   PARAMETER      *         
*                                  ************************************         
         LA    R4,TRLWORK          ->   PARM AREA                               
         SR    R2,R2               CLEAR     REGISTER                           
         ICM   R2,1,TRL#PRMS       NUM  OF   PARMS                              
         BZ    DFAULT30            NONE,     SKIP                               
         MH    R2,=AL2(RPRMLNQ)    ->   NEXT PARM AREA                          
         AR    R4,R2                                                            
*                                                                               
DFAULT30 MVC   RPRMGRP,TRLBYTE     SAVE TYPE OF   PARM GROUP                    
         SR    R2,R2               CLEAR     REGISTER                           
         IC    R2,TRL#PRMS         NUM  OF   PARMS                              
         LA    R2,1(,R2)                                                        
         STC   R2,TRL#PRMS         NEW  NUM  OF   PARMS                         
*                                                                               
         BCTR  R3,0                GET  EX   LEN  OF   ORIG PARM                
         STC   R3,RPRMOEXL         SAVE EX   LEN  OF   ORIG PARM                
         MVC   RPRMDIC,RADDPRM#    SAVE DEFAULT   PARM NUMBER                   
         MVI   RPRMKEY,ESCLFJTQ    LEFT JUSTIFY                                 
         MVC   RPRMKEY+1(2),RADDPRM#    DEFAULT   PARM NUMBER                   
         MVI   RPRMKEY+3,L'RPRMKEY      LEN  4                                  
         GOTO1 VDICTAT,TRLPRMS,C'SU  ',RPRMKEY,0                                
*                                  GET  SIZE OF  DEFAULT    PARM                
         GOTO1 FLEN,TRLPRMS,(L'RPRMKEY,RPRMKEY)                                 
         STC   RF,RPRMLEN          ACTUAL    LENGTH    OF   PARM                
         LR    R1,RF                                                            
         A     R1,TRLFULL                                                       
         ST    R1,TRLFULL          SAVE FULL LEN  OF   ALL  PARMS               
         BCTR  RF,0                                                             
         STC   RF,RPRMFEXL         SAVE EX   LEN  OF   FULL PARM                
         B     DFAULTOK            RETURN    OKAY                               
*                                                                               
*                                  ************************************         
*                                  * RETURN  TO   CALLER              *         
*                                  ************************************         
DFAULTNO CLI   =AL1(1),0           SAY  DEFAULT   NOT  ADDED                    
         B     *+8                 EXIT                                         
DFAULTOK CLI   =AL1(1),1           SAY  DEFAULT   IN   PARM LIST                
         B     TRLXIT              RETURN                                       
         DROP  R4,R7,RC                                                         
         SPACE 3                                                                
         LTORG                                                                  
         DROP  R9,RA                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO VALIDATE ROW AND COLUMN PARAMETERS                       *         
*                                                                     *         
* INPUT PARM LIST:                                                    *         
*   P1 BYTES 0-3 = A(ROW/COLUMN ELEMENT TO MODIFY)                    *         
*   P2 BYTE  0   = NUMBER OF PARAMETERS IN SCAN BLOCK ( >= 1)         *         
*                  (THIS DOES NOT INCLUDE THE KEYWORD PARAMETER)      *         
*      BYTES 1-3 = A(SCAN BLOCK)                                      *         
*   P3 BYTES 0-3 = A(KEYWORD DEFINITION BLOCK)                        *         
*   P4 BYTES 0-3 = A(BLOCK TO RETURN THE ADDRESSES OF THE             *         
*                    PARAMETER DEFINITION BLOCKS; I.E. 4 FULL WORDS)  *         
*                = 0 DO NOT RETURN THESE ADDRESSES                    *         
*                                                                     *         
* ON OUTPUT:                                                          *         
*   FVMSGNO  = IF NO ERRORS WERE FOUND, FVFOK                         *         
*            = IF AN ERROR  WAS  FOUND, MESSAGE NUMBER                *         
*   FVEXTRA  = IF NO ERRORS WERE FOUND, SPACES                        *         
*            = IF AN ERROR  WAS  FOUND, LAST PARAMETER PROCESSED      *         
*                                                                     *         
*   PAREMETER DEFINITION BLOCK ADDRESSES IF REQUESTED                 *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R6                                                       
         USING WORKD,R7                                                         
         USING VPMWRKD,RC                                                       
VALPARMS NMOD1 VPMWRKX-VPMWRKD,**VPRM**,CLEAR=YES                               
         MVI   USEDTYPE,0          INITIALIZE                                   
         MVI   SINGLEPM,0          CLEAR DUPLICATE CHECKS   (GRP5 TYPE)         
         L     R3,0(,R1)           ->    ROW  OR   COLUMN      ELEMENT          
         MVI   TYPEPARM,0          INITIALIZE ROW  OR   COLUMN TYPE             
         CLI   0(R3),RRWELQ        ROW   ELEMENT ?                              
         BNE   *+8                 NO,   SKIP                                   
         MVI   TYPEPARM,PARMROW    YES,  INDICATE  ROW                          
         CLI   0(R3),RCLELQ        COL   ELEMENT ?                              
         BNE   *+8                 NO,   SKIP                                   
         MVI   TYPEPARM,PARMCOL    YES,  INDICATE  COLUMN                       
         ZIC   R0,4(,R1)           GET   NUMBER    OF   PARAMETERS              
         L     R4,4(,R1)           ->    SCAN BLOCK                             
         L     R6,8(,R1)           ->    DEFTAB    ENTRY                        
         ST    R6,CURDEFXN         SAVE  KEYWORD   ENTRY                        
         MVC   SVKWDGRP,DEFRWGRP   SAVE  ROW  KEYWORD   GROUP                   
         TM    TYPEPARM,PARMCOL    COLUMN ?                                     
         BZ    *+10                NO,   SKIP                                   
         MVC   SVKWDGRP,DEFCLGRP   SAVE  COL  KEYWORD   GROUP                   
         ICM   R2,15,12(R1)        ->    OUTPUT    PARM LIST                    
         ST    R2,AOUTPRML         SAVE  ADDRESS   OUTPUT    PARM LIST          
         BZ    VALPAR05            NONE  SKIP                                   
         XC    0(16,R2),0(R2)      CLEAR OUTPUT    PARM LIST                    
*                                                                               
VALPAR05 XC    PARMNUM,PARMNUM     CLEAR PARM NUMBER                            
         XC    RSVMSGNO,RSVMSGNO   SAVE  FVMSGNO                                
         MVC   FVMSGNO,=AL2(ACEPRM)      INVALID   PARAMETER                    
         MVC   FVXTRA,SPACES       CLEAR POSSIBLE  ERROR                        
         MVC   SVUSDTYP,USEDTYPE   SAVE  USEDTYPE                               
         MVC   SVSINGPM,SINGLEPM   SAVE  SINGLEPM                               
         MVC   SVAOUTPL,AOUTPRML   SAVE  OUTPUT    PARM LIST ADDRESS            
         MVI   RRTNSW,0            CLEAR ROUTINE   SWITCH                       
         L     R6,CURDEFXN         ->    KEYWORD   ENTRY                        
         LA    R4,32(,R4)  ->      1ST/NEXT  PARM FIELD                         
         SR    R2,R2                                                            
         IC    R2,0(,R4)           LENGTH     OF   PARAMETER                    
         SH    R2,=H'01'           MINUS ONE  FOR  EXECUTE                      
         BM    VALPARXX            NO    PARM TO   CHECK                        
         EXMVC R2,FVXTRA,12(R4)    SAVE  INPUT     PARAMETER                    
         CLI   0(R4),4             PARAMETER  TOO  LONG ?                       
         BH    VALPARXX            YES,  EXIT                                   
*                                                                               
         TM    DEFCLGRP,GRP4       GROUP 4 ?                                    
         BZ    VALPAR15            NO,   SKIP                                   
         LA    RF,4                                                             
         LA    R5,12(,R4)                                                       
*                                                                               
VALPAR10 CLI   0(R5),C'1'          ANY   NUMBER ?                               
         BNL   VALPAR12            NO,   SKIP                                   
         LA    R5,1(,R5)                                                        
         CLI   0(R5),C'?'          C'?'  NOT  ALLOWED                           
         BE    VALPARXX            ERROR EXIT                                   
         BCT   RF,VALPAR10                                                      
         B     VALPAR15                                                         
*                                                                               
VALPAR12 LA    R6,VPMWORK                                                       
         MVC   VPMWORK,SPACES                                                   
*                                                                               
VALPAR14 MVC   0(1,R6),0(R5)                                                    
         MVI   0(R5),C'?'          REPLACE NUMBER WITH C'?'                     
         LA    R5,1(,R5)                                                        
         LA    R6,1(,R6)                                                        
         CLI   0(R5),C' '                                                       
         BNH   *+8                                                              
         BCT   RF,VALPAR14                                                      
*                                                                               
         LA    RE,VPMWORK                                                       
         SR    R6,RE               R6=LENGTH OF NUMBER                          
         GOTO1 VCASHVAL,VPMPRMS,(C'N',VPMWORK),(R6)                             
         CLI   VPMPRMS,0                                                        
         BNE   VALPARXX            ERROR EXIT                                   
         MVC   PARMNUM,VPMPRMS+6   MOVE IN NUMBER                               
*                                                                               
VALPAR15 SR    R5,R5                                                            
         IC    R5,TYPEPARM         GET ROW/COL TYPE                             
         L     R6,ACPRMTAB         TABLE OF PARAMETERS                          
*                                                                               
         USING PARMD,R6                                                         
VALPAR20 CLI   PARMD,EOT           END   OF   TABLE ?                           
         BE    VALPARX1            YES,  NO   SUCH PARAMETER                    
         EX    R5,*+8              MATCH ROW  OR   COL TYPE PARAMETER           
         BZ    VALPAR27            NO    MATCH,    TRY NEXT                     
         TM    PARMTYPE,0                                                       
         MVC   VPMPARM,PARMCDE                                                  
         CLI   VPMPARM,ESCHIGHQ    TEST FOR ESCAPE SEQUENCE                     
         BNL   VALPAR22                                                         
         GOTO1 VDICTAT,VPMPRMS,SCDICONE,VPMPARM,0                               
*                                                                               
VALPAR22 EXCLC R2,VPMPARM,12(R4)                                                
         BNE   VALPAR27            NO MATCH TRY NEXT                            
         LA    RE,PARMREPS         MATCH REPORT TYPE                            
         LA    RF,L'PARMREPS       UP TO 8 TYPES                                
         CLC   =C'ALL',0(RE)       ALL TYPES ARE EXCEPTED?                      
         BE    VALPAR26                                                         
*                                                                               
VALPAR25 CLI   0(RE),C' '          END OF TYPES                                 
         BE    VALPAR27            NO MATCH TRY NEXT                            
         CLC   APREPJCL,0(RE)      CURRENT RPT TYPE                             
         BE    VALPAR26            MATCH FOUND                                  
         LA    RE,1(,RE)           NEXT REPORT TYPE                             
         BCT   RF,VALPAR25         LOOP                                         
         B     VALPAR27            TRY NEXT PARAMETER                           
*                                                                               
VALPAR26 SR    RE,RE               CLEAR REGISTER                               
         IC    RE,PARMUSED         SEE IF PARAMETER GROUP ALLOWED               
         EX    RE,*+8                  IN THE KEYWORD GROUP LIST                
         BO    VALPAR28            YES, FOUND MATCH                             
         TM    SVKWDGRP,0          CHECK KEYWORD GROUP FIELD                    
*                                                                               
VALPAR27 LA    R6,PARMLNQ(,R6)                                                  
         B     VALPAR20            LOOP FOR  NEXT PARMETER                      
*                                                                               
VALPAR28 ICM   R1,15,AOUTPRML      ->    OUTPUT    PARM LIST ADDRESS            
         BZ    VALPAR29            NONE, SKIP                                   
         ST    R6,0(,R1)           SAVE  ADDRESS                                
         LA    R1,4(,R1)           ->    NEXT PARM ADDRESS                      
         ST    R1,AOUTPRML         SAVE  OUTPUT    PARM LIST ADDRESS            
*                                                                               
VALPAR29 OC    PARMNUM,PARMNUM     CHECK NUMBER RANGE?                          
         BZ    VALPAR30                                                         
         SR    R1,R1                                                            
         IC    R1,PARMX#VP         GET  MAX  VALUE ALLOWED                      
         CH    R1,PARMNUM                                                       
         BL    VALPARER            OUT  OF   RANGE                              
*                                                                               
VALPAR30 SR    R1,R1                                                            
         ICM   R1,1,PARMUSED       GET WHICH GROUP IN                           
         BZ    VALPAR35            NO GROUP TO MARK USED                        
         MVC   FVMSGNO,=AL2(ACEDUPR)                                            
         CLI   PARMUSED,PARMGRP1   SPECIAL GROUP (GRP 1)                        
         BNE   VALPAR32                                                         
         IC    R1,PARMGIND         GROUP PARAMETER INDICATOR                    
         EX    R1,*+8                                                           
         BNZ   VALPARER            DUPLICATE PARAMETER USED                     
         TM    SINGLEPM,0                                                       
         OC    SINGLEPM,PARMGIND   MARK USED                                    
         B     VALPAR35                                                         
*                                                                               
VALPAR32 EX    R1,*+8                                                           
         BNZ   VALPARER            DUPLICATE PARAMETER USED                     
         TM    USEDTYPE,0                                                       
         OC    USEDTYPE,PARMUSED   MARK FOR NEXT TIME IN                        
*                                                                               
VALPAR35 MVC   FVMSGNO,PARMERR#    MOVE IN ERROR NUMBER                         
         CLI   PARMRKWD,0          ANY RULES IN KEYWORD DEFINITION              
         BE    VALPAR45            NO                                           
         SR    RF,RF                                                            
         IC    R1,PARMRKWD         GET DISPLACEMENT INTO KEYWORD                
         IC    RF,PARMROTH         VALUE TO VERIFY                              
         TM    TYPEPARM,PARMROW    IS IT A ROW PARAMETER                        
         BO    VALPAR40                                                         
         IC    R1,PARMCKWD         GET DISPLACEMENT INTO KEYWORD                
         IC    RF,PARMCOTH         VALUE TO VERIFY                              
*                                                                               
VALPAR40 A     R1,CURDEFXN         ADDRESS OF DEFINITION ENTRY                  
         EX    RF,*+8                                                           
         BNO   VALPARER            ERROR                                        
         TM    0(R1),0                                                          
*                                                                               
VALPAR45 SR    R1,R1                                                            
         IC    R1,PARMRRW          DISPLACEMENT TO ROW OPTION                   
         TM    TYPEPARM,PARMROW    IS IT A ROW PARAMETER                        
         BO    *+8                                                              
         IC    R1,PARMRCL          DISPLACEMENT TO COLUMN OPTION                
         AR    R1,R3               POINT TO OPTION INTO ELEMENT                 
         MVC   SV0OFFR1(1),0(R1)   SAVE  0(R1)                                  
         OI    RRTNSW,RRTNUSED     0(R1) IS   SAVED                             
*                                                                               
         CLI   PARMUSED,PARMGRP3   CODE/NAME                                    
         BNE   *+8                                                              
         NI    0(R1),TURNOFF-(NME+CDE)                                          
         CLI   PARMUSED,PARMGRP4   PERIOD BASIS                                 
         BNE   *+8                                                              
         NI    0(R1),TURNOFF-RCLTODTE                                           
*                                                                               
VALPAR60 MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   PARMUSED,PARMGRP2   DATE BASES                                   
         BNE   VALPAR75                                                         
*                                                                               
VALPAR70 TM    TYPEPARM,PARMROW    IS IT A ROW PARAMETER                        
         BZ    VALPAR72                                                         
         TM    PARMGIND,PARMROC    DO 'OC' INSTEAD OF 'MVC'?                    
         BO    VALPAR75                                                         
         MVC   0(1,R1),PARMRIND                                                 
*                                                                               
VALPAR72 TM    TYPEPARM,PARMCOL    IS IT A COL PARAMETER                        
         BZ    VALPAR80                                                         
         TM    PARMGIND,PARMCOC    DO 'OC' INSTEAD OF 'MVC'?                    
         BO    VALPAR75                                                         
         MVC   0(1,R1),PARMCIND                                                 
         B     VALPAR80                                                         
*                                                                               
VALPAR75 TM    TYPEPARM,PARMROW    IS IT A ROW PARAMETER                        
         BZ    *+10                                                             
         OC    0(1,R1),PARMRIND                                                 
         TM    TYPEPARM,PARMCOL    IS IT A COL PARAMETER                        
         BZ    *+10                                                             
         OC    0(1,R1),PARMCIND                                                 
*                                                                               
         USING RCLELD,R3                                                        
VALPAR80 TM    TYPEPARM,PARMCOL    COLUMN ONLY                                  
         BZ    VALPAR85                                                         
         CLI   RCLDATES,RCLBMOA    BILLED MOA DATE?                             
         BE    VALPAR82            YES, SO CHECK DAY PARAMETER USED             
         CLI   RCLDATES,RCLMOS     MOS DATE?                                    
         BE    VALPAR82            YES, SO CHECK DAY PARAMETER USED             
         CLI   RCLDATES,RCLMODT    MOA DATE?                                    
         BNE   VALPAR85            NO,  SO OK                                   
*                                                                               
VALPAR82 TM    RCLDTEFG,RCLDAY     DAY PARAMETER USED ?                         
         BO    VALPAR84            YES, SO ERROR                                
         TM    RCLDTEFG,RCLPERD    CALENDAR PERIOD PARAMETER USED ?             
         BZ    VALPAR85            NO,  SO OK                                   
*                                                                               
VALPAR84 MVC   FVMSGNO,=AL2(ACEDAYPR)   ERROR, INVALID PARM WITH MOA            
         B     VALPARER                                                         
*                                                                               
VALPAR85 L     R5,ATWA             RE-LOAD TWA STARTING ADDRESS                 
         SR    RF,RF                                                            
         ICM   RF,1,PARMROUT       SPECIAL ROUTINE BASED ON PARAMETER?          
         BZ    VALPARX             NONE                                         
         LA    RE,VALPARX          RETURN VIA RE                                
         SLL   RF,2                                                             
         B     *(RF)               BRANCH TO ROUTINE                            
         B     VALPINFO                                                         
*                                                                               
VALPARX  B     VALPARX2                TEST  NEXT    PARAMETER                  
*                                                                               
VALPINFO XC    RCLENDT,RCLENDT         CLEAR                                    
         MVC   RCLENDT+1(1),PARMNUM+1  MAY   CONTAIN NUMBER                     
         BR    RE                                                               
*                                                                               
VALPARER OC    RSVMSGNO,RSVMSGNO       ANY   SAVED   MESSAGE  NUMBER ?          
         BNZ   *+10                    YES,  SKIP                               
         MVC   RSVMSGNO,FVMSGNO        NO,   SAVE    MESSAGE  NUMBER            
         MVC   USEDTYPE,SVUSDTYP       RESTORE       USEDTYPE                   
         MVC   SINGLEPM,SVSINGPM       RESTORE       SINGLEPM                   
         MVC   AOUTPRML,SVAOUTPL       RESTORE       AOUTPRML                   
         TM    RRTNSW,RRTNUSED         WAS   0(R1)   SAVED ?                    
         BZ    *+10                    NO,   SKIP                               
         MVC   0(1,R1),SV0OFFR1        RESTORE       0(R1)                      
         NI    RRTNSW,TURNOFF-RRTNUSED RESET 0(R1)   SAVED    INDICATOR         
         MVC   FVMSGNO,=AL2(ACEPRM)    RESTORE       FVMSGNO                    
         B     VALPAR27                CHECK FOR     A        BETTER            
*                                            MATCHING         PARAMETER         
*                                                                               
VALPARX1 DS    0H                      PARM  NOT     RESOLVED                   
         OC    RSVMSGNO,RSVMSGNO       ANY   SAVED   MESSAGE  NUMBER ?          
         BZ    *+10                    NO,   SKIP                               
         MVC   FVMSGNO,RSVMSGNO        YES,  RESTORE MESSAGE  NUMBER            
         B     VALPARXX                EXIT                                     
*                                                                               
VALPARX2 DS    0H                      GOOD  PARAMETER                          
         BCT   R0,VALPAR05             PROCESS       NEXT     PARAMETER         
         MVC   FVXTRA,SPACES           CLEAR ERROR   PARAMETER                  
*                                                                               
VALPARXX CLC   FVMSGNO,=AL2(FVFOK)                                              
         XIT1                                                                   
         DROP  R3,R6,R7,RC                                                      
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE PFKEY ENTRY PASSED IN R3                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING PFKTABD,R3                                                       
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING RWRKD,RC                                                         
         SPACE 1                                                                
VALPFK   NMOD1 0,**VPFK**                                                       
         L     RC,0(,R1)           ->   WORKING STORAGE                         
         L     R3,4(,R1)           LOAD TABLE PFK ENTRY                         
         CLI   PFKREC,EOT          END OF TABLE                                 
         BE    VALPFKNO            INVALID PFKEY PRESSED                        
         CLI   PFKSCR,0            DO WE NEED TO MATCH SCREEN?                  
         BE    VALPFK05            NO                                           
         CLC   PFKSCR,INSCRN       MATCH TO CURRENT SCREEN?                     
         BNE   VALPFKNO            NO MATCH                                     
*                                                                               
VALPFK05 CLC   PFKREC,SCRECN       CURRENT RECORD                               
         BNE   VALPFKNO            NO MATCH                                     
         CLI   PFKACT,ANYACT       VALID FOR ANY ACTION?                        
         BE    VALPFK20            NO ACTION NEED TO MATCH                      
         TM    PFKIND,PFKISAT      SELECT ACTION MATCHING?                      
         BZ    VALPFK10                                                         
         CLC   PFKACT,SVSELACT     MUST BE SET IN DISKEY                        
         BE    VALPFK20                                                         
         B     VALPFKNO                                                         
*                                                                               
VALPFK10 CLC   PFKACT,INACT        DOES ACTION MATCH? (FOR SELECT)              
         BE    VALPFK20            OK SO FAR                                    
         CLC   PFKACT,SCACTN       DOES ACTION MATCH?                           
         BE    VALPFK20            NO MATCH ON ACTION                           
         TM    TWAMODE,TWAMSEL                                                  
         BZ    VALPFKNO                                                         
         CLI   PFKACT,ACTSEL       DOES ACTION MATCH?                           
         BNE   VALPFKNO            NO MATCH ON ACTION                           
*                                                                               
VALPFK20 CLI   PFKTYPE,ANYTYPE     VALID FOR ANY TYPE?                          
         BE    VALPFK25                                                         
         CLC   PFKTYPE,APREPTY#    DOES TYPE MATCH?                             
         BNE   VALPFKNO            NO, NOT A VALID PFKEY                        
*                                                                               
VALPFK25 TM    PFKIND,PFKDDS       DDS ONLY PFKEY?                              
         BZ    VALPFK30            NO, SO CONTINUE                              
         TM    CUSTAT,CUSDDS       IS IT DDS TERMINAL?                          
         BZ    VALPFKNO            NO, SO GET NEXT PFKEY                        
*                                                                               
VALPFK30 TM    PFKIND,PFKILSM      SPECIAL MODE VALIDATION?                     
         BZ    VALPFK35                                                         
         TM    TWAMODE,TWAMLSM     LSM MODE?                                    
         BZ    VALPFKNO            NOT RIGHT MODE                               
*                                                                               
VALPFK35 TM    PFKIND,PFKISEL      SEL MODE?                                    
         BZ    VALPFK40                                                         
         TM    TWAMODE,TWAMSEL                                                  
         BZ    VALPFKNO            NOT VALID PFK                                
*                                                                               
VALPFK40 TM    PFKIND,PFKILFM                                                   
         BZ    VALPFK45                                                         
         TM    TWAMODE,TWAMSEL+TWAMLSM                                          
         BNZ   VALPFKNO                                                         
*                                                                               
VALPFK45 CLI   PFKACT,ACTHLP       IS    ACTION    HELP ?                       
         BNE   VALPFK60            NO,   SKIP                                   
         CLI   PFKSEL,PFKPASTE     PASTE AND  RETURN    PF   KEY ?              
         BNE   VALPFK60            NO,   SKIP                                   
         NC    CUR@RHLP,CUR@RHLP   ANY   RETURN    ADDRESS ?                    
         BZ    VALPFKNO            NO,   INVALID   PF   KEY                     
*                                                                               
VALPFK60 OC    TWASAGN,TWASAGN     NEW SECURITY?                                
         BZ    VALPFKOK            NO                                           
         MVC   RHALF(1),INREC      CURRENT RECORD                               
         CLI   PFKNEWRE,NULL       IS THERE A NEW RECORD TYPE?                  
         BE    *+10                NO                                           
         MVC   RHALF(1),PFKNEWRE   YES SO USE THAT ONE                          
         MVC   RHALF+1(1),INACT    CURRENT ACTION                               
         TM    TWAMODE,TWAMLSM     LIST/SELECT MODE                             
         BZ    *+10                                                             
         MVC   RHALF+1(1),SCACTN   CURRENT ACTION                               
         CLI   PFKNEWAC,NULL       IS THERE A NEW ACTION TYPE?                  
         BE    *+10                NO                                           
         MVC   RHALF+1(1),PFKNEWAC YES SO USE THAT ONE                          
         TM    PFKIND,PFKIRST                                                   
         BZ    VALPFK80                                                         
         MVI   RHALF+1,ACTDIS      DEFAULT TO DISPLAY                           
         CLI   SVRECORD,0          ANYTHING SAVED?                              
         BE    VALPFK80            NO                                           
         MVC   RHALF(1),SVRECORD                                                
         MVC   RHALF+1(1),SVACTION                                              
*                                                                               
VALPFK80 LA    RF,RHALF                                                         
         GOTO1 VSECRET,RPARM,('SECPRACT',ACASEC),(0(RF),1(RF))                  
         BNE   VALPFKNO            NO GOOD, GET NEXT                            
*                                                                               
VALPFKOK SR    RE,RE                                                            
*                                                                               
VALPFKNO LTR   RE,RE                                                            
         XIT1                                                                   
         DROP  R3,R5,R7,RC                                                      
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
**********************************************************************          
*  MAKE A LIST OUT OF A SCREEN FIELD                                 *          
*                                                                    *          
*  ON INPUT;                                                         *          
*     PARM LIST:                                                     *          
*        P1  BYTE  0   = TYPE OF FILTER LIST (X'C5' ELEMENT)         *          
*        P1  BYTES 1-3 = FIELD HEADER TO MAKE LIST FROM              *          
*        P2  BYTES 0-3 = ADDRESS OF THE RECORD WHERE THE FILTER LIST *          
*                        ELEMENT WILL EXIST                          *          
*        P3  BYTE  0   = MAXIMUM NUMBER OF SCAN LIST ELEMENTS        *          
*                        NOTE: THE USER MAY INPUT NUMBER - 1 ELEMENTS*          
*        P3  BYTES 1-3 = ADDRESS OF SCAN LIST BLOCK                  *          
*        P4  BYTE  0   = COLUMN NUMBER TO SEARCH FOR A MATCH OR ZERO *          
*                          NOTE: NEEDED FOR ACSCR12                  *          
*        P4  BYTES 1-3 = ADDRESS TO BUILD THE CURRENT FILTER ELEMENT *          
*                          NOTE: MUST BE 256 BYTES LONG              *          
*        P5  BYTE  0   = X'80' USE EXTENDED LENGTH LSCANRHS FOR      *          
*                        CALL TO SCANNER                             *          
*                                                                    *          
*     DATADISP = DISPLACEMENT OF FIRST ELEMENT FROM THE BEGINNING OF *          
*                THE  RECORD                                         *          
*     APREPJCL = REPORT JCL ID                                       *          
*                                                                    *          
*  USES:                                                             *          
*     APELCODE = ELEMENT TYPE FOR DELEL                              *          
*     DELEL    = DELETES ANY EXISTING ELEMENTS MARKED FOR DELETION   *          
*     AFVAL    = VALIDATES THE INPUT FIELD                           *          
*                                                                    *          
*  ON OUTPUT:                                                        *          
*     RF = RETURN CODE                                               *          
*           0 = LIST BUILT (GOOD RETURN)                             *          
*           4 = INVALID INPUT                                        *          
*          -1 = NO DATA FOUND                                        *          
*                                                                    *          
*     PARM LIST:                                                     *          
*       P1, P2, P3 = VSCANNER OUTPUT PARMS (IF RF = 0)               *          
*                                                                    *          
*     APELEM   = THE CURRENT FILTER ELEMENT (IF RF = 0)              *          
*     FVMSGNO  = ERROR MESSAGE NUMBER (IF RF NOT = 0)                *          
*     NPARMS   = NUMBER OF SCAN BLOCK ELEMENTS FOUND (IF RF = 0)     *          
*                                                                    *          
*  NOTES:                                                            *          
*     1. THIS SUBROUTINE ALWAYS DELETES ANY EXISTING FILTER LIST     *          
*        ELEMENTS WHICH MATCHES THE REQUESTED 'TYPE OF FILTER LIST'  *          
*        IN THE RECORD WHERE THE ELEMENT WILL EXIST.                 *          
*     2. THIS SUBROUTINE STARTS TO BUILD THE FILTER LIST ELEMENT     *          
*        IN APELEM.                                                  *          
*     3. THIS SUBROUTINE REMOVES THE EXCLUDE CHARACTER (C'*') FROM   *          
*        THE FIRST BYTE OF THE INPUT DATA BEFORE CALLING AFVAL.      *          
*        THE DELETE INDICATOR IS MOVED TO THE FILTER LIST ELEMENT    *          
*        AS RFLXCLD IN RFLIND.                                       *          
*     4. THIS SUBROUTINE ASSUMES THAT THE MINIMUM LENGTH OF THE      *          
*        INPUT DATA IS ZERO.                                         *          
*     5. IF THE COLUMN NUMBER (P4) IS NOT ZERO, THEN                 *          
*           RFLTTYPE AND RFLBUDGT DATA WILL NOT BE MOVED INTO RFLDATA*          
*     6. IF THE COLUMN NUMBER (P4) IS NOT ZERO, AND THE 'TYPE OF     *          
*        FILTER LIST' IS CONTRA RECORD, AND THE REPORT JCL ID IS     *          
*        RECEIVABLES, THEN                                           *          
*           FLTYPE WILL BE CHANGED TO BILLING SOURCE                 *          
*                                                                    *          
**********************************************************************          
         EJECT ,                                                                
         USING RFLELD,R1                                                        
         USING WORKD,R7                                                         
         USING MKLWRKD,RC                                                       
RMAKLIST NMOD1 MKLWRKX-MKLWRKD,**MLST**                                         
         LR    R3,R1               SAVE  PARM LIST                              
         L     R4,0(,R3)           LOAD  HEADER ADDRESS                         
         L     R2,4(,R3)           LOAD  RECORD WHERE LIST WILL EXISTS          
*                                                                               
         MVC   MKLELEM,APELEM      SAVE  CURRENT APELEM                         
         MVC   MKLPARM1,12(R3)                                                  
         MVI   MKLPARM1,0          CLEAR OUT HIGH ORDER BYTE                    
*                                  BECAUSE OF POSSIBLE COL NUMBER               
         LR    R1,R2                                                            
         AH    R1,DATADISP                                                      
*                                                                               
*                                  ************************************         
MAKLST10 DS    0H                  * DELETE OLD COPIES OF THE ELEMENT *         
*                                  ************************************         
         CLI   0(R1),0             END   OF RECORD ?                            
         BE    MAKLST30                                                         
         CLI   0(R1),RFLELQ        DELETE FILTER LIST X'C5' ELEM.               
         BNE   MAKLST20            LOOP  TO NEXT ELEMENT                        
         CLC   RFLSEQ,12(R3)       MAKE  SURE HIGH LEVEL FILTER                 
         BNE   MAKLST20            LOOP  TO NEXT ELEMENT                        
         CLC   RFLTYPE,0(R3)       MATCH TYPE OF FILTER                         
         BNE   MAKLST20            LOOP  TO NEXT ELEMENT                        
         MVI   0(R1),X'FF'         MARK  FOR DELETION                           
*                                                                               
MAKLST20 SR    RF,RF                                                            
         IC    RF,1(,R1)           GET   LENGTH                                 
         AR    R1,RF               BUMP  TO NEXT ELEMENT                        
         B     MAKLST10            LOOP                                         
*                                                                               
*AKLST30 MVI   APELCODE,X'FF'      DELETE X'FF' ELEMENTS                        
MAKLST30 GOTO1 VHELLO,MKLPARM,(C'D',=CL8'ACCVBIG'),(X'FF',(R2)),0               
         CLI   MKLPARM+12,0        ELEMENT ADD OK                               
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(58)                                                 
         GOTO1 AFVAL,(R4)                                                       
         BH    MAKLSTER            INPUT ERROR                                  
         BL    MAKLSTNO            NO    INPUT                                  
*                                                                               
*                                  ************************************         
*                                  * BUILD THE ELEMENT                *         
*                                  ************************************         
         L     R1,12(,R3)          ->    CURRENT FILTER ELEMENT                 
*                                  CLEAR CURRENT FILTER ELEMENT                 
         XC    0(L'APELEM,R1),0(R1)                                             
         MVI   RFLEL,RFLELQ        X'C5' ELEMENT                                
         SR    RF,RF                                                            
         IC    RF,FVXLEN           EXMVC LENGTH                                 
         LA    RE,FVIFLD                                                        
         CLI   FVIFLD,C'*'         EXCLUSION?                                   
         BNE   MAKLST40                                                         
         LA    RE,FVIFLD+1                                                      
         OI    RFLIND,RFLXCLD      MARK  AS SUCH                                
         SH    RF,=H'01'           ONLY  C'*' INPUTED ?                         
         BM    MAKLSTEI            YES,  INVALID INPUT                          
*                                                                               
MAKLST40 DS    0H                                                               
         CLI   12(R3),0            COLUMN NUMBER SPECIFIED ?                    
         BE    MAKLST50            NO,   MOVE THE DATA INTO ELEMENT             
         CLI   0(R3),RFLTTYPE      TRANSACTION TYPES AND BUDGETS CAN'T          
         BE    MAKLST60                  COPY FIELD DIRECTLY, WE HAVE           
         CLI   0(R3),RFLBUDGT            TO MOVE DATA & LENGTH AFTER            
         BE    MAKLST60                  VALIDATION                             
*                                                                               
MAKLST50 DS    0H                                                               
         EXMVC RF,RFLDATA,0(RE)    MOVE  THE DATA INTO THE ELEMENT              
*                                                                               
*                                  ************************************         
MAKLST60 DS    0H                  * SET UP DUMMY HEADER BECAUSE OF   *         
*                                  * EXCLUDE "*" CHARACTER            *         
*                                  ************************************         
         MVC   MKLFLD,SPACES                                                    
         EXMVC RF,MKLFLD,0(RE)                                                  
         MVC   MKLFLDH,FVIHDR                                                   
         NI    MKLFLDH+1,X'FD'     TURN  OFF EXTENDED FIELD HEADER              
         LA    RF,1(,RF)           ADD   ONE BACK FOR EXECUTE INSTR.            
         STC   RF,MKLFLDH+5                                                     
*                                                                               
*                                  ************************************         
*                                  * CONTINUE BUILDING THE FILTER EL  *         
*                                  ************************************         
         LA    RF,RFLLNQ(,RF)      FIGURE OUT LENGTH OF ELEMENT                 
         STC   RF,RFLLN                                                         
*        STC   RF,FVILEN                                                        
         MVC   RFLSEQ,12(R3)       MAKE  HIGH LEVEL FILTER                      
         MVC   RFLTYPE,0(R3)       SET   TYPE OF FILTER                         
*                                                                               
         CLI   12(R3),0            COLUMN NUMBER SPECIFIED ?                    
         BE    MAKLST70            NO,   SCAN THE DATA                          
         CLI   RFLTYPE,RFLCNTR     CONTRA ELEMENT                               
         BNE   MAKLST70            NO,   SCAN THE DATA                          
         CLI   APREPJCL,REPJCLR    REPORT JCL ID IS RECEIVABLES ?               
         BNE   MAKLST70            NO,   SCAN THE DATA                          
         MVI   RFLTYPE,RFLBSR      CHANGE TO BILLING SOURCE                     
*                                                                               
*                                  ************************************         
MAKLST70 DS    0H                  * SCAN THE INPUT BLOCK             *         
*                                  ************************************         
         ZIC   R5,8(,R3)           GET   MAX  NO. OF SCAN LIST ELEMENTS         
         SR    RF,RF               CLEAR REGISTER                               
         ICM   RF,7,9(R3)          ->    SCAN BLOCK                             
         TM    16(R3),X'80'        USE LSCANRHS IN SCANNER?                     
         BZ    MAKLST80                                                         
         GOTO1 VSCANNER,(R3),('LSCANRHS',MKLFLDH),((R5),(RF)),SCNP3NEQ          
         B     MAKLST90                                                         
MAKLST80 GOTO1 VSCANNER,(R3),MKLFLDH,((R5),(RF)),SCNP3NEQ                       
                                                                                
MAKLST90 MVC   NPARMS,4(R3)                                                     
         CLM   R5,1,4(R3)          TOO   MANY ELEMENTS ?                        
         BH    MAKLSTOK            RC =  OK                                     
         MVC   FVMSGNO,=AL2(ACE2MANY)                                           
         B     MAKLSTER            RC =  NOT OK                                 
*                                                                               
MAKLSTEI DS    0H                  NOT   VALID INPUT                            
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
MAKLSTER DS    0H                                                               
         L     RF,=F'4'            SET   ERROR RETURN CODE                      
         B     MAKLSTEX            EXIT                                         
*                                                                               
MAKLSTNO DS    0H                                                               
         L     RF,=F'-1'           SET   NOT A MAKLIST RETURN CODE              
         B     MAKLSTEX            EXIT                                         
*                                                                               
MAKLSTOK DS    0H                                                               
         SR    RF,RF               SET   GOOD RETURN CODE                       
         OI    6(R4),FVOXMT        TRANSMIT  FIELD                              
*                                                                               
MAKLSTEX DS    0H                                                               
         LA    RE,APELEM           ->    CURRENT FILTER ELEMENT                 
         C     RE,MKLPARM1         DID   WE GET APELEM AS CUR FILTER EL         
         BE    *+10                YES,  SKIP                                   
         MVC   APELEM,MKLELEM      RESTORE APELEM                               
         XIT1  REGS=(RF)           RETURN PASSING RF                            
*                                                                               
         DROP  R1,R7,RC                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  ADD MESSAGE ELEMENT                                                *         
*---------------------------------------------------------------------*         
*  PARAMETERS:                                                        *         
*    PARM 1 BYTE  0   COLUMN OR ROW OR HEADER  TYPE                   *         
*           BYTES 1-3 ADDRESS OF A TWO BYTE    MESSAGE NUMBER         *         
*    PARM 2 BYTE  0   SEQUENCE NUMBER TYPE                            *         
*           BYTE  1-3 ADDRESS OF IO AREA OF RECORD                    *         
*    PARM 3 BYTE  0   MESSAGE TYPE                                    *         
*           BYTES 1-3 ADDRESS OF A DATA FIELD (OPTIONAL)              *         
*    PARM 4 BYTE  0   LENGTH  OF   DATA FIELD (IF ANY FIELD)          *         
*           BYTE  3   TYPE    OF   DATA FIELD (IF ANY FIELD)          *         
*---------------------------------------------------------------------*         
*  INPUT AREA:                                                        *         
*    IOAREA1  - RECORD  AREA                                          *         
*                                                                     *         
*  OUTPUT:                                                           *          
*    FVMSGNO  - MESSAGE NUMBER  ON     ERROR                          *         
*    CONDITION                                                        *         
*      CODE   - EQUAL     -     MSG    ADDED                          *         
*             - NOT EQUAL -     MSG    NOT  ADDED                     *         
*                                                                     *         
*  USES:                                                              *         
*    ADMSGELM - SAVE    AREA    FOR    APELEM                         *         
*    ADMSGTYP - MESSAGE TYPE                                          *         
*    ADMSGNUM - MESSAGE NUMBER                                        *         
*    ADMSGSEQ - COLUMN/ROW/HEADER SEQUENCE NUMBER                     *         
*    ADMSGDAT - ADDRESS MESSAGE DATA                                  *         
*    ADMSGDTY - MESSAGE DATA    TYPE                                  *         
*    ADMSGDLN - MESSAGE DATA    LENGTH                                *         
*    ADMSGIO  - A(RECORD)                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ADDMSGWD,RC                                                      
         USING WORKD,R7                                                         
         USING MNOELD,R1           MESSAGE ELEMENT                              
SUB      USING MNOMLN,R2           SUB-MSG-ELEMENT PORTION                      
NEW      USING MNOELD,ADMELEM                                                   
         SPACE 1                                                                
R_ADDMSG NMOD1 ADDMSGWX-ADDMSGWD,**AMSG**,CLEAR=YES,RR=RE                       
         ST    RE,ADMRELO                                                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR   MESSAGE NUMBER                       
         MVC   ADMSGSTY,0(R1)      ROW/COLUMN OR HEADER        (PARM 1)         
         L     RF,0(,R1)           ->      MESSAGE NUMBER                       
         MVC   ADMSGNUM,0(RF)      SAVE    MESSAGE NUMBER                       
*                                                                               
         MVC   ADMSGSEQ,4(R1)      ROW/COL/HEADER SEQUENCE     (PARM 2)         
         L     RF,4(,R1)           A(IO AREA)                                   
         LA    RF,0(,RF)           CLEAR HOB                                    
         ST    RF,ADMSGIO                                                       
*                                                                               
         MVC   ADMSGTYP,8(R1)      SAVE    MESSAGE TYPE        (PARM 3)         
         L     RF,8(,R1)           ->      MESSAGE DATA                         
         LA    RF,0(,RF)           CLEAR   HIGH    ORDER   BYTE                 
         LTR   RF,RF               ANY     MESSAGE DATA ?                       
         BZ    ADDM10              NO,     SKIP                                 
         STCM  RF,15,ADMSGDAT      SAVE    MESSAGE DATA ADDRESS                 
*                                                                               
         MVC   ADMSGDLN,12(R1)     SAVE    MESSAGE DATA LENGTH (PARM 4)         
         MVC   ADMSGDTY,15(R1)     SAVE    MESSAGE DATA TYPE                    
*                                                                               
ADDM10   DS    0H                                                               
         XC    ADMELEM,ADMELEM         INITIALIZE ELEMENT                       
         MVI   NEW.MNOEL,MNOELQ        X'C9'    ELEMENT                         
         MVI   NEW.MNOLN,MNOLN2Q       LENGTH OF BASIC ELEMENT                  
         MVC   NEW.MNOSEQ,ADMSGSEQ     SEQUENCE NUMBER OF   COL/ROW/HDR         
         MVC   NEW.MNOSTYPE,ADMSGSTY   SEQUENCE NUMBER TYPE COL/ROW/HDR         
         MVI   NEW.MNOMLN,MNOMELQ      MINIMUM LENGTH OF SUB-ELEMENT            
         MVC   NEW.MNOMTYPE,ADMSGTYP   MSG TYPE ERROR/WARN/INFO ETC...          
         MVC   NEW.MNOMNUM,ADMSGNUM    MSG NUMBER                               
         LA    R2,NEW.MNOMLN           POINT TO FIRST SUB-ELEMENT               
*                                                                               
         SR    RF,RF                                                            
         L     R1,ADMSGIO          ->      RECORD                               
         AH    R1,DATADISP                                                      
*                                                                               
ADDM15   CLI   0(R1),0             END OF    RECORD ?                           
         BE    ADDM60              NOT FOUND ADD   NEW  ELEMENT                 
         CLI   0(R1),MNOELQ        X'C9'     MESSAGE    ELEMENT ?               
         BNE   ADDM18              KEEP      LOOKING                            
         CLC   MNOSEQ,ADMSGSEQ     RIGHT     SEQUENCE   NUMBER ?                
         BNE   ADDM18              KEEP      LOOKING                            
         CLC   MNOSTYPE,ADMSGSTY   COL/ROW/HDR  TYPE    ELEMENT ?               
         BE    ADDM20              YES FOUND EXAMINE SUB-ELEMENTS               
*                                                                               
ADDM18   IC    RF,1(,R1)                                                        
         AR    R1,RF               NO,     TRY  NEXT    ELEMENT                 
         B     ADDM15                                                           
*                                                                               
ADDM20   DS    0H                  FOUND   MESSAGE      ELEMENT                 
         ZIC   RF,MNOLN            LENGTH  OF           ELEMENT                 
         BCTR  RF,R0               MINUS   ONE     FOR  EXECUTE                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NEW.MNOEL(0),MNOEL  REPLACE  ELEMENT TO   ADMELEM                
         LA    R2,NEW.MNOEL+1(RF)  POINT    TO  END OF ELEMENT                  
         LR    R4,R2               SAVE OFF   A(END OF ELEMENT)                 
*                                  APPEND          SUB-ELEMENT                  
         LA    RF,MNOMELQ+1(,RF)   INCREASE BY   L'SUB-ELEMENT + 1              
         STC   RF,NEW.MNOLN        NEW ELEMENT  LENGTH                          
*                                      APPEND   SUB-ELEMENT                     
         MVI   SUB.MNOMLN,MNOMELQ      MINIMUM  LENGTH  OF SUB-ELEMENT          
         MVC   SUB.MNOMTYPE,ADMSGTYP   MSG TYPE ERROR/WARN/INFO ETC...          
         MVC   SUB.MNOMNUM,ADMSGNUM    MSG NUMBER                               
*                                                                               
*                                  SCAN SUB-ELEMENT, EXCEPT APPENDED 1          
         LA    R2,NEW.MNOMLN       POINT TO FIRST    SUB-ELEMENT                
ADDM30   DS    0H                  SAME    MESSAGE FOUND ?                      
*                                  CHECK   TYPE    ERROR/WARNING/INFO           
         CLC   ADMSGTYP,SUB.MNOMTYPE                                            
         BNE   ADDM40              NO,     GET NEXT SUB-ELEMENT                 
         CLC   ADMSGNUM,SUB.MNOMNUM        SAME MSG NUMBER ?                    
         BE    ADDMSGX             YES,    DO NOTHING ALREADY ON RECORD         
*                                                                               
ADDM40   DS    0H                                                               
         IC    RF,SUB.MNOMLN       INCREMENT TO NEXT SUB-ELEMENT                
         AR    R2,RF                                                            
         CR    R2,R4               ARE WE AT   END  OF X'C9' ELEMENT ?          
         BL    ADDM30              NO, SO LOOK AT   NEXT SUB-ELEMENT            
*                                                                               
         MVI   MNOEL,X'FF'         MARK   ORIGINAL FOR  DELETION                
         L     R4,ADMSGIO                                                       
         GOTO1 VHELLO,ADMPARM,(C'D',=CL8'ACCVBIG'),(X'FF',(R4)),0               
         CLI   ADMPARM+12,0        ELEMENT ADD OK                               
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(58)                                                 
*                                                                               
ADDM60   SR    RF,RF                                                            
         ICM   RF,1,ADMSGDLN       WAS EXTRA DATA MSG SUPPLIED ?                
         BZ    ADDM80              NO                                           
         MVC   SUB.MNOMDTYP,ADMSGDTY   BUILD EXTRA DATA IN SUB-ELEMENT          
*                                                                               
         ICM   RE,15,ADMSGDAT      ->      MESSAGE DATA                         
         BCTR  RF,0                MINUS ONE FOR   EXECUTE                      
         EX    RF,*+8                      INSERT MESSAGE DATA                  
         B     *+10                                                             
         MVC   SUB.MNOMDATA(0),0(RE)                                            
         ZIC   R1,NEW.MNOLN                                                     
         LA    R1,1+L'MNOMDTYP(R1,RF)      NEW LENGTH OF ELEMENT                
         STC   R1,NEW.MNOLN                                                     
         IC    R1,SUB.MNOMLN                                                    
         LA    R1,1+L'MNOMDTYP(R1,RF)      NEW LENGTH OF SUB-ELEMENT            
         STC   R1,SUB.MNOMLN                                                    
*                                                                               
         USING RESRECD,R4                                                       
ADDM80   L     R4,AIOAREA1         ADD ELEMENT TO RECORD                        
         LA    R6,=CL8'ACCBIG'                                                  
         MVI   ADM4KREC,NO                                                      
         CLC   =X'2D02',RESKTYP    SCRIBE RECORD ?                              
         BNE   ADDM82                                                           
         CLI   RESKSEQ,RESKSREG    X'40' REGULAR FORMAT                         
         BNE   ADDM82                                                           
         LA    R6,=CL8'ACCVBIG'                                                 
         MVI   ADM4KREC,YES                                                     
*                                                                               
ADDM82   GOTO1 VHELLO,ADMPARM,(C'P',(R6)),(R4),ADMELEM,0                        
         CLI   ADMPARM+12,0        ELEMENT ADD OK                               
         BE    ADDM84                                                           
         MVC   FVMSGNO,=AL2(58)    ELEMENT NOT ON FILE                          
         CLI   ADMPARM+12,X'05'    RECORD TOO BIG                               
         BNE   ADDMSGX                                                          
         MVC   FVMSGNO,=AL2(ACREC2BG)                                           
         B     ADDMSGX                                                          
*                                                                               
ADDM84   CLI   ADM4KREC,YES            4K TYPE ?                                
         BNE   ADDMSGX                 RECORD OK THEN                           
         MVI   ADM4KREC,NO             RESET FOR SAFETY                         
         CLC   RESRLEN,=AL2(MAXRECQ)                                            
         BNH   ADDMSGX                 RECORD IS UNDER 2K SIZE                  
         LH    R2,RESRLEN          GET RECORD LENGTH                            
         LR    R6,R4               LOAD START OF RECORD                         
         AH    R6,DATADISP         BUMP TO ELEMENTS                             
         LA    R1,ACCORFST+1       R1 = TOTAL LEN OF RECORD                     
         SH    R2,DATADISP         DECREASE TOTAL LENGTH BY LEN OF KEY          
         DROP  R4                                                               
*                                  MAKE SURE RECORD CAN BE SPLIT IN TWO         
         SR    RF,RF                                                            
         MVC   FVMSGNO,=AL2(ACREC2BG)  SET POTENTIAL ERROR                      
ADDM85   IC    RF,1(,R6)               GET LENGTH OF ELEMENT                    
         AR    R1,RF               ADD UNTIL REACH SIZE OF FIRST REC            
         SR    R2,RF               DECREASE TOTAL LENGTH BY LEN OF ELEM         
         AR    R6,RF               BUMP UP IN RECORD                            
         CLM   R1,3,=AL2(MAXRECQ)  HAVE WE REACH THE LIMIT ?                    
         BL    ADDM85              NO, KEEP GOING UNTIL MAX OUT                 
         BE    *+6                                                              
         AR    R2,RF               BACK OUT LAST AR INTSR.                      
*                                                                               
         CLM   R2,3,=AL2(MAXRECQ)  WOULD 2ND RECORD BE OK ?                     
         BH    ADDMSGX             STILL TOO BIG                                
         MVC   FVMSGNO,=AL2(FVFOK) RESET TO OK, RECORD IS SPLITABLE             
*                                                                               
ADDMSGX  DS    0H                                                               
         CLC   FVMSGNO,=AL2(FVFOK) SET   CONDITION CODE                         
         XMOD1 ,                                                                
         DROP  SUB,NEW                                                          
         DROP  R1,RC                                                            
         DROP  R7,RB                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE OFFICE, OFFICE STRING, OR OFFICE LIST                     *         
*---------------------------------------------------------------------*         
*  INPUT  PARAMETERS:                                                 *         
*    P1   C'B' BLOCK                                         P1(1)    *         
*         AL3(SCAN BLOCK)                                    P1+1(3)  *         
*    P2   C'Y' OR C'N' EXCLUDE OPTION                        P2(1)    *         
*         AL1(NUMBER OF ENTERIES IN SCAN BLOCK)              P2+1(3)  *         
*---------------------------------------------------------------------*         
*    P1   C'1'  SINGLE OFFICE/OFFICE LIST                    P1(1)    *         
*         AL3(OFFICE/OFFICE LIST)                            P1+1(3)  *         
*    P2   C'Y' OR C'N' EXCLUDE OPTION                        P2(1)    *         
*         AL1(LENGTH OF OFFICE/OFFICE LIST)                  P2+1(3)  *         
*---------------------------------------------------------------------*         
*    P1   C'R'  OFFICE FILTER ELEMENT ON RECORD              P1(1)    *         
*         AL3(AIO OF RECORD)                                 P1+1(3)  *         
*    P2   AL1(MAX# SCAN BLOCK CAN HANDLE)                    P2(1)    *         
*         AL3(SCAN BLOCK)                                    P2+1(3)  *         
*    P3   AL4(RFLTYPE OFFICE TYPE CODE)                      P3(4)    *         
*---------------------------------------------------------------------*         
*  OUTPUT PARAMETERS:                                                 *         
*    PARM 1 BYTE  0   OFFICE/OFFICE  LIST                             *         
*                     X'00' = OFFICE                                  *         
*                     X'01' = OFFICE LIST                             *         
*  USES:                                                              *         
*    VOFFICED - VOFFICE DSECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING WORKD,R7                                                         
         USING VOFFICED,RC                                                      
R_VOFFIC NMOD1 VOFFICEX-VOFFICED,**VOFF**,CLEAR=YES,RR=RE                       
         ST    RE,VOFRELO                                                       
         ST    R1,VOFCPRML         SAVE    CALLER'S  PARM LIST ADDRESS          
         MVC   VOFTYPE,0(R1)       SAVE    TYPE OF   CALL                       
         MVI   0(R1),X'00'         INITIALIZE   OUTPUT    TO   OFFICE           
         MVC   FVXTRA,SPACES                                                    
         CLI   VOFTYPE,VOFTYPEB    BLOCK   PASSED ?                             
         BE    VOFFC20             YES,    BRANCH                               
         CLI   VOFTYPE,VOFTYPE1    ONE     ELEMENT   PASSED ?                   
         BE    VOFFC25             YES,    BRANCH                               
         CLI   VOFTYPE,VOFTYPER    RECORD  PASSED ?                             
         BE    *+6                 YES,    SKIP                                 
         DC    H'00'               NO,     INVALID   CALL                       
*                                                                               
         L     R2,8(,R1)           GET OFFICE FILTER TYPE                       
         STC   R2,VOFELMTY         SAVE OFF TYPE                                
         L     R2,0(,R1)           ->      RECORD                               
         ST    R2,VOFAREC          SAVE    RECORD    ADDRESS                    
         MVC   VOFMXPRM,4(R1)      SAVE    MAX  NUM  OF   PARMS                 
         L     R3,4(,R1)           ->      BLOCK                                
         LA    R3,0(,R3)           CLEAR   HIGH ORDER     BYTE                  
         ST    R3,VOFBLOCK         SAVE    BLOCK     ADDRESS                    
*                                                                               
         USING RFLELD,R2           MAP     FILTER    DATA ELEMENT               
         SR    R4,R4               CLEAR   REGISTER                             
         AH    R2,DATADISP         ->      1ST  ELEMENT                         
*                                                                               
VOFFC05  DS    0H                  FIND    X'C5'     ELEMENT                    
         CLI   0(R2),0             END     OF   RECORD ?                        
         BE    VOFFCNOF            YES,    NO   OFFICES   INCLUDED              
         CLI   0(R2),RFLELQ        X'C5'   ELEMENT ?                            
         BNE   VOFFC10             NO,     GET  NEXT ELEMENT                    
         CLI   RFLSEQ,0            HIGH    LEVEL     FILTER ?                   
         BNE   VOFFC10             YES,    COLUMN    FILTER (SKIP)              
         CLC   RFLTYPE,VOFELMTY    OFFICE TYPE FILTER ?                         
         BE    VOFFC15             YES, FOUND IT                                
*                                                                               
VOFFC10  IC    R4,RFLLN            ELEMENT LENGTH                               
         AR    R2,R4               ADD     ELEMENT   LENGTH                     
         B     VOFFC05             TEST    NEXT      ELEMENT                    
*                                                                               
VOFFC15  MVI   VOFEXCLD,NO         ASSUME  INCLUDE                              
         TM    RFLIND,RFLXCLD      EXCLUDE ?                                    
         BZ    *+8                 NO,     SKIP                                 
         MVI   VOFEXCLD,YES        EXCLUDE                                      
*                                                                               
         IC    R4,RFLLN            ELEMENT LENGTH                               
         SHI   R4,RFLLNQ+1         EXECUTE MOVE LENGTH                          
         MVC   VOFCARD,SPACES      CLEAR   CARD                                 
         EXMVC R4,VOFCARD,RFLDATA  MOVE    THE  DATA                            
         DROP  R2                                                               
*                                                                               
         GOTO1 VSCANNER,VOFPRMS,(C'C',VOFCARD),(VOFMXPRM,VOFBLOCK),    X        
               SCNP3NEQ                                                         
         MVC   VOF#PRMS,VOFPRMS+4  GET     NUM  OF   PARAMETERS                 
         CLI   VOFPRMS+4,1         ANY     PARAMETERS ?                         
         BL    VOFFCNOF            NO,     NO   OFFICES   INCLUDED              
         B     VOFFC30             CONTINUE                                     
*                                                                               
VOFFC20  DS    0H                  BLOCK   PASSED                               
         L     R2,0(,R1)           ->      BLOCK                                
         ST    R2,VOFBLOCK         SAVE    BLOCK     ADDRESS                    
         MVC   VOFEXCLD,4(R1)      SAVE    EXCLUDE                              
         MVC   VOF#PRMS,7(R1)      SAVE    NUM  OF   PARAMETERS                 
         B     VOFFC30             CONTINUE                                     
*                                                                               
VOFFC25  DS    0H                  SINGLE  OFFICE/OFFICE  LIST                  
         L     R2,0(,R1)           ->      ENTRY                                
         ST    R2,VOFENTRY         SAVE    ENTRY     ADDRESS                    
         XC    VOFSBLKE,VOFSBLKE   CLEAR   SCAN BLOCK     ENTRY                 
         LA    R4,VOFSBLKE         SINGLE  BLOCK ENTRY                          
         ST    R4,VOFBLOCK         SAVE    THE  ADDRESS                         
         SR    R3,R3               CLEAR   REGISTER                             
         IC    R3,7(,R1)           GET     ENTRY     LENGTH                     
         STC   R3,0(,R4)           SAVE    ENTRY     LENGTH                     
         MVC   12(2,R4),0(R2)                                                   
         MVC   VOFEXCLD,4(R1)      SAVE    EXCLUDE                              
         MVI   VOF#PRMS,1          ONLY    ONE  PARAMETER                       
*                                                                               
VOFFC30  DS    0H                  COMMON  PROCESSING                           
         CLI   VOF#PRMS,VOFMXOFC   TOO     MANY PARAMETERS ?                    
         BNH   *+6                 NO,     CONTINUE                             
         DC    H'00'               YES,    INTERNAL  ERROR                      
*                                                                               
         SR    R6,R6               R6 =   # OF  OFFICES TO PROCESS              
         SR    R8,R8               R8 =   # BAD OFFICES                         
         IC    R6,VOF#PRMS         NUMBER   OF  PARMS                           
         L     R4,VOFBLOCK         ->       BLOCK                               
*                                                                               
         USING OFFALD,R1                                                        
VOFFC35  L     R1,AOFFBLK                                                       
         NI    OFFAINDS,TURNOFF-OFFAIUOL-OFFAIXOL                               
         TM    OFFACST4,X'01'      TEST    NEW  OFFICE                          
         BO    VOFFC40             YES,    SKIP                                 
         CLI   0(R4),1             LENGTH  = 1 ?                                
         BNE   VOFFCIOF            NO,     INVALID   OFFICE                     
         MVI   OFFAACT,OFFAPST     ACTION= MAY  OFFAOFFC  BE   POSTED ?         
         B     VOFFC45             CONTINUE                                     
*                                                                               
VOFFC40  CLI   0(R4),2             LENGTH  = 2 ?                                
         BNE   VOFFCIOF            NO,     INVALID   OFFICE                     
         MVC   OFFAREQL,AREQOFFL   OFFICE  LIST OUTPUT    AREA                  
         MVI   OFFAACT,OFFAREQ     ACTION= VALIDATE  REQUESTED OFFICE           
*                                                                               
VOFFC45  DS    0H                  VALIDATE     OFFICE/OFFICE  LIST             
*                                  INSERT  OFFICE    CODE                       
         MVC   OFFAOFFC,12(R4)                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE OR LIST CODE                 
         BE    VOFFC50             OK                                           
         OI    2(R4),X'01'         SET TO SAY INVALID DATA                      
         CLI   VOF#PRMS,1                                                       
         BE    VOFFCNOF            INVALID OFFICE OR LIST                       
         LA    RE,FVXTRA           MOVE IN BAD OFFICE CODE                      
         LR    RF,R8                                                            
         TM    OFFACST4,X'01'      NEW OFFICES                                  
         BZ    VOFFC46                                                          
         SLL   RF,1                MULTIPLY BY 2                                
*                                                                               
VOFFC46  AR    RF,R8               ADJUST FOR COMMA                             
         LA    R8,1(,R8)           INCREASE # OF BAD OFFICES                    
         LA    RE,FVXTRA(RF)       POINT TO AREA TO PUT ERROR DATA              
         MVC   0(2,RE),12(R4)      MOVE IN OFFICE FOR ERROR MSG                 
         LA    RE,1(,RE)                                                        
         CLI   0(R4),1             1 OR 2 BYTE OFFICE ?                         
         BE    *+8                                                              
         LA    RE,1(,RE)                                                        
         MVC   0(1,RE),SCCOMMA                                                  
         B     VOFFC55                                                          
*                                  VALID   OFFICE/OFFICE  LIST                  
VOFFC50  TM    OFFACST4,X'01'      TEST    FOR  NEW  OFFICE ?                   
         BZ    VOFFC55             NO,     VALID     OFFICE                     
         L     RE,AREQOFFL         POINT   TO   OFFICE    LIST                  
         CLC   =H'01',0(RE)        OFFICE  LIST WITH ONLY ONE  OFFICE ?         
         BNE   VOFFC52             NO,     SKIP                                 
*                                  OFFICE  LIST=REAL OFFICE ?                   
         CLC   12(2,R4),2(RE)                                                   
         BE    VOFFC55             YES,    VALID     OFFICE                     
*                                                                               
VOFFC52  OI    2(R4),X'02'         FOUND   LIST OF   OFFICES                    
         L     RE,VOFCPRML         ->      CALLERS   PARM LIST                  
         MVI   0(RE),X'01'         RETURN  OFFICE    LIST                       
         CLI   VOF#PRMS,1          ONLY    ONE  INPUT     PARAMETER ?           
         BNE   VOFFCIOL            NO,     INVALID   OFFICE    LIST             
*                                                                               
VOFFC55  DS    0H                  GOOD    OFFICE                               
         LA    R4,L'VOFSBLKE(,R4)  NEXT    OFFICE                               
         BCT   R6,VOFFC35          CHECK   NEXT OFFICE                          
*                                                                               
         TM    OFFACST4,X'01'      TEST    FOR  NEW  OFFICE                     
         BZ    VOFFCOK             NO,     CAN  NOT  CHECK,    SO  SKIP         
*                                                                               
         CLM   R8,1,VOF#PRMS       ARE ALL OFFICES BAD ?                        
         BE    VOFFCIF2            YES                                          
         CLI   VOFEXCLD,YES        EXCLUDE REQUESTED   ?                        
         BNE   VOFFCOK             NO,     WE   HAVE OFFICES,  SO  SKIP         
*                                                                               
         CLI   VOF#PRMS,1          MORE    THAN ONE  OFFICE ?                   
         BH    VOFFC70             YES,    VALIDATE  EXCLUDE   LIST             
*                                                                               
         MVC   OFFAREQL,AREQOFFL   OFFICE  LIST OUTPUT    AREA                  
         MVI   OFFAACT,OFFAREQ     ACTION= VALIDATE  REQUESTED OFFICE           
         L     R4,VOFBLOCK         ->      SCAN BLOCK                           
*                                  ->      OFFICE    CODE                       
         MVC   OFFAOFFC,12(R4)                                                  
         OI    OFFAINDS,OFFAIXOL   INDICATE     EXCLUDE                         
         GOTO1 VOFFAL              ANY     OFFICES ?                            
         BNE   VOFFCNOF            NONE,   NO   OFFICES   INCLUDED              
         B     VOFFCOK             EXIT    FOUND     OFFICE(S)                  
*                                                                               
NEW      USING OFLELD,VOFELEM                                                   
*                                                                               
VOFFC70  DS    0H                  EXCLUDE WITH      >    1    OFFICE           
         XC    VOFELEM,VOFELEM     CLEAR   DUMMY     OFLEL     ELEMENT          
         MVI   NEW.OFLEL,OFLELQ    X'D2' OFFICE ELEMENT                         
         SR    R3,R3                                                            
         IC    R3,VOF#PRMS         NUMBER  OF   OFFICES                         
         LR    R6,R3               SAVE    FOR  LOOP                            
         SLL   R3,1                TIMES   2    FOR  OFFICE    LENGTH           
         LA    R3,OFLLN1Q(,R3)     PLUS    BASE D2   ELEMENT   LENGTH           
         STC   R3,NEW.OFLLN        PASS    THE  LENGTH                          
         LA    R3,NEW.OFLNTRY      START OF OFFICE CODES                        
         L     R4,VOFBLOCK         ->      SCAN BLOCK                           
*                                                                               
VOFFC75  DS    0H                                                               
         MVC   0(2,R3),12(R4)      OFFICE    CODE                               
         LA    R3,L'OFLNTRY(,R3)   NEXT    OFFICE    ENTRY                      
         LA    R4,L'VOFSBLKE(,R4)  NEXT    BLOCK     ENTRY                      
         BCT   R6,VOFFC75          LOOP    FOR  MORE OFFICES                    
*                                                                               
         MVI   OFFAACT,OFFAREQ     ACTION= VALIDATE  REQUESTED OFFICE           
         XC    OFFAOFFC,OFFAOFFC   CLEAR   OFFICE    ELEMENT                    
         LA    R3,NEW.OFLEL        ->      DUMMY     OFLEL     ELEMENT          
         ST    R3,OFFAUSRL         PASS    THE  TWO  CHAR CODES                 
         MVC   OFFAREQL,AREQOFFL   ->      REQ  OFFICE    OUTPUT   AREA         
*                                  INDICATE     EXCLUDE   &    OFLELD           
         OI    OFFAINDS,OFFAIXOL+OFFAIUOL                                       
         GOTO1 VOFFAL              VALIDATE     OFFICE    LIST ELEMENT          
         BNE   VOFFCNOF            NONE,   NO   OFFICES   INCLUDED              
         B     VOFFCOK             EXIT    FOUND     OFFICE(S)                  
         DROP  NEW                                                              
*                                                                               
VOFFCIOF DS    0H                  INVALID      OFFICE                          
         MVC   FVXTRA(20),12(R4)   ->      BAD  DATA                            
*                                                                               
VOFFCIF2 MVC   FVMSGNO,=AL2(ACEIVOF)                                            
         LTR   RF,R8                                                            
         BZ    VOFFXIT                                                          
         TM    OFFACST4,X'01'      TEST    FOR  NEW  OFFICE                     
         BZ    *+8                                                              
         SLL   RF,1                                                             
         AR    RF,R8                                                            
         LA    RE,FVXTRA-1(RF)     POINT TO LAST COMMA                          
         MVI   0(RE),C' '                                                       
         B     VOFFXIT             EXIT                                         
*                                                                               
VOFFCIOL DS    0H                  INVALID OFFICE    LIST                       
         MVC   FVMSGNO,=AL2(ACEIVOFL)                                           
         MVC   FVXTRA(20),12(R4)   ->      BAD  DATA                            
         B     VOFFXIT             EXIT                                         
*                                                                               
VOFFCNOF DS    0H                  NO      OFFICES   INCLUDED                   
         MVC   FVMSGNO,=AL2(ACENOOFF)                                           
         B     VOFFXIT             EXIT                                         
*                                                                               
VOFFCOK  DS    0H                  FOUND   OFFICES                              
         SR    RE,RE               SAY     GOOD EXIT                            
*                                                                               
VOFFXIT  DS    0H                                                               
         LTR   RE,RE               SET     CONDITION CODE                       
         XMOD1 ,                                                                
         DROP  R1                                                               
         DROP  R7,RC                                                            
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'HOOK GEGEN00 IO, AIO, ROUTINE TO SPLIT 4K RECORDS'              
         USING WORKD,R7                                                         
         USING RESRECD,R3                                                       
         USING IOWRKD,RC                                                        
IOHOOK   NMOD1 IOWRKX-IOWRKD,**IOHK**                                           
         MVI   IOSW,0                                                           
         LTR   R1,R1                                                            
         JZ    IO08                SPECIAL CASE, SO GO TO REAL AIO              
         ST    R1,IOPARM                                                        
         L     R3,AIOAREA0         Test bits to find io area                    
         TM    IOACT,IO3           IO3 = IO1+IO2, none supplied then            
         JZ    IO06                use alternate IOAREA set by app. in          
*                                   IOADDR                                      
         L     R3,AIOAREA3         TEST BITS TO FIND IO AREA                    
         TM    IOACT,IO3           USED IO3  ?                                  
         JO    IO06                YES                                          
         L     R3,AIOAREA2                                                      
         TM    IOACT,IO2           USED IO2 ?                                   
         JO    IO06                YES                                          
         L     R3,AIOAREA1         MUST BE IO1                                  
*                                                                               
IO06     ST    R3,IOUSED           SAVE OF ADDRESS                              
         MVC   IOKEYSV,IOKEY                                                    
         TM    IOFIL,TURNOFF-X'01' IOACCFIL=X'01'    HOOK FOR ACCFIL            
         BNZ   IO08                SOME OTHER BIT WAS ON, NOT ACCFIL            
         LA    RE,IO50                                                          
         MVC   IOMSK,IOACT         Mask IOACT for testing                       
         NI    IOMSK,X'0F'         Turn of high order nibble                    
         CLI   IOMSK,IOWRITE       ACTION WRITE ?                               
         BE    IO07                YES, SO SPLIT RECORD                         
         CLI   IOMSK,IOSOXWRT      ACTION WRITE ?                               
         BE    IO07                YES, SO SPLIT RECORD                         
         CLI   IOMSK,IOADD         ACTION ADD ?                                 
         BE    IO07                YES, SO SPLIT RECORD                         
         CLI   IOMSK,IOSOXADD      ACTION ADD ?                                 
         BE    IO07                YES, SO SPLIT RECORD                         
         TM    IOMSK,IOSEQ         Read X'02'/ hi X'01' / seq X'03'             
         BNZ   *+6                 Any one of these                             
         DC    H'00'               COULDN'T FIGURE OUT ACTION                   
         LA    R3,IOKEY                                                         
         LA    RE,IO10                                                          
*                                                                               
IO07     CLC   =X'2D02',RESKTYP    SCRIBE RECORD                                
         BNE   IO08                                                             
         CLI   RESKSEQ,RESKSREG    X'40' REGULAR FORMAT                         
         BER   RE                  READ OR WRITE CODE                           
         CLI   RESKSEQ,RESKS2ND    X'41' 2ND RECORD                             
         BNE   IO08                PROCESS AS NORMAL                            
         DC    H'00'               I DIDN'T ALLOW READ OF 2ND RECORD            
*                                                                               
IO08     GOTO1 VIO                 PROCESS AS IF WE NEVER CHECKED               
         B     IOXIT                                                            
*                                                                               
IO10     GOTO1 VIO                 GET RECORD AS USUAL                          
         BE    IO15                RECORD MAY HAVE BEEN FOUND                   
         BL    IOXIT               HARDWARE ERROR, EXIT                         
         TM    IOERR,IOEDEL        DELETED RECORD ?                             
         BZ    IOXITHI             NO, SO RETURN ERROR                          
*                                                                               
IO15     L     R3,IOUSED           SAVE OF ADDRESS                              
         CLC   =X'2D02',RESKEY     IS IT STILL A SCRIBE RECORD ?                
         BNE   IOXITEQ             MUST HAVE BEEN READ HI OR SEQ                
         CLI   RESKSEQ,RESKS5TH    WAS IT ANOTHER TYPE OF FORMAT REC ?          
         BNL   IOXITEQ             YES, SO NOT A STANDARD FORMAT REC.           
         CLI   RESKSEQ,RESKS2ND    WAS IT THE 2ND RECORD ?                      
         BNE   IO18                                                             
         TM    IOACT,IOSEQ                                                      
         BNO   IOXITEQ             USER MAY HAVE READ FOR THIS RECORD           
         TM    IOERR,IOEDEL        MUST HAVE BEEN DELETED                       
         BO    IO10                SKIP OVER THIS RECORD                        
         DC    H'00'                                                            
*                                                                               
IO18     MVC   IOKEY1ST,RESKEY     SAVE OFF FORMAT CODE RETRIEVED               
         LR    R2,R3               R2 = LOAD START OF RECORD                    
         AH    R2,DATADISP         POINT TO START OF ELEMENTS                   
         SR    RF,RF                                                            
*                                  ** FIND REPORT TYPE ELEMENT **               
IO20     CLI   0(R2),0             EOR                                          
         BNE   *+6                                                              
         DC    H'00'               MUST HAVE ELEMENT LOOKING FOR                
         CLI   0(R2),STYELQ        X'25' REPORT TYPE ELEMENT                    
         BE    IO22                                                             
         IC    RF,1(,R2)                                                        
         AR    R2,RF                                                            
         B     IO20                NEXT                                         
*                                                                               
         USING STYELD,R2                                                        
IO22     MVI   ISLOCKED,NO                                                      
         TM    STYSTAT,STYSLOCK    Format is locked                             
         BZ    *+8                                                              
         MVI   ISLOCKED,YES                                                     
         TM    STYSTAT,STYS2ND     IS THERE A 2ND RECORD ?                      
         BO    IO25                NO SO FINISHED                               
         BAS   RE,CONVERT                                                       
         GOTO1 =A(TRNSLATE),IOPRMS,('READREC',(R3)),0,RR=ACRELO                 
         B     IOXITEQ             NO SO FINISHED                               
         DROP  R2                                                               
*                                  ** SAVE OFF RECORD IN ASAVEIO **             
IO25     LR    R0,R3               SOURCE                                       
         LH    R1,RESRLEN          LENGTH  OF  RECORD                           
         L     RE,ASAVEIO          TEMPOARY IO AREA                             
         LA    RF,MAXRECQ*2        MAX SIZE OF RECORD                           
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,IOKEY            READ 2ND RECORD                              
         MVC   IOKEY(L'IOKEY1ST),IOKEY1ST   MAKE SURE WHOLE KEY IS SET          
         MVI   RESKSEQ,RESKS2ND    SET IOKEY FOR 2ND RECORD                     
         L     R1,IOPARM           COULD STILL BE READ / HI / SEQ               
         GOTO1 VIO                                                              
         BE    IO30                NO ERRORS FOUND                              
         BL    IOXIT               HARDWARE ERROR                               
         TM    IOERR,IOEDEL        MARKED DELETED ?                             
         BO    IO30                USER MUST HAVE WANT DELETED RECS.            
         TM    IOERR,IOERNF        RECORD NOT FOUND ? (IF READ)                 
         BZ    IOXITHI             SOMETHING AIN'T RIGHT                        
         DC    H'00'               1ST REC SAID IT WAS HERE BUT AIN'T           
*                                                                               
IO30     L     R3,IOUSED           LOAD IO OF CALLER                            
         CLC   IOKEY1ST,0(R3)      MATCH FROM 1ST RECORD'S FORMAT CODE          
         BE    *+6                                                              
         DC    H'00'               SHOULD BE, CHECK LOGIC                       
*                                                                               
         CLI   RESKSEQ,RESKS2ND    MAKE SURE MARKED CORRECT                     
         BE    *+6                                                              
         DC    H'00'               SHOULD BE, CHECK LOGIC                       
*                                                                               
         MVC   IOKEY,IOKEYSV       RESTORE KEY                                  
         L     RE,ASAVEIO                                                       
         LH    R4,RESRLEN-RESRECD(,RE)    LEN OF 1ST READ RECORD                
         LH    R1,RESRLEN                 LEN OF 2ND READ RECORD                
         LR    RF,R4                      SAVE ORIGINAL LENGTH                  
         AR    R4,R1                      ADD BOTH LENGTH TOGETHER              
         SH    R4,DATADISP                ONLY ONE KEY, SO SHORTEN              
         BCTR  R4,0                       ONLY ONE EOR MARKER                   
         STH   R4,RESRLEN-RESRECD(,RE)    SAVE NEW LENGTH                       
         LR    R0,R3                      R0 = SOURCE                           
         AH    R0,DATADISP                POINT TO ELEMENTS TO APPEND           
         SH    R1,DATADISP                LESS KEY LENGTH                       
         AR    RE,RF                      RE = DEST,   POINT TO EOR             
         BCTR  RE,0                       POINT AT EOR MARKER NOT AFTER         
         LR    RF,R1                      MAKE LENGTH FOR MVCL SAME             
         MVCL  RE,R0              COMBINE RECORD   IN AIOSAVE  AREA             
*                                                                               
         L     R0,ASAVEIO                  SOURCE                               
         LA    R1,MAXRECQ*2                MAX SIZE OF RECORD                   
         L     RE,IOUSED                   DESTINATION (USER IO)                
         LR    RF,R1                                                            
         MVCL  RE,R0                       MOVE IT INTO IOAREA                  
*                                                                               
         BAS   RE,CONVERT                                                       
         GOTO1 =A(TRNSLATE),IOPRMS,('READREC',(R3)),0,RR=ACRELO                 
         B     IOXITEQ                                                          
         EJECT ,                                                                
***********************************************************************         
*        DEAL WITH WRITES AND ADDING A RECORD                         *         
***********************************************************************         
IO50     GOTO1 =A(TRNSLATE),IOPRMS,('WRITEREC',(R3)),0,RR=ACRELO                
         TM    RESRSTA,X'80'       IS RECORD MARKED DELETED ?                   
         BZ    IO51                NO                                           
         OI    IOSW,IODEL1+IODEL2  YES, DELETION OF BOTH RECORDS                
*                                                                               
IO51     SR    RF,RF                                                            
         LR    R2,R3               R3 = START OF IO AREA                        
         AH    R2,DATADISP         POINT TO ELEMENTS                            
*                                                                               
IO52     CLI   0(R2),EOR           END OF RECORD                                
         BNE   *+6                                                              
         DC    H'00'               MUST HAVE X'25' ELEMENT                      
*                                                                               
         CLI   0(R2),STYELQ        X'25'                                        
         BE    IO53                                                             
         IC    RF,1(,R2)                                                        
         AR    R2,RF                                                            
         B     IO52                                                             
*                                  R2 = POINTS AT STYELD, UNTIL DROP            
         USING STYELD,R2                                                        
IO53     DS    0H                                                               
         MVI   ISLOCKED,NO                                                      
         TM    STYSTAT,STYSLOCK    Is format locked?                            
         BZ    IO54                No                                           
         MVI   ISLOCKED,YES                                                     
         TM    INOPT2,INOUNLK      Trying to unlock format?                     
         BO    IO56                Yes                                          
         CLI   INACT,ACTCHA        Action change?                               
         BE    IO58                Yes, so don't allow changes                  
         B     IO59                No so continue                               
*                                                                               
IO54     TM    INOPT2,INOLOCK      Trying to lock format?                       
         BZ    IO59                No                                           
         OI    STYSTAT,STYSLOCK    Lock format and continue                     
         MVI   ISLOCKED,YES                                                     
         B     IO59                                                             
*                                                                               
IO56     NI    STYSTAT,TURNOFF-STYSLOCK  UNLOCK FORMAT                          
         MVI   ISLOCKED,NO                                                      
         B     IO59                                                             
*                                                                               
IO58     DS    0H                                                               
*&&US*&& MVC   FVMSGNO,=AL2(0341)  MOVE IN ERROR MESSAGE                        
*&&UK*&& MVC   FVMSGNO,=AL2(0712)                                               
         CR    RE,RE                                                            
         B     IOXIT                                                            
                                                                                
IO59     TM    STYSTAT,STYS2ND     IS THERE ALREADY A 2ND RECORD ?              
         BZ    *+8                 NO, 2ND RECORD NO PRESENT                    
         OI    IOSW,IOREC2         SET TO SAY 2ND RECORD ALREADY EXIST          
         NI    STYSTAT,TURNOFF-STYS2ND    TURN OF INDICATOR ON RECORD           
*                                                                               
         LR    R0,R3               SOURCE                                       
         LH    R1,RESRLEN          R1 = LENGTH OF RECORD                        
         L     RE,ASAVEIO          DESTINATION                                  
         LHI   RF,4*K                                                           
         MVCL  RE,R0               SAVE OFF RECORD                              
*                                                                               
         CLC   RESRLEN,=AL2(MAXRECQ)  ONE OR TWO RECORDS NEEDED ?               
         BH    IO60                   TWO RECORDS NEEDED                        
         OI    IOSW,IODEL2            SET TO DELETE 2ND RECORD                  
         B     IO70                   ONE RECORD  NEEDED                        
*                                                                               
IO60     OI    STYSTAT,STYS2ND     TURN ON BIT TO SAY 2 RECS ON FILE            
         OI    IOSW,IOWRT2+IORST   SET TO WRITE OR ADD 2ND RECORD               
         DROP  R2                  ADDRESS OF STYELD LOST                       
*                                                                               
         LR    R2,R3               CURRENT RECORD TO WRITE/ADD                  
         LA    R1,ACCORFST+1       KEY + ONE FOR EOR MARKER                     
         SR    RF,RF                                                            
         AH    R2,DATADISP         POINT TO ELEMENTS                            
*                                                                               
IO62     ICM   RF,1,1(R2)          GET LENGTH OF FIRST ELEMENT                  
         BNZ   *+6                                                              
         DC    H'00'               RECORD SIZE MUST BE WRONG                    
                                                                                
         AR    R1,RF                                                            
         AR    R2,RF               BUMP TO NEXT ELEMENT                         
         CLM   R1,3,=AL2(MAXRECQ)  MAX RECORD SIZE LESS KEY                     
         BL    IO62                NOT OVER THE LIMIT YET                       
         BE    IO64                                                             
*                                                                               
         SR    R2,RF               BUMP BACK ONE ELEMENT                        
         SR    R1,RF               REDUCE SIZE OF RECORD BY LAST ELEM           
*                                                                               
IO64     STH   R1,RESRLEN          SAVE NEW LENGTH IN RECORD                    
         BCTR  R1,0                ONE LESS BECAUSE OF EOR MARKER               
         STH   R1,IONEWLN          SAVE LOCATION OF LAST ELEMENT                
         MVI   0(R2),0             MARK END OF RECORD BY CLEARING               
*                                                                               
IO70     MVC   IOKEYSV,IOKEY       SAVE KEY                                     
         L     R1,IOPARM                                                        
         GOTO1 VIO                 WRITE/ADD FIRST RECORD                       
         BE    *+6                                                              
         DC    H'00'               BAD WRITE OR ADD                             
*                                                                               
         CLI   IOMSK,IOSOXADD      IF ADDING REC THEN ASSUME NO RECORD          
         BE    IO74                IF YES ASSUME NO 2ND RECORD ON FILE          
         CLI   IOMSK,IOADD         IF ADDING REC THEN ASSUME NO RECORD          
         BE    IO74                IF YES ASSUME NO 2ND RECORD ON FILE          
         N     R1,=X'0000FFC0'         LEAVE ON ACCFIL AND IO INFO              
         LA    R1,IORDD+IOLOCK(,R1)    READ TO SEE IF ON FILE & UPDATE          
         MVC   IOKEY,0(R3)             KEY MAY HAVE NOT BEEN SET OK             
         LA    R3,IOKEY                                                         
         OI    IOSW,IORST          RESTORE RECORD WHEN DONE                     
         MVI   RESKSEQ,RESKS2ND    LOOK FOR 2ND RECORD                          
         GOTO1 VIO                 SEE IF THERE IS ONE ALREADY ?                
         L     R3,IOUSED           LOAD IO AREA                                 
         BNE   IO72                RECORD NOT FOUND OR OTHER ERROR              
         OI    IOSW,IOREC2         2ND RECORD FOUND                             
         B     IO74                                                             
*                                                                               
IO72     TM    IOERR,IOERNF        RECORD NOT ON FILE ?                         
         BO    IO74                                                             
         TM    IOERR,IOEDEL        IS IT A DELETED RECORD ?                     
         BO    *+6                 RECORD FOUND                                 
         DC    H'00'               CHECK OUT ERROR IN IOERR                     
*                                                                               
         OI    IOSW,IOREC2         2ND RECORD FOUND BUT DELETED                 
         TM    IOSW,IODEL2         DID WE WANT TO DELETE IT                     
         BO    IO90                YES, SO JUST RESTORE DATA                    
*                                                                               
IO74     NI    IOERR,TURNOFF-IOERNF      TURN OFF ERROR, DON'T CARE NOW         
         L     RE,ASAVEIO                BUILD 2ND RECORD                       
         MVC   0(ACCORFST,R3),0(RE)      RESTORE KEY                            
         MVI   RESKSEQ,RESKS2ND          CHANGE  KEY FOR 2ND RECORD             
         LH    R1,DATADISP                                                      
         LA    R1,1(,R1)           R1 =  LENGTH OF RECORD                       
         LR    R2,R3               R2 =  START OF NEW RECORD                    
         AH    R2,DATADISP         POINT TO WHERE ELEMENTS GO                   
*                                                                               
         TM    IOSW,IOWRT2                                                      
         BZ    IO80                NOT  ADDING 2ND RECORD                       
         AH    RE,IONEWLN          POINT AT START OF REMAINING ELEMENTS         
         SR    RF,RF                                                            
*                                                                               
IO76     ICM   RF,1,1(RE)          GET LENGTH OF ELEMENT TO ADD                 
         BZ    IO85                EOR                                          
         AR    R1,RF               KEEP TRACK OF SIZE                           
         SHI   RF,1                                                             
         BP    *+6                 LENGTH MUST BE 2 OR GREATER                  
         DC    H'00'                                                            
*                                                                               
         EXMVC RF,0(R2),0(RE)      MOVE IN AN ELEMENT                           
         LA    RE,1(RF,RE)         BUMP UP IN SAVED COPY                        
         LA    R2,1(RF,R2)         BUMP UP IN NEW RECORD                        
         B     IO76                                                             
*                                  ** NEED TO DELETE RECORD **                  
IO80     TM    IOSW,IODEL2                                                      
         BZ    IO90                FINISHED                                     
         TM    IOSW,IOREC2         IS THERE A 2ND RECORD TO DELETE ?            
         BZ    IO90                NO, SO CAN'T DELETE                          
         OI    RESRSTA,X'80'       MARK DELETED                                 
*                                                                               
IO85     MVI   0(R2),0             MARK END OF RECORD                           
         STH   R1,RESRLEN          SAVE LENGTH OF RECORD                        
         L     R1,IOPARM           RESTORE ORIGINAL PARAMETERS                  
         CLI   IOMSK,IOADD         ADDING A RECORD ?                            
         BE    IO88                YES                                          
         CLI   IOMSK,IOSOXADD      ADDING A RECORD ?                            
         BE    IO88                YES                                          
         TM    IOSW,IOREC2         IS THERE A 2ND RECORD ?                      
         BO    IO88                SET TO WRITE ALREADY, SO OK                  
                                                                                
         N     R1,=X'FFFFFFF0'     Turnoff action write                         
         LA    R0,IOADD            Change to add from write or                  
         CLI   IOMSK,IOWRITE       Regular write                                
         BE    IO86                No                                           
         LA    R0,IOSOXADD         Change to SOX add from SOX write             
         CLI   IOMSK,IOSOXWRT      Sox write ?                                  
         BE    IO86                Must be this                                 
         DC    H'00'               What was I thinking?                         
                                                                                
IO86     OR    R1,R0               Change to SOX add from SOX write             
*                                                                               
IO88     GOTO1 VIO                 WRITE OR ADD RECORD                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
IO90     MVC   IOKEY,IOKEYSV                                                    
         TM    IOSW,IORST          RESTORE FULL RECORD                          
         BZ    IO92                                                             
         L     R0,ASAVEIO          SAVED RECORD                                 
         LR    RE,R3               IO AREA TO MOVE TO                           
         LH    R1,=Y(4*K)                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
IO92     GOTO1 =A(TRNSLATE),IOPRMS,('READREC',(R3)),0,RR=ACRELO                 
         B     IOXITEQ                                                          
*                                                                               
IOXITHI  CLI   *,0                 Set condition code high                      
         B     IOXIT                                                            
*                                                                               
IOXITEQ  MVC   FVMSGNO,=AL2(FVFOK)                                              
         CR    RE,RE               Set condition code equal                     
*                                                                               
IOXIT    XIT1                                                                   
         DROP  R3                                                               
         EJECT ,                                                                
         USING RESRECD,R2                                                       
CONVERT  NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK) RESET                                        
         L     R2,ASAVEIO                                                       
         XC    0(80,R2),0(R2)      CLEAR                                        
         LH    R1,DATADISP                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,RESRLEN          NO ELEMENT ON RECORD                         
         L     R3,IOUSED           A(IO) OF RECORD TO CONVERT                   
         AH    R3,DATADISP         POINT TO START OF ELEMENTS                   
         MVC   0(42,R2),0(R3)                                                   
         DROP  R2                                                               
*                                                                               
CONVRT10 CLI   0(R3),0             EOR                                          
         BE    CONVRT80                                                         
         CLI   0(R3),RHDELQ        X'C1' HEADING ELEMENT                        
         BE    CONVRT18                                                         
         CLI   0(R3),RRWELQ        X'C2' ROW     ELEMENT                        
         BE    CONVRT20                                                         
         CLI   0(R3),RCLELQ        X'C3' COLUMN  ELEMENT                        
         BE    CONVRT50                                                         
         CLI   0(R3),STYELQ        X'25' SCRIBE TYPE ELEMENT                    
         BE    CONVRT75                                                         
*                                                                               
CONVRT15 SR    RF,RF                                                            
         IC    RF,1(,R3)           BUMP TO NEXT  ELEMENT                        
         AR    R3,RF                                                            
         B     CONVRT10                                                         
         EJECT ,                                                                
***********************************************************************         
*        CONVERT HEADING ELEMENTS                                     *         
***********************************************************************         
NEW      USING RHDELD,IOELEM                                                    
         USING RHDELD,R3                                                        
*                                                                               
CONVRT18 TM    RHDFRM,RHDDEF+RHDDICT                                            
         BNO   CONVRT15            NOTHING TO CONVERT                           
         CLC   =AL2(AC#RSBLK),RHDDATA                                           
         BNE   CONVRT15            NO CONVERTION                                
         CLI   RHDXDATA,0                                                       
         BNE   CONVRT15            ALREADY CONVERTED                            
         XC    IOELEM,IOELEM                                                    
         SR    RF,RF                                                            
         IC    RF,RHDLN                                                         
         BCTR  RF,0                                                             
         EXMVC RF,NEW.RHDEL,RHDEL                                               
         MVC   NEW.RHDXDATA,RHDDATA+2       MOVE DATA TO NEW FIELD              
         NI    NEW.RHDXDATA,TURNOFF-X'F0'   MAKE BINARY                         
         STC   RF,NEW.RHDLN                 SAVE NEW LENGTH                     
         MVI   RHDEL,X'FF'                  MARK DELETED                        
         L     R1,ASAVEIO                                                       
         GOTO1 CVTAELM             ADD  ELEMENT                                 
         BE    CONVRT15            NEXT ELEMENT                                 
         DC    H'00'               ERROR WHEN ADDING ?                          
         DROP  R3                                                               
         DROP  NEW                                                              
         EJECT ,                                                                
***********************************************************************         
*        CONVERT ROW    ELEMENTS                                      *         
***********************************************************************         
         SPACE 1                                                                
NEW      USING RRWELD,IOELEM                                                    
         USING RRWELD,R3                                                        
*                                                                               
CONVRT20 TM    RRWOPT,RRWNEWEL     HAS THIS BEEN CONVERTED ?                    
         BO    CONVRT15            NEXT ELEMENT                                 
         XC    IOELEM,IOELEM                                                    
         MVC   NEW.RRWEL(RRWDATA-RRWEL),RRWEL                                   
         TM    RRWOPT2,RRWDICT     TRANSLATED DATA ?                            
         BZ    CONVRT25                                                         
         CLI   RRWDATA+11,0        SPECIAL DATA SAVED                           
         BE    *+10                        NO                                   
         MVC   NEW.RRWXDATA,RRWDATA+11     YES, MOVE IT TO EXTRA DATA           
*                                                                               
         SR    R1,R1                                                            
         LA    RF,5                MAX OF THREE PARAMETERS + KEYWORD            
         LA    RE,RRWDATA          POINT TO START OF PARAMETERS                 
*                                                                               
CONVRT22 OC    0(2,RE),0(RE)       ANY DATA                                     
         BZ    CONVRT24                                                         
         LA    R1,1(,R1)                                                        
         LA    RE,2(,RE)           NEXT ENTRY                                   
         BCT   RF,CONVRT22                                                      
         DC    H'00'               WENT OVER THE MAX                            
*                                                                               
CONVRT24 SLL   R1,1                MULTIPLY BY 2 TO FIND LENGTH OF DATA         
         SH    R1,=H'01'                                                        
         BP    CONVRT30                                                         
         DC    H'00'               HAD TO HAVE AT LEAST KEYWORD                 
*                                                                               
CONVRT25 LA    R1,L'RRWDATA                                                     
         LA    RE,RRWDATA-1(R1)    POINT TO END                                 
*                                                                               
CONVRT26 CLI   0(RE),C' '                                                       
         BH    CONVRT28            FOUND LAST CHARACTER                         
         BCTR  RE,0                                                             
         BCT   R1,CONVRT26                                                      
         DC    H'00'                                                            
*                                                                               
CONVRT28 BCTR  R1,0                ONE FOR EX. INSTR.                           
*                                                                               
CONVRT30 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NEW.RRWNDATA(0),RRWDATA                                          
         LA    R1,1(,R1)                                                        
         STC   R1,NEW.RRWDATLN     SAVE LENGTH OF DATA                          
         LA    RE,NEW.RRWNDATA(R1) POINT TO END OF DATA                         
         SR    RF,RF                                                            
         IC    RF,RRWLN                                                         
         SH    RF,=Y(RRWLNQ)                                                    
         BZ    CONVRT35                                                         
         STC   RF,NEW.RRWPFXLN     SAVE LENGTH OF PREFIX                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),RRWPRFX     MOVE IN PREFIX                               
         LA    RE,1(RF,RE)         POINT TO END OF NEW ELEMENT                  
*                                                                               
CONVRT35 LA    RF,NEW.RRWEL        RF = START OF ELEMENT                        
         SR    RE,RF               RE = END OF ELEMENT                          
         STC   RE,NEW.RRWLN        SAVE ELEMENT LENGTH                          
         OI    NEW.RRWOPT,RRWNEWEL                                              
         MVI   RRWEL,X'FF'         MARK DELETED                                 
         L     R1,ASAVEIO                                                       
         GOTO1 CVTAELM             CONVERT'S ADD ELEMENT                        
         BE    CONVRT15            NEXT ELEMENT                                 
         DC    H'00'               ERROR WHEN ADDING ?                          
         DROP  R3                                                               
         DROP  NEW                                                              
         EJECT ,                                                                
***********************************************************************         
*        CONVERT COLUMN ELEMENTS                                      *         
***********************************************************************         
         SPACE 1                                                                
NEW      USING RCLELD,IOELEM                                                    
         USING RCLELD,R3                                                        
*                                                                               
CONVRT50 TM    RCLOPT,RCLNEWEL     HAS THIS BEEN CONVERTED ?                    
         BO    CONVRT15            YES,     NEXT ELEMENT                        
         XC    IOELEM,IOELEM                                                    
         CLI   APREPJCL,REPJCL1            PERSON SCRIBE ?                      
         BNE   *+8                                                              
         NI    RCLDTEFG,TURNOFF-RCLCNDR    ** CORRECTION BUG **                 
         MVC   NEW.RCLEL(RCLDATA-RCLELD),RCLEL                                  
         TM    RCLOPT2,RCLDICT             TRANSLATED DATA ?                    
         BZ    CONVRT55                                                         
         CLI   RCLDATA+11,0                SPECIAL DATA SAVED                   
         BE    *+10                        NO                                   
         MVC   NEW.RCLXDATA,RCLDATA+11     YES, MOVE IT TO EXTRA DATA           
*                                                                               
         SR    R1,R1                                                            
         LA    RF,5                MAX OF THREE PARAMETERS + KEYWORD            
         LA    RE,RCLDATA          POINT TO START OF PARAMETERS                 
CONVRT52 OC    0(2,RE),0(RE)       ANY DATA                                     
         BZ    CONVRT54                                                         
         LA    R1,1(,R1)                                                        
         LA    RE,2(,RE)           NEXT ENTRY                                   
         BCT   RF,CONVRT52                                                      
         DC    H'00'               WENT OVER THE MAX                            
*                                                                               
CONVRT54 SLL   R1,1                MULTIPLY BY 2 TO FIND LENGTH OF DATA         
         SHI   R1,1                                                             
         BP    CONVRT60                                                         
         DC    H'00'               HAD TO HAVE KEYWORD AT LEAST                 
*                                                                               
CONVRT55 LA    R1,L'RCLDATA                                                     
         LA    RE,RCLDATA-1(R1)    POINT TO END                                 
*                                                                               
CONVRT56 CLI   0(RE),C' '                                                       
         BH    CONVRT58            FOUND LAST CHARACTER                         
         BCTR  RE,0                                                             
         BCT   R1,CONVRT56                                                      
         DC    H'00'                                                            
*                                                                               
CONVRT58 BCTR  R1,0                ONE FOR EX. INSTR.                           
*                                                                               
CONVRT60 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NEW.RCLNDATA(0),RCLDATA                                          
         LA    R1,1(,R1)                                                        
         STC   R1,NEW.RCLDATLN     SAVE LENGTH OF DATA                          
         LA    RE,NEW.RCLNDATA(R1) POINT TO END OF DATA                         
*                                                                               
         CLI   RCLLN,RCLLNQ1       ANY HEADINGS ?                               
         BL    CONVRT70            FINISHED SET ELEMENT LENGTH                  
         LA    R2,RCLHDL2-1        POINT TO END OF HEAD LINE 1                  
         LA    R1,L'RCLHDL1                                                     
*                                                                               
CONVRT62 CLI   0(R2),C' '                                                       
         BH    CONVRT64                                                         
         BCTR  R2,0                                                             
         BCT   R1,CONVRT62                                                      
*                                                                               
CONVRT64 STC   R1,NEW.RCLHD1LN     R1 = LENGTH OF HEADING 1                     
         SHI   R1,1                R1 = LENGTH OF HEADING 1 LESS ONE            
         BM    CONVRT65                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),RCLHDL1     MOVE IN HEADING 1                            
         LA    RE,1(R1,RE)         POINT TO END OF ELEMENT SO FAR               
*                                                                               
CONVRT65 CLI   RCLLN,RCLLNQ2       IS THERE A 2ND HEADING ?                     
         BNE   CONVRT70            NO, FINISHED SET ELEMENT LENGTH              
         LA    R2,RCLEL+RCLLNQ2-1  POINT TO END OF HEAD LINE 2                  
         LA    R1,L'RCLHDL2        FULL LENGTH OF FIELD                         
*                                                                               
CONVRT66 CLI   0(R2),C' '                                                       
         BH    CONVRT68                                                         
         BCTR  R2,0                                                             
         BCT   R1,CONVRT66                                                      
         DC    H'00'                                                            
*                                                                               
CONVRT68 STC   R1,NEW.RCLHD2LN     R1 = LENGTH OF HEADING 1                     
         BCTR  R1,0                R1 = LENGTH OF HEADING 1 LESS ONE            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),RCLHDL2     MOVE IN HEADING 1                            
         LA    RE,1(R1,RE)         POINT TO END OF ELEMENT SO FAR               
*                                                                               
CONVRT70 LA    RF,NEW.RCLEL        RF = START OF ELEMENT                        
         SR    RE,RF               RE = END OF ELEMENT                          
         STC   RE,NEW.RCLLN        SAVE LENGTH OF NEW ELEMENT                   
         OI    NEW.RCLOPT,RCLNEWEL                                              
         MVI   RCLEL,X'FF'         MARK DELETED                                 
         L     R1,ASAVEIO                                                       
         GOTO1 CVTAELM             CONVERT'S ADD ELEMENT                        
         BE    CONVRT15            NEXT ELEMENT                                 
         DC    H'00'               ERROR WHEN ADDING ?                          
*                                                                               
         DROP  R3                                                               
         DROP  NEW                                                              
*                                                                               
NEW      USING STYELD,IOELEM                                                    
         USING STYELD,R3                                                        
*                                                                               
CONVRT75 CLI   STYLN,STYLNQ2       NEW  LENGTH TYPE ?                           
         BE    CONVRT15            NEXT ELEMENT                                 
         XC    IOELEM,IOELEM                                                    
         MVC   NEW.STYEL(STYLNQ),STYEL  KEEP OLD DATA                           
         MVI   NEW.STYLN,STYLNQ2                                                
         MVI   STYEL,X'FF'         MARK FOR DELETION                            
         SR    RF,RF                                                            
         LA    R1,1                CALCULATE REPORT WIDTH                       
         LR    RE,R3                                                            
*                                                                               
         USING RCLELD,RE                                                        
CONVRT76 CLI   0(RE),0             END OF                                       
         BE    CONVRT79                                                         
         CLI   0(RE),RCLELQ        X'C3' COLUMN ELEMENT                         
         BNE   CONVRT78                                                         
         TM    RCLOPT,RCLHIDE      DON'T SHOW COLUMN ?                          
         BO    CONVRT78            YES,  DON'T SHOW                             
         CLI   RCLSTACK,0          STACKED UNDER ANOTHER COLUMN ?               
         BNE   CONVRT78            YES,  COLUMN TAKES UP NO NEW SPACE           
         CLI   RCLWDTH,0           COL   WIDTH ZERO ?                           
         BE    CONVRT78            YES,  SKIP                                   
         IC    RF,RCLWDTH          GET WIDTH                                    
         LA    R1,1(RF,R1)                                                      
*                                                                               
CONVRT78 IC    RF,RCLLN            LENGTH                                       
         AR    RE,RF                                                            
         B     CONVRT76                                                         
         DROP  RE,R3                                                            
*                                                                               
CONVRT79 STH   R1,NEW.STYWIDTH     SAVE REPORT WIDTH                            
         L     R1,ASAVEIO                                                       
         GOTO1 CVTAELM             CONVERT'S ADD ELEMENT                        
         BE    CONVRT15            NEXT ELEMENT                                 
         DC    H'00'               ERROR WHEN ADDING ?                          
         DROP  NEW                                                              
*                                                                               
CONVRT80 L     R3,IOUSED           REMOVE ALL DELETED ELEMENTS                  
         GOTO1 VHELLO,IOPRMS,(C'D',=CL8'ACCVBIG'),(X'FF',(R3)),0                
         CLI   IOPRMS+12,0         ELEMENTS DELETED                             
         BE    *+6                 YES                                          
         DC    H'00'                                                            
         XC    IOELEM,IOELEM                                                    
         SR    R6,R6                                                            
         L     R2,ASAVEIO                                                       
         AH    R2,DATADISP                                                      
*                                                                               
CONVRT85 CLI   0(R2),0             EOR ?                                        
         BE    CONVRTX                                                          
         IC    R6,1(,R2)           ELEMENT LENGTH                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   IOELEM(0),0(R2)     MOVE IN ELEMENT TO ADD                       
         GOTO1 CVTAELM,(R3)                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R2,1(R6,R2)         GOTO NEXT ELEMENT                            
         B     CONVRT85                                                         
*                                                                               
CONVRTX  B     IOXIT                                                            
         EJECT ,                                                                
         USING RESRECD,R4                                                       
CVTAELM  NTR1                                                                   
         LR    R4,R1                                                            
         MVI   IO4KREC,NO                                                       
         LA    R6,=CL8'ACCBIG'                                                  
         CLC   =X'2D02',RESKTYP    SCRIBE FORMAT TYPE ?                         
         BNE   CVTAEL10                                                         
         CLI   RESKSEQ,RESKSREG    X'40' REGULAR FORMAT                         
         BNE   CVTAEL10            NO                                           
         MVI   IO4KREC,YES                                                      
         LA    R6,=CL8'ACCVBIG'                                                 
*                                                                               
CVTAEL10 GOTO1 VHELLO,IOPRMS,(C'P',(R6)),(R4),IOELEM,0                          
         CLI   IOPRMS+12,0         ELEMENT ADD OK SO FAR                        
         BE    CVTAEL12                                                         
         MVC   FVMSGNO,=AL2(58)    ELEMENT NOT ON FILE                          
         CLI   IOPRMS+12,X'05'     RECORD TOO BIG                               
         BNE   CVTAELX                                                          
         MVC   FVMSGNO,=AL2(ACREC2BG)                                           
         B     CVTAELX                                                          
*                                                                               
CVTAEL12 CLI   IO4KREC,YES         4K RECORD TYPE ?                             
         BNE   CVTAELX                                                          
         MVI   IO4KREC,NO              RESET FOR SAFETY                         
         CLC   RESRLEN,=AL2(MAXRECQ)                                            
         BNH   CVTAELX                 IS IT A 4K CANDIDATE ?                   
         LH    R2,RESRLEN              GET RECORD LENGTH                        
         LR    R6,R4               LOAD START OF RECORD                         
         AH    R6,DATADISP         BUMP TO ELEMENTS                             
         LA    R1,ACCORFST+1       R1 = TOTAL LEN OF RECORD                     
         DROP  R4                                                               
*                                  MAKE SURE WE CAN SPLIT INTO TWO              
         SR    RF,RF                                                            
         MVC   FVMSGNO,=AL2(ACREC2BG)  NO                                       
CVTAEL20 IC    RF,1(,R6)              GET LENGTH OF ELEMENT                     
         AR    R1,RF               ADD UNTIL REACH SIZE OF FIRST REC            
         SR    R2,RF               DECREASE TOTAL LENGTH BY LEN OF ELEM         
         AR    R6,RF               BUMP UP IN RECORD                            
         CLM   R1,3,=AL2(MAXRECQ)  HAVE WE REACH THE LIMIT ?                    
         BL    CVTAEL20            NO, KEEP GOING UNTIL MAX OUT                 
         BE    *+6                                                              
         AR    R2,RF               BACK OUT LAST SR INTSR.                      
*                                                                               
         CLM   R2,3,=AL2(MAXRECQ)  WOULD 2ND RECORD BE OK ?                     
         BH    CVTAELX             STILL TOO BIG                                
         MVC   FVMSGNO,=AL2(FVFOK) RESET TO OK, RECORD IS SPLITABLE             
*                                                                               
CVTAELX  CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     IOXIT                                                            
         SPACE 1                                                                
         DROP  R7,RC                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
CONADDRS DS    0F                                                               
         DC    A(PHASES)                                                        
         DC    A(HOOK)                                                          
         DC    A(OPTIONS)                                                       
         DC    A(COMMON)                                                        
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         EJECT ,                                                                
***********************************************************************         
* TABLE TO USE DIFFERENT DELIMETERS FOR DIFFERENT LANGUAGES           *         
***********************************************************************         
         SPACE 1                                                                
DELMTAB  DC    AL1(LANGENG),CL10'()CRYNOWE '                                    
DELMTLN  EQU   *-DELMTAB                                                        
         DC    AL1(LANGEUK),CL10'()CRYNOWE '                                    
         DC    AL1(LANGEUS),CL10'()CRYNOWE '                                    
         DC    AL1(LANGGER),CL10'()SZJNXAE '                                    
         DC    AL1(LANGFRE),CL10'()CEONSWE '                                    
         DC    AL1(LANGSPA),CL10'()CRSNSWE '                                    
         DC    AL1(LANGITA),CL10'()CRCNSWE '                                    
         DC    AL1(LANGDUT),CL10'()KRJNSWE '                                    
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
* TABLE TO RFP FIELDS AND THERE MSG# VALUES                           *         
***********************************************************************         
         SPACE 1                                                                
FLDTAB   DC    AL1(FLDNSTDT)       START DATE FIELD                             
         DC    AL1(2)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#STRT)       START                                        
         DC    AL2(MSG#TODY)       TODAY                                        
*                                                                               
         DC    AL1(FLDNENDT)       END DATE                                     
         DC    AL1(2)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#END)        END                                          
         DC    AL2(MSG#TODY)       TODAY                                        
*                                                                               
         DC    AL1(FLDNACTS)       ACTIVITY START DATE                          
         DC    AL1(2)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#ASTR)       ACT START                                    
         DC    AL2(MSG#TODY)       TODAY                                        
*                                                                               
         DC    AL1(FLDNACTE)       ACTIVITY END   DATE                          
         DC    AL1(2)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#AEND)       ACT END                                      
         DC    AL2(MSG#TODY)       TODAY                                        
*                                                                               
         DC    AL1(FLDNALTS)       ALTERNATE START DATE                         
         DC    AL1(2)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#ALTS)       ALT START                                    
         DC    AL2(MSG#TODY)       TODAY                                        
*                                                                               
         DC    AL1(FLDNALTE)       ALTERNATE END   DATE                         
         DC    AL1(2)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#ALTE)       ALT END                                      
         DC    AL2(MSG#TODY)       TODAY                                        
*                                                                               
         DC    AL1(FLDNMOAR)       MOA RANGE                                    
         DC    AL1(1)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#MOA)        MOA                                          
*                                                                               
         DC    AL1(FLDNPRDR)       PERIOD RANGE                                 
         DC    AL1(1)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#RNGE)       RANGE                                        
*                                                                               
         DC    AL1(FLDNCNDR)       CALENDAR RANGE                               
         DC    AL1(1)              NUMBER OF ENTRIES                            
         DC    AL2(MSG#RNGM)       RANGEM                                       
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
TTYPETAB DS    0C                                                               
         DC    CL8' '                                                           
         DC    AL1(TY30DI)        DIFFERENCE TYPE 30                            
         DC    CL4'DI'                                                          
*        DCDD  AC#RSTDI,4                                                       
         DC    CL8' '                                                           
         DC    AL1(TY06MN)        MANUAL BILLING TYPE 06                        
         DC    CL4'M'                                                           
         DC    CL8' '                                                           
         DC    AL1(TY30CH)        CHECK TYPE 30                                 
         DCDD  AC#RSTCH,4                                                       
         DC    CL8' '                                                           
         DC    AL1(TY30OF)        OFFSET TYPE 30                                
         DCDD  AC#RSTOF,4                                                       
         DC    CL8' '                                                           
         DC    AL1(TY30WO)        WRITE-OFF TYPE 30                             
         DCDD  AC#RSTWO,4                                                       
         DC    CL8' '                                                           
         DC    AL1(TY30TT)        TRANSFERED FROM TYPE 30                       
         DCDD  AC#RSTTT,4                                                       
         DC    CL8' '                                                           
         DC    AL1(TY30TF)        TRANSFERED TO TYPE 30                         
         DCDD  AC#RSTTF,4                                                       
         DC    CL8' '                                                           
         DC    AL1(TY30IA)        INTERAGENCY                                   
         DC    CL4'IA'                                                          
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DEFVAL VALIDATION VALID UNIT/LEDGER TABLE                           *         
***********************************************************************         
         SPACE 1                                                                
DEFULTBL DS    0CL2                                                             
         DC    C'13'               UNIT   LEDGER 13 - DEFRUL13                  
         DC    C'1C'               UNIT   LEDGER 1C - DEFRUL1C                  
         DC    C'29'               UNIT   LEDGER 29 - DEFRUL29                  
         DC    C'2D'               UNIT   LEDGER 2D - DEFRUL2D                  
         DC    C'2P'               UNIT   LEDGER 2P - DEFRUL2P                  
DEFULTBQ EQU   *-DEFULTBL          TABLE  LENGTH                                
         EJECT ,                                                                
***********************************************************************         
*        LOCAL DSECT                                                  *         
***********************************************************************         
RPRMD    DSECT                                                                  
RPRMLEN  DS    XL1                 LENGTH OF TRANSLATED DICT VALUE              
RPRMOEXL DS    XL1                 ORIGINAL EXECUTE LENGTH OF PARM              
RPRMFEXL DS    XL1                 FULL     EXECUTE LENGTH OF PARM              
RPRMGRP  DS    XL1                 PARM     GROUP   TYPE                        
RPRMDIC  DS    XL2                 DICTIONARY VALUE                             
RPRMKEY  DS    CL4                 TRNASLATED VALUE                             
RPRMLNQ  EQU   *-RPRMD                                                          
         EJECT ,                                                                
***********************************************************************         
*        WORK DSECT FOR SCR CONTROLLER                                *         
***********************************************************************         
         SPACE 1                                                                
RWRKD    DSECT                                                                  
*                                  ***** START KEEP TOGETHER *****              
RDUB     DS    D                                                                
RWORK    DS    XL80                                                             
         ORG   RDUB                                                             
DUMFLDH  DS    XL8                                                              
DUMFLD   DS    XL80                                                             
         ORG   ,                                                                
*                                  ***** END   KEEP TOGETHER *****              
DUB      DS    D                                                                
WORK     DS    CL40                                                             
SVREGS   DS    6A                                                               
RADDRS   DS    A                                                                
RKYWHDR  DS    A                                                                
ASELHDR1 DS    A                   A(X'FD' SCREEN ADDRESS) LINE 1               
SELLEN1  DS    H                   LENGTH OF SCREEN SELECT HELP FIELD           
         DS    H                   SPARE                                        
ASELHDR2 DS    A                   A(X'FD' SCREEN ADDRESS) LINE 1               
SELLEN2  DS    H                   LENGTH OF SCREEN SELECT HELP FIELD           
         DS    H                   SPARE                                        
APFKHDR1 DS    A                   A(X'FE' SCREEN ADDRESS) LINE 1               
PFKLEN1  DS    H                   LENGTH OF SCREEN PFKEY HELP FIELD            
         DS    H                   SPARE                                        
APFKHDR2 DS    A                   A(X'FE' SCREEN ADDRESS) LINE 1               
PFKLEN2  DS    H                   LENGTH OF SCREEN PFKEY HELP FIELD            
         DS    H                   SPARE                                        
RPARM    DS    8F                                                               
RPARM2   DS    8F                                                               
RSVR1    DS    A                                                                
RFULL    DS    F                                                                
RHALF    DS    H                                                                
RLOCAL   DS    CL1                 CALL TO ROUTINE IS LOCAL, YES OR NO          
RBYTE    DS    XL1                                                              
RCHAR    DS    CL1                                                              
R4KREC   DS    CL1                 YES OR NO, HAVE 4K RECORD TYPE               
*                                                                               
RPFKNAM  DS    CL9                                                              
RKEYWRD  DS    CL(KEYWDLN)                                                      
RKEYHD   DS    CL(L'APKEYHD)                                                    
*                                                                               
RREPCDE  DS    CL(L'APREPCDE)                                                   
RREPTYP  DS    CL(L'APREPTYP)                                                   
RREPLNQ  EQU   *-RREPCDE                                                        
*                                                                               
RREPNUM  DS    CL(L'APREPNUM)                                                   
*                                                                               
ROUT#    DS    XL1                                                              
RSVKYWRD DS    CL6                 SAVE INPUT KEYWORD                           
RXKEYWRD DS    CL6                                                              
RXTRALN  DS    XL1                                                              
RXTRA    DS    CL3                                                              
RXMODE   DS    XL1                                                              
RXM1ST   EQU   1                   1ST PASS MODE                                
RXM2ND   EQU   2                   2ND PASS MODE                                
RFLAG    DS    XL1                                                              
RLEN     DS    XL1                                                              
LEVLAST  DS    CL1                 YES,NO                                       
WORDNUM  DS    XL1                 NUMBER OF WORDS                              
WORDONE  DS    XL1                 END OF FIRST WORD                            
WORDEND  DS    XL1                 LENGTH OF ALL WORDS                          
WORDBRK  DS    XL1                 LENGTH OF ALL WORDS UNDER LENGTH 12          
TYPECDE  DS    CL4                                                              
FORMTYPE DS    XL1                                                              
SCRIBE   EQU   C'S'                SCRIBE FORMAT TYPES                          
APG      EQU   C'A'                APG FORMAT TYPES                             
*                                                                               
RELEMENT DS    XL(L'APELEM)                                                     
RFCURY   DS    CL(CURTABL)                                                      
RBLOCK   DS    6CL32                                                            
RCARD    DS    CL80                                                             
RIOSAVE  DS    XL(IOAREAX-IOAREA)                                               
RIOSVKEY DS    CL(L'IOKEY)                                                      
RPFKAREA DS    XL170               PFKEY DISPLAY BIULD AREA                     
MAXPFLEN EQU   158                                                              
RHLPAREA DS    XL150               HELP  DISPLAY BUILD AREA                     
         ORG   RPFKAREA                                                         
KYWFLDH  DS    XL8                 KEYWORD DUMMY FIELD HEADER                   
KYWFLD   DS    CL12                                                             
         ORG   ,                                                                
RWRKX    EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*        DSECT FOR TRNASLATING X'2D02' RECORDS                        *         
***********************************************************************         
TRLWRKD  DSECT                                                                  
RPARMDEF DS    4F                  ADDRESSES OF PARAMETER DEFINITIONS           
R@ELEMNT DS    A                   A(ELEMENT)                                   
TRLSTYEL DS    A                   A(STYEL) WRITE MODE ONLY                     
TRLAIO   DS    A                   A(IO OF RECORD TO TRANSLATE)                 
TRLPRMS  DS    8F                                                               
TRLFULL  DS    F                                                                
TRLDUB   DS    D                                                                
TRLHALF  DS    H                                                                
TRLBYTE  DS    XL1                                                              
TRL4KREC DS    CL1                 YES OR NO 4K RECORD                          
RACTION  DS    CL1                 READ/WRITE TYPE TRANSLATIONS                 
RSINGLE  DS    CL1                 YES/NO PROCESS SINGLE ELEMENT                
RELEMCDE DS    XL1                 ELEMENT CODE TO PROCESS                      
TRLJCLID DS    CL(L'REPJCLID)      JCL        ID     FOR RECORD                 
RKEYDATA DS    CL21                                                             
RELMDATA DS    CL21                                                             
RKEYWORD DS    CL(KEYWDLN)                                                      
TRLBLOCK DS    6CL32                                                            
*                                                                               
RRECDIC# DS    CL(L'PARDDNUM)      DICTIONARY NUMBER FOR RECORD                 
RKEYWRDL DS    AL1                 LENGTH  OF   KEYWORD                         
TRL#PRMS DS    AL1                 NUMBER  OF   PARMS  (SO FAR)                 
RNUM2CHR DS    AL1                 NUMBER TO CHANGE TO CHARACTER                
RADDPRM# DS    AL2                 DEFAULT PARM NUMBER                          
RADDPRML DS    AL1                 DEFAULT PARM MINIMUM LENGTH                  
RDFLTPLQ DS    CL4                 DEFAULT PARM LENGTH (FOR DICTATE)            
RFIXCTL  DS    XL1                 VALUE TO FIX RCLSPCL/RRWSPCL                 
TRLSEC#1 DS    XL1                 SECURITY FOR KEYWORD                         
*                                                                               
TRLRCDE  DS    CL(L'APREPCDE)                                                   
TRLRTYP  DS    CL(L'APREPTYP)                                                   
TRLRLNQ  EQU   *-TRLRCDE                                                        
*                                                                               
RSVMODE  DS    CL1                                                              
TRLFLDH  DS    XL8                                                              
TRLFLD   DS    0CL80                                                            
TRLWORK  DS    CL80                                                             
TRLWORK2 DS    CL80                                                             
TRLNEWEL DS    CL1                 YES/NO DELETE AND ADD ELEMENTS               
TRLELEM  DS    XL256               SAVED ELEMENT                                
TRLTAB   DS    XL256                                                            
TRLIOKEY DS    XL(L'IOKEY)                                                      
TRLWRKX  EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*        DSECT FOR VALIDATION OF PARAMETERS                           *         
***********************************************************************         
VPMWRKD  DSECT                                                                  
CURDEFXN DS    A                   CURRENT DEFTAB ENTRY                         
AOUTPRML DS    A                   ADDRESS OF   OUTPUT PARAMETER LIST           
SVAOUTPL DS    A                   SAVE    ADDR OUTPUT PARAMETER LIST           
VPMPRMS  DS    8F                                                               
PARMNUM  DS    H                                                                
VPMWORK  DS    XL80                                                             
VPMPARM  DS    CL15                                                             
RSVMSGNO DS    CL(L'FVMSGNO)       SAVE AREA FOR  FVMSGNO                       
USEDTYPE DS    AL1                 FLAG IF PARAMETER IS USED YET                
SVUSDTYP DS    AL1                 SAVE AREA FOR USEDTYPE                       
SINGLEPM DS    AL1                                                              
SVSINGPM DS    AL1                 SAVE AREA FOR SINGLEPM                       
TYPEPARM DS    AL1                                                              
RRTNSW   DS    XL1                 LOCAL ROUTINE SWITCH                         
SV0OFFR1 DS    XL1                 SAVE  AREA FOR 0(R1)                         
RRTNUSED EQU   X'80'               .     AREA USED                              
SVKWDGRP DS    XL1                 SAVE  KEYWORD GROUP                          
*                                                                               
VPMWRKX  EQU   *                                                                
         EJECT .                                                                
***********************************************************************         
*        DSECT FOR HOOKING IO TO SPLIT/PASTE X'2D02' RECORDS          *         
***********************************************************************         
IOWRKD   DSECT                                                                  
IOPARM   DS    0F                                                               
         DS    XL1                                                              
         DS    XL1                                                              
IOFIL    DS    XL1                                                              
IOACT    DS    XL1                                                              
IOUSED   DS    A                                                                
IOPRMS   DS    8F                                                               
IONEWLN  DS    H                   SAVE LENGTH OF 1ST REC WRITEN                
IOKEYSV  DS    XL(L'IOKEY)                                                      
IOKEY1ST DS    XL(RESKFRML)                                                     
IOMSK    DS    XL1                 Mask of low order nibble of IOACT            
IOSW     DS    XL1                                                              
IOREC2   EQU   X'80'               2ND RECORD ALREADY PRESENT                   
IODEL2   EQU   X'40'               2ND RECORD DELETE  IT                        
IOWRT2   EQU   X'20'               2ND RECORD WRITE   OR ADD                    
IORST    EQU   X'10'               RESET ORIGINAL RECORD IN IO AREA             
IODEL1   EQU   X'04'               1ST RECORD IS BEING DELETED                  
*                                                                               
IO4KREC  DS    CL1                 4K RECORD TYPE                               
IOELEM   DS    XL256                                                            
IOWRKX   EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*        ADDMSG DSECT                                                 *         
***********************************************************************         
ADDMSGWD DSECT                                                                  
ADMRELO  DS    A                                                                
ADMSGIO  DS    A                   A(RECORD)                                    
ADMSGSTY DS    XL1                 ROW / COL / HEADER SEQUENCE # TYPE           
ADMSGNUM DS    AL2                 MSG NUMBER                                   
ADMSGSEQ DS    XL1                 ROW / COL / HEADER SEQUENCE NUMBER           
ADMSGTYP DS    AL1                 MSG TYPE                                     
ADMSGDAT DS    XL4                 EXTRA DATA (OPTIONAL)                        
ADMSGDTY DS    XL1                 TYPE   OF EXTRA DATA                         
ADMSGDLN DS    XL1                 LENGTH OF EXTRA DATA                         
ADMSGSW  DS    XL1                                                              
ADMSGDEL EQU   X'80'                                                            
ADMELEM  DS    XL256                                                            
ADMPARM  DS    8F                                                               
ADM4KREC DS    CL1                 4K SPLITABLE RECORD TYPE                     
ADDMSGWX EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*        VOFFICE DSECT                                                *         
***********************************************************************         
MKLWRKD  DSECT                                                                  
MKLPARM1 DS    A                   PASSED FROM CALLER                           
MKLPARM  DS    8F                  PARAMETER LIST WHEN CALLING                  
MKLFLDH  DS    CL8                                                              
MKLFLD   DS    CL80                                                             
MKLELEM  DS    XL(L'APELEM)                                                     
MKLWRKX  EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*        VOFFICE DSECT                                                *         
***********************************************************************         
VOFFICED DSECT                                                                  
VOFRELO  DS    A                   RELOCATION   FACTOR                          
VOFCPRML DS    A                   ADDRESS OF   CALLER'S  PARM LIST             
VOFAREC  DS    A                   ADDRESS OF   RECORD                          
VOFENTRY DS    A                   ADDRESS OF   ONE  OFFICE/OFFICE LIST         
VOFBLOCK DS    A                   ADDRESS OF   SCAN BLOCK                      
*                                                                               
VOFPRMS  DS    6F                  PARM    LIST                                 
*                                                                               
VOFTYPE  DS    CL1                 TYPE OF CALL                                 
VOFEXCLD DS    CL1                 EXCLUDE Y/N                                  
VOFMXPRM DS    XL1                 MAX NUM OF PARMS                             
VOF#PRMS DS    XL1                 NUMBER  OF PARMS                             
VOFELMTY DS    XL1                 TYPE OF OFFICE FILTER ELEMENT                
*                                                                               
VOFCARD  DS    CL80                CARD    FOR  VSCANNER  CALL                  
*                                                                               
VOFSBLKE DS    XL32                SINGLE BLOCK ENTRY FOR SCANNER               
VOFMXOFC EQU   20                  ASSUMES MAX  OF   20   OFFICES               
VOFELEM  DS    XL256                                                            
*                                                                               
VOFFICEX EQU   *                                                                
         EJECT ,                                                                
       ++INCLUDE ACSCRWRK                                                       
       ++INCLUDE DDCURTABD                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSOFDATD                                                      
*        SPACE 2                                                                
*WAD     DSECT                                                                  
*        ORG   SCROVLYH                                                         
*        INCLUDE ACSCRF8D                                                       
*        PRINT ON                                                               
*        ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101ACSCR00   09/21/15'                                      
         END                                                                    
