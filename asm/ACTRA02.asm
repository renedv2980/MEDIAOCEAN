*          DATA SET ACTRA02    AT LEVEL 046 AS OF 02/15/19                      
*PHASE T62202B                                                                  
         TITLE 'TRA02 T62202  BILLING TRANSFER - MAINT OVERLAY'                 
T62202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T62202**,R9,RR=RE                                              
*                                                                               
         USING TWAD,R5            R5 = A(TWA)                                   
         USING SAVAREA,R6         R6 = A(SAVE AREA)                             
         USING WORKD,R7           R7 = A(GLOBAL WORKING STORAGE)                
         L     RC,APALOCAL                                                      
         USING LOCALD,RC          RC = A(LOCAL WORKING STORAGE)                 
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO          RELOCATION FACTOR                             
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   APMODE,APMVALK     VALIDATE KEY                                  
         BE    VALKEY                                                           
         CLI   APMODE,APMVALR     VALIDATE RECORD                               
         BE    VALREC                                                           
         CLI   APMODE,APMDISR     DISPLAY RECORD                                
         BE    DISREC                                                           
         CLI   APMODE,APMDISK     DISPLAY KEY (FOR SELECT)                      
         BE    DISKEY                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
YES      CR    RB,RB                                                            
         B     EXIT                                                             
NO       LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*=====================================*                                         
* VALKEY - VALIDATES KEY              *                                         
*=====================================*                                         
*                                                                               
VALKEY   TM    TWAFLAG,TWAFTRC    DID WE COME FROM TRACE                        
         BNO   VALK1              NO                                            
         CLI   APPFKEY,PFK03      FIRST TIME IN VALKEY                          
         BE    VALKOK                                                           
         DC    H'0'                                                             
VALK1    LA    R2,APRECKEY                                                      
         USING MPRRECD,R2                                                       
*                                GET SPECIFIC POSTING INFORMATION               
         GOTO1 ADOSPEC           SETS APTABLE & PMAXNUM                         
*                                                                               
         GOTO1 AVALSYS,POSSYSH    VALIDATE SYSTEM/MEDIA FILE                    
         BNE   VALKX                                                            
         MVC   MPRKALPH,QALPH     AGY ALPHA FOR SPLIT MED FILE(IF SET)          
         MVC   MPRKSYS,QSYS       SYSTEM                                        
*                                                                               
         LA    R3,POSMEDH                                                       
         GOTO1 AVALMED,(R3)       VALIDATE MEDIA                                
         BNE   VALKX                                                            
         MVC   MPRKMED,QMED                                                     
         OC    QMED,QMED          IF NO MEDIA THEN                              
         BNZ   VALOFF                                                           
         GOTO1 AVALFLD,APPARM,POSOFFH,3                                         
         CLI   APPARM,X'FF'       NO CLIENT/PRODUCT ALLOWED                     
         BE    MISSERR            MISSING INPUT FIELD                           
*                                 VALIDATE OFFICE                               
VALOFF   MVI   OCFLAG,0           MARK NO OFFICE INPUTTED                       
         GOTO1 AVALOFF,POSOFFH                                                  
         BNE   VALKX                                                            
         MVC   MPRKOFC,QOFF                                                     
         OC    QOFF,QOFF          IF NO OFFICE                                  
         BZ    VALKCLT            CHECK CLIENT INPUT                            
         MVI   OCFLAG,C'O'        MARK OFFICE INPUTTED                          
*                                                                               
VALKCLT  LA    R3,POSCLTH                                                       
         GOTO1 AVALCLT,(R3)       VALIDATE CLIENT                               
         BNE   VALKX                                                            
         MVC   MPRKCLI,QCLT                                                     
         OC    QCLT,QCLT          IF NO CLIENT REQUESTED                        
         BNZ   VALKCLT5                                                         
         GOTO1 AVALFLD,APPARM,POSPRDH,1                                         
         CLI   APPARM,X'FF'       CAN'T HAVE PRODUCT                            
         BE    MISSERR            IF PROD -MISSING INPUT FIELD TO CLT           
         B     VALKPRD                                                          
*                                                                               
VALKCLT5 CLI   OCFLAG,C'O'        YES - CAN'T HAVE OFFICE TOO                   
         BE    VALKNOTV           ERROR                                         
         MVI   OCFLAG,C'C'        SET CLIENT REQUESTED ALONE                    
*                                                                               
VALKPRD  GOTO1 AVALPRD,POSPRDH    VALIDATE PRODUCT                              
         BNE   VALKX                                                            
         MVC   MPRKPRD,QPRD                                                     
*                                                                               
         GOTO1 ASETFILE           CHECK FOR ACCFILE CHANGE OR LIMIT ACC         
         BE    VALKPST                                                          
         LA    R2,POSSYSH                                                       
         ST    R2,APCURSOR                                                      
         B     VALKX                                                            
*                                                                               
VALKPST  LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         USING ACPOSTD,R3                                                       
         BAS   RE,CHKPRF          CHECK FOR PROFILE VALUES FIRST                
         BAS   RE,SETPST                                                        
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS(ACPOSTER READS            
*                                 MESS IT UP)                                   
         BAS   RE,SVSIS           SAVE ALL INCOME ACCOUNTS                      
*                                                                               
VALKOK   MVI   NEWREC,C'N'           NOT NEW RECORD                             
         MVI   NEWELE,C'Y'           NEW ELEMENT TYPE                           
         BAS   RE,RDREC              GET CURRENT KEY RECORD INTO AIO1           
         MVC   FVMSGNO,=AL2(FVFOK)   RESET ERROR MSG                            
         LA    R1,POSL1ACH                                                      
         STCM  R1,15,APCURSOR        SET CURSOR POSITION                        
         MVC   POSMSG,SPACES                                                    
         OI    POSMSGH+6,X'80'                                                  
         CLI   NEWELE,C'Y'           DOES RECORD EXIT FOR KEY LEVEL             
         BNE   VALKX                 YES                                        
         MVC   POSMSG(30),=C'** NEW RECORD WILL BE ADDED **'                    
*                                                                               
VALKX    B     EXIT                                                             
         SPACE 2                                                                
VALKNOTV MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKX                                                            
MISSERR  STCM  R3,15,APCURSOR                                                   
         MVC   FVMSGNO,=AL2(FVIMISS)                                            
         B     VALKX                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
*====================================================*                          
* CHKPRF - CALLS ACPOSTER FOR PROFILE RECORDS        *                          
*====================================================*                          
*                                                                               
CHKPRF   NTR1                                                                   
         CLI   QSYS,C'P'                                                        
         BNE   CHKPRFX                                                          
         CLI   RECTYPE,MPRKPPB                                                  
         BNE   CHKPRFX                                                          
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         BAS   RE,SETBLOCK        BASIC CALL TO ACPOSTER                        
         MVI   ACPTYPE,1          POST RECORDS                                  
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         MVI   SCSWSE,0           CLEAR SAVED ACC SYS(ACPOSTER READS            
*                                 MESS IT UP)                                   
         DROP  R3                                                               
         USING ACPRTND,R3                                                       
         MVI   APBYTE,MTPFSUFX    SUSPENSE SUFFIX                               
         BAS   RE,GETROW                                                        
         MVC   SUSPX,ACPFVAL                                                    
         MVI   APBYTE,MTPFRBFX    REBATE SUFFIX                                 
         BAS   RE,GETROW                                                        
         MVC   REBX,ACPFVAL                                                     
CHKPRFX  B     EXIT                                                             
         DROP  R3                                                               
         SPACE                                                                  
*====================================================*                          
* SETPST - SETS UP POST MAINT CALL                   *                          
*====================================================*                          
*                                                                               
SETPST   NTR1                                                                   
         LA    R3,PSTBLK                                                        
         USING ACPOSTD,R3                                                       
         BAS   RE,SETBLOCK        BASIC CALL TO ACPOSTER                        
         MVI   ACPTYPE,0          POST RECORDS                                  
         MVC   ACPRBFX,REBX                                                     
         MVC   ACPSUFX,SUSPX                                                    
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*====================================================*                          
* SETBLOCK - SETS UP BLOCK TO ACPOSTER               *                          
*====================================================*                          
*                                                                               
SETBLOCK NTR1                                                                   
         XC    IOKEY,IOKEY        READ DUMMY ACC REC TO SWITCH                  
         MVC   IOKEY(1),COMPANY   TO NATIVE ACC SSYTEM BEFORE CALLING           
         GOTO1 AMIOACC,APPARM,IOHI+IOACCFIL+IO3,=C'SE1'                         
         LA    RE,MAXPNUM         CLEAR POSTING TABLE                           
         L     RF,APSTTBL                                                       
SETBLK5  XC    0(ACPRTNL,RF),0(RF)                                              
         LA    RF,ACPRTNL(RF)                                                   
         BCT   RE,SETBLK5                                                       
*                                                                               
         LA    RE,PSTBLK          CLEAR BLOCK                                   
         LA    RF,ACPOSTL                                                       
         XCEFL                                                                  
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         USING ACPOSTD,R3                                                       
         MVC   ACPACOM,ACOM       A(COMFACS)                                    
         MVC   ACPSW,VSWITCH      A(SWITCH)                                     
         MVC   ACPPOST,APSTTBL    A(POSTINGS RETURNED)                          
         MVC   ACPCMPC,COMPANY    NATIVE COMPANY CODE                           
         MVC   ACPCMPC2,COMPANY2  OTHER COMPANY CODE                            
         MVC   ACPSPROD,SVPROD    PRODUCTION UNIT/LEDGER CODE                   
         MVC   ACPCOST,SVCOST     C'Y' - USE COST,BILL,REV                      
         MVI   ACPTYPE,0          POST RECORDS                                  
         MVC   ACPMI,SVMI         C'Y' IF MI RECORDS IN USE                     
         MVC   ACPALPH,QALPH      AGENCY ALPHA FOR SPLIT MEDIA FILES            
         MVC   ACPSYS,QSYS        SYSTEM                                        
         MVC   ACPMED,QMED        MEDIA CODE                                    
         MVC   ACPSE1,SVSE1       NATIVE SE NUMBER                              
         MVC   ACPSE2,SVSE2       OTHER ACC FILE SE NUMBER                      
         OC    QCLT,QCLT          CLIENT ENTERED?                               
         BNZ   SETBLK20                                                         
         MVC   ACPOFC,QOFF        OFFICE                                        
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
         CLI   QOFFIND,C'O'       OFFICE CODE ENTERED?                          
         BE    SETBLK30                                                         
         MVI   ACPOFC,0           OFFICE                                        
         MVC   ACPOFG,QOFF        OFFICE GROUP                                  
         B     SETBLK30                                                         
*                                                                               
SETBLK20 MVC   ACPOFC,SVCOFF      OFFICE                                        
         MVC   ACPOFG,SVOFFG      OFFICE GROUP                                  
SETBLK30 MVC   ACPCLT,QCLT        CLIENT                                        
         MVC   ACPPRD,QPRD        PRODUCT                                       
         MVC   ACPTYPE2,RECTYPE   REG,AOR,RET,PRD (REPLACES A(BILL))            
         MVI   ACPIND,ACPEXP      ASK FOR AMT & MEMO EXPRESSIONS                
*                                                                               
         CLI   RECTYPE,MPRKPST     IF READING TPOST MAINT                       
         BNE   SETBLK40                                                         
         BAS   RE,SETPSTO         SET OUTPUT PST INFO AT CLI/PRD LEVEL          
         BAS   RE,SETPSTI         SET INPUT PST INFO                            
*                                                                               
SETBLK40 MVC   ACPGSTO,SVCGST     GST CODE                                      
         CLI   SVPGST,0                                                         
         BE    SETBLKX                                                          
         MVC   ACPGSTO,SVPGST     OVERRIDE W/PRD GST CODE                       
SETBLKX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO SET PST VALUES IN ACPOSTER BLOCK                      
         USING ACPOSTD,R3                                                       
SETPSTO  NTR1                                                                   
         MVI   BYTE,C'C'          SET INDICATOR PROCESSING CLIENT               
         LA    R1,SVCPST          R1=A(CLIENT PST CODES)                        
*                                                                               
SETPSTO3 LA    R0,L'SVCPST                                                      
         LA    RE,ACPPSTO         RE=A(PST OUTPUT CODES)                        
         LA    RF,PRVTAB          RF=A(PROVINCE TABLE)                          
         USING PRVTABD,RF                                                       
*                                                                               
SETPSTO5 CLI   PRVTOUT,0          IF ENTRY IS ACTIVE                            
         BE    SETPSTO7                                                         
         MVC   0(2,RE),PRVTCODE   SET PROVINCE CODE                             
         MVC   2(1,RE),PRVTOUT    SET EQUATED ELEMENT CODE FOR OUTPUT           
         CLI   BYTE,C'C'          IF CLIENT LEVEL                               
         BNE   *+8                                                              
         MVI   3(RE),C'S'         SET STANDARD AS DEFAULT                       
         CLI   0(R1),0            IF TAX CODE                                   
         BE    *+10                                                             
         MVC   3(1,RE),0(R1)      SET IT                                        
*                                                                               
SETPSTO7 LA    RE,L'ACPPSTO(RE)                                                 
         LA    RF,L'PRVTAB(RF)    BUMP TO NEXT PROVINCE IN TABLE                
         LA    R1,1(R1)           BUMP TO NEXT CLIENT VALUE                     
         BCT   R0,SETPSTO5                                                      
*                                                                               
         CLI   BYTE,C'C'          IF FINISHED WITH CLIENT VALUES                
         BNE   SETPSTOX                                                         
         MVI   BYTE,C'P'          SET TO GET PRODUCT VALUES                     
         LA    R1,SVPPST          R1=A(PRODUCT PST CODES)                       
         B     SETPSTO3                                                         
*                                                                               
SETPSTOX B     EXIT                                                             
         DROP  R3,RF                                                            
         EJECT                                                                  
*              ROUTINE TO SET INPUT PST INFO                                    
*                                                                               
         USING ACPOSTD,R3                                                       
SETPSTI  NTR1                                                                   
         LA    R1,ACPPSTI                                                       
         LA    RF,PRVTAB          RF=A(PROVINCE TABLE)                          
         USING PRVTABD,RF                                                       
*                                                                               
SETPSTI5 CLI   0(RF),X'FF'                                                      
         BE    SETPSTIX                                                         
         CLI   PRVTIN,0            IF ENTRY IS ACTIVE                           
         BE    SETPSTI8                                                         
         MVC   0(2,R1),PRVTCODE    SET PROVINCE CODE                            
         MVC   2(1,R1),PRVTIN      SET EQUATED ELEMENT INPUT ACCOUNT            
SETPSTI8 LA    RF,L'PRVTAB(RF)                                                  
         LA    R1,L'ACPPSTI(R1)                                                 
         B     SETPSTI5                                                         
*                                                                               
SETPSTIX B     EXIT                                                             
         DROP  R3,RF                                                            
         EJECT                                                                  
*===============================================================*               
* RDREC - BUILD LOWEST LEVEL POSTING RULES KEY & READ INTO IO1  *               
*===============================================================*               
*                                                                               
RDREC    NTR1                                                                   
         LA    R4,IOKEY           BUILD POST MAINT(RULES) RECORD                
         USING MPRRECD,R4                                                       
         XC    MPRKEY,MPRKEY                                                    
         MVI   MPRKTYP,MPRKTYPQ   X'2F'                                         
         MVI   MPRKSUB,MPRKSUBQ   X'01'                                         
         MVC   MPRKCPY,COMPANY    NATIVE COMPANY CODE                           
         MVC   MPRKALPH,QALPH     ALPHA AGENCY FOR SPLIT MEDIA FILES            
         MVC   MPRKSYS,QSYS       SYSTEM                                        
         MVC   MPRKMED,QMED       MEDIA                                         
         MVC   MPRKOFC,QOFF       OFFICE                                        
         MVC   MPRKCLI,QCLT       CLIENT                                        
         MVC   MPRKPRD,QPRD       PRODUCT                                       
         MVC   MPRKPRO,RECTYPE    BILLING TYPE                                  
         MVC   SVKEY,MPRKEY                                                     
         CLI   APACTN,ACTDIS                                                    
         BNE   RDREC1                                                           
         GOTO1 AMIOACC,APPARM,IORD+IOACCFIL+IO1+IOLOCK,=C'SE1'                  
         B     RDREC3                                                           
RDREC1   GOTO1 AMIOACC,APPARM,IORD+IOACCFIL+IO1,=C'SE1'                         
*                                                                               
RDREC3   MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    IOERR,IOERNF       RECORD NOT FOUND?                             
         BNO   RDREC10                                                          
         MVI   APINDS,APIOKDIS+APIOKCHA     OKAY TO ADD                         
         MVI   NEWREC,C'Y'        MARK NEW RECORD                               
         L     R4,AIOAREA1                                                      
         XC    0(256,R4),0(R4)    RESET IO AREA                                 
         MVC   MPRKEY,SVKEY       MOVE IN KEY                                   
         B     RDRECX                                                           
*                                                                               
RDREC10  L     R4,AIOAREA1                                                      
         AH    R4,DATADISP                                                      
         USING MBTELD,R4                                                        
RDREC15  CLI   0(R4),0                                                          
         BE    RDRECX                                                           
         CLI   0(R4),MBTELQ       ANY POST ELEMENTS?                            
         BNE   RDREC18                                                          
         CLI   INREC,RECAPOST     IF APOST RECORD                               
         BNE   RDREC16                                                          
         CLI   MBTTYP,MBTTINTI    MUST HAVE ELEM OTHER THAN INTERNAL            
         BL    RDREC17                                                          
         CLI   MBTTYP,MBTTINTR                                                  
         BH    RDREC17                                                          
         B     RDREC18                                                          
*                                                                               
RDREC16  CLI   INREC,RECAPOS2     IF APOS2 RECORD                               
         BNE   RDREC17                                                          
         CLI   MBTTYP,MBTTINTI    MUST HAVE ELEM OF INTERNAL TYPE               
         BL    RDREC18                                                          
         CLI   MBTTYP,MBTTINTR                                                  
         BH    RDREC18                                                          
*                                                                               
RDREC17  MVI   NEWELE,C'N'        SET RECORD/ELEMENTS FOUND                     
         B     RDRECX                                                           
*                                                                               
RDREC18  SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     RDREC15                                                          
*                                                                               
RDRECX   B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*==============================================*                                
* SVSIS - SAVE INCOME ACCOUNTS FOR LATER CHECK *                                
*==============================================*                                
*                                                                               
SVSIS    NTR1                                                                   
         USING ACPRTND,R3                                                       
         MVI   APBYTE,MBTTINC     GET INCOME ROW                                
         BAS   RE,GETROW                                                        
         MVC   INCACC,ACPACC      SAVE INCOME ACCOUNT                           
         MVC   INCLVL,ACPLVL      SAVE INCOME ACCOUNT LEVEL                     
         OC    SINCACC,SPACES                                                   
         MVI   APBYTE,MBTTARI     GET AOR INCOME ROW                            
         BAS   RE,GETROW                                                        
         MVC   AINCACC,ACPACC     SAVE AOR INCOME ACCOUNT                       
         MVC   AINCLVL,ACPLVL     SAVE AOR INC ACCOUNT LEVEL                    
         MVI   APBYTE,MBTTSEL     GET SELLOFF INCOME ROW                        
         BAS   RE,GETROW                                                        
         MVC   SINCACC,ACPACC     SAVE SELLOFF INCOME ACCOUNT                   
         MVC   SINCLVL,ACPLVL     SAVE SELL INC ACCOUNT LEVEL                   
         MVI   APBYTE,MBTTINTI    GET INTERNAL INCOME ROW                       
         BAS   RE,GETROW                                                        
         MVC   IINCACC,ACPACC     SAVE INTERNAL INCOME ACCOUNT                  
         MVC   IINCLVL,ACPLVL     SAVE INTERNAL INC ACCOUNT LVL                 
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=================*                                                             
* VALIDATE RECORD *                                                             
*=================*                                                             
*                                                                               
VALREC   TM    TWAFLAG,TWAFTRC    DID WE COME FROM TRACE                        
         BNO   VALR10                                                           
         OI    TWALSCTL,TWALSRTN                                                
         OI    TWALSCTL,TWALSHLD                                                
         MVI   TWALSACT,ACTMAI                                                  
         NI    TWAFLAG,FF-TWAFTRC TURN OFF INDICATOR                            
         B     VALRX                                                            
*                                                                               
VALR10   MVI   RECHANG,C'N'       RECORD HAS NOT BEEN CHANGED                   
         GOTO1 VDATCON,APPARM,(5,APWORK),(3,LCHDT)                              
         LA    R1,1               FIRST POSTING                                 
         USING ACPRTND,R3                                                       
         LA    R2,POSL1DH         POINT TO FIRST LINE ON SCREEN                 
         USING DISD,R2                                                          
         L     R4,APTABLE         CURRENT RECORD ROW TABLE                      
*                                                                               
VALR20   MVC   APBYTE,0(R4)       ROW NUMBER                                    
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         OC    DACC,SPACES                                                      
         MVC   APWORK(L'ACPACC),ACPACC                                          
         OC    APWORK,SPACES                                                    
         CLC   DACC,APWORK        CHANGE IN ACCOUNT?                            
         BE    VALR30                                                           
         OI    ACPBCHG,X'80'      INDICATE ACCOUNT HAS CHANGED                  
         CLI   0(R4),MBTTCD       IF CASH DISCOUNT                              
         BNE   VALR30                                                           
         CLI   QSYS,C'P'                                                        
         BNE   VALRNOCD           NOT APPLICABLE                                
*                                                                               
VALR30   CLC   ACPAMT,DAMT        CHANGE IN AMOUNT?                             
         BNE   VALR35             YES - ADD/CHANGE LOWEST LEVEL RECORD          
         CLC   ACPMEMO,DMEMO      CHANGE IN MEMO?                               
         BE    VALR45             NO CHANGE IN AMOUNT/MEMO                      
VALR35   BAS   RE,CHKBLNK         IF BLANKING OUT - MUST DO BOTH                
         BNE   VALRX                                                            
         CLI   0(R4),MBTTCD       IF CASH DISCOUNT                              
         BNE   VALR38                                                           
         CLI   QSYS,C'P'                                                        
         BNE   VALRNOCD           NOT APPLICABLE                                
VALR38   OI    ACPBCHG,X'40'      INDICATE ACCOUNT HAS CHANGED                  
*                                                                               
VALR40   MVC   TEXP,DAMT                                                        
         MVI   TEXPTYPE,C'A'                                                    
         BAS   RE,CHKEXP          CHANGE IN ROW - SAVE ELEMENT TYPE             
         BE    VALR43                                                           
         LA    R1,DAMTH           NOT A VALID AMOUNT                            
         STCM  R1,15,APCURSOR                                                   
         B     VALRTERR                                                         
VALR43   MVC   TEXP,DMEMO                                                       
         MVI   TEXPTYPE,C'M'                                                    
         BAS   RE,CHKEXP          CHANGE IN ROW - SAVE ELEMENT TYPE             
         BE    VALR45                                                           
         LA    R1,DMEMOH          NOT A VALID MEMO                              
         STCM  R1,15,APCURSOR                                                   
         B     VALRTERR                                                         
*                                                                               
VALR45   CLI   ACPBCHG,0          ANY CHANGE?                                   
         BE    VALR50             NO -                                          
         MVC   ELETYPE,0(R4)      CHANGE IN ROW - SAVE ELEMENT TYPE             
         BAS   RE,DOELEM          ADD/CHANGE ELEMENT IN RECORD                  
         BNE   VALRX              ERROR IN ACCOUNT                              
*                                 NO CHANGE TO THIS POSTING LEVEL               
VALR50   LA    R2,DISDL(R2)       PT TO NEXT SCREEN LINE                        
         LA    R4,APTABLN(R4)     NEXT RECORD ROW TABLE ENTRY                   
         LA    R1,1(R1)           NEXT POSTING                                  
         ZIC   R0,PMAXNUM         FINISHED ALL POSTINGS?                        
         CR    R1,R0                                                            
         BNH   VALR20             NO -CHECK NEXT POSTING ROW FOR CHANGE         
         CLI   SVCOST,C'Y'        ARE THEY COSTING AGENCY?                      
         BE    VALR52                                                           
         BAS   RE,CHKCST          NO COSTING CHANGES ALLOWED                    
         BNE   VALRX                                                            
         B     VALR53                                                           
*                                                                               
VALR52   BAS   RE,CHKMAT          CHK INCOME,BILLING,REVENUE MATCHES            
         BNE   VALRX                                                            
         BAS   RE,CHKROB          CHK BILL & REV ACCOUNTS MATCH SI ACC          
         BNE   VALRX                                                            
         BAS   RE,CHKARB          CHK AOR BILL & REV ACCS MATCH AOR SI          
         BNE   VALRX                                                            
         BAS   RE,CHKSRB          CHK SELL BIL & REV ACCS MATCH SELL SI         
         BNE   VALRX                                                            
         BAS   RE,CHKIRB          CHK INT BIL & REV ACCS MATCH INT SI           
         BNE   VALRX                                                            
*                                                                               
VALR53   CLI   RECHANG,C'Y'       HAS RECORD CHANGED?                           
         BE    VALR55                                                           
         MVC   FVADDR,AACTHDR     NO - KEEP SAME                                
         MVC   FVMSGNO,=AL2(FVFCHG1)                                            
         MVI   FVOMTYP,C'I'                                                     
         B     VALR65                                                           
*                                                                               
VALR55   MVC   POSMSG,SPACES      CLEAR MESSAGE                                 
         CLI   NEWREC,C'Y'        RECORD HAS CHANGED - IS IT NEW?               
         BE    VALR60             YES - ADD IT                                  
         GOTO1 AMIOACC,APPARM,IOACCFIL+IOWRITE+IO1,=C'SE1'                      
         B     VALR65                                                           
VALR60   GOTO1 AMIOACC,APPARM,IOACCFIL+IOADD+IO1,=C'SE1'                        
         MVC   FVADDR,AACTHDR             AND HAVE MSG REFLECT                  
         MVC   FVMSGNO,=AL2(FVFADD1)      NEW ADD                               
         MVI   FVOMTYP,C'I'                                                     
*                                                                               
VALR65   TM    TWAMODE,TWAMLSM    IF COMING FROM LIST                           
         BNO   VALR70                                                           
         OI    TWALSCTL,TWALSHLD  ALWAYS HOLD SCREEN                            
         OI    TWALSCTL,TWALSRTN  AND RETURN TO MAINT                           
VALR70   BAS   RE,DISCHANG        TRANSMIT CHANGED RECORD                       
         BAS   RE,CHKPFKS         CHECK PFKEY HIT                               
VALRX    B     EXIT                                                             
         SPACE 2                                                                
VALRTERR MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                                                               
         SPACE                                                                  
VALRNOCD MVC   FVMSGNO,=AL2(FVINOCD)                                            
         STM   R2,15,APCURSOR                                                   
         B     NO                                                               
         EJECT                                                                  
*=====================================================*                         
* CHKBLNK-IF BLANKING OUT AMOUNT MUST BLANK OUT MEMO  *                         
*         AND VISA VERSA CC SET IF NOT THIS CASE      *                         
*=====================================================*                         
*                                                                               
CHKBLNK  NTR1                                                                   
         OC    DAMT,DAMT          ANY AMOUNT ON SCREEN                          
         BNZ   CHKBLNK5                                                         
         OC    ACPAMT,ACPAMT      NO - DID THEY JUST BLANK IT OUT               
         BZ    CHKBLNK5           NO - BLANK BEFORE                             
*                                                                               
         OC    DMEMO,DMEMO        AMOUNT WAS JUST BLANKED OUT                   
         BZ    YES                SO MUST MEMO BE                               
         LA    R1,DMEMOH                                                        
         STCM  R1,15,APCURSOR                                                   
         B     CHKBLNKN           PUT OUT ERROR                                 
*                                                                               
CHKBLNK5 OC    DMEMO,DMEMO        ANY MEMO ON SCREEN                            
         BNZ   YES                                                              
         OC    ACPMEMO,ACPMEMO    NO - DID THEY JUST BLANK IT OUT               
         BZ    YES                NO - BLANK BEFORE                             
*                                                                               
         OC    DAMT,DAMT          MEMO  WAS JUST BLANKED OUT                    
         BZ    YES                SO MUST AMOUNT BE                             
         LA    R1,DAMTH                                                         
         STCM  R1,15,APCURSOR     PUT OUT ERROR                                 
CHKBLNKN MVC   FVMSGNO,=AL2(FVIBLANK) MUST BLANK OUT BOTH                       
         B     NO                                                               
         EJECT                                                                  
*=====================================================*                         
* DOELEM- ADDS NEW ELEMENT OR CHANGES CURRENT ELEMENT *                         
*    NTRY - PROWNUM = POSTING NUM , R2 ->NEW ON SCRN  *                         
*=====================================================*                         
*                                                                               
DOELEM   NTR1                                                                   
         XC    OLDACC,OLDACC                                                    
         XC    OLDAMT,OLDAMT                                                    
         XC    OLDMEMO,OLDMEMO                                                  
         L     R4,AIOAREA1                                                      
         ST    R4,AIOAREA         SET ADDRESS OF RECORD                         
         AH    R4,DATADISP        PT TO FIRST ELEMENT                           
         USING MBTELD,R4                                                        
         SR    R0,R0                                                            
DOELEM20 CLI   0(R4),0            END OF RECORD?                                
         BE    DOELEM50           YES - NEW ELEMENT MUST BE ADDED               
         CLI   0(R4),MBTELQ        MEDIA TRANSFER ELEMENT = X'2E'               
         BNE   DOELEM30           YES -                                         
         CLC   ELETYPE,MBTTYP     CORRECT POSTING TYPE?                         
         BE    DOELEM40           YES                                           
DOELEM30 ICM   R0,1,1(R4)         NO - GET ELEMENT LENGTH                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0              AND BUMP TO NEXT ELEMENT                      
         B     DOELEM20                                                         
*                                 ELEMENT FOUND - NEEDS CHANGING?               
DOELEM40 MVI   BYTE3,C'N'         INDICATE -NO REAL CHANGE TO ELEMENT           
         TM    ACPBCHG,X'80'      CHANGE TO ACC ON SCREEN?                      
         BNO   DOELEM45                                                         
         MVC   APWORK(L'DACC),MBTULA YES -                                      
         OC    APWORK,SPACES                                                    
         CLC   DACC,APWORK          BUT ALSO CHANGE TO ELEMENT?                 
         BE    *+8                                                              
         MVI   BYTE3,C'Y'         YES                                           
DOELEM45 TM    ACPBCHG,X'40'      CHANGE TO AMOUNT/MEMO ON SCREEN?              
         BNO   DOELEM48                                                         
         CLC   DAMT,MBTAMTX       YES - BUT CHANGE TO AMOUNT IN ELE?            
         BNE   *+14                                                             
         CLC   DMEMO,MBTMEMOX     OR TO MEMO IN ELE?                            
         BE    *+8                                                              
         MVI   BYTE3,C'Y'         YES                                           
*                                                                               
DOELEM48 CLI   BYTE3,C'N'         ANY REAL CHANGE TO ELEMENT?                   
         BE    DOELEMX            NO - GET OUT                                  
         MVC   OLDACC,MBTULA                                                    
         MVC   OLDAMT,MBTAMTX                                                   
         MVC   OLDMEMO,MBTMEMOX                                                 
         MVI   APELCD,MBTELQ      YES -DELETE THE X'2E' ELEMENT                 
         GOTO1 ADELL,APPARM,(X'01',ELETYPE)                                     
         MVI   RECHANG,C'Y'       INDICATE RECORD HAS BEEN CHANGED              
         DROP  R4                                                               
         SPACE                                                                  
DOELEM50 XC    APELEM,APELEM                                                    
         LA    R1,APELEM          NEW ELEMENT MUST BE ADDED                     
         USING MBTELD,R1                                                        
         MVI   MBTEL,MBTELQ       X'2D'                                         
         MVI   MBTLLN,MBTPRLNQ    ELEMENT LENGTH                                
         MVC   MBTTYP,ELETYPE     POSTING TYPE                                  
         MVC   MBTCHNG,LCHDT      TODAYS DATE                                   
         MVC   MBTPERID,PERSON    SET PERSON                                    
*                                                                               
         MVC   MBTULA(L'OLDACC),OLDACC MOVE IN OLD ACC VALUE                    
         TM    ACPBCHG,X'80'      CHANGE IN ACCOUNT MADE?                       
         BZ    DOELEM55           NO - DON'T TOUCH ACCOUNT IN ELEM              
         BAS   RE,CHKACCNT        YES - CHECK IF ACCOUNT EXITS                  
         BNE   DOELEMX            ERROR - DOESN'T EXIST                         
         MVC   MBTULA(L'DACC),DACC SET UNIT/LEDGER/ACCOUNT                      
*                                                                               
DOELEM55 MVC   MBTAMTX,OLDAMT     MOVE IN OLD AMOUNT AND                        
         MVC   MBTMEMOX,OLDMEMO   MEMO VALUE                                    
         TM    ACPBCHG,X'40'      CHANGE MADE TO AMOUNTS?                       
         BZ    DOELEM60                                                         
         MVC   MBTAMTX,DAMT       SET AMOUNT                                    
         MVC   MBTMEMOX,DMEMO     SET MEMO                                      
*                 IF NO AMT,MEMO OR ACC -DON'T ADD ELEM                         
DOELEM60 OC    MBTAMTX,MBTAMTX                                                  
         BNZ   DOELEM70                                                         
         OC    MBTMEMOX,MBTMEMOX                                                
         BNZ   DOELEM70                                                         
         OC    MBTULA,MBTULA                                                    
         BZ    YES                                                              
DOELEM70 GOTO1 AADDL                                                            
         MVI   RECHANG,C'Y'       INDICATE RECORD HAS BEEN CHANGED              
         B     YES                                                              
*                                                                               
DOELEMX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*=====================================*                                         
* CHKACCNT - READS FOR ACCOUNT RECORD *                                         
*=====================================*                                         
*                                                                               
CHKACCNT NTR1                                                                   
         MVC   APWORK,SPACES                                                    
         MVC   APWORK(L'DACC),DACC                                              
         CLI   ELETYPE,MBTTRCR    REBATE RCVBL ROW                              
         BE    CHKACC40           MAY NEVER BE CHANGED                          
         CLI   ELETYPE,MBTTRCS    SUSPENSE RCVBL ROW                            
         BE    CHKACC40           MAY NEVER BE CHANGED                          
         OC    QCLT,QCLT          IF NO CLIENT                                  
         BNZ   CHKACC1                                                          
         CLI   ELETYPE,MBTTRCV   MAY NOT CHANGE RECEIVABLE                      
         BE    CHKACC40                                                         
         CLI   ELETYPE,MBTTCOS   MAY NOT CHANGE COSTING                         
         BE    CHKACC40                                                         
         CLI   ELETYPE,MBTTINTC                                                 
         BE    CHKACC40                                                         
*                                                                               
CHKACC1  CLC   APWORK(L'DACC),SPACES                                            
         BNE   CHKACC5                                                          
         XC    DACC,DACC          DON'T WANT ACCOUNT NAME IN REC                
         B     YES                                                              
*                                                                               
CHKACC5  ZIC   RE,DACCH+5         GET LENGTH OF NEW ACCOUNT INPUT               
         BCTR  RE,0                                                             
         CLC   =C'NONE',DACC                                                    
         BE    CHKACC9            THIS POSTING NOT WANTED                       
         LA    R1,DACC                                                          
         AR    R1,RE              PT TO LAST CHAR IN ACCOUNT                    
         CLI   0(R1),C'+'                                                       
         BE    CHKACC45           CAN'T END WITH A +                            
*                                                                               
         ZIC   RE,DACCH+5         IF + NEXT POS MUST BE A NUMBER 1-4            
         LA    R1,DACC                                                          
         SR    R4,R4                                                            
CHKACC7  CLI   0(R1),C'+'                                                       
         BNE   CHKACC8                                                          
         CLI   1(R1),C'1'                                                       
         BL    CHKACC45                                                         
         CLI   1(R1),C'4'                                                       
         BH    CHKACC45                                                         
         LA    R4,1(R4)                                                         
CHKACC8  LA    R1,1(R1)                                                         
         BCT   RE,CHKACC7                                                       
         LTR   R4,R4                                                            
         BZ    CHKACC10           NO + SIGN IN ACCOUNT                          
         CH    R4,=H'2'                                                         
         BH    CHKACC45           NO MORE THAN 2 + SIGNS                        
         CLI   INREC,RECRPOST      + ONLY ALLOWED FOR RETAIL                    
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                                                               
*                                                                               
CHKACC9  MVC   APWORK,SPACES                                                    
         ZIC   RE,DACCH+5                                                       
         BCTR  RE,0               ONE FOR EX STATEMENT                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),DACC                                                   
         B     YES                                                              
*                                                                               
CHKACC10 LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY2           OTHER COMPANY CODE                    
         MVC   ACTKUNT(14),APWORK         UNIT & LEDGER & ACCOUNT               
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   CHKACC35                                                         
         DROP  R4                                                               
CHKACC20 L     R4,AIOAREA3                                                      
         AH    R4,DATADISP                                                      
         USING ABLELD,R4          ACCOUNT BALANCE ELEMENT                       
CHKACC25 CLI   ABLEL,0            END OF RECORD?                                
         BE    CHKACC30                                                         
         CLI   ABLEL,ABLELQ       BALANCE ELEMENT?                              
         BE    YES                                                              
         SR    R0,R0                                                            
         ICM   R0,1,ABLLN                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     CHKACC25                                                         
*                                                                               
CHKACC30 MVC   FVMSGNO,=AL2(FVIACCP) ACCOUNT NOT VALID FOR POSTING              
         B     CHKACCN                                                          
CHKACC35 MVC   FVMSGNO,=AL2(FVIACC)  INVALID ACCOUNT                            
         B     CHKACCN                                                          
CHKACC40 MVC   FVMSGNO,=AL2(FVINOC)  CHANGE NOT ALLOWED                         
         B     CHKACCN                                                          
CHKACC45 MVC   FVMSGNO,=AL2(FVILEVEL) INVALID LEVEL FORMAT                      
         B     CHKACCN                                                          
*                                                                               
CHKACCN  LA    R1,DACCH-DDISH     SET CURSOR TO ACCOUNT WITH PROBLEM            
         AR    R2,R1                                                            
         STCM  R2,15,APCURSOR                                                   
         B     NO                 ACCOUNT DOESN'T EXIST                         
         DROP  R2,R4,R3                                                         
         EJECT                                                                  
*===========================================*                                   
* SETTBL - SETS THE START ADDRESS FOR EACH  *                                   
*          POSTING LINE - SO CAN KNOW WHICH *                                   
*          LINE TO TRACE                    *                                   
*===========================================*                                   
*                                                                               
SETTBL   NTR1                                                                   
         LA    R3,TRACTBL          FIRST CLEAR OUT TRCTBL                       
         LA    RF,MAXPNUM                                                       
SETTBL2  XC    0(L'TRACTBL,R3),0(R3)                                            
         BCT   RF,SETTBL2                                                       
*                                                                               
         LA    R3,TRACTBL         SET UP TRCTBL                                 
         ZIC   RF,PMAXNUM                                                       
         LA    R2,POSL1DH                                                       
SETTBL5  LR    R1,R2                                                            
         SR    R1,R5              DISPLAY OF POSL1D FROM TOP OF TWA             
         STH   R1,0(R3)                                                         
         LA    RE,POSL2DH-POSL1DH LENGTH OF LINE                                
         AR    R2,RE              PT TO NEXT LINE ON SCREEN                     
         LA    R3,L'TRACTBL(R3)   PT TO NEXT TABLE ENTRY                        
         BCT   RF,SETTBL5                                                       
         B     EXIT                                                             
         EJECT                                                                  
* CHKEXP - CHECKS IF EXPRESSION VALID FOR SCRN,CNTRY,ETC..                      
*                                  CC CODE SET ON EXIT                          
*                                                                               
CHKEXP   NTR1                                                                   
         OC    TEXP,SPACES                                                      
         CLC   TEXP,SPACES        NO EXPRESSION IS VALID                        
         BE    YES                                                              
*                                                                               
         LA    R2,EXPTBL          BASIC VALID EXPRESSION TABLE                  
         BAS   RE,CHKTBL                                                        
         BE    CHKEXP50                                                         
*                                                                               
         CLI   INREC,RECAPOST      IF AOR TYPE CHECK AOR EXTENTION TAB          
         BE    CHKEXP10                                                         
         CLI   INREC,RECAPOS2                                                   
         BE    CHKEXP10                                                         
         CLI   INREC,RECUANPT                                                   
         BE    CHKEXP10                                                         
         CLI   INREC,RECXAPST                                                   
         BE    CHKEXP10                                                         
         CLI   INREC,RECXAPS2                                                   
         BE    CHKEXP10                                                         
         CLI   INREC,RECUACPT                                                   
         BNE   CHKEXP20                                                         
CHKEXP10 LA    R2,EXPAORT                                                       
         BAS   RE,CHKTBL                                                        
         BNE   NO                                                               
         B     CHKEXP50                                                         
*                                                                               
CHKEXP20 CLI   INREC,RECSPOST      IF SPECIAL BILLING                           
         BNE   NO                                                               
         LA    R2,EXPSPPT          CHECK ITS EXTENSION TABLE                    
         BAS   RE,CHKTBL                                                        
         BNE   NO                                                               
*                                                                               
CHKEXP50 CLI   TEXPTYPE,C'A'       CHECK EXPRESSION VALID FOR ROW               
         BNE   YES                                                              
         LA    RE,EXPBYROW                                                      
CHKEXP55 CLI   0(RE),X'FF'         TEST END OF TABLE                            
         BE    YES                                                              
         CLC   0(1,R4),0(RE)                                                    
         BE    CHKEXP60                                                         
         LA    RE,L'EXPBYROW(RE)                                                
         B     CHKEXP55                                                         
*                                                                               
CHKEXP60 CLC   TEXP,1(RE)          MUST EQUAL EXPRESSION                        
         B     EXIT                                                             
         EJECT                                                                  
         USING EXPD,R2                                                          
CHKTBL   NTR1                                                                   
CHKTBL5  CLI   EXPC,C'C'          CANADIAN EXPRESSION?                          
         BNE   CHKTBL8                                                          
         CLI   QSYS,C'P'                                                        
         BE    CHKTBL8            DON'T CHECK CANADIAN FOR PRINT                
         CLI   AGYPRF7,C'C'       YES -MUST BE CANADIAN AGENCY                  
         BNE   CHKTBL10                                                         
CHKTBL8  CLI   EXPSYS,C' '        ALL SYSTEMS?                                  
         BE    *+14                                                             
         CLC   EXPSYS,QSYS        NO - SYSTEMS MUST MATCH                       
         BNE   CHKTBL10                                                         
         CLC   TEXP,EXPEXP        MATCH ON EXPRESSION?                          
         BE    YES                YES - GET OUT                                 
*                                                                               
CHKTBL10 LA    R2,L'EXPTBL(R2)                                                  
         CLI   0(R2),X'FF'        TEST END OF TABLE                             
         BNE   CHKTBL5                                                          
         B     NO                 END OF TABLE                                  
         DROP  R2                                                               
         EJECT                                                                  
*=========================================================*                     
* CHKCST -CHECKS IF ROW CHANGING IS A COSTING RELATED ROW *                     
*=========================================================*                     
*                                                                               
CHKCST   NTR1                                                                   
         ZIC   R0,PMAXNUM                                                       
         L     R4,APTABLE                                                       
         LA    R2,POSL1DH                                                       
         USING DISD,R2                                                          
*                                                                               
CHKCST5  LA    RE,CSTTBL           TABLE OF COST RELATED POSTINGS               
CHKCST8  CLI   0(RE),X'FF'                                                      
         BNE   CHKCST20                                                         
         CLC   0(1,R4),0(RE)       IF MATCH ON POSTING                          
         BE    CHKCST10                                                         
         LA    RE,L'CSTTBL(RE)     BUMP TO NEXT IN TABLE                        
         B     CHKCST8                                                          
*                                                                               
CHKCST10 CLI   DACCH+5,0           IF ACCOUNT INPUT                             
         BNE   CHKCST30            GIVE ACCOUNT ERROR                           
         CLI   DAMTH+5,0           IF AMOUNT INPUT                              
         BNE   CHKCST31            GIVE AMOUNT ERROR                            
         CLI   DMEMOH+5,0          IF MEMO INPUT                                
         BNE   CHKCST32            GIVE MEMO ERROR                              
*                                                                               
CHKCST20 LA    R2,DISDL(R2)                                                     
         LA    R4,APTABLN(R4)                                                   
         BCT   R0,CHKCST5                                                       
         B     YES                                                              
*                                                                               
CHKCST30 LA    R1,DACCH            ACCOUNT ERRO                                 
         B     CHKCST35                                                         
CHKCST31 LA    R1,DAMTH            AMOUNT ERROR                                 
         B     CHKCST35                                                         
CHKCST32 LA    R1,DMEMOH           MEMO ERROR                                   
CHKCST35 MVC   FVMSGNO,=AL2(FVINOCST)                                           
         ST    R1,APCURSOR                                                      
         B     NO                                                               
         DROP  R2                                                               
         SPACE 2                                                                
CSTTBL   DS    0XL1                                                             
         DC    AL1(MBTTCOS)        COSTING ROW                                  
         DC    AL1(MBTTBIL)        BILLING ROW                                  
         DC    AL1(MBTTREV)        REVENUE ROW                                  
         DC    AL1(MBTTABL)        AOR BILLING ROW                              
         DC    AL1(MBTTARR)        AOR REVENUE ROW                              
         DC    AL1(MBTTSBL)        SELLOFF BILLING ROW                          
         DC    AL1(MBTTSRV)        SELLOFF REVENUE ROW                          
         DC    AL1(MBTTINTC)       INTERNAL COSTING ROW                         
         DC    AL1(MBTTINTB)       INTERNAL BILLING ROW                         
         DC    AL1(MBTTINTR)       INTERNAL REVENUE ROW                         
         DC    X'FF'                                                            
         EJECT                                                                  
*=====================================================*                         
* CHKMAT -CHECK THAT BILLING AMT = INCOME MEMO AND    *                         
*         REVENUE AMOUNT= INCOME AMOUNT               *                         
*=====================================================*                         
*                                                                               
CHKMAT   NTR1                                                                   
         ZIC   R0,PMAXNUM                                                       
         L     R4,APTABLE                                                       
         LA    R2,POSL1DH                                                       
         USING DISD,R2                                                          
CHKMAT5  CLI   0(R4),MBTTINC      IF INCOME ROW                                 
         BNE   CHKMAT10                                                         
         MVC   INCAMT,DAMT        SAVE ITS AMOUNT                               
         MVC   INCMEMO,DMEMO      AND MEMO                                      
         B     CHKMAT90                                                         
*                                                                               
CHKMAT10 CLI   0(R4),MBTTARI      IF AOR INCOME ROW                             
         BNE   CHKMAT20                                                         
         MVC   AINCAMT,DAMT       SAVE ITS AMOUNT                               
         MVC   AINCMEMO,DMEMO     AND MEMO                                      
         B     CHKMAT90                                                         
*                                                                               
CHKMAT20 CLI   0(R4),MBTTSEL      IF SELLOFF INCOME ROW                         
         BNE   CHKMAT25                                                         
         MVC   SINCAMT,DAMT       SAVE ITS AMOUNT                               
         MVC   SINCMEMO,DMEMO     AND MEMO                                      
         B     CHKMAT90                                                         
*                                                                               
CHKMAT25 CLI   0(R4),MBTTINTI     IF INTERNAL INCOME ROW                        
         BNE   CHKMAT30                                                         
         MVC   IINCAMT,DAMT       SAVE ITS AMOUNT                               
         MVC   IINCMEMO,DMEMO     AND MEMO                                      
         B     CHKMAT90                                                         
*                                                                               
CHKMAT30 CLI   0(R4),MBTTBIL      IF BILLING ROW                                
         BNE   CHKMAT40                                                         
         CLC   DAMT,INCMEMO       BILLING AMOUNT = INCOME MEMO                  
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVIBILM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT40 CLI   0(R4),MBTTREV      IF REVENUE ROW                                
         BNE   CHKMAT50                                                         
         CLC   DAMT,INCAMT        REVENUE AMOUNT = INCOME AMOUNT                
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVIREVM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT50 CLI   0(R4),MBTTABL      IF AOR BILLING ROW                            
         BNE   CHKMAT60                                                         
         CLC   DAMT,AINCMEMO      AOR BILLING AMOUNT = AOR INCOME MEMO          
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVIABLM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT60 CLI   0(R4),MBTTARR      IF AOR REVENUE ROW                            
         BNE   CHKMAT70                                                         
         CLC   DAMT,AINCAMT       REVENUE AMOUNT = INCOME AMOUNT                
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVIARVM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT70 CLI   0(R4),MBTTSBL      IF SELLOFF BILLING ROW                        
         BNE   CHKMAT80                                                         
         CLC   DAMT,SINCMEMO      SELF BILL AMOUNT = SELL INCOME MEMO           
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVISBLM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT80 CLI   0(R4),MBTTSRV      IF SELLOFF REVENUE ROW                        
         BNE   CHKMAT82                                                         
         CLC   DAMT,SINCAMT       REVENUE AMOUNT = INCOME AMOUNT                
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVISRVM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT82 CLI   0(R4),MBTTINTB     IF INTERNAL BILLING ROW                       
         BNE   CHKMAT84                                                         
         CLC   DAMT,IINCMEMO      INT BILL AMOUNT = INT INCOME MEMO             
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVIIBLM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT84 CLI   0(R4),MBTTINTR     IF INTERNAL REVENUE ROW                       
         BNE   CHKMAT90                                                         
         CLC   DAMT,IINCAMT       REVENUE AMOUNT = INCOME AMOUNT                
         BE    CHKMAT90                                                         
         MVC   FVMSGNO,=AL2(FVIIRVM)                                            
         B     CHKMATN                                                          
*                                                                               
CHKMAT90 LA    R2,DISDL(R2)                                                     
         LA    R4,APTABLN(R4)                                                   
         BCT   R0,CHKMAT5                                                       
         B     YES                                                              
*                                                                               
CHKMATN  LA    R2,DAMTH                                                         
         STCM  R2,15,APCURSOR                                                   
         B     NO                                                               
         EJECT                                                                  
*==================================================================*            
* CHKROB -CHECKS IF SI ACCOUNT OVERRIDEN THAT BILLING AND REVENUE  *            
*         ACC OVERRIDEN WITH EITHER UNIT= OR ANAL= FROM SI ACCOUNT *            
*==================================================================*            
*                                                                               
CHKROB   NTR1                                                                   
         ZIC   R0,PMAXNUM                                                       
         L     R4,APTABLE                                                       
         LA    R2,POSL1DH                                                       
         USING DISD,R2                                                          
*                                                                               
CHKROB10 CLI   0(R4),MBTTINC      FIND INCOME ACCOUNT                           
         BE    CHKROB15                                                         
         BAS   RE,CHKNXT          PT TO NXT ENTRY IN TABLE                      
         BCT   R0,CHKROB10                                                      
         B     YES                                                              
*                                                                               
CHKROB15 MVC   APWORK,SPACES      ACCOUNT THAT SHOULD BE                        
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES        DID THEY BLANK OUT ACCOUNT?                   
         BE    CHKROB20           YES                                           
         XC    ACCLVL,ACCLVL      LVL NO LONGER APPLIES IF                      
         CLC   DACC,INCACC        THEY CHANGED THE ACCOUNT                      
         BNE   *+10                                                             
         MVC   ACCLVL,INCLVL                                                    
         BAS   RE,GETFILT         GET UNIT OR ANALYSIS= (FROM SI OR MI)         
         BE    CHKROB20                                                         
         MVC   FVMSGNO,=AL2(FVIMISSA)                                           
         B     CHKROBN                                                          
*                                                                               
CHKROB20 CLI   0(R4),MBTTREV      LOOK FOR REVNEUE ACCOUNT                      
         BE    *+12                                                             
         CLI   0(R4),MBTTBIL      LOOK FOR BILLING ACCOUNT                      
         BNE   CHKROB40                                                         
         CLC   APWORK,SPACES                                                    
         BNE   CHKROB30                                                         
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES                                                      
         BE    CHKROB40           BLANKED OUT BOTH                              
         MVC   FVMSGNO,=AL2(FVISIACC)                                           
         MVC   FVXTRA(5),=C'BLANK'                                              
         B     CHKROBN                                                          
*                                                                               
CHKROB30 ZIC   R1,APWORK                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DACC+2(0),APWORK+1  DOES IT MATCH?                               
         BNE   CHKRBERR           PUT OUT ERROR MSG                             
CHKROB40 BAS   RE,CHKNXT          GET NEXT ENTRY IN TABLE                       
         BCT   R0,CHKROB20                                                      
         B     YES                                                              
         EJECT                                                                  
*==================================================================*            
* CHKARB- CHECKS IF SI ACCOUNT OVERRIDEN THAT BILLING AND REVENUE  *            
*         ACC OVERRIDEN WITH EITHER UNIT= OR ANAL= FROM SI ACCOUNT *            
*==================================================================*            
*                                                                               
CHKARB   NTR1                                                                   
         ZIC   R0,PMAXNUM                                                       
         L     R4,APTABLE                                                       
         LA    R2,POSL1DH                                                       
         USING DISD,R2                                                          
*                                                                               
CHKARB10 CLI   0(R4),MBTTARI      FIND AOR INC ACCOUNT                          
         BE    CHKARB15                                                         
         BAS   RE,CHKNXT          PT TO NXT ENTRY IN TABLE                      
         BCT   R0,CHKARB10                                                      
         B     YES                                                              
*                                                                               
CHKARB15 MVC   APWORK,SPACES      ACCOUNT THAT SHOULD BE                        
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES        DID THEY BLANK OUT ACCOUNT?                   
         BE    CHKARB20           YES                                           
         XC    ACCLVL,ACCLVL      LVL NO LONGER APPLIES IF                      
         CLC   DACC,AINCACC       THEY CHANGED THE ACCOUNT                      
         BNE   *+10                                                             
         MVC   ACCLVL,AINCLVL                                                   
         BAS   RE,GETFILT         GET UNIT OR ANALYSIS= (FROM SI OR MI)         
         BE    CHKARB20                                                         
         MVC   FVMSGNO,=AL2(FVIMISSA)                                           
         B     CHKROBN                                                          
*                                                                               
CHKARB20 CLI   0(R4),MBTTARR      LOOK FOR AOR REVNEUE ACCOUNT                  
         BE    *+12                                                             
         CLI   0(R4),MBTTABL      LOOK FOR AOR BILLING ACCOUNT                  
         BNE   CHKARB40                                                         
         CLC   APWORK,SPACES                                                    
         BNE   CHKARB30                                                         
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES                                                      
         BE    CHKARB40           BLANKED OUT BOTH                              
         MVC   FVMSGNO,=AL2(FVISIACC)                                           
         MVC   FVXTRA(5),=C'BLANK'                                              
         B     CHKROBN                                                          
*                                                                               
CHKARB30 ZIC   R1,APWORK                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DACC+2(0),APWORK+1  DOES IT MATCH?                               
         BNE   CHKRBERR           PUT OUT ERROR MSG                             
CHKARB40 BAS   RE,CHKNXT          GET NEXT ENTRY IN TABLE                       
         BCT   R0,CHKARB20                                                      
         B     YES                                                              
         EJECT                                                                  
*==================================================================*            
* CHKSRB -CHECKS IF SI ACCOUNT OVERRIDEN THAT BILLING AND REVENUE  *            
*         ACC OVERRIDEN WITH EITHER UNIT= OR ANAL= FROM SI ACCOUNT *            
*==================================================================*            
*                                                                               
CHKSRB   NTR1                                                                   
         ZIC   R0,PMAXNUM                                                       
         L     R4,APTABLE                                                       
         LA    R2,POSL1DH                                                       
         USING DISD,R2                                                          
*                                                                               
CHKSRB10 CLI   0(R4),MBTTSEL      FIND AOR INC ACCOUNT                          
         BE    CHKSRB15                                                         
         BAS   RE,CHKNXT          PT TO NXT ENTRY IN TABLE                      
         BCT   R0,CHKSRB10                                                      
         B     YES                                                              
*                                                                               
CHKSRB15 MVC   APWORK,SPACES      ACCOUNT THAT SHOULD BE                        
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES        DID THEY BLANK OUT ACCOUNT?                   
         BE    CHKSRB20           YES                                           
         XC    ACCLVL,ACCLVL      LVL NO LONGER APPLIES IF                      
         CLC   DACC,SINCACC       THEY CHANGED THE ACCOUNT                      
         BNE   *+10                                                             
         MVC   ACCLVL,SINCLVL                                                   
         BAS   RE,GETFILT         GET UNIT OR ANALYSIS= (FROM SI OR MI)         
         BE    CHKSRB20                                                         
         MVC   FVMSGNO,=AL2(FVIMISSA)                                           
         B     CHKROBN                                                          
*                                                                               
CHKSRB20 CLI   0(R4),MBTTSRV      LOOK FOR SELL REVNEUE ACCOUNT                 
         BE    *+12                                                             
         CLI   0(R4),MBTTSBL      LOOK FOR SELL BILLING ACCOUNT                 
         BNE   CHKSRB40                                                         
         CLC   APWORK,SPACES                                                    
         BNE   CHKSRB30                                                         
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES                                                      
         BE    CHKSRB40           BLANKED OUT BOTH                              
         MVC   FVMSGNO,=AL2(FVISIACC)                                           
         MVC   FVXTRA(5),=C'BLANK'                                              
         B     CHKROBN                                                          
*                                                                               
CHKSRB30 ZIC   R1,APWORK                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DACC+2(0),APWORK+1  DOES IT MATCH?                               
         BNE   CHKRBERR           PUT OUT ERROR MSG                             
CHKSRB40 BAS   RE,CHKNXT          GET NEXT ENTRY IN TABLE                       
         BCT   R0,CHKSRB20                                                      
         B     YES                                                              
         EJECT                                                                  
*==================================================================*            
* CHKIRB -CHECKS IF SI ACCOUNT OVERRIDEN THAT BILLING AND REVENUE  *            
*         ACC OVERRIDEN WITH EITHER UNIT= OR ANAL= FROM SI ACCOUNT *            
*==================================================================*            
*                                                                               
CHKIRB   NTR1                                                                   
         ZIC   R0,PMAXNUM                                                       
         L     R4,APTABLE                                                       
         LA    R2,POSL1DH                                                       
         USING DISD,R2                                                          
*                                                                               
CHKIRB10 CLI   0(R4),MBTTINTI     FIND INTERNAL INCOME ACCOUNT                  
         BE    CHKIRB15                                                         
         BAS   RE,CHKNXT          PT TO NXT ENTRY IN TABLE                      
         BCT   R0,CHKIRB10                                                      
         B     YES                                                              
*                                                                               
CHKIRB15 MVC   APWORK,SPACES      ACCOUNT THAT SHOULD BE                        
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES        DID THEY BLANK OUT ACCOUNT?                   
         BE    CHKIRB20           YES                                           
         CLC   =C'NONE',DACC      IF NONE SPECIFIED                             
         BE    YES                JUST GET OUT                                  
         XC    ACCLVL,ACCLVL      LVL NO LONGER APPLIES IF                      
         CLC   DACC,IINCACC       THEY CHANGED THE ACCOUNT                      
         BNE   *+10                                                             
         MVC   ACCLVL,IINCLVL                                                   
         BAS   RE,GETFILT         GET UNIT OR ANALYSIS= (FROM SI OR MI)         
         BE    CHKIRB20                                                         
         MVC   FVMSGNO,=AL2(FVIMISSA)                                           
         B     CHKROBN                                                          
*                                                                               
CHKIRB20 CLI   0(R4),MBTTINTR     LOOK FOR INTERNAL REVENUE ACCOUNT             
         BE    *+12                                                             
         CLI   0(R4),MBTTINTB     LOOK FOR INTERNAL BILLING ACCOUNT             
         BNE   CHKIRB40                                                         
         CLC   APWORK,SPACES                                                    
         BNE   CHKIRB30                                                         
         OC    DACC,SPACES                                                      
         CLC   DACC,SPACES                                                      
         BE    CHKIRB40           BLANKED OUT BOTH                              
         MVC   FVMSGNO,=AL2(FVISIACC)                                           
         MVC   FVXTRA(5),=C'BLANK'                                              
         B     CHKROBN                                                          
*                                                                               
CHKIRB30 ZIC   R1,APWORK                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DACC+2(0),APWORK+1  DOES IT MATCH?                               
         BNE   CHKRBERR           PUT OUT ERROR MSG                             
CHKIRB40 BAS   RE,CHKNXT          GET NEXT ENTRY IN TABLE                       
         BCT   R0,CHKIRB20                                                      
         B     YES                                                              
         EJECT                                                                  
CHKROBN  LA    R2,DACCH           PT CURSOR TO ACCOUNT IN QUESTION              
         STCM  R2,15,APCURSOR                                                   
         B     NO                                                               
         SPACE                                                                  
CHKNXT   LA    R4,APTABLN(R4)     SKIP TO NEXT TABLE ENTRY AND                  
         LA    R2,DISDL(R2)       DISPLAY LINE                                  
         BR    RE                                                               
*                                                                               
*                           ACCOUNT DOESN'T MATCH IT'S SI ACCOUNT               
CHKRBERR MVC   FVMSGNO,=AL2(FVISIACC)                                           
         MVC   FVXTRA(2),=C'11'                                                 
         CLI   0(R4),MBTTBIL                                                    
         BE    CHKRBER5                                                         
         CLI   0(R4),MBTTABL                                                    
         BE    CHKRBER5                                                         
         CLI   0(R4),MBTTSBL                                                    
         BE    CHKRBER5                                                         
         CLI   0(R4),MBTTINTB                                                   
         BE    CHKRBER5                                                         
         MVC   FVXTRA(2),=C'12'                                                 
CHKRBER5 ZIC   R1,APWORK          LENGTH                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA+2(0),APWORK+1                                             
         B     CHKROBN                                                          
         EJECT                                                                  
*==================================================================*            
* GETFILT  - IF INCOME ACCOUUNT IS MI DFLT ACC LOOKS UP 12 FROM MI *            
* OTHERWISE, LOOKS UP 12 CHAR FROM UNIT1= FROM SPECIAL POSTING ELE *            
*            IF NOT FOUND - USES ANALYSIS = AS DEFAULT             *            
*==================================================================*            
*                                                                               
GETFILT  NTR1                                                                   
         CLC   ACCLVL(2),=C'MI'   MI LVL                                        
         BNE   *+12                                                             
         BAS   RE,RDMI            READ MI ACCOUNT FOR ANALYSIS=                 
         B     GETFTX                                                           
*                                                                               
         LA    R4,IOKEY                                                         
         USING ACTKEY,R4                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY2   OTHER COMPANY CODE                            
         MVC   ACTKUNT(L'DACC),DACC                                             
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BNE   NO                                                               
         L     R4,AIOAREA3                                                      
         AH    R4,DATADISP                                                      
         USING SPAELD,R4                                                        
GETFT3   CLI   0(R4),0            END OF RECORD                                 
         BE    GETFT10                                                          
         CLI   0(R4),SPAELQ       SPECIAL POSTING A/C ELEMENT                   
         BE    GETFT5                                                           
         SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETFT3                                                           
GETFT5   CLI   SPATYPE,SPATANAL   ANALYSIS ACCOUNT?                             
         BNE   GETFT10                                                          
         LA    R1,L'SPAAANAL                                                    
         STC   R1,APWORK                                                        
         MVC   APWORK+1(L'SPAAANAL),SPAAANAL                                    
         B     YES                                                              
         DROP  R4                                                               
*                                                                               
*                                                                               
GETFT10  L     R4,AIOAREA3                                                      
         AH    R4,DATADISP                                                      
         USING RSTELD,R4                                                        
GETFT13  CLI   0(R4),0            END OF RECORD                                 
         BE    YES                                                              
         CLI   0(R4),RSTELQ       RECORD STATUS ELE?                            
         BE    GETFT15                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     GETFT13                                                          
GETFT15  CLI   RSTCOSTG,0                                                       
         BE    YES                                                              
         LA    R1,L'RSTCOSTG                                                    
         STC   R1,APWORK                                                        
         MVC   APWORK+1(1),RSTCOSTG                                             
GETFTX   CLC   APWORK+1(12),SPACES                                              
         BNH   NO                                                               
         B     YES                ANAL =/UNIT= FOUND                            
         DROP  R4                                                               
         EJECT                                                                  
*=================================================================*             
* RDMI - READ MI RECORD FOR 12 CHARS FOR BILLING AND REVENUE ACCS *             
*            CHANGE                                               *             
*=================================================================*             
*                                                                               
RDMI     NTR1                                                                   
         LA    R4,IOKEY                                                         
         USING MINRECD,R4                                                       
         MVC   MINKEY,SPACES                                                    
         MVI   MINKTYP,MINKTYPQ    X'08'                                        
         MVC   MINKCPY,COMPANY2    OTHER COMPANY CODE                           
         MVC   MINKMED(1),QSYS     SYSTEM                                       
         MVC   MINKMED+1(1),QMED   MEDIA                                        
         GOTO1 AMIOACC,APPARM,IOACCFIL+IORD+IO3,=C'SE2'                         
         CLI   MYIOERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         L     R4,AIOAREA3                                                      
         AH    R4,DATADISP                                                      
         USING MDIELD,R4                                                        
         SR    R0,R0                                                            
RDMI5    CLI   0(R4),0            END OF RECORD?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),MDIELQ       X'19'                                         
         BE    RDMI10                                                           
         ICM   R0,1,1(R4)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
         B     RDMI5                                                            
*                                                                               
RDMI10   LA    R1,L'MDICOST                                                     
         STC   R1,APWORK                                                        
         MVC   APWORK+1(L'MDICOST),MDICOST                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*===================================*                                           
* DISCHANG - REDISPLAY RECORD AFTER *                                           
*            CHANGE                 *                                           
*===================================*                                           
*                                                                               
DISCHANG NTR1                                                                   
         LA    R3,PSTBLK          CONTROL BLOCK FOR ACPOSTER                    
         USING ACPOSTD,R3                                                       
         BAS   RE,SETPST                                                        
         GOTO1 ACPOSTER,APPARM,(R3)                                             
         BAS   RE,DISPTBL         TABLE GENERATED - NOW DISPLAY IT              
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*=====================================================*                         
* CHKPFKS  - CHECK PFKEY HIT BEFORE EXITING TO SCREEN*                          
*=====================================================*                         
*                                                                               
CHKPFKS  NTR1                                                                   
*                      PFK8 & PFK11 -SWAP W/NO INTENTION OF RETURNING           
         CLI   APPFKEY,PFK08      BACK UP ONE RECORD?                           
         BNE   CHKPFK10                                                         
         ZIC   R1,INREC           CURRENT RECORD NUMBER                         
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,RECSPROF        LAST RECORD #                                 
         STC   R1,APPARM          RECORD TO SWAP TOO                            
         MVI   APPARM+1,ACTMAI    ACTION TO SWAP TOO                            
         MVI   APMODE,APMSWP      SWAP                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CHKPFKX                                                          
*                                                                               
CHKPFK10 CLI   APPFKEY,PFK11      FORWARD ONE RECORD?                           
         BNE   CHKPFK20                                                         
         ZIC   R1,INREC           CURRENT RECORD NUMBER                         
         LA    RE,RECSPROF        MAX RECORD NUMBER                             
         CR    R1,RE                                                            
         BNE   *+8                                                              
         LA    R1,0               START AT ONE AGAIN                            
         LA    R1,1(R1)           BUMP TO NEXT RECORD NUMBER                    
         STC   R1,APPARM          RECORD TO SWAP TOO                            
         MVI   APPARM+1,ACTMAI    ACTION TO SWAP TOO                            
         MVI   APMODE,APMSWP      SWAP                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     CHKPFKX                                                          
*                                                                               
CHKPFK20 CLI   APPFKEY,PFK10      TRACE PRESSED?                                
         BNE   CHKPFKX            NO                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   APMODE,APMSWP      YES - INDICATE SWAP AND SAVE SCREEN           
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(2,0),TWAD                          
         BAS   RE,SETTBL          SET TRACE TABLE                               
         LA    R2,TRACTBL                                                       
         CLC   0(L'TRACTBL,R2),ACCURD                                           
         BH    VALRTERR           CAN'T BE BEFORE FIRST POSTING                 
         LA    R1,1               FIRST POSTING                                 
         SR    RF,RF                                                            
CHKPFK25 ICM   RF,3,0(R2)                                                       
         LA    RE,POSL2DH-POSL1DH                                               
         AR    RF,RE              RF = DISPLACEMENT TILL END OF CURRENT         
         CLM   RF,3,ACCURD        IF CURSOR BEFORE END OF LINE                  
         BNL   CHKPFK30           TRACE THAT LINE                               
         LA    R2,L'TRACTBL(R2)                                                 
         LA    R1,1(R1)                                                         
         ZIC   R0,PMAXNUM                                                       
         CR    R1,R0                                                            
         BH    VALRTERR           CAN'T BE GREATER THAN MAX POSTS               
         B     CHKPFK25                                                         
*                                                                               
CHKPFK30 STC   R1,PROWNUM         POSTING NUMBER (PASS TO TRACE PGM)            
         OI    TWAFLAG,TWAFMAI    INDICATE SWAPPED FROM MAINT SCREEN            
         MVC   APPARM(1),INREC    KEEP RECORD TYPE THE SAME                     
         MVI   APPARM+1,ACTDIS    ACTION CODE TO SWAP TO                        
*                                                                               
CHKPFKX  B     EXIT                                                             
         EJECT                                                                  
*=====================================*                                         
* DISREC - DISPLAYS RECORD EITHER ON  *                                         
*          A MAINT ACTION OR A SELECT *                                         
*          ACTION                     *                                         
*=====================================*                                         
*                                                                               
DISREC   BAS   RE,DISOFFG         DISPLAY OFFICE/AND OFFICE GROUP               
         BAS   RE,DISPTBL         TABLE GENERATED - NOW DISPLAY IT              
DISRX    B     EXIT                                                             
         SPACE                                                                  
*=====================================*                                         
* DISOFFG - DISPLAYS OFFICE AND OFFICE*                                         
*           GROUP IF POSSIBLE         *                                         
*=====================================*                                         
*                                                                               
DISOFFG  NTR1                                                                   
         LA    R3,POSCOD                                                        
         XC    POSCOD,POSCOD                                                    
         OC    QCLT,QCLT          IF NO CLIENT                                  
         BNZ   DISOF10                                                          
         OC    QOFF,QOFF          AND NO OFFICE                                 
         BZ    DISOFX             DON'T DISPLAY ANYTHING                        
*                                                                               
DISOF10  MVC   APHALF(1),SVCOFF                                                 
         OC    QCLT,QCLT                                                        
         BNZ   *+10                                                             
         MVC   APHALF(1),QOFF     SET OFFICE CODE                               
         CLI   APHALF,0                                                         
         BE    DISOFX             NO OFFICE CODE                                
*                                                                               
         MVI   0(R3),C'('                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   APBYTE,SVPROF+1    OFFICE GROUP PROFILES                         
         CLI   QSYS,C'P'                                                        
         BE    *+10                                                             
         MVC   APBYTE,SVPROF                                                    
*                                                                               
         CLI   APBYTE,C'Y'        USE OFFICE GROUPS?                            
         BNE   DISOF20            NO - JUST DISPLAY OFFICE                      
         OC    SVOFFG,SVOFFG      YES - BUT IS THERE ANY?                       
         BZ    DISOF20            NO -SKIP                                      
         MVC   0(4,R3),=C'OFG='                                                 
         LA    R3,4(R3)                                                         
*                                                                               
         XC    OFCBLK,OFCBLK      SET UP PARAMATER BLOCK                        
         LA    R4,OFCBLK                                                        
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'N'        START WITH NET                                
         CLI   QSYS,C'S'          SPOT SYSTEM?                                  
         BNE   *+8                                                              
         MVI   OFCSYS,C'S'                                                      
         CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BE    *+8                                                              
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH        USE ALPHA AGENCY OF OTHER MEDIA FILE          
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH       (FOR SPLIT FILES)                             
*                                                                               
         MVC   OFCMOL,SVOFFG+1    PASS INTERNAL OFFICE LIST NUMBER              
         OI    OFCINDS,OFCIMOLC   CONVERT TO DISPLAYABLE 2 CHARACTERS           
         GOTO1 VOFFICER,APPARM,(C'2',OFCBLK),(0,ACOM)                           
         MVC   0(L'OFCMOL2,R3),OFCMOL2                                          
         LA    R3,L'OFCMOL2(R3)                                                 
         MVI   0(R3),C')'                                                       
         CLI   QOFFIND,C'G'       IF OFFICE GROUP IN KEY                        
         BE    DISOF30            CAN'T POSSIBLY KNOW OFFICE CODE               
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
DISOF20  XC    OFCBLK,OFCBLK      SET UP PARAMATER BLOCK                        
         LA    R4,OFCBLK                                                        
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'N'        START WITH NET                                
         CLI   QSYS,C'S'          SPOT SYSTEM?                                  
         BNE   *+8                                                              
         MVI   OFCSYS,C'S'                                                      
         CLI   QSYS,C'P'          PRINT SYSTEM?                                 
         BNE   *+8                                                              
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,TWAAGY                                                    
         OC    QALPH,QALPH        USE ALPHA AGENCY OF OTHER MEDIA FILE          
         BZ    *+10                                                             
         MVC   OFCAGY,QALPH       (FOR SPLIT FILES)                             
*                                                                               
         MVC   OFCOFC,APHALF      PASS INTERNAL CLIENT OFFICE                   
         GOTO1 VOFFICER,APPARM,(C'2',OFCBLK),(0,ACOM)                           
         TM    OFCINDS,OFCIOINV   IS OFFICE CODE INVALID?                       
         BO    DISOF30                                                          
         MVC   0(4,R3),=C'OFC='                                                 
         LA    R3,4(R3)                                                         
         MVC   0(L'OFCOFC2,R3),OFCOFC2  MOVE IN 2 BYTE OFFICE                   
         LA    R3,2(R3)                                                         
DISOF30  MVI   0(R3),C')'                                                       
DISOFX   OI    POSCODH+6,X'80'                                                  
         B     EXIT                                                             
         EJECT                                                                  
*============================================*                                  
* DISPTBL- LOOPS THROUGH TABLE GENERATED BY  *                                  
*          ACPOSTER AND MOVES INFO TO SCREEN *                                  
*          ACTION                            *                                  
*============================================*                                  
*                                                                               
DISPTBL  NTR1                                                                   
         ZIC   R0,PMAXNUM         MAX NUMBER OF POSTS                           
         USING ACPRTND,R3                                                       
         L     R4,APTABLE                                                       
         LA    R2,POSL1DH         PT TO FIRST LINE ON SCREEN                    
         USING DISD,R2                                                          
*                                                                               
DISR30   MVC   APBYTE,0(R4)                                                     
         BAS   RE,GETROW          PT R3 TO CORRECT ROW                          
         XC    DDOC,DDOC                                                        
         TM    ACPSTAT,ACPCR      MOVE IN CREDIT OR DEBIT                       
         BNO   *+10                                                             
         MVC   DDOC,=C'CR'                                                      
         TM    ACPSTAT,ACPDEB                                                   
         BNO   *+10                                                             
         MVC   DDOC,=C'DR'                                                      
         OI    DDOCH+6,X'80'                                                    
*                                                                               
         MVC   DACC,ACPACC        MOVE ACCOUNT TO SCREEN                        
         OI    DACCH+6,X'80'                                                    
         MVC   DACCLVL,ACPLVL     MOVE ACCOUNT LEVEL TO SCREEN                  
         OI    DACCLVLH+6,X'80'                                                 
         MVC   DAMT,ACPAMT        MOVE AMOUNT TO SCREEN                         
         OI    DAMTH+6,X'80'                                                    
         MVC   DMEMO,ACPMEMO      MOVE MEMO TO SCREEN                           
         OI    DMEMOH+6,X'80'                                                   
         MVC   DLVL,ACPLVL2       MOVE LVL TO SCREEN                            
         OI    DLVLH+6,X'80'                                                    
         MVC   DLSTACT,ACPID      MOVE INITIALS TO SCREEN                       
         MVI   DLSTACT+3,C' '                                                   
         GOTO1 VDATCON,APPARM,(3,ACPCHDT),(8,DLSTACT+4)                         
         OI    DLSTACTH+6,X'80'                                                 
*                                                                               
         LA    R2,DISDL(R2)       PT TO NEXT SCREEN LINE                        
         LA    R4,APTABLN(R4)     NEXT RECORD ROW TABLE ENTRY                   
         BCT   R0,DISR30                                                        
         LA    R1,POSL1ACH        PT TO FIRST LINE ON SCREEN                    
         STCM  R1,15,APCURSOR     SET CURSOR THERE                              
DISPTBLX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*=====================================*                                         
* GETROW - PTS R3 TO CORRECT ROW      *                                         
*          IN POSTINGS RETURNED       *                                         
*=====================================*                                         
*                                                                               
GETROW   NTR1                                                                   
         ZIC   R1,APBYTE          ROW NUMBER                                    
         BCTR  R1,0                                                             
         SR    RE,RE                                                            
         LA    RF,ACPRTNL                                                       
         MR    RE,R1                                                            
         L     R3,APSTTBL                                                       
         AR    R3,RF                                                            
         XIT1  REGS=(R3)                                                        
                                                                                
***********************************************************************         
* DISKEY - DISPLAY RECORD SELECTED                                              
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         USING MPRRECD,R2                                                       
         MVC   POSSYS(L'MPRKSYS),MPRKSYS   DISPLAY SYSTEM                       
         MVC   POSSYS+1(L'MPRKALPH),MPRKALPH                                    
         OI    POSSYSH+6,X'80'                                                  
         MVC   POSMED,MPRKMED   DISPLAY MEDIA                                   
         OI    POSMEDH+6,X'80'                                                  
         GOTO1 ADISOFF,APPARM,MPRKOFC,POSOFF                                    
         BNE   *+8                                                              
         OI    POSOFFH+6,X'80'                                                  
         MVC   POSCLT,MPRKCLI   DISPLAY CLIENT                                  
         OI    POSCLTH+6,X'80'                                                  
         MVC   POSPRD,MPRKPRD   DISPLAY PRODUCT                                 
         OI    POSPRDH+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXPRESSION TABLE - NOTE ANY CHANGES HERE MUST CORRESPOND TO                   
*                    CHANGES IN ACPOSTER'S TABLE                                
***********************************************************************         
EXPTBL   DS    0CL10                                                            
         DC    C' ',C' ',CL8'B'        ALL,ALL,ALL,BILL AMOUNT                  
         DC    C'C',C' ',CL8'B+T'      CAN,ALL,ALL,BILL AMT+GST+PST             
         DC    C' ',C'P',CL8'B+C'      ALL,PRT,ALL,BILL AMT+CD                  
         DC    C'C',C'P',CL8'B+C+T'    CAN,PRT,ALL,BILL+CD+GST+PST              
*                                                                               
         DC    C' ',C' ',CL8'NET'      ALL,ALL,ALL,NET AMT                      
         DC    C'C',C' ',CL8'T'        ALL,ALL,ALL,GST OR PST AMT               
         DC    C'C',C' ',CL8'IT'       ALL,ALL,ALL,IN GST OR PST AMT            
         DC    C' ',C' ',CL8'Z'        ALL,ALL,ALL,ZERO                         
         DC    C' ',C'P',CL8'C'        ALL,ALL,ALL,CD AMOUNT                    
         DC    C' ',C' ',CL8'INC'      ALL,ALL,ALL,(GRS-NET)                    
         DC    C' ',C' ',CL8'IOR'      ALL,ALL,ALL,(% OF GRS)                   
         DC    C' ',C' ',CL8'INC,-IOR' ALL,ALL,ALL,(INC, -% OF GRS)             
         DC    C' ',C' ',CL8'G'        ALL,ALL,ALL,GROSS AMOUNT                 
         DC    C'C',C' ',CL8'G+T'      CAN,ALL,ALL,GROSS+GST+PST                
         DC    C' ',C'P',CL8'G-C'      ALL,ALL,ALL,GROSS-CD                     
         DC    C'C',C'P',CL8'G-C+T'    CAN,ALL,ALL,GROSS-CD+GST+PST             
         DC    C'C',C' ',CL8'G-X'      CAN,ALL,ALL,GROSS-TAX                    
         DC    C'C',C' ',CL8'OBASIS'   A,A,A,BACTUAL-TAX                        
         DC    C'C',C' ',CL8'IBASIS'   A,A,A,(FOR GST=OBAS,PST-BILL)            
*                                                                               
         DC    C' ',C' ',CL8'G,Z'     ALL,ALL,ALL,GRS IN,Z OUT                  
         DC    C'C',C' ',CL8'G+T,Z'   CAN,ALL,ALL,GRS+GST+PST IN,Z OUT          
         DC    C' ',C'P',CL8'G-C,Z'   ALL,ALL,ALL,GRS-CD IN,Z OUT               
         DC    C'C',C'P',CL8'G-C+T,Z' CAN,ALL,ALL,GRS-CD+GST+PST,Z OUT          
*                                                                               
         DC    C' ',C' ',CL8'B,Z'     ALL,ALL,ALL,BIL IN,Z OUT                  
         DC    C'C',C' ',CL8'B+T,Z'   CAN,ALL,ALL,BIL+GST+PST IN,Z OUT          
         DC    C' ',C'P',CL8'B+C,Z'   ALL,ALL,ALL,BIL+CD IN, "                  
         DC    C'C',C'P',CL8'B+C+T,Z' CAN,ALL,ALL,BIL+CD+GST+PST,Z OUT          
*                                                                               
         DC    C' ',C'S',CL8'XINC'     ACTUAL-BASIS*% (SPOT)                    
         DC    C' ',C'S',CL8'XINC,-TR' ACTUAL-BASIS*% (SPOT)                    
         DC    C' ',C'S',CL8'TR'       BASIS*% (FROM XPROF) (SPOT)              
*                                                                               
         DC    C' ',C' ',CL8'OINC'     MIDAS INCOME (ALL SYSTEMS)               
         DC    C' ',C' ',CL8'AINC'     AGENCY INCOME (ALL SYSTEMS)              
         DC    C' ',C' ',CL8'NB'       NET BUDGET MEMO (ALL SYSTEMS)            
         DC    C' ',C' ',CL8'TC'       TRADE CREDIT (ALL SYSTEMS)               
         DC    C' ',C' ',CL8'%OINC'    MIDAS INCOME % OF OINC                   
         DC    C' ',C' ',CL8'OINC%'    MIDAS INCOME OINC -%                     
         DC    C' ',C'S',CL8'%XINC-TR' SAME AS %OINC  (SPOT)                    
         DC    C' ',C'S',CL8'XINC-TR%' SAME AS OINC%  (SPOT)                    
         DC    X'FF'                                                            
         SPACE                                                                  
* EXPSPPT - EXTENTION TABLE FOR SPECIAL PRINT PRODUCTION BILLING                
*                                                                               
EXPSPPT  DS    0CL10                                                            
         DC    C' ',C'P',CL8'DIFF'   ALL,PRT,SPOST,OPEN-CONTRACT                
         DC    C' ',C'P',CL8'CON'    ALL,PRT,SPOST,CONTRACT RATE                
         DC    C'C',C'P',CL8'CON+T'  CAN,PRT,SPOST,CONT RT+GST+PST              
         DC    C' ',C'P',CL8'CON+C'  ALL,PRT,SPOST,CONT RT +CD                  
         DC    C'C',C'P',CL8'CON+C+T' CAN,PRT,SPOST,CONT RT+CD+GST+PST          
         DC    X'FF'                                                            
         SPACE                                                                  
* EXPAORT - EXTENTION TABLE FOR AOR                                             
*                                                                               
EXPAORT  DS    0CL10                                                            
         DC    C' ',C' ',CL8'0B'     ALL,ALL,ALL,ZERO BILL AMT                  
         DC    C'C',C' ',CL8'0(B+T)  ' C,A,A, " BILL+GST+PST                    
         DC    C' ',C'P',CL8'0(B+C)'   A,P,A, " BILL+ CD                        
         DC    C'C',C'P',CL8'0(B+C+T)' C,P,A, " BILL+CD+GST+PST                 
*                                                                               
         DC    C' ',C' ',CL8'Z,Z'    ALL,ALL,ALL,ZERO IN, Z OUT                 
         DC    C' ',C' ',CL8'AOR'    ALL,ALL,ALL,AOR                            
         DC    C' ',C' ',CL8'-AOR'   ALL,ALL,ALL,-AOR                           
         DC    C'C',C' ',CL8'AOR+IT' ALL,ALL,ALL,AOR+INPUT GST OR PST           
         DC    C' ',C' ',CL8'0INC'   ALL,ALL,ALL,ZERO INC                       
         DC    C' ',C' ',CL8'-INC'   ALL,ALL,ALL,MINUS INC                      
         DC    C' ',C' ',CL8'AFEE'   ALL,ALL,ALL,AFEE                           
         DC    C' ',C' ',CL8'AFEE,Z' ALL,ALL,ALL,AFEE IN,Z OUT                  
         DC    C' ',C' ',CL8'INC,-AOR' A,A,A,INC IN, MINUS AOR O                
         DC    C' ',C' ',CL8'IN,-I,-A' ALL,ALL,ALL,(INC, -%,-AOR)               
*                                                                               
         DC    C' ',C' ',CL8'0G'      ALL,ALL,ALL,ZERO OUT GROSS                
         DC    C'C',C' ',CL8'0(G+T)'  CAN,ALL,ALL, " GROSS+GST+PST              
         DC    C' ',C'P',CL8'0(G-C)'  ALL,ALL,ALL, " GROSS-CD                   
         DC    C'C',C'P',CL8'0(G-C+T)' CAN,ALL,ALL," GRS-CD+GST+PST             
*                                                                               
         DC    C' ',C' ',CL8'-G'      ALL,ALL,ALL,MINUS OUT GRS                 
         DC    C'C',C' ',CL8'-(G+T)'  CAN,ALL,ALL, " GROSS+GST+PST              
         DC    C' ',C'P',CL8'-(G-C)'  ALL,ALL,ALL, " GROSS-CD                   
         DC    C'C',C'P',CL8'-(G-C+T)' CAN,ALL,ALL," GRS-CD+GST+PST             
*                                                                               
         DC    C' ',C' ',CL8'-B'      ALL,ALL,ALL,MINUS OUT BILL                
         DC    C'C',C' ',CL8'-(B+T)'  CAN,ALL,ALL, " BILL+GST+PST               
         DC    C' ',C'P',CL8'-(B+C)'  ALL,ALL,ALL, " BILL+CD                    
         DC    C'C',C'P',CL8'-(B+C+T)' CAN,ALL,ALL," BIL+CD+GST+PST             
*                                                                               
         DC    C' ',C' ',CL8'B,Z,Z'   ALL,ALL,ALL,BIL IN,Z OUT,Z OUT            
         DC    C'C',C' ',CL8'B+T,Z,Z' CAN,ALL,ALL,B+GST+PST IN,2Z OUT           
         DC    C' ',C'P',CL8'B+C,Z,Z' ALL,ALL,ALL,BIL+CD IN, "                  
         DC    X'FF'                                                            
         SPACE 2                                                                
* VALID EXPRESSIONS BY ROW                                                      
*                                                                               
EXPBYROW DS    0CL9                                                             
         DC    AL1(MBTTGST),CL8'T'                                              
         DC    AL1(MBTTPQO),CL8'T'                                              
         DC    AL1(MBTTNBO),CL8'T'                                              
         DC    AL1(MBTTNSO),CL8'T'                                              
         DC    AL1(MBTTNFO),CL8'T'                                              
         DC    AL1(MBTTBCO),CL8'T'                                              
         DC    AL1(MBTTONO),CL8'T'                                              
         DC    AL1(MBTTPEO),CL8'T'                                              
         DC    AL1(MBTTGSTI),CL8'IT'                                            
         DC    AL1(MBTTPQI),CL8'IT'                                             
         DC    AL1(MBTTNBI),CL8'IT'                                             
         DC    AL1(MBTTNSI),CL8'IT'                                             
         DC    AL1(MBTTNFI),CL8'IT'                                             
         DC    AL1(MBTTBCI),CL8'IT'                                             
         DC    AL1(MBTTONI),CL8'IT'                                             
         DC    AL1(MBTTPEI),CL8'IT'                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*=============*                                                                 
* LITERAL POOL*                                                                 
*=============*                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
         SPACE 2                                                                
       ++INCLUDE ACTRAPRV                                                       
         SPACE 2                                                                
* CTGENFILE                                                                     
* ACTRAWRK                                                                      
         SPACE                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
LOCALD   DSECT                                                                  
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
RECHANG  DS    CL1                                                              
NEWREC   DS    CL1                                                              
NEWELE   DS    CL1                                                              
ELETYPE  DS    XL1                ELEMENT TYPE                                  
BYTE     DS    CL1                                                              
BYTE3    DS    CL1                                                              
LCHDT    DS    XL3                TODAYS BINARY DATE                            
PRVCODE  DS    CL2                PROVINCE CODE                                 
TEXP     DS    CL8                EXPRESSION AMOUNT                             
TEXPTYPE DS    CL1                EXPRESSION TYPE A=AMOUNT/M=MEMO               
ACCLVL   DS    CL(L'ACPLVL)       GENERAL ACCOUNT LEVEL                         
INCACC   DS    CL(L'ACPACC)                                                     
INCLVL   DS    CL(L'ACPLVL)                                                     
INCAMT   DS    CL8                                                              
INCMEMO  DS    CL8                                                              
AINCACC  DS    CL(L'ACPACC)                                                     
AINCLVL  DS    CL(L'ACPLVL)                                                     
AINCAMT  DS    CL8                                                              
AINCMEMO DS    CL8                                                              
SINCACC  DS    CL(L'ACPACC)                                                     
SINCLVL  DS    CL(L'ACPLVL)                                                     
SINCAMT  DS    CL8                                                              
SINCMEMO DS    CL8                                                              
IINCACC  DS    CL(L'ACPACC)                                                     
IINCLVL  DS    CL(L'ACPLVL)                                                     
IINCAMT  DS    CL8                                                              
IINCMEMO DS    CL8                                                              
OLDACC   DS    CL(L'ACPACC)                                                     
OLDAMT   DS    CL(L'MBTAMTX)                                                    
OLDMEMO  DS    CL(L'MBTMEMOX)                                                   
REBX     DS    CL5                                                              
SUSPX    DS    CL5                                                              
TRACTBL  DS    (MAXPNUM)XL2                                                     
SVKEY    DS    CL44                                                             
OFCBLK   DS    XL(OFCLENQ)         BLOCK FOR OFFICER                            
         EJECT                                                                  
*                                                                               
* EXPD - DSECT TO COVER EXPRESSION TABLE                                        
*                                                                               
EXPD     DSECT                                                                  
EXPC     DS    CL1                C=CANADIAN ONLY                               
EXPSYS   DS    CL1                SYSTEM APPLICABLE FOR                         
EXPEXP   DS    CL8                EXPRESSION                                    
*                                                                               
* DISD - DSECT TO COVER DISPLAY SCREEN                                          
*                                                                               
DISD     DSECT                                                                  
DDISH    DS    CL8                FOR HEADER                                    
DDIS     DS    CL16               DISCRIPTION                                   
DDOCH    DS    CL8                FOR HEADER                                    
DDOC     DS    CL2                DEBIT OR CREDIT                               
DACCH    DS    CL8                FOR HEADER                                    
DACC     DS    CL14               ACCOUNT                                       
DACCLVLH DS    CL8                FOR HEADER                                    
DACCLVL  DS    CL3                ACCOUNT LEVEL                                 
DAMTH    DS    CL8                FOR HEADER                                    
DAMT     DS    CL8                AMOUNT                                        
DMEMOH   DS    CL8                FOR HEADER                                    
DMEMO    DS    CL8                MEMO                                          
DLVLH    DS    CL8                FOR HEADER                                    
DLVL     DS    CL3                LEVEL                                         
DLSTACTH DS    CL8                FOR HEADER                                    
DLSTACT  DS    CL13               LAST ACTIVITY                                 
DISDL    EQU   *-DDISH            LENGTH OF ONE DISPLAY LINE                    
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFED                                                       
         SPACE 2                                                                
         ORG                                                                    
         SPACE 2                                                                
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAEED                                                       
         SPACE 2                                                                
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRACBD                                                       
         ORG                                                                    
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRACCD                                                       
         SPACE 2                                                                
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRADBD                                                       
         ORG                                                                    
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRADCD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046ACTRA02   02/15/19'                                      
         END                                                                    
