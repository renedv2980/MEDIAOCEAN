*          DATA SET ACMAC00    AT LEVEL 039 AS OF 08/27/04                      
*PHASE T61100A,*                                                                
         TITLE 'T61100 - MULTIPLE ACCOUNT REC HANDLER ROOT.'                    
T61100   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MACWRKX-MACWRKD,*MAC00**,R9,CLEAR=YES,RR=R8                      
         USING MACWRKD,RC                                                       
*                                                                               
         LA    R1,0(R1)                                                         
         ST    R1,FAPARM           SAVE A(PARAMETER LIST)                       
         MVC   ACCFACS+9(3),9(R1)       A(SYSFACS)                              
         MVC   ATIA+1(3),13(R1)         A(TIA)                                  
         MVC   COMFACS+1(3),17(R1)      A(COMFACS)                              
         MVC   ATWA+1(3),5(R1)          A(TWA)                                  
         ST    R8,WRELOC                                                        
*                                                                               
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         MVC   COMRTNS(COMRTNLQ),0(RF)  A(ROUTINES FROM COMFACS).               
         MVC   AXTRAINF,CXTRAINF                                                
         DROP  RF                                                               
         L     RF,ACCFACS                                                       
         USING ACCFACSD,RF                                                      
         MVC   RECUP,ARECUP             A(ROUTINES UNIQUE TO FACPAK).           
         DROP  RF                                                               
*                                                                               
         USING T611FFD,RA                                                       
         L     RA,ATWA             RA = A(TWA).                                 
         MVC   COMPANY,0(R1)            COMPANY CODE.                           
         XC    GERROR,GERROR       INIT ERROR NUMBER                            
         LA    RE,ADCONTAB         RELOCATE AND SET ADCONS IN W/STORAGE         
         LA    RF,ADCONS                                                        
INT100   CLI   0(RE),X'FF'                                                      
         BE    INT120                                                           
         L     R1,0(RE)                                                         
         A     R1,WRELOC                                                        
         ST    R1,0(RF)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         B     INT100                                                           
*                                                                               
INT120   ST    RD,SAVED            SAVE RD FOR FAST EXIT                        
         ST    RB,MACBASE                                                       
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   UPDADV,SPACES                                                    
         SR    RE,RE                                                            
         LA    R7,MACMSGH          RETRANSMIT HEADER FIELDS                     
         LA    RF,MACHED1H                                                      
         OI    6(R7),X'80'                                                      
         IC    RE,0(R7)                                                         
         BXLE  R7,RE,*-8                                                        
         LA    RF,(ACRECORD-ACKEYD)                                             
         STH   RF,DATADISP                                                      
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A62',0                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OFFAL,0(R1)                                                      
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A84',0                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETOPT,0(R1)                                                     
*                                                                               
         LH    R1,=Y(OFFBLK-MACWRKD)                                            
         LA    R1,MACWRKD(R1)                                                   
         ST    R1,AOFFBLK                                                       
         LH    R1,=Y(CLIREC-MACWRKD)                                            
         LA    R1,MACWRKD(R1)                                                   
         ST    R1,ACLIREC                                                       
         LH    R1,=Y(CMPREC-MACWRKD)                                            
         LA    R1,MACWRKD(R1)                                                   
         ST    R1,ACMPREC                                                       
         LH    R1,=Y(LDGREC-MACWRKD)                                            
         LA    R1,MACWRKD(R1)                                                   
         ST    R1,ALDGREC                                                       
         LH    R1,=Y(PROREC-MACWRKD)                                            
         LA    R1,MACWRKD(R1)                                                   
         ST    R1,APROREC                                                       
         LH    R1,=Y(GOBLK-MACWRKD)                                             
         LA    R1,MACWRKD(R1)                                                   
         ST    R1,AGOBLOCK                                                      
*                                                                               
         B     GETCOMP                                                          
         SPACE 1                                                                
FASTEXT  L     RD,SAVED            IMMEDIATE RETURN TO FACPAK.                  
         B     EXIT                                                             
*                                                                               
OKEXIT   CR    RB,RB                                                            
         B     EXIT                RETURN WITH CC OF EQUAL.                     
*                                                                               
ERREXIT  LTR   RB,RB                                                            
         B     EXIT                RETURN WITH CC OF NOT EQUAL.                 
*                                                                               
ERREXT2  MVI   FERN,X'FE'                                                       
         XMOD1 2                   BOUNCE UP TWO SYSTEM                         
*                                                                               
XITR1    CR    RB,RB                                                            
         XIT1  REGS=(R1)                                                        
*                                                                               
EXIT     DC    0H'0'               SIMPLE RETURN.                               
         XMOD1                                                                  
         EJECT                                                                  
GETCOMP  MVC   KEY,SPACES          SET UP TO READ COMPANY RECORD.               
         MVC   KEY(1),COMPANY                                                   
         MVI   IOMODE,0                                                         
         GOTO1 READ                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R8,IO+ACRECORD-ACKEYD                                            
         USING ACCOMPD,R8                                                       
*                                                                               
GCMP020  CLI   ACMPEL,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ACMPEL,ACMPELQ                                                   
         BE    GCMP040                                                          
         ZIC   R1,ACMPLEN                                                       
         AR    R8,R1                                                            
         B     GCMP020                                                          
*                                                                               
GCMP040  MVC   COMPSTAT,ACMPSTAT                                                
         MVC   COMPSTA2,ACMPSTA2                                                
         MVC   COMPSTA3,ACMPSTA3                                                
         MVC   COMPSTA4,ACMPSTA4                                                
*                                                                               
         USING ACKEYD,R8                                                        
         LA    R8,IO                                                            
*                                                                               
         L     R2,ACMPREC                                                       
         LH    R3,=Y(ACCRECLN)                                                  
         LR    R4,R8                                                            
         SR    R5,R5                                                            
         ICM   R5,3,ACLENGTH                                                    
         MVCL  R2,R4                                                            
         B     INOFF                                                            
         DROP  R8                                                               
         EJECT                                                                  
* INITIALIZE OFFAL BLOCK                                                        
*                                                                               
INOFF    L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         L     R6,ATWA                                                          
         USING TWAD,R6                                                          
         MVC   OFFACOMF,COMFACS                                                 
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTAT                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI                                                  
         OC    OFFASAV(OFFASAVL),SAVEOFFA                                       
         BZ    *+8                 FIRST TIME FOR PROGRAM                       
         MVI   OFFAACT,OFFARES                                                  
         GOTO1 OFFAL                                                            
         BE    *+6                                                              
         DC    H'0'                OFFAL INITIALIZATION ERROR                   
         MVC   SAVEOFFA,OFFASAV    TUCK AWAY OFFAL SAVE AREA                    
         B     VALACT                                                           
         DROP  R1,R6                                                            
         EJECT                                                                  
VALACT   LA    RF,MACACTH          VALIDATE ACTION                              
         ST    RF,FADR                                                          
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         GOTO1 ANY                                                              
         BNE   MISFIELD            ACTION MISSING                               
         CLC   MACACT(2),=C'HE'                                                 
         BE    ACTHELP                                                          
         LA    RE,ACTTAB                                                        
         USING ACTTABD,RE                                                       
*                                                                               
VAC020   CLI   0(RE),X'FF'                                                      
         BE    INVACT                                                           
         EX    R1,ACTCLC           CLC   MACACT(0),ACTNAM                       
         BE    VAC040                                                           
         LA    RE,ACTBLNQ(RE)                                                   
         B     VAC020                                                           
ACTCLC   CLC   MACACT(0),ACTNAM                                                 
*                                                                               
VAC040   CLC   MACKEY(2),=C'SJ'    SJ LEDGER?                                   
         BNE   VAC060              NO, OK                                       
         MVC   MACMSG,SPACES       YES, ERROR THEN                              
         MVC   MACMSG(31),=CL31'FOR SJ - USE PRODUCTION PROGRAM'                
         OI    MACMSGH+6,X'80'                                                  
         OI    MACACTH+6,X'40'     CURSOR TO ACTION FIELD                       
         B     OKEXIT                                                           
*                                                                               
VAC060   MVC   MACOLAY,ACTOLAY     GET OVERLAY NUMBER,                          
         MVC   MACSCRN,ACTSCRN         SCREEN NUMBER,                           
         MVC   MACAUTH,ACTAUTH         AND AUTHORISATION CODE.                  
         MVC   WUNIT,ACTUNIT                                                    
         MVC   WLEDGER,ACTLEDG                                                  
         MVC   WAULLIST,ACTLIST                                                 
         MVC   DMINBTS,ACTINBTS    SAVE INPUT SPECS FOR ACTION.                 
         MVC   DMOUTBTS,ACTOUBTS   SAVE DMGR ERROR SPECS FOR ACTION.            
         MVI   IOMODE,0                                                         
         CLC   ACTION,ACTNAM                                                    
         BE    VAC070                                                           
         MVC   ACTION,ACTNAM                                                    
         BAS   RE,RESTART                                                       
         XC    SUNIT(2),SUNIT      FORCE REVALIDATION OF U/L.                   
         DROP  RE                                                               
*                                  CHECK TERMINAL AUTHORIZATION.                
         USING TWAD,RE                                                          
VAC070   L     RE,ATWA                                                          
         CLI   TWAACCS,C' '                                                     
         BE    VAC100              NO OFFICE/CLIENT RESTRICTION.                
         CLI   TWAACCS,0                                                        
         BE    VAC100                                                           
         CLI   TWAACCS,C'*'                                                     
         BE    VAC100                                                           
         CLI   TWAACCS,C'$'        LIST                                         
         BE    VAC100                                                           
         MVC   WCLIAUTH,TWAACCS    SAVE CLIENT SECURITY FOR APPLICS.            
*                                                                               
VAC100   ZIC   RF,MACAUTH                                                       
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    TWAAUTH,0                                                        
         BNO   INVACT              TERMINAL NOT AUTHORIZED FOR ACTION.          
*                                                                               
VACEND   DS    0H         AKE U/L FOR ACTION OR IF NO I/P ASK USER.             
         EJECT                                                                  
*                                                                               
VALKEY   LA    RF,MACKEYH                                                       
         ST    RF,FADR                                                          
         MVI   ULINPUT,C'N'                                                     
         CLI   MACKEYH+5,0                                                      
         BNE   VKEY020                                                          
VKEY010  CLI   WUNIT,0             IF NO KEY INPUT, ACTION MUST HAVE            
         MVI   FERN,UNTINVAL                                                    
         BE    MACERRS             SUPPLIED UNIT AND LEDGER.                    
         MVI   FERN,LDGINVAL                                                    
         CLI   WLEDGER,0                                                        
         BE    MACERRS                                                          
         B     VUN020                                                           
*                                                                               
VKEY020  CLI   MACKEYH+5,4                                                      
         BNE   *+14                                                             
         CLC   MACKEY(4),=C'HELP'                                               
         BE    KEYHELP                                                          
         CLI   WUNIT,0                                                          
         BNE   VUN020                                                           
         MVC   WUNIT(2),MACKEY             USER IS SPECIFYING                   
         MVI   ULINPUT,C'Y'                UNIT AND LEDGER                      
*                                                                               
VUN020   MVI   FERN,UNTINVAL                                                    
         CLI   WUNIT,C' '                  MAKE SURE USER DIDNT SPECIFY         
         BE    MACERRS                     SPACES FOR EITHER UNIT OR            
         MVI   FERN,LDGINVAL               LEDGER                               
         CLI   WLEDGER,C' '                                                     
         BE    MACERRS                                                          
         CLC   SUNIT,WUNIT                                                      
         BE    VUN040              SAME UNIT, NO REREAD, NO RESTART.            
         BAS   RE,RESTART          NEW UNIT RESTART,                            
         MVI   SLEDGER,0           FORCE REREAD OF LEDGER.                      
         MVC   SUNIT,WUNIT         SAVE NEW UNIT.                               
VUN040   MVC   KEY,SPACES          SET KEY TO READ UNIT.                        
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(1),WUNIT                                                   
         GOTO1 READ                                                             
         BE    *+12                GOOD READ.                                   
         MVI   FERN,UNTINVAL                                                    
         B     MACERRS                                                          
         GOTO1 SECAUTH                                                          
         BNE   UNAUTH              USER IS NOT AUTHORIZED FOR UNIT.             
*                                                                               
VLG030   CLC   SLEDGER,WLEDGER                                                  
         BE    VLG035              SAME LEDGER, NO REREAD, NO RESTART.          
         BAS   RE,RESTART          RESTART.                                     
         MVC   SLEDGER,WLEDGER     SAVE NEW LEDGER.                             
VLG035   MVC   KEY+2(1),WLEDGER    SET KEY TO READ LEDGER.                      
         GOTO1 READ                                                             
         BE    *+12                GOOD READ.                                   
         MVI   FERN,LDGINVAL                                                    
         B     MACERRS                                                          
         GOTO1 SECAUTH                                                          
         BNE   UNAUTH              USER NOT AUTHORIZED FOR LEDGER.              
*                                                                               
VLG040   LA    R8,IO               DERIVE LENGTH OF ACC'T KEY AT                
         AH    R8,DATADISP         EACH LEVEL & DISPLACEMENT OF                 
         USING ACHEIRD,R8          LOW ORDER ACC'T KEY BYTE FOR                 
VLG050   CLI   ACHREL,0            LOWEST LEVEL.                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ACHREL,ACLTELQ                                                   
         BNE   VLG055                                                           
         MVC   OFFPOS,ACLTOFF-ACLEDGD(R8)                                       
         B     VLG058                                                           
VLG055   CLI   ACHREL,ACHRELQ                                                   
         BE    VLG060                                                           
VLG058   ZIC   R1,ACHRLEN                                                       
         AR    R8,R1                                                            
         B     VLG050                                                           
*                                                                               
VLG060   MVI   SKEYEND,0                                                        
         LA    RF,ACHRLEVA         RF = A(1ST LEVEL INFO).                      
         LA    RE,SLVLLENS         RE = A(LIST OF LEVEL LENGTHS).               
         LA    R3,4                                                             
         XC    SLVLLENS,SLVLLENS                                                
VLG070   CLI   0(RF),0                                                          
         BE    VLG080              NO MORE LEVELS IN LEDGER.                    
         ZIC   R1,0(RF)            R1 = L'ACCT CODE AT THIS LEVEL.              
         LR    R0,R1                                                            
         ZIC   R2,SKEYEND          R2 = L'ACCT CODE AT PREVIOUS LEVEL.          
         SR    R0,R2               R0 = L'THIS ACCT LEVEL.                      
         STC   R0,0(RE)            SAVE L'THIS LEVEL.                           
         STC   R1,SKEYEND          SAVE L'ACCT SO FAR                           
         LA    RF,16(RF)           GET A(NEXT LEVELS INFO).                     
         LA    RE,1(RE)            GET A(NEXT LEVELS LNTH LIST ENTRY).          
         BCT   R3,VLG070                                                        
*                                                                               
VLG080   ZIC   R1,SKEYEND                                                       
         LA    R1,2(R1)            R1 = DISP INTO KEY OF LAST ACCT BYTE         
         STC   R1,SKEYEND                                                       
*                                                                               
         ZICM  RF,WAULLIST,3                                                    
         BZ    VLGEND              ALL UNITS/LEDGERS OK.                        
         A     RF,WRELOC           RF = A(UNIT/LEDGER EXCEPTION LIST).          
*                                                                               
VLG090   CLI   0(RF),X'FF'                                                      
         BE    VLG110              THIS U/L NOT ALLOWED.                        
         CLC   WUNIT,0(RF)                                                      
         BNE   VLG094              NO MATCH HERE.                               
         CLI   1(RF),C' '                                                       
         BE    VLGEND              ALL LEDGERS FOR THIS UNIT ARE OK.            
         CLC   WLEDGER,1(RF)                                                    
         BE    VLGEND              THIS LEDGER IS OK.                           
VLG094   LA    RF,2(RF)            OK SO FAR, TRY THE NEXT ONE.                 
         B     VLG090                                                           
*                                                                               
VLG110   MVI   FERN,INVFRACT                                                    
         B     MACERRS                                                          
*                                                                               
VLGEND   DS    0H                                                               
         CLC   WUNIT(2),=C'SJ'                                                  
         BNE   VLGENDX                                                          
         L     R2,ALDGREC                                                       
         USING ACKEYD,R8                                                        
         LA    R8,IO                                                            
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,ACLENGTH                                                    
         LH    R3,=Y(ACCRECLN)                                                  
         LR    R4,R8                                                            
         MVCL  R2,R4                                                            
VLGENDX  DS    0H                                                               
         EJECT                                                                  
*                                                                               
*              VALIDATE FILTERS AND CHECK FOR CONSISTENCY.                      
*                                                                               
VALOPT   LA    RF,MACOPTH                                                       
         ST    RF,FADR                                                          
         MVI   WFILTERC,C' '       SPACE PAD MORE THAN 80 BYTES                 
         MVC   WFILTERC+1(WFILCLNQ-1),WFILTERC                                  
         XC    WFILTERB(WFILBLNQ),WFILTERB                                      
         CLI   MACOPTH+5,0                                                      
         BE    VOPT020             NO FILTER INPUT.                             
         XC    WOPTIONS(WOPTLNQ),WOPTIONS                                       
         MVI   FERN,X'FF'                                                       
         GOTO1 VALGEN,FILTAB                                                    
         BNE   FILHELP                                                          
VOPT020  ZICM  R1,MACKEYH+5        R1 = L'KEY INPUT.                            
         BZ    VALFILTX                                                         
         BCTR  R1,0                                                             
         LA    RE,MACKEY           RE = A(KEY INPUT).                           
         CLI   ULINPUT,C'Y'                                                     
         BNE   *+12                NO U/L SUPPLIED BY USER.                     
         SH    R1,=H'2'            U/L SUPPLIED SO ACCOUNT CODE IS 3            
         LA    RE,2(RE)            BYTES INTO KEY INPUT.                        
         LTR   R1,R1                                                            
         BM    VALFILTX            UNIT/LEDGER ONLY INPUT.                      
         MVC   ACCFILT,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACCFILT(0),0(RE)                                                 
*                                  CONSISTENCY CHECKS HERE.                     
VALFILTX DS    0H                                                               
         CLC   SFILTERS,WFILTERS                                                
         BE    VALOPTX                                                          
         MVC   SFILTERS,WFILTERS   SAVE NEW FILTER VALUES.                      
         BAS   RE,RESTART          CHANGE OF FILTER FORCES RESTART.             
VALOPTX  DS    0H                  ANY CONSISTENCY CHECKS HERE.                 
         EJECT                                                                  
*              LOAD OVERLAYS AND SCREENS AS NECESSARY.                          
*                                                                               
LOAD000  MVC   LASTOLAY,MACOLAY                                                 
         GOTO1 CALLOV,DMCB,(MACOLAY,0),0                                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APPLIC,PARA1                                                     
*                                                                               
LOAD020  CLC   MACSCRN,LASTSCRN                                                 
         BE    LOADEND             SAME SCREEN, NO LOAD.                        
         MVC   LASTSCRN,MACSCRN                                                 
         CLI   MACSCRN,0                                                        
         BE    LOADEND             NO SCREEN REQUIRED.                          
         GOTO1 CALLOV,DMCB,(MACSCRN,MACHED1H),0                                 
         CLI   PARA2,X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,MACHED1H                                                      
LOAD040  OI    6(RE),X'80'    TRANSMIT ALL FIELDS TO END OF TWA.                
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BNE   LOAD040                                                          
*                                                                               
LOADEND  DS    0H                                                               
         EJECT                                                                  
*              SET UP DUMMY START AND END KEYS AND PASS TO                      
*              APPLICATION FOR CUSTOMIZING.                                     
*                                                                               
PROC000  CLI   MODE,0                                                           
         BE    PROC010             JUST STARTING.                               
         CLI   MODE,PROCREC                                                     
         BE    PROC010             APPLIC SAYSPROCESS MORE RECORDS.             
         CLI   MODE,MARKREC                                                     
         BE    MARK000             APPLIC SAYS MARK RECORDS NOW.                
         CLI   MODE,EOF                                                         
         BE    MACEOF                                                           
         DC    H'0'                NO OTHER MODES.                              
*                                                                               
PROC010  MVC   STRTKEY,SPACES                                                   
         MVI   ENDKEY,X'FF'                                                     
         MVC   ENDKEY+1(L'ENDKEY-1),ENDKEY                                      
         NI    DMINBTS,X'7F'       DON'T READ FOR UPDATE.                       
         MVI   MODE,BUILDKEY                                                    
         BAS   RE,GO               GOTO APPLICATION TO SET KEY.                 
         BNE   MACERRS             ERROR FROM APPLICATION.                      
         CLI   PASS,0                                                           
         BNE   PROC020             NOT 1ST PASS, CONTINUE READING.              
*                                                                               
         GOTO1 HIGH                FIRST I/O IS ALWAYS READ-HIGH.               
         BNE   MACERRS             I/O ERROR                                    
         B     PROC040                                                          
*                                                                               
PROC020  TM    IOMODE,X'0F'        SUBSEQUENT I/O COMMANDS ARE                  
         BNZ   *+6                 SPECIFIED BY THE APPLICATION.                
         DC    H'0'                APPLICATION DID NOT DO SO.                   
         IC    RF,IOMODE                                                        
         BCTR  RF,0                                                             
         SLL   RF,28               REMOVE HI ORDER NIBBLE.                      
         SRL   RF,26               RF = INDEX FOR A(I/O ROUTINE).               
         L     RF,AIORTNS(RF)      RF = A(SPECIFIED I/O ROUTINE).               
         BASR  RE,RF               GO FOR IT.                                   
         BE    PROC040                                                          
         MVC   DUB(1),DMCB+8                                                    
         NC    DUB(1),DMOUTBTS                                                  
         BZ    *+6                 USER HANDLES THIS I/O ERROR.                 
         DC    H'0'                USER DOES NOT HANDLE THIS I/O ERROR.         
         MVI   MODE,IOERROR                                                     
         BAS   RE,GO               PASS ERROR TO USER.                          
         BE    PROC020             HE'S HANDLED IT SUCCESSFULLY.                
         DC    H'0'                HE'S NOT HANDLED IT SUCCESSFULLY.            
*                                                                               
PROC040  CLI   KEY,X'FF'                                                        
         BE    PROCEND             LOGICAL END OF FILE.                         
         CLI   KEY,X'FE'                                                        
         BE    PROC060             I/O LIMIT EXCEEDED.                          
         LA    R8,IO               POINT R8 TO PROPER IO AREA.                  
         TM    IOMODE,AREA2                                                     
         BZ    *+8                                                              
         LA    R8,IO2                                                           
         CLC   0(31,R8),STRTKEY                                                 
         BL    PROC020                                                          
         CLC   0(31,R8),ENDKEY                                                  
         BH    PROCEND                                                          
         BAS   RE,GETLVLNO         DERIVE LEVEL OF ACCOUNT.                     
         BAS   RE,FILTER                                                        
         BNE   PROC020             RECORD FAILS FILTER.                         
         GOTO1 SECAUTH                                                          
         BNE   PROC020             TERMINAL NOT AUTHORIZED FOR ACCOUNT.         
*                                                                               
         L     RE,ATWA                                                          
         CLC   TWAACCS,SPACES      TEST ANY OFFICE ACCESS CONTROL               
         BNH   PROC060                                                          
         OC    ADACBAL,ADACBAL     TEST BALANCE ELEM PRESENT                    
         BNZ   *+12                YES                                          
         CLI   ACCLVLNO,ACCLOW     TEST LOWEST LEVEL                            
         BNE   PROC060                                                          
*                                                                               
         MVC   HALF,SPACES         FIND COMPOSITE OFFICE CODE                   
         LA    R1,OFFICES+6        R1=A(OFFICE CODE)                            
         LA    R0,4                R0=LOOP COUNTER                              
*                                                                               
PROC044  CLC   0(2,R1),SPACES      TEST FOR AN OFFICE CODE                      
         BH    PROC045             YES                                          
         SH    R1,=H'2'            BACK UP ONE ENTRY                            
         BCT   R0,PROC044                                                       
         B     PROC046                                                          
*                                                                               
PROC045  MVC   HALF,0(R1)                                                       
*                                                                               
PROC046  L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         ST    R8,OFFAREC                                                       
         MVC   OFFAOPOS,OFFPOS                                                  
         MVC   OFFAOFFC,HALF                                                    
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BE    PROC060                                                          
         B     PROC020             RECORD HAS BEEN REJECTED                     
         DROP  R1                                                               
*                                                                               
PROC060  MVI   MODE,PROCREC                                                     
         BAS   RE,GO               GOTO APPLICATION TO PROCESS RECORD.          
         BNE   MACERRS             ERROR FROM APPLICATION.                      
         CLI   KEY,X'FE'           IO COUNT EXCEEDED                            
         BE    PROC065             YES, MARK THE PARTIAL SCREEN                 
         CLI   IOMODE,FINISHED                                                  
         BNE   PROC020             APPLIC STIL ROLLING.                         
PROC065  MVI   MODE,MARKREC        NEXT TIME IN WE MARK THEM.                   
         B     PROCMSG                                                          
*                                                                               
PROCEND  MVI   MODE,EOF                                                         
         BAS   RE,GO               GOTO APPLICATION FOR END OF READING.         
         BNE   MACERRS             ERROR.                                       
         CLI   LASTLYN,0           IF NOTHING WAS DISPLAYED, END                
         BE    MACEOF2             FUNCTION IMMEDIATELY.                        
*                                                                               
PROCMSG  MVC   MACMSG,SPACES                                                    
         CLI   KEY,X'FE'           IF I/O COUNT IS EXCEEDED AND NOTHING         
         BNE   PMSG020             IS ON SCREEN, PUT OUT A MESSAGE,             
         CLI   LASTLYN,0           SET MODE TO PROCREC, MAKE PASS NON-          
         BE    PROCMSG1            ZERO                                         
         OI    MAFMARKH+6,X'C0'                                                 
         B     PMSG020                                                          
PROCMSG1 MVC   MACMSG(L'MAXMSG),MAXMSG                                          
         MVI   MODE,PROCREC                                                     
         MVI   PASS,1                                                           
         B     PMSG040                                                          
*                                                                               
PMSG020  MVC   MACMSG(39),=C'MARK ACCOUNTS OR ENTER FOR NEXT SCREEN.'           
         CLI   MODE,EOF                                                         
         BNE   *+10                                                             
         MVC   MACMSG+23(15),=C'TO END FUNCTION'                                
PMSG040  OI    MACMSGH+6,X'80'                                                  
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         IC    R1,MACAUTH                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TWAAUTH,0                                                        
         BO    OKEXIT                                                           
         CLI   MODE,PROCREC        FOR EMPTY SCREEN AND I/O COUNT EX-           
         BE    *+16                CEEDED, DON'T PLAY WITH MESSAGE.             
         MVC   MACMSG(22),MACMSG+17                                             
         MVC   MACMSG+22(17),SPACES                                             
*                                                                               
         LA    R7,MACHED2H         START AT THE TOP                             
PMSG060  ZIC   RE,0(R7)                                                         
         AR    R7,RE                                                            
         CLI   0(R7),0                                                          
         BE    PMSG070             E-O-TWA                                      
         TM    1(R7),X'20'                                                      
         BNZ   PMSG060             PERMANENT PROTECT                            
         OI    6(R7),X'A0'         PROTECT AND TRANSMIT.                        
         NI    6(R7),X'BF'         TURN OFF CURSOR.                             
         MVI   8(R7),0             ERASE THE FIELD.                             
         LR    RF,R7               SAVE LAST FIELD ADD.                         
         B     PMSG060                                                          
PMSG070  OI    6(RF),X'40'                                                      
         B     OKEXIT                                                           
         EJECT                                                                  
*                                                                               
*              GO TO APPLICATION WITH MODE OF MARKREC.                          
*                                                                               
         USING TWAD,RE                                                          
MARK000  MVI   MODE,MARKREC                                                     
         L     RE,ATWA                                                          
         IC    R1,MACAUTH                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TWAAUTH,0                                                        
         BNO   PROC010                                                          
         OI    DMINBTS,X'80'       NOW READ FOR UPDATE.                         
         BAS   RE,GO                                                            
         BNE   MACERRS                                                          
         B     PROC010                                                          
         SPACE 1                                                                
*              ENTER APPLICATION TO MARK LAST SCREEN FOR FUNCTION.              
*                                                                               
MACEOF   MVI   MODE,MARKREC                                                     
         L     RE,ATWA                                                          
         IC    R1,MACAUTH                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TWAAUTH,0                                                        
         BNO   MACEOF2                                                          
         BAS   RE,GO                                                            
         BNE   MACERRS                                                          
MACEOF2  BAS   RE,RESTART                                                       
         MVC   MACMSG,SPACES                                                    
         MVC   MACMSG(33),=C'END OF RECORDS FOR THIS FUNCTION.'                 
         OI    MACMSGH+6,X'80'                                                  
         OI    MACACTH+6,X'40'     CURSOR TO ACTION FIELD.                      
         B     OKEXIT                                                           
         DROP  RE                                                               
         SPACE                                                                  
*              DETERMINE ACCOUNT LEVEL.                                         
*                                                                               
         USING ACKEYD,R8                                                        
GETLVLNO NTR1                                                                   
         LA    RE,SLVLLENS+1       POINT TO LVL 2 LENGTH                        
         SR    R1,R1                                                            
GLV010   CLI   0(RE),0                                                          
         BE    GLV015                                                           
         LA    RE,1(RE)            POINT TO L'NEXT LEVEL                        
         LA    R1,1(R1)            BUMP NO. OF LEVELS                           
         B     GLV010                                                           
GLV015   LA    RE,SLVLLENS                                                      
         LA    RF,ACKEYACC+3                                                    
         LR    R0,R1               SO, R0 HAS NO. OF LEVELS-1                   
         SR    R1,R1                                                            
*                                                                               
GLV020   IC    R1,0(RE)            R1 = L'ACCKEY FOR LEVEL                      
         LA    RF,0(R1,RF)         RF = A(1ST BYTE OF NEXT LVL)                 
         CLI   0(RF),C' '                                                       
         BE    GLV040              THIS IS THE LEVEL.                           
         LA    RE,1(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         SR    R0,R0               NO MORE LEVELS, THIS IS LOWEST.              
         B     GLV040                                                           
         BCT   R0,GLV020                                                        
*                                                                               
GLV040   STC   R0,ACCLVLNO         PASS THE LEVEL NO. CODE.                     
*                                                                               
* RE-DO THE ABOVE IN AN UNDERSTANDABLE WAY  -- SETS LEVEL                       
*                                                                               
GLV050   LA    R1,SLVLLENS+L'SLVLLENS-1                                         
         LA    RE,4                RE=LEVEL COUNTER                             
GLV052   CLI   0(R1),0             TEST FOR LEVEL IN USE                        
         BNE   GLV055                                                           
         BCTR  R1,0                NO-BACK UP ONE                               
         BCT   RE,GLV052                                                        
*                                                                               
GLV055   SR    R1,R1                                                            
         SR    R4,R4                                                            
         LA    RF,SLVLLENS                                                      
         LA    R2,1                                                             
*                                                                               
GLV060   IC    R1,0(RF)            GET LENGTH OF A LEVEL                        
         AR    R1,R4               PLUS LENGTH OF PREVIOUS LEVELS               
         LA    R3,ACKEYACC+3(R1)                                                
         CLI   0(R3),C' '          TEST FOR END OF KEY                          
         BE    GLV065                                                           
         LR    R4,R1               SAVE TOTAL LENGTH                            
         LA    RF,1(RF)                                                         
         LA    R2,1(R2)            INCREMENT LEVEL NUMBER                       
         BCT   RE,GLV060                                                        
*                                                                               
GLV065   STC   R2,LEVEL                                                         
*                                                                               
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
         USING ACKEYD,R8                                                        
FILTER   NTR1                                                                   
*              FIRST PASS A(ALL RELEVANT ELEMENTS).                             
*                                                                               
         LA    RE,ELEMTAB                                                       
         USING ELEMTABD,RE                                                      
FIL020   CLI   ELEMCODE,X'FF' CLEAR ALL ELEMENT ADDRESSES.                      
         BE    FIL040                                                           
         ZICM  RF,ELEMADD,3                                                     
         BZ    FIL030                                                           
         LA    RF,MACWRKD(RF)                                                   
         XC    0(4,RF),0(RF)                                                    
FIL030   LA    RE,ELEMTBLQ(RE)                                                  
         B     FIL020                                                           
*                                                                               
FIL040   LA    R6,ACRECORD         NOW POST A(EACH ELEMENT PRESENT).            
FIL050   LA    RE,ELEMTAB                                                       
         CLI   0(R6),0                                                          
         BE    FIL072                                                           
FIL054   CLC   ELEMCODE,0(R6)                                                   
         BNE   FIL070                                                           
         ZICM  RF,ELEMADD,3                                                     
         BZ    FIL070                                                           
         LA    RF,MACWRKD(RF)                                                   
         OC    0(4,RF),0(RF)                                                    
         BNZ   FIL060                                                           
         ST    R6,0(RF)                                                         
FIL060   ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     FIL050                                                           
*                                                                               
FIL070   LA    RE,ELEMTBLQ(RE)                                                  
         CLI   ELEMCODE,X'FF'                                                   
         BE    FIL060                                                           
         B     FIL054                                                           
         DROP  RE                                                               
*                                                                               
FIL072   MVC   HALF,SPACES         INITIALIZE OFFICE CODE                       
         CLI   OFFPOS,1            TEST OFFICE IN KEY LEDGER                    
         BL    FIL073                                                           
         CLI   OFFPOS,12                                                        
         BH    FIL073                                                           
         ZIC   R1,OFFPOS                                                        
         LA    R1,ACKEYACC+2(R1)                                                
         MVC   HALF(1),0(R1)                                                    
         B     FIL076                                                           
*                                                                               
FIL073   TM    OFFPOS,X'F0'        TEST OFFICE IN FILTER LEDGER                 
         BNO   FIL075                                                           
*                                                                               
         ICM   RE,15,ADACSTAT                                                   
         USING ACSTATD,RE                                                       
         LA    R1,ACSTFILT                                                      
         CLI   OFFPOS,C'1'                                                      
         BE    FIL074                                                           
         LA    R1,ACSTFILT+1                                                    
         CLI   OFFPOS,C'2'                                                      
         BE    FIL074                                                           
         LA    R1,ACSTANAL                                                      
         CLI   OFFPOS,C'3'                                                      
         BE    FIL074                                                           
         LA    R1,ACSTSUB                                                       
*                                                                               
FIL074   MVC   HALF(1),0(R1)       EXTRACT OFFICE VALUE                         
         B     FIL076                                                           
         DROP  RE                                                               
*                                                                               
FIL075   CLI   OFFPOS,0                                                         
         BE    FIL076                                                           
         CLC   WUNIT(2),=C'SJ'     TEST FOR PRODUCTION LEDGER                   
         BNE   FIL076                                                           
         ICM   RE,15,ADACPROF                                                   
         BZ    FIL076              NO PROFILE ELEMENT                           
         USING ACPROFD,RE                                                       
         MVC   HALF,ACPROFFC                                                    
         DROP  RE                                                               
*                                                                               
FIL076   ZIC   R1,LEVEL                                                         
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,OFFICES(R1)                                                   
         MVC   0(2,R1),HALF        SLOT OFFICE INTO TABLE                       
*                                                                               
         LA    RE,OFFICES+L'OFFICES POINT PAST TABLE                            
         LA    R1,2(R1)            POINT TO NEXT POSITION                       
         SR    RE,R1                                                            
         BZ    FIL077              NOTHING AFTER THIS LEVEL                     
         BCTR  RE,0                                                             
         EX    RE,*+8              CLEAR REST OF TABLE                          
         B     *+10                                                             
         MVC   0(0,R1),SPACES                                                   
*                                                                               
FIL077   CLC   WUNIT(2),=C'SJ'     TEST FOR PRODUCTION LEDGER                   
         BNE   FIL078                                                           
         L     R2,ACLIREC          SAVE THE CLIENT OR PRODUCT FOR               
         CLI   LEVEL,1             GETOPT                                       
         BE    FIL077A                                                          
*                                                                               
         L     R2,APROREC                                                       
         CLI   LEVEL,2                                                          
         BNE   FIL077B                                                          
*                                                                               
FIL077A  LH    R3,=Y(ACCRECLN)                                                  
         SR    R5,R5                                                            
         ICM   R5,3,ACLENGTH                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    R4,R8                                                            
         MVCL  R2,R4                                                            
         B     FIL078                                                           
*                                                                               
         USING ACGOD,R2                                                         
FIL077B  CLI   LEVEL,3                                                          
         BNE   FIL078                                                           
*                                                                               
         L     R2,AGOBLOCK                                                      
         MVC   GOACOMP,ACMPREC                                                  
         MVC   GOALEDG,ALDGREC                                                  
         MVC   GOADM,DATAMGR                                                    
         LA    R3,SAVEKEY                                                       
         ST    R3,GOAKEY                                                        
*                                                                               
         MVC   GOSELCUL(1),COMPANY                                              
         MVC   GOSELCUL+1(2),=C'SJ'                                             
*                                                                               
         XC    GOACLI,GOACLI                                                    
         XC    GOAPRO,GOAPRO                                                    
*                                                                               
         SR    R1,R1                                                            
         LA    R3,ACLIREC                                                       
         IC    R1,SLVLALEN                                                      
         LR    R4,R1               SAVE LEVEL A LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R8)       IS THIS CLIENT SAVED                         
         BNE   *+10                NO                                           
         MVC   GOACLI,ACLIREC      YES, PASS IT TO GETOPT                       
*                                                                               
         LA    R3,APROREC                                                       
         IC    R1,SLVLBLEN                                                      
         AR    R1,R4               ADD LEVEL A LENGTH                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),0(R8)       IS THIS PRODUCT SAVED                        
         BNE   *+10                NO                                           
         MVC   GOAPRO,APROREC      YES, PASS IT TO GETOPT                       
*                                                                               
         ST    R8,GOAJOB                                                        
*                                                                               
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELJOB,SPACES                                                  
*                                                                               
         MVC   GOSELCLI(3),3(R8)                                                
         MVC   GOSELPRO(3),6(R8)                                                
         MVC   GOSELJOB(6),9(R8)                                                
         MVC   GOSELMED(1),9(R8)                                                
*                                                                               
         GOTO1 GETOPT,DMCB,(R2)                                                 
*                                                                               
FIL078   TM    IOMODE,NOFILTER                                                  
         BNZ   OKEXIT                                                           
*                                                                               
*              NOW DO FILTERING DRIVEN BY FILTAB                                
         USING GENTABD,R7                                                       
FIL080   CLC   WFILTERC(WFILCLNQ),SPACES                                        
         BNE   FIL090                                                           
         OC    WFILTERB(WFILBLNQ),WFILTERB                                      
         BZ    OKEXIT                                                           
FIL090   LA    R7,FILTAB                                                        
*                                                                               
FIL100   CLI   GENNAM,X'FF'                                                     
         BE    OKEXIT                                                           
         ZIC   R1,GENLFLD          IF FILTER FIELD IS EMPTY, IGNORE IT.         
         ZICM  RF,GENAFLD,3                                                     
         BZ    FIL160              PASSIVE OR OPTION.                           
         LA    RF,MACWRKD(RF)      R1 = L'FILTER FIELD - 1.                     
         ZIC   RE,GENTYPE          RF = A(FILTER FIELD).                        
         SLL   RE,2                RE = A(COMPARE APT TO FIELD TYPE).           
         L     RE,NULCHKS-4(RE)                                                 
         A     RE,WRELOC                                                        
         EX    R1,0(RE)                                                         
         BE    FIL160              IT'S EMPTY.                                  
*                                                                               
FIL140   ZICM  RF,GENFRTN,3                                                     
         BZ    FIL160                                                           
         A     RF,WRELOC                                                        
         LA    RE,FIL150                                                        
         NTR1                                                                   
         BASR  RE,RF                                                            
FIL150   BNE   ERREXIT                                                          
FIL160   LA    R7,GENTBLNQ(R7)                                                  
         B     FIL100                                                           
         DROP  R7                                                               
*                                                                               
CHARNUL  CLC   0(0,RF),SPACES                                                   
BINNUL   OC    0(0,RF),0(RF)                                                    
*                                                                               
NULCHKS  DC    A(CHARNUL)                                                       
         DC    A(BINNUL)                                                        
         DC    A(CHARNUL)                                                       
*                                                                               
*              FILTER ROUTINES FOR EACH FILTER.                                 
*                                  CLIENT/LEVEL 1 FILTER.                       
CLIFIL   ZIC   R1,SLVLALEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EXIT                                                             
         CLC   ACKEYACC+3(0),CLIENT                                             
*                                  PRODUCT/LEVEL 2 FILTER.                      
PROFIL   GOTO1 GETLVL,2            R1 = L'LVL-1, R0 = DISP OF LEVEL.            
         LA    RE,ACKEYACC                                                      
         AR    RE,R0               RE = A(LEVEL).                               
         EX    R1,*+8                                                           
         B     EXIT                                                             
         CLC   0(0,RE),PRODUCT                                                  
*                                  JOB/LEVEL 3 FILTER.                          
JOBFIL   GOTO1 GETLVL,3            R1 = L'LVL-1, R0 = DISP OF LEVEL.            
         LA    RE,ACKEYACC                                                      
         AR    RE,R0                                                            
         EX    R1,*+8                                                           
         B     EXIT                                                             
         CLC   0(0,RE),JOB                                                      
*                                  ACCOUNT LEVEL 4 FILTER.                      
LVL4FIL  GOTO1 GETLVL,4                                                         
         LA    RE,ACKEYACC                                                      
         AR    RE,R0                                                            
         EX    R1,*+8                                                           
         B     EXIT                                                             
         CLC   0(0,RE),LEVEL4                                                   
*                                                                               
         USING ACSTATD,R6          ACTIVITY DATE FILTER                         
ACTDTFIL ZICM  R6,ADACSTAT,4                                                    
         BZ    OKEXIT                                                           
         CLC   ACSTLAST,ACTDATE                                                 
         BNH   OKEXIT                                                           
         B     ERREXIT                                                          
*                                  CLOSING DATE FILTER.                         
         USING ACJOBD,R6                                                        
CLSDTFIL ZICM  R6,ADACJOB,4        ACCEPT THOSE JOBS WITH CLOSING               
         BZ    OKEXIT              DATES AT OR PRIOR TO FILTER DATE.            
         CLC   ACJBCLOS,CLOSDATE                                                
         BNH   OKEXIT                                                           
         B     ERREXIT                                                          
*                                  INVOICED AMOUNT/DR BAL FILTER.               
         USING ACBALD,R6                                                        
INVAFIL  ZICM  R6,ADACBAL,4                                                     
         BZ    OKEXIT                                                           
         CP    ACBLDR,INVAMT                                                    
         B     EXIT                                                             
*                                                                               
BILAFIL  ZICM  R6,ADACBAL,4        BILLED AMOUNT/CR BAL FILTER.                 
         BZ    OKEXIT                                                           
         CP    ACBLCR,BILAMT                                                    
         B     EXIT                                                             
*                                                                               
MEDIAFIL CLC   WUNIT(2),=C'SJ'                                                  
         BNE   OKEXIT                                                           
         GOTO1 GETLVL,3            IN 'SJ' MEDIA IS 1ST CHARACTER               
         LA    RE,ACKEYACC         OF JOB CODE.                                 
         AR    RE,R0                                                            
         CLC   MEDIA,0(RE)                                                      
         B     EXIT                                                             
*                                  BALANCE FILTER.                              
         USING ACBALD,R6                                                        
BALFIL   ZICM  R6,ADACBAL,4                                                     
         BZ    OKEXIT                                                           
         LA    R1,X'70'            ACCEPT THOSE WITH A BALANCE.                 
         CLI   BALANCE,C'Y'                                                     
         BE    BALF040                                                          
         LA    R1,X'80'            ACCEPT THOSE WITH ZERO BALANCE.              
         CLI   BALANCE,C'N'                                                     
         BE    BALF040                                                          
         LA    R1,X'20'            ACCEPT THOSE WITH A CR BALANCE.              
         CLI   BALANCE,C'C'                                                     
         BE    BALF040                                                          
         LA    R1,X'40'            ACCEPT THOSE WITH DR BALANCE.                
BALF040  CP    ACBLCR,ACBLDR                                                    
         EX    R1,*+8                                                           
         B     ERREXIT                                                          
         BC    0,OKEXIT                                                         
*                                                                               
         USING ACSTATD,R6                                                       
OPENFIL  ZICM  R6,ADACSTAT,4                                                    
         BZ    OKEXIT                                                           
         LA    R1,X'80'            ACCEPT OPEN JOBS ONLY.                       
         CLI   OPEN,C'Y'                                                        
         BE    *+8                                                              
         LA    R1,X'70'            ACCEPT CLOSED JOBS ONLY.                     
         TM    ACSTSTAT,X'40'                                                   
         EX    R1,*+8                                                           
         B     ERREXIT                                                          
         BC    0,OKEXIT                                                         
*                                                                               
         USING ACSTATD,R6                                                       
FIL1FIL  ZICM  R6,ADACSTAT,4       FILTER1 FILTERING.                           
         BZ    OKEXIT                                                           
         LA    RE,FILTER1                                                       
         LA    RF,ACSTFILT                                                      
         B     FILFILCK                                                         
*                                                                               
         USING ACSTATD,R6                                                       
FIL2FIL  ZICM  R6,ADACSTAT,4       FILTER2 FILTERING.                           
         BZ    OKEXIT                                                           
         LA    RE,FILTER2                                                       
         LA    RF,ACSTFILT+1                                                    
         B     FILFILCK                                                         
*                                                                               
         USING ACSTATD,R6                                                       
FIL3FIL  ZICM  R6,ADACSTAT,4       FILTER3 FILTERING.                           
         BZ    OKEXIT                                                           
         LA    RE,FILTER3                                                       
         LA    RF,ACSTANAL                                                      
         B     FILFILCK                                                         
*                                                                               
         USING ACSTATD,R6                                                       
FIL4FIL  ZICM  R6,ADACSTAT,4       FILTER4 FILTERING.                           
         BZ    OKEXIT                                                           
         LA    RE,FILTER4                                                       
         LA    RF,ACSTSUB                                                       
         B     FILFILCK                                                         
*                                                                               
         USING ACSTATD,R6                                                       
FIL5FIL  ZICM  R6,ADACSTAT,4       FILTER5 FILTERING.                           
         BZ    OKEXIT                                                           
         LA    RE,FILTER5                                                       
         LA    RF,ACSTFLT5                                                      
         B     FILFILCK                                                         
*                                                                               
         USING TWAD,RE                                                          
OFFFIL   CLI   ACCLVLNO,ACCLOW     ONLY DO LOW LEVEL ACCOUNTS                   
         BNE   OKEXIT                                                           
         LA    RE,SLVLLENS+1                                                    
         LA    RF,1                                                             
OFFFIL2  CLI   0(RE),0                                                          
         BE    OFFFIL4                                                          
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     OFFFIL2                                                          
OFFFIL4  LA    RE,OFFICES+3                                                     
         CLI   OFFICE,C'*'         TWINKLE MEANS EVERYTHING BUT                 
         BE    OFFFIL8                                                          
OFFFIL6  CLC   OFFICE(1),0(RE)                                                  
         BE    OKEXIT              FOUND A GOOD OFFICE                          
         CLI   0(RE),X'40'                                                      
         BH    ERREXIT                                                          
         BCTR  RE,0                                                             
         BCT   RF,OFFFIL6                                                       
         B     ERREXIT                                                          
OFFFIL8  CLC   OFFICE+1(1),0(RE)   WHEN WE'RE EXCLUDING                         
         BE    ERREXIT             A MATCH IS A FAIL                            
         CLC   0(1,RE),X'40'       AND A MIS-MATCH WITH A SIGNIFICANT           
         BH    OKEXIT              CODE IS A PASS                               
         BCTR  RE,0                                                             
         BCT   RF,OFFFIL8                                                       
         B     OKEXIT              NO CODE AT ALL IS A PASS ALSO                
*                                                                               
FILFILCK CLI   0(RE),C'*'                                                       
         BE    *+14                NEGATIVE FILTER.                             
         CLC   0(1,RF),0(RE)                                                    
         B     EXIT                                                             
         CLC   0(1,RF),1(RE)       NEGATIVE FILTER COMPARE,                     
         BE    ERREXIT                                                          
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
VALGEN   NTR1                                                                   
         LR    R8,R1               R8 = A(TABLE TO BE USED).                    
         MVI   FNDX,0                                                           
         GOTO1 SCANNER,DMCB,FADR,(30,SCANBLK)                                   
         MVI   FERN,INVALID                                                     
         ZICM  R2,DMCB+4           R2 = NUMBER OF ENTRIES INPUT.                
         BZ    ERREXIT                                                          
         LA    R7,SCANBLK          R7 = A(SCANNED I/P DATA).                    
         MVI   FNDX,1                                                           
         MVI   FERN,INVALID                                                     
         ST    R8,AGENTAB                                                       
*                                                                               
         USING GENTABD,R8                                                       
VGEN020  L     R8,AGENTAB                                                       
*                                                                               
VGEN030  CLI   GENNAM,X'FF'                                                     
         BE    ERREXIT                                                          
         ZIC   R1,0(R7)            R1 = L'1ST HALF OF SCANNED ITEM.             
         BCTR  R1,0                                                             
         EX    R1,GENCLC           CLC   GENNAM(0),12(R7)                       
         BE    VGEN040                                                          
         LA    R8,GENTBLNQ(R8)                                                  
         B     VGEN030                                                          
*                                                                               
VGEN040  CLI   GENTYPE,1           CHECK THAT L'INPUT IS WITHIN FIELD           
         BNE   VGEN0405            CAPACITY FOR CHARACTER FIELDS ONLY.          
         ZIC   R1,1(R7)            R1 = L'INPUT.                                
         BCTR  R1,0                                                             
         CLM   R1,X'01',GENLFLD                                                 
         BNH   VGEN0405                                                         
VGEN0402 MVI   FERN,LONGDAT                                                     
         B     ERREXIT                                                          
*                                                                               
VGEN0405 ZICM  RF,GENOLLST,3       IS KEYWORD VALID FOR THIS O'LAY.             
         BZ    VGEN060             ENTRY VALID FOR ALL OVERLAYS.                
         A     RF,WRELOC                                                        
VGEN042  CLI   0(RF),0                                                          
         BE    ERREXIT             NO.                                          
         CLC   MACOLAY,0(RF)                                                    
         BE    VGEN060             YES.                                         
         LA    RF,1(RF)                                                         
         B     VGEN042                                                          
*                                                                               
VGEN060  ZICM  RF,GENULLST,3       IS KEYWORD VALID FOR THIS U/L.               
         BZ    VGEN080             VALID FOR ALL U/LS FOR OVERLAY.              
         A     RF,WRELOC                                                        
VGEN070  CLI   0(RF),X'FF'                                                      
         BE    ERREXIT             IT IS EXCLUDED.                              
         CLC   WUNIT,0(RF)                                                      
         BNE   VGEN072             NO MATCH HERE.                               
         CLI   1(RF),C' '                                                       
         BE    VGEN080             ALL LEDGERS FOR THIS UNIT ARE OK.            
         CLC   WLEDGER,1(RF)                                                    
         BE    VGEN080             THIS U/L IS NOT EXCLUDED.                    
VGEN072  LA    RF,2(RF)            NO MATCH, TRY THE NEXT.                      
         B     VGEN070                                                          
*                                                                               
VGEN080  ZICM  RF,GENVRTN,3                                                     
         A     RF,WRELOC           RF = A(VALIDATION ROUTINE).                  
         LA    RE,VGEN100                                                       
FLT      NTR1                                                                   
         BASR  RE,RF                                                            
VGEN100  BNE   ERREXIT             FAILED VALIDATION.                           
         LA    R7,32(R7)                                                        
         ZIC   R1,FNDX             BUMP FIELD INDEX.                            
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         MVI   FERN,INVALID                                                     
         BCT   R2,VGEN020                                                       
VGENEND  DS    0H                                                               
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         B     OKEXIT                                                           
*                                                                               
GENCLC   CLC   GENNAM(0),12(R7)                                                 
*                                                                               
INVACT   MVI   FERN,INVALID                                                     
         B     MACERRS                                                          
*                                                                               
UNAUTH   MVI   FERN,SECLOCK                                                     
         B     MACERRS                                                          
*                                                                               
MISFIELD MVI   FERN,MISSING                                                     
         B     MACERRS                                                          
*                                                                               
*                                                                               
RESTART  MVC   LASTKEY,SPACES                                                   
         MVI   PASS,0                                                           
         MVI   MODE,0                                                           
         MVI   ACCLVLNO,0                                                       
         MVI   SECLVL,0                                                         
         BR    RE                                                               
         EJECT                                                                  
*              FILTER VALIDATION ROUTINES.                                      
*                                                                               
CLIVAL   LA    RE,CLIENT                                                        
         CLC   SLVLALEN,1(R7)                                                   
         BL    VGEN0402            INPUT IS TOO LONG.                           
         B     FILTMVC                                                          
*                                                                               
PROVAL   LA    RE,PRODUCT                                                       
         CLC   CLIENT,SPACES                                                    
         BE    ERREXIT                                                          
         CLC   SLVLBLEN,1(R7)                                                   
         BL    VGEN0402            TOO LONG                                     
         B     FILTMVC                                                          
*                                                                               
JOBVAL   LA    RE,JOB                                                           
         CLC   CLIENT,SPACES                                                    
         BE    ERREXIT                                                          
         CLC   PRODUCT,SPACES                                                   
         BE    ERREXIT                                                          
         CLC   SLVLCLEN,1(R7)                                                   
         BL    VGEN0402            TOO LONG.                                    
         B     FILTMVC                                                          
BALVAL   ZIC   R1,1(R7)                                                         
         BCTR  R1,0                                                             
         LA    RE,BALCLCS                                                       
BALV020  CLI   0(RE),X'FF'         END OF CLCS.                                 
         BE    ERREXIT             DIDN'T QUALIFY.                              
         EX    R1,0(RE)                                                         
         BE    BALV040             QUALIFIED.                                   
         LA    RE,6(RE)                                                         
         B     BALV020                                                          
*                                                                               
BALV040  EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   BALANCE(0),22(R7)                                                
*                                                                               
BALCLCS  CLC   22(0,R7),=C'Y'                                                   
         CLC   22(0,R7),=C'N'                                                   
         CLC   22(0,R7),=C'CR'                                                  
         CLC   22(0,R7),=C'DR'                                                  
         DC    AL1(255)                                                         
*                                                                               
YORNVAL  CLI   1(R7),1                                                          
         BNE   VGEN0402                                                         
         CLI   22(R7),C'Y'                                                      
         BE    *+12                                                             
         CLI   22(R7),C'N'                                                      
         BNE   ERREXIT                                                          
         ZICM  RF,GENAFLD,3                                                     
         LA    RF,MACWRKD(RF)                                                   
         MVC   0(0,RF),22(R7)                                                   
         B     OKEXIT                                                           
*                                                                               
ACTDTVAL LA    R0,ACTDATE                                                       
         B     FDATVAL                                                          
*                                                                               
CLSDTVAL LA    R0,CLOSDATE                                                      
         B     FDATVAL                                                          
*                                                                               
FDATVAL  MVI   FERN,DATERR                                                      
         GOTO1 VALIDATE,22(R7)                                                  
         BNE   ERREXIT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(1,(R0))                                    
         B     OKEXIT                                                           
*                                                                               
AMTVAL   ZIC   R0,1(R7)                                                         
         GOTO1 ,DMCB,(0,22(R7)),(R0)                                            
         GOTO1 VALICASH                                                         
         BNE   ERREXIT                                                          
         ZICM  RE,GENAFLD,3        RE = DISP OF FIELD INTO MACWRKD.             
         LA    RE,MACWRKD(RE)      RE = A(FIELD).                               
         ZIC   R1,GENLFLD          R1 = L'FILTER FIELD.                         
         SLL   R1,4                POSITION LNTH MASK FOR ZAP.                  
         EX    R1,VAMTZAP                                                       
         BO    VGEN0402            OVERFLOW, INPUT IS TOO BIG.                  
         B     OKEXIT                                                           
VAMTZAP  ZAP   0(0,RE),DUB                                                      
*                                                                               
CHARVAL  CLI   1(R7),1                                                          
         BNE   VGEN0402            L'INPUT MUST BE 1 BYTE.                      
         MVC   MEDIA,22(R7)                                                     
         B     OKEXIT                                                           
*                                                                               
         USING TWAD,RE                                                          
FILTVAL  LA    RE,T611FFD                                                       
         CLC   GENNAM(3),=C'OFF'   NO OFFICE FILTER FOR OFFICE TERMINAL         
         BNE   FILV020                                                          
         CLI   TWAACCS,C'*'                                                     
         BE    ERREXIT             OFFICE TERMINAL, NO OFFICE FILTERING         
         DROP  RE                                                               
FILV020  CLI   1(R7),2                                                          
         BH    VGEN0402            2 BYTES IS MAX LENGTH                        
         BL    *+12                                                             
         CLI   22(R7),C'*'         2 BYTE INPUT MUST BE NEGATIVE FILTER         
         BNE   ERREXIT                                                          
         ZICM  RE,GENAFLD,3                                                     
         LA    RE,MACWRKD(RE)      RE = A(DESTINATION FIELD).                   
         B     FILTMVC                                                          
*                                                                               
FILTMVC  ZIC   R1,1(R7)            R1 = L'INPUT FILTER VALUE.                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   0(0,RE),22(R7)                                                   
         DROP  R8                                                               
         EJECT                                                                  
*              VALIDATION ROUTINES                                              
         SPACE 3                                                                
VVALIDT  NTR1  BASE=MACBASE                                                     
         LR    R0,R1                                                            
         GOTO1 DATVAL,DMCB,(R0),WORK                                            
         L     R1,DMCB                                                          
         LTR   R1,R1                                                            
         BNZ   XITR1                                                            
         MVI   FERN,DATERR                                                      
         B     ERREXIT                                                          
         SPACE 1                                                                
VVALICSH NTR1  BASE=MACBASE                                                     
         GOTO1 CASHVAL,(R1)                                                     
         CLI   0(R1),0                                                          
         BE    VVALCSH2                                                         
         MVI   FERN,CASHERR                                                     
         B     ERREXIT                                                          
*                                                                               
VVALCSH2 L     R1,4(R1)                                                         
         CVD   R1,DUB                                                           
         B     XITR1                                                            
         SPACE 1                                                                
VANY     NTR1  BASE=MACBASE                                                     
         L     RF,FADR             RF = A(FIELD HEADER IN QUESTION).            
         SR    R1,R1                                                            
         IC    R1,5(RF)                                                         
         LTR   R1,R1                                                            
         BZ    VANY2                                                            
         BCTR  R1,0                                                             
         EX    R1,VANYCLC          SPACES ONLY = MISSING                        
         BE    VANY2                                                            
         B     XITR1                                                            
*                                                                               
VANYCLC  CLC   8(0,RF),SPACES                                                   
*                                                                               
VANY2    DS    0H                                                               
         MVI   FERN,MISSING                                                     
         B     ERREXIT                                                          
         SPACE 1                                                                
VNUMERIC NTR1  BASE=MACBASE                                                     
         L     RF,FADR             RF = A(FIELD HEADER).                        
         TM    4(RF),X'08'                                                      
         BO    VPACK2                                                           
         MVI   FERN,NOTNUMRC                                                    
         B     ERREXIT                                                          
         EJECT                                                                  
*              OTHER DATA HANDLING ROUTINES                                     
         SPACE 3                                                                
VPACK    NTR1  BASE=MACBASE                                                     
VPACK2   SR    R1,R1                                                            
         L     RF,FADR                                                          
         IC    R1,5(RF)                                                         
         ZAP   DUB,=P'0'                                                        
         LTR   R1,R1                                                            
         BZ    XITR1                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,RF)                                                      
         CVB   R1,DUB                                                           
         B     XITR1                                                            
         SPACE 1                                                                
VMOVE    NTR1  BASE=MACBASE                                                     
         MVC   WORK,SPACES                                                      
         L     RF,FADR                                                          
         SR    R1,R1                                                            
         IC    R1,5(RF)                                                         
         LTR   R1,R1                                                            
         BZ    OKEXIT                                                           
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   WORK(0),8(RF)                                                    
         SPACE 1                                                                
GETLVL   NTR1                                                                   
         LA    R0,3                R1 = BASIC DISP OF ACCOUNT CODE.             
         SR    RE,RE                                                            
         LA    RF,SLVLLENS                                                      
GLVL020  AR    R0,RE               RE = L'PREVIOUS LEVEL.                       
         ZIC   RE,0(RF)            RE = L'THIS LEVEL.                           
         LA    RF,1(RF)                                                         
         BCT   R1,GLVL020                                                       
         LR    R1,RE               R0 = DISP THIS LVL, R1 = L'THIS LVL.         
         BCTR  R1,0                                                             
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
*              ROUTINE TO GET NAME OUT OF A RECORD                              
*                                                                               
VNAMOUT  NTR1  BASE=MACBASE                                                     
         LR    RF,R1                                                            
         LA    RE,8(RF)                                                         
         MVC   0(36,RE),SPACES                                                  
         OI    6(RF),X'80'                                                      
         LA    R8,IO                                                            
         AH    R8,DATADISP                                                      
         SR    R1,R1                                                            
*                                                                               
VNAMOUT2 CLI   0(R8),0                                                          
         BE    OKEXIT                                                           
         CLI   0(R8),X'20'                                                      
         BNE   VNAMOUT4                                                         
         USING ACNAMED,R8                                                       
         IC    R1,ACNMLEN                                                       
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,38                                                            
         CH    R1,=H'38'                                                        
         BL    *+8                                                              
         LA    R1,38                                                            
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     OKEXIT                                                           
         MVC   0(0,RE),ACNMNAME                                                 
*                                                                               
VNAMOUT4 IC    R1,1(R8)                                                         
         AR    R8,R1                                                            
         B     VNAMOUT2                                                         
         DROP  R8                                                               
         EJECT                                                                  
*              ROUTINE TO BUILD A NAME ELEMENT                                  
*                                                                               
VNAMIN   NTR1  BASE=MACBASE                                                     
         GOTO1 HELLO,DMCB,(C'D',ACCOUNT),(X'20',IO2),0,0                        
         CLI   DMCB+12,0                                                        
         BE    *+14                SUCCESSFUL DELETE OF OLD NAME EL.            
         CLI   DMCB+12,6                                                        
         BE    *+6                 THERE WAS NO OLD NAME EL.                    
         DC    H'0'                MALFORMED RECORD.                            
         L     RF,FADR                                                          
         LA    RE,ELEMENT                                                       
         USING ACNAMED,RE                                                       
         SR    R1,R1                                                            
         IC    R1,5(RF)                                                         
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,2(R1)                                                         
         MVI   ACNMEL,X'20'                                                     
         STC   R1,ACNMLEN                                                       
         MVC   ACNMNAME(36),8(RF)                                               
         GOTO1 HELLO,DMCB,(C'P',ACCOUNT),IO2,ELEMENT,0                          
         CLI   DMCB+12,0                                                        
         BE    OKEXIT              SUCCESSFUL ADD OF NEW NAME ELEMENT.          
         DC    H'0'                BAD ADD.                                     
         DROP  RE                                                               
         SPACE 1                                                                
*              CONTROL OF BRANCHING TO APPLICATION                              
*                                                                               
GO       NTR1                                                                   
         MVI   FERN,X'FF'                                                       
         GOTO1 APPLIC,DMCB+12,(RA),(RC),(R8)                                    
         CLI   FERN,X'FF'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              COMMUNICATIONS WITH DATA MANAGER (FILE)                          
*                                                                               
VREAD    NTR1  BASE=MACBASE                                                     
         MVC   COMMAND,=CL8'DMREAD'                                             
         B     DIRLINK                                                          
*                                                                               
VSEQ     NTR1  BASE=MACBASE                                                     
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRLINK                                                          
*                                                                               
VHIGH    NTR1  BASE=MACBASE                                                     
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRLINK                                                          
*                                                                               
VADD     NTR1  BASE=MACBASE                                                     
         MVC   COMMAND,=CL8'DMADD'                                              
         B     DIRLINK                                                          
*                                                                               
VWRITE   NTR1  BASE=MACBASE                                                     
*                                                                               
         MVC   MACMSG,SPACES       YES, ERROR THEN                              
         L     R2,AXTRAINF                                                      
         USING XTRAINFD,R2                                                      
         TM    XIFLAG1,XIROSYS     CONNECTED TO READ ONLY SYS                   
         BNO   WRIT10                                                           
         MVC   GERROR,=AL2(358)                                                 
         B     ERREXT2                                                          
*                                                                               
WRIT10   TM    XIFLAG1,XIROMODE    CONNECTED IN READ ONLY MODE                  
         BNO   WRIT20                                                           
         MVC   GERROR,=AL2(360)                                                 
         B     ERREXT2                                                          
*                                                                               
WRIT20   TM    XIFLAG1,XIWRONGF    CONNECTED TO WRONG FACPAK                    
         BNO   WRIT30                                                           
         MVC   GERROR,=AL2(357)                                                 
         MVC   UPDADV,XIUPDFAC     GET THE UPDATIVE ADV NAME                    
         B     ERREXT2                                                          
         DROP  R2                                                               
*                                                                               
WRIT30   MVC   COMMAND,=CL8'DMWRT'                                              
         B     DIRLINK                                                          
*                                                                               
VSKSEQ   NTR1  BASE=MACBASE                                                     
         ZIC   R1,SKEYEND                                                       
         LA    R1,KEY(R1)                                                       
         ZIC   RE,0(R1)                                                         
         LA    RE,1(RE)                                                         
         STC   RE,0(R1)                                                         
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRLINK                                                          
*                                                                               
DIRLINK  LA    R8,IO               POINT TO FIRST IO AREA.                      
         TM    IOMODE,AREA2                                                     
         BZ    *+8                 FIRST IS WANTED.                             
         LA    R8,IO2              SECOND WAS WANTED, POINT TO IT.              
         GOTO1 GETFACT,DMCB,0                                                   
         L     RE,DMCB                                                          
         USING FACTSD,RE                                                        
*        ZICM  RF,FATMAXIO,2       ENSURE WERE NOT GOING TO EXCEED              
*        SH    RF,=H'10'           MAXIMUM I/O COUNT.                           
*        CLM   RF,3,FATIOCNT                                                    
*        BH    *+12                WE'RE ALL RIGHT.                             
         CLC   FATIOCNT,=H'500'                                                 
         BL    DIRL010                                                          
         MVI   KEY,X'FE'           TELL USER HE'S EXCEEDED I/O LIMIT.           
         CLI   KEY,X'FE'                                                        
         B     OKEXIT                                                           
         DROP  RE                                                               
DIRL010  CLC   COMMAND,=CL8'DMWRT'                                              
         BE    DIRL040                                                          
         XC    0(49,R8),0(R8)                                                   
         MVC   0(42,R8),SPACES                                                  
         MVC   0(15,R8),KEY                                                     
         CLI   KEY+14,0                                                         
         BNE   *+10                                                             
         XC    15(27,R8),15(R8)                                                 
DIRL040  GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',             X        
               (R8),(R8),(TERMINAL,DMWORK)                                      
*                                                                               
         MVC   SAVEKEY,0(R8)       SAVE LAST READ FOT GETOPT                    
*                                                                               
         CLI   IOMODE,0                                                         
         BNE   *+12                I/O FOR ROOT, KEEP IT SIMPLE.                
         CLI   DMCB+8,0                                                         
         B     EXIT                                                             
*                                                                               
         CLC   COMMAND(3),=C'DMR'                                               
         BNE   DMCHECK             NOT A READ.                                  
         CLC   COMMAND(4),=C'DMRE'                                              
         BE    DMCHECK             NOT A SEQ OR DIRECT READ.                    
         CLC   0(3,R8),KEYSAVE                                                  
         BE    *+12                NOT LOGICAL EOF.                             
         MVI   KEY,X'FF'           SHOW LOGICAL E-O-F.                          
         B     OKEXIT                                                           
*                                                                               
DMCHECK  CLC   COMMAND(3),=C'DMW'                                               
         BNE   DMCHECK4            NOT A WRITE.                                 
         TM    DMCB+8,X'C0'        FOR WRITES, EOF AND NON-RECVRBL DISK         
         BZ    OKEXIT              ERROR ARE FATAL.                             
         DC    H'0'                                                             
*                                                                               
DMCHECK4 TM    DMCB+8,X'80'        FOR READS, EOF IS O.K., BUT                  
         BZ    *+12                THE HEAVY DISK ERROR IS FATAL.               
         MVI   KEY,X'FF'           SHOW EOF.                                    
         B     OKEXIT                                                           
         TM    DMCB+8,X'40'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,0(R8)                                                        
         B     OKEXIT                                                           
         EJECT                                                                  
VSECAUTH NTR1                                                                   
         CLC   SECLVL,ACCLVLNO                                                  
         BH    ERREXIT             HI LEVEL ACCT IS LOCKED, FAIL.               
         MVI   SECLVL,0                                                         
         ZICM  R8,ADACSTAT,4       R8 = A(STATUS ELEMENT).                      
         BZ    OKEXIT              NO IT DOESN'T.                               
*                                                                               
         USING ACSTATD,R8                                                       
SEC4     LR    RE,RA                                                            
         USING TWAD,RE                                                          
         CLC   TWAAUTH+1(1),ACSTSECY+1                                          
         BNL   OKEXIT                                                           
         OI    DMCB+8,X'04'        DONT HAVE HIGH ENOUGH AUTHORIZATION          
         MVI   FERN,0                   SO SET SECURITY LOCK-OUT                
         MVC   SECLVL,ACCLVLNO     SAVE LEVEL OF LOCK OUT.                      
         B     ERREXIT                                                          
         DROP  RE                                                               
         SPACE 1                                                                
MACERRS  OC    GERROR,GERROR                                                    
         BNZ   MACERR2                                                          
         GOTO1 GETMSG,DMCB+12,(FERN,MACMSG),(FNDX,DMCB),DATAMGR                 
         OI    MACMSGH+6,X'80'                                                  
MACCURS  L     RF,FADR                                                          
         OI    6(RF),X'40'         SET CURSOR AT SPECIFIED FIELD.               
         OI    MACMSGH+6,X'80'                                                  
         B     FASTEXT                                                          
*                                                                               
         USING GETTXTD,R1          BUILD PARAM LIST FOR GETTXT                  
MACERR2  LA    R1,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         MVI   GTMTYP,GTMERR                                                    
         MVC   GTMSGNO,GERROR      SETTING INVALID AT THIS LEVEL                
         CLC   UPDADV,SPACES                                                    
         BNH   *+16                                                             
         MVI   GTLTXT,4                                                         
         LA    RE,UPDADV                                                        
         STCM  RE,7,GTATXT         PASS NEW TEXT'S BLOCK'S ADDRESS              
         GOTO1 GETTXT,DMCB                                                      
         DROP  R1                                                               
         OI    MACMSGH+1,X'08'     HIGH INTENSITY                               
         OI    MACMSGH+7,60        DEFAULT ERR MSG LENGTH                       
         OI    MACMSGH+6,X'80'     TRANSMIT MESSAGE                             
         OI    MACMSGH+6,X'40'     CURSOR TO ACTION FIELD                       
         B     FASTEXT                                                          
         EJECT                                                                  
ACTHELP  BAS   RE,CALLIT                                                        
         OI    MACACTH+6,X'40'     CURSOR                                       
         MVC   MACHED1+3(13),=C'VALID ACTIONS'                                  
         MVC   MACHED1+25(32),=C'VALID UNITS && LEDGERS FOR ACTION'             
         MVI   MACHED2+3,C'-'                                                   
         MVC   MACHED2+4(12),MACHED2+3                                          
         MVI   MACHED2+25,C'-'                                                  
         MVC   MACHED2+26(31),MACHED2+25                                        
         LA    RE,MALSTA1H                                                      
         LA    RF,ACTTAB                                                        
*                                                                               
         USING CLOSLYND,RE                                                      
         USING ACTTABD,RF                                                       
AHLP020  CLI   ACTNAM,X'FF'                                                     
         BE    FHLPEXT                                                          
         ZIC   R1,ACTAUTH                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    T611FFD+(TWAAUTH-TWAD),0                                         
         BO    *+10                                                             
         MVC   CLYNACC+9(12),=C'DISPLAY ONLY'                                   
         MVC   CLYNACC(8),ACTNAM                                                
         MVC   CLYNACC+22(3),=C'ALL'                                            
         ZICM  R1,ACTLIST,3        SET UP TO DISPLAY LIST OF VALID              
         BZ    AHLP040             U/LS FOR ACTION.                             
         A     R1,WRELOC                                                        
         LA    R8,CLYNACC+22                                                    
         MVC   0(3,R8),SPACES                                                   
         B     AHLP030                                                          
*                                                                               
AHLP028  CLI   0(R1),X'FF'                                                      
         BE    AHLP040             END OF U/L LIST.                             
         MVI   0(R8),C' '                                                       
         LA    R8,1(R8)                                                         
AHLP030  MVC   0(2,R8),0(R1)       MOVE IN A U/L.                               
         LA    R8,2(R8)                                                         
         LA    R1,2(R1)                                                         
         B     AHLP028                                                          
*                                                                               
         DROP  RE                                                               
AHLP040  LA    RE,CLYNLNQ(RE)                                                   
         LA    RF,ACTBLNQ(RF)                                                   
         B     AHLP020                                                          
*                                                                               
KEYHELP  BAS   RE,CALLIT                                                        
         OI    MACKEYH+6,X'40'     CURSOR                                       
         MVC   MACHED1+6(14),=C'KEY FORMAT IS:'                                 
         MVC   MACHED2+6(18),=C'LENGTH/DESCRIPTION'                             
         LA    RE,MALLYN1                                                       
         CLC   WUNIT(2),=H'0'                                                   
         BNE   KEYHLP2                                                          
         MVC   3(12,RE),=C'1       UNIT'                                        
         LA    RE,CLYNLNQ(RE)                                                   
         MVC   3(14,RE),=C'1       LEDGER'                                      
         LA    RE,CLYNLNQ(RE)                                                   
KEYHLP2  MVC   3(15,RE),=C'12      ACCOUNT'                                     
         B     FHLPEXT                                                          
*                                                                               
FILHELP  BAS   RE,CALLIT                                                        
         OI    MACOPTH+6,X'40'                                                  
         LA    R7,FILTAB                                                        
         LA    R8,MALSTA1H                                                      
         MVC   MACHED1+3(29),=C'VALID OPTIONS FOR THIS ACTION'                  
         MVI   MACHED2+3,C'-'                                                   
         MVC   MACHED2+4(28),MACHED2+3                                          
         MVC   MACHED1+42(29),MACHED1+3                                         
         MVC   MACHED2+42(29),MACHED2+3                                         
*                                                                               
         USING GENTABD,R7                                                       
         USING CLOSLYND,R8                                                      
FHLP020  CLI   GENNAM,X'FF'                                                     
         BE    FHLPEXT             TABLE END.                                   
         ZICM  RF,GENOLLST,3       IS FILTER USED IN THIS O'LAY.                
         BZ    FHLP040             YES                                          
         A     RF,WRELOC                                                        
FHLP030  CLI   0(RF),0                                                          
         BE    FHLP120             NO, TRY NEXT FILTER.                         
         CLC   MACOLAY,0(RF)                                                    
         BE    FHLP040             YES.                                         
         LA    RF,1(RF)                                                         
         B     FHLP030                                                          
*                                                                               
FHLP040  ZICM  RF,GENULLST,3       IS IT RELEVANT TO THIS U/L.                  
         BZ    FHLP060             YES.                                         
         A     RF,WRELOC                                                        
FHLP050  CLI   0(RF),X'FF'                                                      
         BE    FHLP120             NO, TRY THE NEXT ONE.                        
         CLC   WUNIT,0(RF)                                                      
         BNE   FHLP052             NO MATCH HERE.                               
         CLI   1(RF),C' '                                                       
         BE    FHLP060             ALL LEDGERS FOR THIS UNIT ARE OK.            
         CLC   WLEDGER,1(RF)                                                    
         BE    FHLP060             THIS LEDGER IS OK.                           
FHLP052  LA    RF,2(RF)                                                         
         B     FHLP050                                                          
*                                                                               
         USING TWAD,RE                                                          
FHLP060  LA    RE,T611FFD                                                       
         CLC   GENNAM(3),=C'OFF'                                                
         BNE   FHLP066                                                          
         CLI   TWAACCS,C'*'                                                     
         BE    FHLP120             OFFICE TERMINAL, NO OFFICE FILTERING         
         DROP  RE                                                               
*                                                                               
FHLP066  MVC   CLYNACC(8),GENNAM                                                
         CLC   GENNAM(4),=C'HELP'                                               
         BE    FHLP100                                                          
         MVI   CLYNACC+8,C'('                                                   
         MVC   CLYNACC+9(3),GENNAM                                              
         MVC   CLYNACC+12(2),=C')='                                             
         MVC   CLYNACC+14(15),GENNARR                                           
*                                                                               
FHLP100  LA    R8,CLYNLNQ(R8)                                                   
         LA    RF,MALTABH                                                       
         CR    R8,RF                                                            
         BL    FHLP120                                                          
         BH    FHLPEXT             FINISHED 2-UP DISPLAY.                       
         LA    R8,MALLYN1H+30      SET UP FOR 2ND COLUMN OF TWO UP.             
FHLP120  LA    R7,GENTBLNQ(R7)                                                  
         B     FHLP020                                                          
*                                                                               
FHLPEXT  CLI   FERN,X'FF'                                                       
         BNE   MACERRS             HERE BECAUSE OF INPUT ERROR.                 
         B     FASTEXT                                                          
*        HELP  SCREEN USES CLOSE/LOCK OVERLAY                                   
CALLIT   NTR1                                                                   
         GOTO1 CALLOV,DMCB,(X'FE',MACHED1H),0                                   
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LASTSCRN,X'FE'                                                   
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
RELOC    DC    A(*)                                                             
MAXMSG   DC    C'MAXIMUM RECORDS READ,  ENTER TO CONTINUE'                      
ACCOUNT  DC    C'ACCOUNT'                                                       
*                                                                               
ACTTAB   DS    0C                  TABLE OF ACTIONS AND ATTRIBUTES.             
         DC    CL8'CLOSE',X'01FE',2X'00',AL3(CLIULS),X'01FF80FD'                
         DC    CL8'OPEN',X'01FE',2X'00',AL3(CLIULS),X'01FF80FD'                 
         DC    CL8'LOCK',X'02FE',2X'00',AL3(0),X'11FF80FD'                      
         DC    CL8'UNLOCK',X'02FE',2X'00',AL3(0),X'11FF80FD'                    
         DC    CL8'FILTER',X'03FD',2X'00',AL3(0),X'01FF80FD'                    
         DC    AL1(255)                                                         
*                                                                               
OPTTAB   DS    0C                                                               
         DC    AL1(255)                                                         
*                                                                               
ELEMTAB  DS    0C                                                               
         DC    X'20',AL3(ADACNAM-MACWRKD)                                       
         DC    X'24',AL3(ADACPROF-MACWRKD)                                      
         DC    X'26',AL3(ADACJOB-MACWRKD)                                       
         DC    X'30',AL3(ADACSTAT-MACWRKD)                                      
         DC    X'31',AL3(ADACAST-MACWRKD)                                       
         DC    X'32',AL3(ADACBAL-MACWRKD)                                       
         DC    AL1(255)                                                         
*                                                                               
ADCONTAB DS    0A                                                               
         DC    A(VVALIDT)                                                       
         DC    A(VVALICSH)                                                      
         DC    A(VANY)                                                          
         DC    A(VNUMERIC)                                                      
         DC    A(VPACK)                                                         
         DC    A(VMOVE)                                                         
         DC    A(VNAMOUT)                                                       
         DC    A(VNAMIN)                                                        
         DC    A(VSECAUTH)                                                      
         DC    5A(0)                                                            
*                                                                               
         DC    A(VREAD)                                                         
         DC    A(VSEQ)                                                          
         DC    A(VHIGH)                                                         
         DC    A(VADD)                                                          
         DC    A(VWRITE)                                                        
         DC    A(VSKSEQ)                                                        
         DC    AL1(255)                                                         
*                                                                               
CLIOLAYS DC    AL1(1,0)            OVERLAY 01 ONLY.                             
*                                                                               
JOBULS   DC    C'SJ',AL1(255)                                                   
CLIULS   DC    C'SJ'                                                            
*&&US*&& DC    C'SMT '                                                          
         DC    AL1(255)                                                         
LCKLEDG  DC    C'SI'                                                            
*&&US*&& DC    C'SMT '                                                          
         DC    AL1(255)                                                         
*                                                                               
FILTAB   DS    0C                                                               
         DC    CL8'ACTDATE',AL3(0,0,ACTDTVAL,ACTDTFIL)                          
         DC    AL3(ACTDATE-MACWRKD),AL1(L'ACTDATE-1,2)                          
*&&UK*&& DC    CL15'DDMMMYY'                                                    
*&&US*&& DC    CL15'MMMDD/YY'                                                   
*                                                                               
         DC    CL8'BALANCE',AL3(0,0,BALVAL,BALFIL)                              
         DC    AL3(BALANCE-MACWRKD),AL1(L'BALANCE-1,1)                          
         DC    CL15'Y, N, CR, OR DR'                                            
*                                                                               
*&&UK                                                                           
         DC    CL8'BILAMT',AL3(0,0,AMTVAL,BILAFIL)                              
         DC    AL3(BILAMT-MACWRKD),AL1(L'BILAMT-1,3)                            
         DC    CL15'11 DIGITS'                                                  
*&&                                                                             
*                                                                               
         DC    CL8'CLIENT',AL3(CLIOLAYS,CLIULS,CLIVAL,CLIFIL)                   
         DC    AL3(CLIENT-MACWRKD),AL1(L'CLIENT-1,1)                            
         DC    CL15'CLIENT CODE'                                                
*                                                                               
         DC    CL8'LVA',AL3(CLIOLAYS,JOBULS,CLIVAL,CLIFIL)                      
         DC    AL3(CLIENT-MACWRKD),AL1(L'CLIENT-1,1)                            
         DC    CL15'LEV A ACCOUNT'                                              
*                                                                               
         DC    CL8'CLOSDATE',AL3(CLIOLAYS,CLIULS,CLSDTVAL,CLSDTFIL)             
         DC    AL3(CLOSDATE-MACWRKD),AL1(L'CLOSDATE-1,2)                        
*&&UK*&& DC    CL15'DDMMMYY'                                                    
*&&US*&& DC    CL15'MMMDD/YY'                                                   
*                                                                               
         DC    CL8'ELIGIBLE',AL3(0,0,YORNVAL,0)                                 
         DC    AL3(ELIGIBL-MACWRKD),AL1(L'ELIGIBL-1,1)                          
         DC    CL15'Y OR N'                                                     
*                                                                               
         DC    CL8'F1',AL3(0,0,FILTVAL,FIL1FIL)                                 
         DC    AL3(FILTER1-MACWRKD),AL1(L'FILTER1-1,1)                          
         DC    CL15'VALUE OR *VALUE'                                            
*                                                                               
         DC    CL8'F2',AL3(0,0,FILTVAL,FIL2FIL)                                 
         DC    AL3(FILTER2-MACWRKD),AL1(L'FILTER2-1,1)                          
         DC    CL15'VALUE OR *VALUE'                                            
*                                                                               
         DC    CL8'F3',AL3(0,0,FILTVAL,FIL3FIL)                                 
         DC    AL3(FILTER3-MACWRKD),AL1(L'FILTER3-1,1)                          
         DC    CL15'VALUE OR *VALUE'                                            
*                                                                               
         DC    CL8'F4',AL3(0,0,FILTVAL,FIL4FIL)                                 
         DC    AL3(FILTER4-MACWRKD),AL1(L'FILTER4-1,1)                          
         DC    CL15'VALUE OR *VALUE'                                            
*                                                                               
         DC    CL8'F5',AL3(0,0,FILTVAL,FIL5FIL)                                 
         DC    AL3(FILTER5-MACWRKD),AL1(L'FILTER5-1,1)                          
         DC    CL15'VALUE OR *VALUE'                                            
*                                                                               
         DC    CL8'HELP',AL3(0,0,FILHELP,0,0),AL1(0,0),CL15' '                  
*                                                                               
         DC    CL8'INVAMT',AL3(0,0,AMTVAL,INVAFIL)                              
         DC    AL3(INVAMT-MACWRKD),AL1(L'INVAMT-1,3)                            
         DC    CL15'11 DIGITS'                                                  
*                                                                               
         DC    CL8'JOB',AL3(CLIOLAYS,CLIULS,JOBVAL,JOBFIL)                      
         DC    AL3(JOB-MACWRKD),AL1(L'JOB-1,1)                                  
         DC    CL15'JOB CODE'                                                   
*                                                                               
         DC    CL8'LVC',AL3(CLIOLAYS,JOBULS,JOBVAL,JOBFIL)                      
         DC    AL3(JOB-MACWRKD),AL1(L'JOB-1,1)                                  
         DC    CL15'LEV C ACCOUNT'                                              
*                                                                               
         DC    CL8'MARK',AL3(0,0,YORNVAL,0)                                     
         DC    AL3(MARK-MACWRKD),AL1(L'MARK-1,1)                                
         DC    CL15'Y ONLY'                                                     
*                                                                               
         DC    CL8'MEDIA',AL3(0,0,CHARVAL,MEDIAFIL)                             
         DC    AL3(MEDIA-MACWRKD),AL1(L'MEDIA-1,1)                              
         DC    CL15'1 CHARACTER'                                                
*                                                                               
         DC    CL8'OFFICE',AL3(CLIOLAYS,JOBULS,FILTVAL,OFFFIL)                  
         DC    AL3(OFFICE-MACWRKD),AL1(L'OFFICE-1,1)                            
         DC    CL15'VALUE OR *VALUE'                                            
*                                                                               
         DC    CL8'OPEN',AL3(CLIOLAYS,CLIULS,YORNVAL,OPENFIL)                   
         DC    AL3(OPEN-MACWRKD),AL1(L'OPEN-1,1)                                
         DC    CL15'Y OR N'                                                     
*                                                                               
         DC    CL8'ORDERS',AL3(CLIOLAYS,JOBULS,YORNVAL,0)                       
         DC    AL3(ORDERS-MACWRKD),AL1(L'ORDERS-1,1)                            
         DC    CL15'Y OR N'                                                     
*                                                                               
         DC    CL8'HELD  ',AL3(CLIOLAYS,JOBULS,YORNVAL,0)                       
         DC    AL3(HELD-MACWRKD),AL1(L'HELD-1,1)                                
         DC    CL15'Y OR N'                                                     
*                                                                               
         DC    CL8'UNBILL',AL3(CLIOLAYS,JOBULS,YORNVAL,0)                       
         DC    AL3(UNBILL-MACWRKD),AL1(L'UNBILL-1,1)                            
         DC    CL15'Y OR N'                                                     
*                                                                               
         DC    CL8'UNBL SK',AL3(CLIOLAYS,JOBULS,YORNVAL,0)                      
         DC    AL3(UNBLSK-MACWRKD),AL1(L'UNBLSK-1,1)                            
         DC    CL15'Y OR N'                                                     
*                                                                               
         DC    CL8'PRODUCT',AL3(CLIOLAYS,CLIULS,PROVAL,PROFIL)                  
         DC    AL3(PRODUCT-MACWRKD),AL1(L'PRODUCT-1,1)                          
         DC    CL15'2 CHARACTERS'                                               
*                                                                               
         DC    CL8'LVB',AL3(CLIOLAYS,JOBULS,PROVAL,PROFIL)                      
         DC    AL3(PRODUCT-MACWRKD),AL1(L'PRODUCT-1,1)                          
         DC    CL15'LEV B ACCOUNT'                                              
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE ACMACWRK                                                       
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
         SPACE 2                                                                
         ORG   TWAUSER                                                          
SAVEOFFA DS    CL(OFFASAVL)        OFFAL SAVE AREA                              
         EJECT                                                                  
ACTTABD  DSECT                                                                  
ACTNAM   DS    CL8                 ACTION NAME.                                 
ACTOLAY  DS    AL1                 OVERLAY NUMBER FOR ACTION.                   
ACTSCRN  DS    AL1                 SCREEN NUMBER FOR ACTION. 0 = BASE.          
ACTUNIT  DS    C                   UNIT FOR ACTION. 0 = ANY UNIT.               
ACTLEDG  DS    C                   LEDGER FOR ACTION. 0 = ANY LEDGER.           
ACTLIST  DS    AL3                 A(LIST OF VALID U/LS FOR ACTION).            
*                                   NON-ZERO ONLY IF ACTUNIT/LEDGER             
*                                   ARE ZERO.                                   
ACTAUTH  DS    AL2                 AUTHORIZATION NEEDED FOR ACTION.             
ACTINBTS DS    X                   DATAMGR READ CONTROL BITS.                   
*                                   X'80' = READ FOR UPDATE.                    
*                                   X'08' = ACCEPT DELETED RECORDS.             
ACTOUBTS DS    X                   DATAMGR ERROR BITS.                          
*              IF THE BIT IS OFF, USER HANDLES ERROR CONDITION.                 
*                                   X'20' = DUPLICATE ON ADD.                   
*                                   X'10' = RECORD NOT FOUND.                   
*                                   X'02' = RECORD IS DELETED.                  
ACTBLNQ  EQU   *-ACTTABD                                                        
         SPACE 1                                                                
GENTABD  DSECT                                                                  
GENNAM   DS    CL8                 NAME OF ENTRY.                               
GENOLLST DS    AL3                 A(LIST OF VALID OLAYS FOR ENTRY).            
GENULLST DS    AL3                 A(LIST OF VALID U/LS FOR ENTRY).             
GENVRTN  DS    AL3                 A(VALIDATION ROUTINE FOR ENTRY).             
GENFRTN  DS    AL3                 A(FILTER ROUTINE FOR ENTRY).                 
GENAFLD  DS    AL3                 DISP OF FIELD IN MACWRKD.                    
GENLFLD  DS    AL1                 L'FIELD - 1.                                 
GENTYPE  DS    AL1                 1 = CHARACTER                                
*                                  2 = BINARY/PWOS                              
*                                  3 = PACKED.                                  
GENNARR  DS    CL15                HELP NARRATIVE FOR FILTER.                   
GENTBLNQ EQU   *-GENTABD                                                        
         SPACE 1                                                                
ELEMTABD DSECT                                                                  
ELEMCODE DS    AL1                 ELEMENT CODE.                                
ELEMADD  DS    AL3                 DISP INTO MACWRKD OF A(1ST EL FOR CD         
ELEMTBLQ EQU   *-ELEMTABD                                                       
         SPACE 1                                                                
CLOSLYND DSECT                                                                  
CLYNSTAH DS    CL8                                                              
CLYNSTA  DS    CL1                                                              
CLYNH    DS    CL8                                                              
CLYN     DS    0CL75                                                            
CLYNACC  DS    CL12                ACCOUNT CODE.                                
         DS    CL1                 SPARE.                                       
CLYNNAME DS    CL36                ACCOUNT NAME.                                
         DS    CL1                 SPARE                                        
CLYNACT  DS    CL9                 ACTIVITY DATE.                               
CLYNCLOS DS    CL9                 CLOSING DATE.                                
CLYNWHY  DS    CL7                 REASON ACCOUNT CAN'T BE CLOSED.              
CLYNLNQ  EQU   *-CLOSLYND                                                       
CLOSMAX  EQU   17                                                               
         EJECT                                                                  
       ++INCLUDE ACMACEQU                                                       
*              INCLUDED HERE ARE - ACGENBOTH                                    
*                                  DDCOMFACS                                    
*                                  DDACCFACS                                    
*                                  FAFACTS                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039ACMAC00   08/27/04'                                      
         END                                                                    
