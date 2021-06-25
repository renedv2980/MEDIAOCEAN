*          DATA SET GEPFM00    AT LEVEL 004 AS OF 09/18/17                      
*PHASE TF0100A                                                                  
*INCLUDE DECODE                                                                 
         TITLE 'PFM00 - ROOT CONTROLLER AND TABLES'                             
         PRINT NOGEN                                                            
PFM00    CSECT                                                                  
         NMOD1 PFMTEMPX-PFMTEMPD,**PF00**,RA,CLEAR=YES,RR=RE                    
         LR    R9,RC                                                            
         USING PFMTEMPD,R9         R9=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
                                                                                
***********************************************************************         
* RELOCATE EXTERNALS                                                  *         
***********************************************************************         
RELOC    L     RF,=A(SYSTBL)                                                    
         LA    RF,0(RE,RF)                                                      
         ST    RF,ASYSTBL                                                       
         L     RF,=A(FILETBL)                                                   
         LA    RF,0(RE,RF)                                                      
         ST    RF,AFILETBL                                                      
         L     RF,=A(PERMTBL)                                                   
         LA    RF,0(RE,RF)                                                      
         ST    RF,APERMTBL                                                      
         L     RF,=V(DECODE)                                                    
         LA    RF,0(RE,RF)                                                      
         ST    RF,ADECODE                                                       
*                                  RELOCATE ROUTINES                            
RELO010  LA    RF,OLAY                                                          
         ST    RF,AOLAY                                                         
         LA    RF,CLEAR                                                         
         ST    RF,ACLEAR                                                        
         LA    RF,DISP                                                          
         ST    RF,ADISP                                                         
         LA    RF,DISKIO                                                        
         ST    RF,ADISKIO                                                       
*                                  RELOCATE TABLES                              
         LA    RF,KEYTBL                                                        
         ST    RF,AKEYTBL                                                       
         LA    RF,ACTNTBL                                                       
         ST    RF,AACTNTBL                                                      
         LA    RF,DMCMDS                                                        
         ST    RF,ADMCMDS                                                       
         L     RF,=A(DISPTBLL)                                                  
         LA    RF,0(RE,RF)                                                      
         ST    RF,ADISPLWR                                                      
         L     RF,=A(DISPTBLU)                                                  
         LA    RF,0(RE,RF)                                                      
         ST    RF,ADISPUPR                                                      
                                                                                
***********************************************************************         
* PFM INITIALISATION                                                  *         
***********************************************************************         
INIT     L     R3,4(R1)            OTHERS GET NORMAL PLIST                      
         L     RF,16(R1)                                                        
         ST    R1,APARM                                                         
         ST    RB,ABASE                                                         
         ST    RA,ABASE1                                                        
         ST    R3,ASAVE                                                         
         ST    R9,ATEMP                                                         
         USING PFMSAVED,R3         R3=A(TWA)                                    
         USING COMFACSD,RF                                                      
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   AGETFACT,CGETFACT                                                
         MVC   AGLOBBER,CGLOBBER                                                
         MVC   ADATCON,CDATCON                                                  
         DROP  RF                                                               
*                                                                               
         BAS   RE,GLOBALS          TEST FOR GLOBALS                             
*                                                                               
         CLI   DISPDAT,C'H'        DECIMAL OR HEX SCREEN SHOWN?                 
         BNE   INIT005             DEFAULT IS DECIMAL                           
         MVC   VWDISP,HWDISP                                                    
         MVC   VNDISP,HNDISP                                                    
         B     INIT006                                                          
*                                                                               
INIT005  MVC   VWDISP,DWDISP                                                    
         MVC   VNDISP,DNDISP                                                    
         CLI   DISPDAT,C'D'        DEFAULT SCREEN REQUIRED?                     
         BE    INIT006             NO                                           
         MVI   DISPDAT,C'D'        LOAD IN DEFAULT SCREEN                       
         GOTO1 ACALLOV,DUB,(X'FD',PFMLAST),0                                    
         CLI   4(R1),0                                                          
         BE    INIT006                                                          
         DC    H'0'                                                             
*                                                                               
INIT006  L     RF,AGETFACT         GET CONNECTED SYSTEM FROM GETFACT            
         OI    PFMSRH+6,X'81'      CHANGE TO MODIFIED FIELD (TRANSMIT)          
         L     R4,0(R1)            A(TIOB)                                      
         USING TIOBD,R4                                                         
         CLI   TIOBAID,0           PLAIN ENTER KEY?                             
         BE    INIT010             YES                                          
*                                                                               
         ZIC   R0,TIOBAID          LOAD FUNCTION KEY NUMBER                     
         DROP  R4                  DON`T NEED DSECT ANYMORE                     
         CH    R0,=H'12'           PF13 - PF24?                                 
         BNH   *+8                 NO                                           
         SH    R0,=H'12'           PF13 -> PF1 .. PF24 -> PF12                  
         STC   R0,PFKEY            SAVE IT                                      
         CLI   PFKEY,8             PAGE DOWN?                                   
         BE    INIT020             NEED THE SAVED DISPLACEMENT                  
         CLI   PFKEY,9             NEXT KEY?                                    
         BE    INIT020             DISPLACEMENT NOT NEEDED                      
INIT010  XC    DISPLACE,DISPLACE   SAVED DISPLACEMENT NOT NEEDED                
         MVI   PFKEY,0             NO OTHER PFKEY ALLOWED YET                   
*                                                                               
INIT020  GOTO1 (RF),DMCBWS,0                                                    
         L     R1,0(R1)                                                         
         MVC   CONOV,FAOVSYS-FACTSD(R1)                                         
         MVC   CONSYS,FASYS-FACTSD(R1)                                          
         MVC   CONLANG,FACTRY-FACTSD(R1)                                        
         MVC   ALANGTAB,FAXLATES-FACTSD(R1)                                     
*                                                                               
         ZIC   RF,CONLANG                                                       
         SLL   RF,4                                                             
         A     RF,ALANGTAB                                                      
         MVC   AUPPER,8(RF)                                                     
         MVC   ALOWER,12(RF)                                                    
*                                                                               
         MVI   BLANKS,C' '                                                      
         MVC   BLANKS+1(L'BLANKS-1),BLANKS                                      
         XC    FERRS,FERRS                                                      
*                                                                               
         OC    SLIFINFO,SLIFINFO   BUILD SYSTEM LIST IF FIRST TIME              
         BNZ   CHECKIN                                                          
         LA    RF,WRK                                                           
         XC    WRK,WRK             BUILD KEY OF SYSTEM LIST RECORD              
         USING CTWREC,RF                                                        
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         GOTO1 ADATAMGR,DMCBWS,=C'DMREAD',=C'CTFILE',WRK,IOAREA                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,IOAREA                                                        
         LA    RF,CTWDATA          RF=A(FIRST ELEMENT)                          
         DROP  RF                                                               
         LA    RE,SLSELIST                                                      
         USING SLSELIST,RE         RE=A(SAVE SYSTEM LIST)                       
         USING CTLSTD,RF                                                        
         LA    R0,SLSEMAX          R0=MAXIMUM # OF SAVE LIST ENTRIES            
         SR    R1,R1                                                            
*                                                                               
INIT030  CLI   CTLSTEL,0           TEST E-O-R                                   
         BE    CHECKIN                                                          
         CLI   CTLSTEL,CTLSTELQ    TEST SYSTEM ELEMENT                          
         BNE   INIT040                                                          
         MVC   SLSELIST(SLSELEN),CTLSTDTA                                       
         LA    RE,SLSELEN(RE)                                                   
         XC    SLSELIST(SLSELEN),SLSELIST                                       
         BCT   R0,*+8                                                           
         B     CHECKIN                                                          
*                                                                               
INIT040  IC    R1,CTLSTLEN         BUMP TO NEXT ELEMENT                         
         LA    RF,0(R1,RF)                                                      
         B     INIT030                                                          
         DROP  RE,RF                                                            
                                                                                
***********************************************************************         
* CHECK FOR INPUT IN FF SCREEN                                        *         
***********************************************************************         
CHECKIN  LA    R2,PFMFILEH                                                      
         USING FLDHDRD,R2                                                       
         XR    R1,R1                                                            
         LA    R0,10               NUMBER OF FIELDS IN FF SCREEN                
CHEC010  TM    FLDIIND,FINPTHIS                                                 
         BO    CHEC020                                                          
         IC    R1,FLDLEN                                                        
         LA    R2,0(R2,R1)                                                      
         BCT   R0,CHEC010                                                       
         DROP  R2                                                               
*                                                                               
         CLI   STATUS,1            ALL INPUT IN LOWER AREA?                     
         BE    STAT1               OK FOR STATUS=1                              
*NOP*    MVI   FERN,28             NEED TO BYPASS INPUT TOO LONG                
*NOP*    LA    R6,PFMFILEH         NOT NEEDED FOR INPUTS ABOVE                  
*NOP*    ST    R6,FERRS                                                         
*NOP*    OI    FIND,X'01'                                                       
*NOP*    B     OHDRMSG                                                          
CHEC020  MVI   STATUS,0            SOME INPUT IN FF SCREEN                      
                                                                                
***********************************************************************         
* INPUT IS REC DEFN & ACTION                                          *         
***********************************************************************         
STAT0    MVI   OLAYOP,1            OVERLAY #1                                   
         GOTO1 AOLAY               SYNTAX INPUT & DISPLAY                       
         TM    FIND,X'01'                                                       
         BO    OHDRMSG             DISPLAY ERROR MESSAGE & EXIT                 
         MVI   STATUS,1                                                         
         CLI   STIRA,1             DISPLAY                                      
         BE    TRANSEND                                                         
         CLI   STIRA,2             CHANGE                                       
         BE    STAT010                                                          
         CLI   STIRA,3             ADD                                          
         BE    STAT010                                                          
         CLI   STIRA,4             BROWSE                                       
         BE    TRANSEND                                                         
         CLI   STIRA,5             REP                                          
         BE    STAT110                                                          
*                                                                               
STAT010  MVC   SLIFINFO,STIFINFO   SAVE FILE INFORMATION                        
         MVC   SLIRINFO,STIRINFO   SAVE RECORD INFORMATION                      
         MVC   SLIPINFO,STIP10     SAVE I/O INFORMATION                         
         MVC   SLDISPDL,DISPDL     SAVE DISPLAYED DATA LENGTH                   
         B     OHDRMSG                                                          
                                                                                
***********************************************************************         
* INPUT IS UPDATE DATA                                                *         
***********************************************************************         
STAT1    MVC   STIFINFO,SLIFINFO   RESTORE SAVED INFORMATION                    
         MVC   STIRINFO,SLIRINFO                                                
         MVC   STIP10(6),SLIPINFO                                               
         MVC   DISPDL,SLDISPDL                                                  
         CLC   STISYS,CONSYS       SWITCH TO SYSTEM IF NECESSARY                
         BE    STAT110                                                          
*                                                                               
         L     RF,APARM                                                         
         L     RF,16(RF)                                                        
         L     RF,CSWITCH-COMFACSD(RF)                                          
         MVC   DMCBWS(1),STISYS                                                 
         MVC   DMCBWS+1(3),=X'FFFFFF'                                           
         XC    DMCBWS+4(4),DMCBWS+4                                             
         GOTO1 (RF),DMCBWS                                                      
         CLI   4(R1),0                                                          
         BE    STAT110                                                          
         MVI   FERN,44                                                          
         B     OP_ERR                                                           
*                                                                               
STAT110  MVI   OLAYOP,2            OVERLAY #2                                   
         GOTO1 AOLAY               CHECK HEX & UPDATE                           
         TM    FIND,X'80'          GOTO BIT SET?                                
         BZ    STAT120             NO, CHECK IF IN ERROR                        
         MVI   STATUS,0            ALL OK SO RESET STATUS                       
         NI    FIND,X'FF'-X'80'    TAKE OFF THAT BIT                            
         B     CHECKIN                                                          
*                                                                               
STAT120  TM    FIND,X'01'                                                       
         BO    OHDRMSG             DISPLAY ERROR MESSAGE AND EXIT               
         MVI   STATUS,0            ALL OK SO RESET STATUS                       
         MVI   HDRN,0                                                           
*                                                                               
TRANSEND MVC   SLDISPDL,DISPDL     END OF TRANSACTION                           
         MVC   SLIRA,STIRA                                                      
         MVI   STATUS,0                                                         
         B     OHDRMSG                                                          
                                                                                
***********************************************************************         
* OUTPUT HEADER MESSAGE                                               *         
***********************************************************************         
OHDRMSG  XC    WSS,WSS                                                          
         TM    FIND,X'01'                                                       
         BZ    OP_OK                                                            
         CLI   FERN,X'FF'          NO ERROR OR MESSAGE?                         
         BE    EXIT                NONE                                         
OP_ERR   SR    R6,R6               SET WSS=ERROR NN - XXX...                    
         IC    R6,FERN                                                          
         CLI   FERN,48             REGULAR MESSAGES?                            
         BL    OP_ERR1             YES, DON`T PUT ERROR PREFIX                  
         BCTR  R6,0                -1 FOR OFFSET                                
         B     OP_ERR2                                                          
OP_ERR1  CVD   R6,DUB                                                           
         UNPK  DUB(3),DUB+6(2)                                                  
         OI    DUB+2,X'F0'                                                      
         MVC   WSS(10),=C'ERROR NN -'                                           
         MVC   WSS+6(2),DUB+1                                                   
         SH    R6,=H'1'                                                         
         BNM   OP_ERR2                                                          
         L     R6,FERN             SET SPECIAL MESSAGE                          
         MVC   WSS+11(30),0(R6)                                                 
         B     OP_ERR3                                                          
OP_ERR2  XC    DUB(2),DUB                                                       
         MVI   DUB+1,L'ERRMSGS                                                  
         MH    R6,DUB                                                           
         L     RF,=A(ERRMSGS)                                                   
         A     RF,RELO                                                          
         AR    R6,RF                                                            
         MVC   WSS+11(L'ERRMSGS),0(R6)                                          
OP_ERR3  L     R6,FERRS            SET CURSOR ON BAD FIELD                      
         OI    6(R6),OI1C                                                       
         B     OMSG                                                             
*                                                                               
OP_OK    SR    R6,R6               SET OK HEADER MESSAGE                        
         IC    R6,HDRN                                                          
         XC    DUB(2),DUB                                                       
         MVI   DUB+1,L'OKMSGS                                                   
         MH    R6,DUB                                                           
         L     RF,=A(OKMSGS)                                                    
         A     RF,RELO                                                          
         AR    R6,RF                                                            
         MVC   WSS(L'OKMSGS),0(R6)                                              
         CLI   STATUS,0            POSN CURSOR DEPENDING ON STATUS              
         BNE   *+12                                                             
         OI    PFMFILEH+6,OI1C                                                  
         B     OMSG                                                             
         CLI   STATUS,1                                                         
         BNE   OMSG                                                             
         CLI   DISPDAT,C'H'                                                     
         BNE   *+12                                                             
         LA    R2,PFMHL1HH                                                      
         B     *+8                                                              
         LA    R2,PFMDL1HH                                                      
         OI    6(R2),OI1C                                                       
         B     OMSG                                                             
         DC    H'0'                                                             
*                                                                               
OMSG     FOUT  PFMHDRH,WSS                                                      
         B     EXIT                                                             
                                                                                
***********************************************************************         
* EXIT POINT - SET ON ALL TRANSMIT FLAGS                              *         
***********************************************************************         
EXIT     DS    0H                                                               
         LA    R1,PFMHDRH                                                       
         USING FLDHDRD,R1                                                       
         SR    RF,RF                                                            
*                                                                               
EXIT010  ICM   RF,1,FLDLEN                                                      
         BZ    EXIT020                                                          
         LA    R1,0(R1,RF)                                                      
         OI    FLDOIND,FOUTTRN                                                  
         B     EXIT010                                                          
*                                                                               
EXIT020  MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
         XMOD1 1                                                                
                                                                                
***********************************************************************         
* THIS ROUTINE TESTS IF WE WERE CALLED BY ANOTHER APPLICATION         *         
***********************************************************************         
GLOBALS  NTR1  BASE=ABASE                                                       
         L     RA,ABASE1                                                        
         MVI   GLOBFLAG,0                                                       
         GOTO1 AGLOBBER,DMCBWS,=C'GETD',IOAREA,54,8                             
         TM    8(R1),X'10'                                                      
         BO    GLOBX               EXIT IF NO PFM GLOBAL PRESENT                
         GOTO1 (RF),(R1),=C'DELE'                                               
         GOTO1 (RF),(R1),=C'DELE',,,4                                           
         MVI   GLOBFLAG,1          SET FLAG TO SHOW USED GLOBAL INFO            
         LA    R4,IOAREA                                                        
         USING GLPFMFIL,R4                                                      
*                                                                               
GLOB1    MVC   PFMFILE(8),GLPFMFIL SET FILE NAME IN FIELD                       
         LA    R0,8                                                             
         LA    R1,PFMFILE+7                                                     
GLOB1A   CLI   0(R1),C' '                                                       
         BH    GLOB1B                                                           
         MVI   0(R1),0                                                          
         BCTR  R1,0                                                             
         BCT   R0,GLOB1A                                                        
GLOB1B   STC   R0,PFMFILEH+5                                                    
*                                                                               
GLOB2    OC    GLPFMDA,GLPFMDA     SET DISK ADDRESS IN FIELD                    
         BZ    GLOB3                                                            
         MVC   PFMRID1(2),=C'A,'                                                
         GOTO1 AHEXOUT,DMCBWS,GLPFMDA,PFMRID1+2,4,=C'TOG'                       
         MVI   PFMRID1H+5,10                                                    
         OI    PFMRID1H+6,X'80'                                                 
         MVI   PFMRACT,C'D'        SET DISP AS DEFAULT ACTION                   
         MVI   PFMRACTH+5,1                                                     
         OI    PFMRACTH+6,X'80'                                                 
         MVC   PFMEID(2),=C'FI'    SET DEFAULT TO FI                            
         MVI   PFMEIDH+5,2                                                      
         OI    PFMEIDH+6,X'80'                                                  
GLOB2A   CLI   GLPFMFLG,C'*'       TEST IF WANT TO SET ACTN/ELID                
         BNE   GLOBX                                                            
         CLI   GLPFMACT,C' '       TEST IF ACTION PASSED                        
         BNH   *+10                                                             
         MVC   PFMRACT(1),GLPFMACT                                              
*                                                                               
         MVC   PFMEID(2),GLPFMELI                                               
         MVI   PFMEIDH+5,2                                                      
         CLI   GLPFMELI,C' '       TEST IF ELEMENT ID PASSED                    
         BH    *+16                                                             
         MVI   PFMEIDH+5,0                                                      
         OI    PFMEIDH+6,X'80'                                                  
         B     GLOBX                                                            
         OI    PFMEIDH+6,X'80'                                                  
         CLC   PFMEID(2),=C'FI'    TEST IF FI (FIRST) PASSED                    
         BE    GLOBX               YES                                          
         MVC   PFMEID(2),=C'I,'    NO ASSUME 2CHR HEX EL ID (I,XX)              
         MVC   PFMEID+2(2),GLPFMELI                                             
         MVI   PFMEIDH+5,4                                                      
         B     GLOBX                                                            
*                                                                               
GLOB3    LA    R0,L'GLPFMKEY       SET KEY IN FIELD                             
         LA    R1,GLPFMKEY+L'GLPFMKEY-1                                         
GLOB3A   CLI   0(R1),0                                                          
         BH    GLOB3B                                                           
         BCTR  R1,0                                                             
         BCT   R0,GLOB3A                                                        
         LA    R0,1                                                             
         LA    R1,GLPFMKEY                                                      
GLOB3B   LA    RF,IOAREA+256                                                    
         MVC   0(2,RF),=C'K,'                                                   
         GOTO1 AHEXOUT,DMCBWS,GLPFMKEY,2(RF),(R0),=C'TOG'                       
         LR    RE,R0                                                            
         SLL   RE,1                                                             
         LA    RE,2(RE)            RE=LENGTH OF KEY STRING                      
         LA    RF,IOAREA+256                                                    
         LA    R1,L'PFMRID1                                                     
         CR    RE,R1               TEST IF KEY LONGER THAN ONE LINE             
         BH    GLOB3C              YES                                          
         STC   RE,PFMRID1H+5                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PFMRID1(0),0(RF)                                                 
         OI    PFMRID1H+6,X'80'                                                 
         B     GLOBX                                                            
GLOB3C   MVC   PFMRID1,0(RF)       SET FIRST LINE OF TWO LINE KEY               
         STC   R1,PFMRID1H+5                                                    
         OI    PFMRID1H+6,X'80'                                                 
         SR    RE,R1               RE=LEN OF DATA IN SECOND KEY FIELD           
         AR    RF,R1               RF=A(START OF DATA FOR SECOND FIELD)         
         STC   RE,PFMRID2H+5                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PFMRID2(0),0(RF)    SET SECOND LINE OF TWO LINE KEY              
         OI    PFMRID2H+6,X'80'                                                 
         NI    PFMRID2H+6,255-FOUTPRT                                           
         NI    PFMRID2H+1,255-FATBPROT                                          
*                                                                               
GLOBX    CLI   GLOBFLAG,0          EXIT WITH CC EQL IF NO GLOBALS               
         XIT1                                                                   
                                                                                
***********************************************************************         
* THIS ROUTINE LOADS IN OVERLAY # OLAYOP & PASSES CONTROL TO IT       *         
***********************************************************************         
OLAY     NTR1  BASE=ABASE                                                       
         L     RA,ABASE1                                                        
         LA    R1,COVWS                                                         
         USING CALLOVD,R1                                                       
         XC    OVPARM1,OVPARM1                                                  
         ST    R3,OVPARM2                                                       
         MVC   OVNO(1),OLAYOP                                                   
         L     RF,ACALLOV                                                       
         BASR  RE,RF                                                            
         CLI   OVERR,X'FF'                                                      
         BE    OLAYERR                                                          
         L     RF,OVPARM1                                                       
         BASR  RE,RF                                                            
         B     OLAYX                                                            
OLAYERR  DC    H'0'                                                             
OLAYX    XIT1                                                                   
                                                                                
***********************************************************************         
* THIS ROUTINE LOADS IN THE CORRECT SCREEN DEPENDING ON THE DISPLAY   *         
* TYPE CURRENTLY REQUESTED                                            *         
***********************************************************************         
CLEAR    NTR1  BASE=ABASE                                                       
         L     RA,ABASE1                                                        
         CLI   DISPDAT,C'H'                                                     
         BE    HEXCLR                                                           
         MVI   DISPDAT,C'D'                                                     
         GOTO1 ACALLOV,DUB,(X'FD',PFMLAST),0                                    
         CLI   4(R1),0                                                          
         BE    CLR010                                                           
         DC    H'0'                                                             
*                                                                               
HEXCLR   GOTO1 ACALLOV,DUB,(X'FE',PFMLAST),0                                    
         CLI   4(R1),0                                                          
         BE    CLR010                                                           
         DC    H'0'                                                             
*                                                                               
CLR010   CLI   DISPDAT,C'H'        HEX DISPLAY?                                 
         BE    ULC010              YES                                          
         LA    RE,DUPRHDR          DECIMAL DISPLAY                              
         CLI   STICASE,C'U'        UPPERCASE?                                   
         BE    ULC020              YES                                          
         LA    RE,DLWRHDR                                                       
         B     ULC020                                                           
*                                                                               
ULC010   LA    RE,HUPRHDR          HEX DISPLAY                                  
         CLI   STICASE,C'U'        UPPERCASE?                                   
         BE    ULC020              YES                                          
         LA    RE,HLWRHDR                                                       
*                                                                               
ULC020   EQU   *                                                                
         LA    RF,PFMTIT1H         CHANGE HEADLINE TO SHOW LOWER CASE           
         LA    R1,L'PFMTIT1                                                     
         CLI   DISPDAT,C'H'                                                     
         BNE   *+12                                                             
         LA    RF,PFMHIT1H                                                      
         LA    R1,L'PFMHIT1                                                     
         USING FLDHDRD,RF                                                       
         BCTR  R1,0                                                             
         EX    R1,ULCOMP           SAME AS BEFORE?                              
         BE    UCL030              YES                                          
         EX    R1,ULMOVE           MOVE IN THE CORRECT ONE                      
         OI    FLDOIND,FOUTTRN     TRANSMIT IT                                  
*                                                                               
ULCOMP   CLC   FLDDATA(0),0(RE)                                                 
ULMOVE   MVC   FLDDATA(0),0(RE)                                                 
         DROP  RF                                                               
*                                                                               
UCL030   EQU   *                                                                
         LA    R1,PFMDL1AH         SET CHAR I/P FIELDS TO LOWERCASE             
         LH    RE,DLINLEN          LENGTH OF A DECIMAL DISPLAY LINE             
         LA    RF,PFMDLFAH         LAST LINE                                    
         CLI   DISPDAT,C'H'        HEX DISPLAY?                                 
         BNE   ULC040              NO                                           
         LA    R1,PFMHL1AH                                                      
         LH    RE,HLINLEN          LENGTH OF A HEX DISPLAY LINE                 
         LA    RF,PFMHLGAH         LAST LINE                                    
*                                                                               
ULC040   EQU   *                                                                
         USING FLDHDRD,R1                                                       
         CLI   STICASE,C'U'        SHOULD THIS BE UPPERCASE?                    
         BNE   ULC050              NO                                           
         TM    FLDATB,FATBLC       UPPERCASE ALREADY?                           
         BZ    ULC060              YES                                          
         NI    FLDATB,255-FATBLC   SET OFF LOWERCASE BIT                        
         OI    FLDOIND,FOUTTRN     TRANSMIT IT                                  
         B     ULC060                                                           
ULC050   TM    FLDATB,FATBLC       LOWERCASE ALREADY                            
         BNZ   ULC060              YES                                          
         OI    FLDATB,FATBLC       SET ON LOWERCASE BIT                         
         OI    FLDOIND,FOUTTRN     TRANSMIT IT                                  
ULC060   BXLE  R1,RE,ULC040                                                     
         XIT1                                                                   
         DROP  R1                                                               
                                                                                
***********************************************************************         
* THIS ROUTINE DISPLAYS AS MUCH OF THE RECORD IN IOAREA HTAT WILL FIT *         
* ON THE SCREEN STARTING AT DISPLAY LINE # DISPSLN - DISPOP BITS AS   *         
* FOLLOWS  X'01'=1 DISP PART, X'01'=0 DISP WHOLE,X'02'=1 DISP HEX,    *         
*          X'04'=1 DISP BYTES , X'08'=1 DISP CHARS                    *         
***********************************************************************         
DISP     NTR1  BASE=ABASE                                                       
         L     RA,ABASE1                                                        
         LH    R4,DISPDL           CALLER SETS DISPDL                           
         SRDL  R4,32               R4&R5=DISP DATA LENGTH                       
         LH    R7,VWDISP           DISPLAY LINE WIDTH                           
         STH   R7,DISPLW                                                        
         LH    R8,VNDISP           # OF DIPLAY LINES                            
         STH   R8,DISPNL                                                        
         DR    R4,R7                                                            
         LTR   R4,R4               REMAINDER?                                   
         BZ    *+8                 NO                                           
         LA    R5,1(R5)            BUMP QUOTIENT BY 1                           
         STH   R5,DISPNLR          R5=NO OF LINES REQUIRED                      
         LH    R4,DISPSLN          START LINE                                   
         LA    R4,0(R5,R4)                                                      
         CH    R4,DISPNL           WILL IT ALL FIT?                             
         BNH   DISP010             YES                                          
*                                                                               
         MVI   DISPRES,1           SET DISPLAY ONLY PART                        
         TM    DISPOP,X'01'        REQUEST PART DISPLAY?                        
         BZ    DISPX               NO - EXIT                                    
         LH    R4,DISPNL                                                        
         SH    R4,DISPSLN                                                       
         STH   R4,DISPNLR          SET NEW LINES REQ TO REMAINDER               
         MH    R4,DISPLW                                                        
         STH   R4,DISPDL           SET NEW DISPLAY LENGTH                       
         B     DISP020                                                          
*                                                                               
DISP010  MVI   DISPRES,0           SET DISPLAY WHOLE                            
*                                  INITIALISE LOOP REGISTERS                    
DISP020  OI    DISPOP,X'80'        SET DATA DISPLAYED FLAG                      
         LA    R1,HEXWS            R1=A(HEXOUT W/S)                             
         USING HEXOUTD,R1                                                       
         LA    R7,=C'MIX'                                                       
         ST    R7,HOAO                                                          
         L     RF,AHEXOUT          RF=A(HEXOUT CSECT)                           
         CLI   DISPDAT,C'H'                                                     
         BNE   DISP030                                                          
*                                                                               
         LH    R4,HLINLEN          LENGTH OF A HEX LINE                         
         MH    R4,DISPSLN                                                       
         LA    R4,PFMHL1OH(R4)     R4=A(DISPLAY LINE IN TWA)                    
         B     DISP040                                                          
*                                                                               
DISP030  LH    R4,DLINLEN          LENGTH OF A DECIMAL LINE                     
         MH    R4,DISPSLN                                                       
         LA    R4,PFMDL1OH(R4)     R4=A(DISPLAY LINE IN TWA)                    
*                                                                               
DISP040  LH    R5,DISPSB           CALLER SETS DISPSB                           
         STH   R5,HDRST                                                         
         LA    R5,IOAREA(R5)       R5=A(DATA IN IOAREA)                         
         LH    R6,DISPLW           R6=L'DISPLAY LINE DATA                       
         LH    R7,DISPNLR          R7=# OF DISPLAY LINES REQUIRED               
         MVC   DISPCT,DISPDL       COUNT OF RESIDUAL BYTES                      
*                                                                               
         USING FLDHDRD,R4          LOOP FOR EACH LINE DISPLAY                   
DISP050  CH    R7,=H'1'                                                         
         BNE   *+8                                                              
         LH    R6,DISPCT           PARTIAL DISP FOR LAST LINE                   
         TM    DISPOP,X'04'                                                     
         BZ    DISP060                                                          
         LH    R8,DISPSB           SET UP SSSSS-FFFFF                           
         CLI   DISPDAT,C'H'                                                     
         BE    DISP055                                                          
         CVD   R8,DUB                                                           
         UNPK  DUB(5),DUB+5(3)                                                  
         OI    DUB+4,X'F0'                                                      
         B     DISP056                                                          
*                                                                               
DISP055  GOTO1 (RF),HEXWS,DISPSB,DUB+1,2,=C'MIX'                                
         MVI   DUB,C'0'                                                         
*                                                                               
DISP056  MVC   FLDDATA(5),DUB                                                   
         MVI   FLDDATA+5,C' '                                                   
         CLC   DISPCT,DISPDL                                                    
         BNE   DISP060                                                          
         MVI   FLDDATA+5,C'-'      SET 1ST LINE FULL                            
         CLI   DISPRES,1                                                        
         BNE   DISP060                                                          
         MVI   FLDDATA+5,C'*'      SET 1ST LINE PART                            
DISP060  LH    R8,DISPSB           SET DISPSB TO END OF LINE                    
         AR    R8,R6                                                            
         BCTR  R8,R0                                                            
         STH   R8,DISPSB                                                        
         TM    DISPOP,X'04'                                                     
         BZ    DISP070                                                          
         CLI   DISPDAT,C'H'                                                     
         BE    DISP065                                                          
         CVD   R8,DUB                                                           
         UNPK  DUB(5),DUB+5(3)                                                  
         OI    DUB+4,X'F0'                                                      
         B     DISP066                                                          
*                                                                               
DISP065  GOTO1 (RF),HEXWS,DISPSB,DUB+1,2,=C'MIX'                                
         MVI   DUB,C'0'                                                         
*                                                                               
DISP066  MVC   FLDDATA+6(5),DUB                                                 
         MVI   FLDOLEN,7                                                        
         OI    FLDOIND,OI1T                                                     
*                                                                               
DISP070  ZIC   RE,0(R4)                                                         
         LA    R4,0(RE,R4)         BUMP TO HEX/DEC FIELD                        
*                                                                               
DISP080  TM    DISPOP,X'02'                                                     
         BZ    DISP090                                                          
         ST    R5,HOAS             MOVE IN HEX                                  
         ST    R6,HOLS                                                          
         LA    R8,FLDDATA                                                       
         ST    R8,HOAD                                                          
         BASR  RE,RF                                                            
         MVC   FLDILEN(1),HOLD+3                                                
         MVC   FLDOLEN(1),HOLD+3                                                
         OI    FLDOIND,OI1T                                                     
*                                                                               
DISP090  ZIC   RE,0(R4)            CHARACTER FIELD                              
         LA    R4,0(RE,R4)                                                      
         TM    DISPOP,X'08'                                                     
         BZ    DISP100                                                          
         FOUT  (R4),(R5),(R6)      MOVE IN CHARACTERS                           
         STC   R6,FLDILEN                                                       
         L     R8,ADISPTBL                                                      
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         TR    FLDDATA(0),0(R8)    CHANGE INVALID CHARS TO ?                    
         LA    R6,1(R6)                                                         
*                                                                               
DISP100  LH    R8,DISPSB           UP DISPSB TO NEXT LINE                       
         LA    R8,1(R8)                                                         
         STH   R8,DISPSB                                                        
         LH    R8,DISPCT           DOWN RESIDUAL COUNT                          
         SR    R8,R6                                                            
         STH   R8,DISPCT                                                        
         ZIC   RE,0(R4)                                                         
         LA    R4,0(RE,R4)         UP TWA POINTER                               
         AR    R5,R6               UP IOAREA POINTER                            
         BCT   R7,DISP050                                                       
*                                                                               
         XR    R0,R0                                                            
         CLI   DISPDAT,C'H'                                                     
         BNE   DISP110                                                          
*                                                                               
         LH    R4,HLINLEN          LENGTH OF A HEX LINE                         
         MH    R4,DISPNL                                                        
         LA    R4,PFMHL1OH(R4)     R4=A(DISPLAY LINE IN TWA)                    
         B     DISP120                                                          
*                                                                               
DISP110  LH    R4,DLINLEN          LENGTH OF A DECIMAL LINE                     
         MH    R4,DISPNL                                                        
         LA    R4,PFMDL1OH(R4)     R4=A(DISPLAY LINE IN TWA)                    
*                                                                               
DISP120  ICM   R0,1,FLDLEN         FIND END OF TWA                              
         BZ    *+10                                                             
         AR    R4,R0                                                            
         B     DISP120                                                          
         MVC   1(2,R4),=X'0000'    SET BEFORE/AFTER     *****                   
*                                                                               
DISP130  CLI   STIEA,0             DISPLAYING ELEMENTS?                         
         BE    *+12                                                             
         LH    R0,STIBE                                                         
         B     *+8                                                              
         LH    R0,HDRST                                                         
         CLI   DISPDAT,C'H'                                                     
         BNE   DISP140                                                          
         SRDL  R0,4                                                             
         SRL   R1,27               R1 HOLDS HEX DISPLACEMENT                    
*                                                                               
         LA    RF,HCOUNT(R1)                                                    
         LA    R4,PFMHIT2H                                                      
         MVC   PFMHIT2(L'PFMHIT2),0(RF)                                         
         LA    RE,L'PFMHIT2                                                     
         STC   RE,FLDILEN                                                       
         STC   RE,FLDOLEN                                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDATB,FATBXHDR                                                  
         SRL   R1,1                                                             
         LA    RF,HCHARS(R1)                                                    
         LA    R4,PFMHIT3H                                                      
         MVC   PFMHIT3(L'PFMHIT3),0(RF)                                         
         LA    RE,L'PFMHIT3                                                     
         STC   RE,FLDILEN                                                       
         STC   RE,FLDOLEN                                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDATB,FATBXHDR                                                  
         B     DISPX                                                            
*                                                                               
DISP140  CVD   R0,DUB                                                           
         ZIC   R1,DUB+L'DUB-1                                                   
         SRL   R1,4                                                             
         SLL   R1,1                R1 HAS THE DECIMAL DISPLACEMENT              
         LA    RF,DCOUNT(R1)                                                    
         LA    R4,PFMTIT2H                                                      
         MVC   PFMTIT2(L'PFMTIT2),0(RF)                                         
         LA    RE,L'PFMTIT2                                                     
         STC   RE,FLDILEN                                                       
         STC   RE,FLDOLEN                                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDATB,FATBXHDR                                                  
         SRL   R1,1                                                             
         LA    RF,DCHARS(R1)                                                    
         LA    R4,PFMTIT3H                                                      
         MVC   PFMTIT3(L'PFMTIT3),0(RF)                                         
         LA    RE,L'PFMTIT3                                                     
         STC   RE,FLDOLEN                                                       
         STC   RE,FLDILEN                                                       
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDATB,FATBXHDR                                                  
         B     DISPX                                                            
*                                                                               
DISPX    LA    R4,PFMHIT1H                                                      
         LA    RE,L'PFMHIT1                                                     
         CLI   DISPDAT,C'H'                                                     
         BE    *+12                                                             
         LA    R4,PFMTIT1H                                                      
         LA    RE,L'PFMTIT1                                                     
         STC   RE,FLDOLEN                                                       
         STC   RE,FLDILEN                                                       
         OI    FLDIIND,FINPTHIS                                                 
         OI    FLDOIND,FOUTTRN                                                  
         OI    FLDATB,FATBXHDR                                                  
         XIT1                                                                   
         DROP  R4                                                               
                                                                                
***********************************************************************         
* THIS ROUTINE DOES ALL THE I/O VIA DATA MANAGER                      *         
***********************************************************************         
DISKIO   NTR1  BASE=ABASE                                                       
         L     RA,ABASE1                                                        
         XC    WRK,WRK                                                          
         LA    R1,DMCBWS                                                        
         USING DMCBD,R1                                                         
         SR    R4,R4                                                            
         IC    R4,DISKIOOP                                                      
         MH    R4,=H'3'                                                         
         CLI   STATUS,0                                                         
         BE    *+8                                                              
         LA    R4,6(R4)                                                         
         LA    R4,STIPERM+4(R4)    R4=A(I/O INFO)                               
         SR    R5,R5                                                            
         IC    R5,0(R4)            R5=COMMAND NO                                
         BCTR  R5,R0                                                            
         MH    R5,=H'6'                                                         
         L     R6,ADMCMDS                                                       
         AR    R6,R5               R6=A(COMMAND TO DATAMGR)                     
         ST    R6,DMCB1                                                         
         MVI   DMCTL1,X'09'        RETURN DELETS & REC LEN                      
*                                                                               
DIO0     TM    STIFTL,X'01'        TEST IF REQUEST FILE                         
         BZ    *+8                                                              
         OI    DMCTL1,X'20'        SET NEW STYLE REQUEST HDRS                   
*                                                                               
         CLC   0(3,R6),=C'DMR'     CLEAR IOAREA BEFORE READ                     
         BE    *+14                                                             
         CLC   0(3,R6),=C'GET'                                                  
         BNE   DIO1                                                             
         LA    RE,IOAREA                                                        
         L     RF,=A(IOAREAX-IOAREA)                                            
         XCEF                                                                   
*                                                                               
         CLI   STATUS,0                                                         
         BE    DIO1                                                             
         CLI   STIRA,2                                                          
         BNE   DIO1                                                             
         OI    DMCTL1,X'80'        SET READ FOR UPDATE ON CHANGE                
*                                                                               
DIO1     SR    R5,R5                                                            
         SR    R5,R5                                                            
         IC    R5,STIFN            R5=FILE NUM                                  
         BCTR  R5,R0                                                            
         MH    R5,=H'20'                                                        
         SR    R6,R6                                                            
         ICM   R6,3,STIFDSP                                                     
         A     R6,AFILETBL                                                      
         AR    R6,R5               R6=A(FILE NAME)                              
         ST    R6,DMCB2                                                         
*&&US                                                                           
         CLC   0(3,R6),=C'SPT'     TEST SPTFILE                                 
         BNE   DIO1A                                                            
         L     RE,DMCB1                                                         
         CLC   =C'PUT',0(RE)       TEST PUTREC                                  
         BNE   DIO1A                                                            
         NI    DMCB1,X'FF'-X'01'   TURN OFF 'RETURN REC LEN'                    
*&&                                                                             
DIO1A    CLI   1(R4),1                                                          
         BH    *+12                                                             
         LA    R6,STIK                                                          
         B     DIO2                                                             
         CLI   1(R4),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,SLIK                                                          
DIO2     ST    R6,DMCB3            R6=A(KEY OR ADR)                             
*                                                                               
         LA    R6,IOAREA                                                        
         ST    R6,DMCB4                                                         
*                                                                               
         LA    R6,IOWORK                                                        
         ST    R6,DMCB5                                                         
         MVC   DMCTL5,0(R3)                                                     
         XC    DMCB6,DMCB6                                                      
*                                                                               
         CLI   STIFRT,1                                                         
         BNE   DIO4                                                             
         SR    R6,R6                                                            
         ICM   R6,3,STIFRL         SET REC LEN FOR F/L                          
         LA    R6,1(R6)                                                         
         STH   R6,SLRL                                                          
         MVC   DMRECL(2),SLRL      SET LEN IN DMCB6 STUPIDO                     
*                                                                               
DIO4     L     RF,ADATAMGR                                                      
         BASR  RE,RF                                                            
         TM    DMCTL3,X'FC'                                                     
         BC    5,DIOMISS           EOF/ERR/DKEY/NOTF/PHL/SECL                   
*                                                                               
DIO4A    SR    RE,RE               THIS FIXES CTBUFF RETURNING FF REC           
         ICM   RE,7,DMCB2+1                                                     
         CLC   0(3,RE),=C'CTF'     TEST CTFILE                                  
         BNE   DIO4B                                                            
         ICM   RE,7,DMCB4+1                                                     
         CLC   00(2,RE),=X'FFFF'   TEST FF'S KEY RETURN                         
         BNE   DIO4B                                                            
         CLC   25(2,RE),=X'0080'   TEST NEW EOF RECORD LEN (CTLD)               
         BNE   *+14                                                             
         CLC   28(2,RE),=X'0164'   TEST NEW EOF RECORD FIRST ELEMENT            
         BE    DIO4B                                                            
         CLC   25(2,RE),=X'0022'   TEST OLD EOF RECORD LEN (CONCRETE)           
         BNE   *+14                                                             
         CLC   28(2,RE),=X'0105'   TEST OLD EOF RECORD FIRST ELEMENT            
         BE    DIO4B                                                            
         MVC   25(2,RE),=X'001C'   SET VALID LENGTH AND NO ELEMENTS             
         MVC   27(2,RE),=X'0000'                                                
         OI    DMCTL3,X'80'        SET EOF RETURN FOR THIS SET OF RECS          
         B     DIOMISS                                                          
*                                                                               
DIO4B    CLI   STIFRLBN,X'FF'      RECORD LEN IN RECORD                         
         BE    DIOHIT              NO                                           
         SR    R6,R6               YES SET IN DMCB                              
         IC    R6,STIFRLBN                                                      
         LA    R6,IOAREA(R6)                                                    
         MVC   DMRECL,0(R6)                                                     
*                                                                               
DIO5     CLI   STIFT,4             TEST FOR ACTIVITY ELEMENT                    
         BNE   DIOHIT                                                           
         CLI   STIFRT,3                                                         
         BNE   DIOHIT                                                           
         CLI   STIFSL,4            FILE IS DAL & VLELEM & 4 BYTE LINK           
         BNE   DIOHIT                                                           
         SR    RE,RE                                                            
         IC    RE,STIFKL                                                        
         SR    RF,RF                                                            
         IC    RF,STIFCL                                                        
         AR    RE,RF                                                            
         LA    R6,IOAREA(RE)       POINT TO LINKAGE AREA                        
         TM    3(R6),X'01'                                                      
         BZ    DIOHIT              NO ACTIVITY DATA PRESENT                     
         LH    R6,DMRECL                                                        
         LA    R6,IOAREA(R6)       POINT TO END OF RECORD                       
         OC    0(2,R6),0(R6)                                                    
         BNZ   DIOHIT              DONT LOOK LIKE ACTIVITY                      
         GOTO1 AHEXOUT,HEXWS,2(R6),WRK+30,4,=C'MIX'                             
         GOTO1 ADATCON,HEXWS,(12,2(R6)),(25,WORK2)                              
         MVI   WRK+38,C' '                                                      
         LA    RE,WORK2            WORK2=YMDHMS IN BINARY                       
         LA    RF,WRK+39                                                        
         LA    R1,6                                                             
         SR    R0,R0                                                            
DIO6     IC    R0,0(RE)                                                         
         CHI   R0,100              ADJUST FOR YR2000                            
         BL    *+8                                                              
         AHI   R0,-100                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,RF),DUB                                                      
         LA    RE,1(RE)                                                         
         LA    RF,2(RF)                                                         
         BCT   R1,DIO6                                                          
         LA    R1,DMCBWS           RESTORE R1 TO POINT TO DMCBWS                
*                                                                               
DIOHIT   SR    R6,R6               SET READ INDIC & LENGTH                      
         IC    R6,2(R4)                                                         
         LA    R6,1(R6)                                                         
         STC   R6,SLRI                                                          
DIOHL    MVC   SLRL,DMRECL                                                      
         MVC   SLRF,STIFN                                                       
         MVI   HDRN,0                                                           
         LH    R6,SLRL                                                          
         MVC   WRK(3),=C'RL='                                                   
         CLI   DISPDAT,C'H'        DO WE DISPLAY LENGTH IN HEX OR               
         BE    DIOH00              IN DECIMAL?                                  
         CVD   R6,DUB                                                           
         UNPK  WRK+3(5),DUB                                                     
         OI    WRK+7,X'F0'                                                      
         B     DIOH0                                                            
*                                                                               
DIOH00   GOTO1 AHEXOUT,HEXWS,SLRL,WRK+3,2,=C'MIX'                               
         MVI   WRK+7,C' '                                                       
         LA    R1,DMCBWS                                                        
*                                                                               
DIOH0    CLI   2(R4),0             SET KEY AS INSTRUCTED                        
         BE    DISKIOX                                                          
         CLI   2(R4),4                                                          
         BNE   DIOH1                                                            
         ZIC   RF,STIFCL           CONTROL BYTES                                
         ZIC   RE,STIFKL           FILE KEY LENGTH                              
         LA    RF,0(RE,RF)                                                      
         LA    RF,IOAREA(RF)                                                    
         ICM   R6,15,0(RF)         DA                                           
         XC    STIK,STIK                                                        
         ST    R6,STIK                                                          
         B     DISKIOX                                                          
*                                                                               
DIOH1    CLI   2(R4),1                                                          
         BNE   DIOH2                                                            
         TM    STIFTL,X'40'                                                     
         BZ    DIOH111                                                          
         MVC   DUB+4(2),PFMRID1+8  EXTRACT LAST TWO CHRS OF DISK ADDR           
         OC    DUB+4(2),BLANKS     CONVERT TO UPPER CASE                        
         GOTO1 AHEXIN,HEXWS,DUB+4,DUB,2,0                                       
         ZIC   RE,DUB                                                           
         LTR   RE,RE                                                            
         BZ    DIOH111                                                          
         ZIC   R1,IOAREA+3                                                      
         MVI   FERN,46             RECORD NUMBER INPUT WAS TOO HIGH             
         CR    R1,RE                                                            
         BL    DIOMISS1                                                         
         MVI   FERN,0                                                           
         LA    R5,IOAREA+6                                                      
         XR    RF,RF                                                            
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    DIOH101                                                          
*                                                                               
DIOH10   ICM   RF,3,0(R5)                                                       
         LA    R5,0(RF,R5)                                                      
         BCT   RE,DIOH10                                                        
*                                                                               
DIOH101  LR    R2,R5                                                            
         LA    RF,IOAREA                                                        
         SR    R2,RF                                                            
         STH   R2,DISPBLK                                                       
         XR    R2,R2                                                            
         ICM   R2,3,0(R5)                                                       
         STH   R2,SLRL                                                          
         CLI   DISPDAT,C'H'        DO WE DISPLAY LENGTH IN HEX OR               
         BE    DIOH001             IN DECIMAL?                                  
         CVD   R2,DUB                                                           
         UNPK  WRK+3(5),DUB                                                     
         OI    WRK+7,X'F0'                                                      
         B     DIOH111                                                          
*                                                                               
DIOH001  GOTO1 AHEXOUT,HEXWS,SLRL,WRK+3,2,=C'MIX'                               
         MVI   WRK+7,C' '                                                       
*                                                                               
DIOH111  LA    R1,DMCBWS                                                        
         L     R5,DMCB3                                                         
         LA    R6,4                                                             
         MVC   WRK+9(3),=C'RA='                                                 
         GOTO1 AHEXOUT,HEXWS,(R5),WRK+12,4,=C'MIX'                              
         B     DIOHX                                                            
*                                                                               
DIOH2    CLI   2(R4),2                                                          
         BNE   DIOH3                                                            
         CLC   STIPFKO+1(2),=X'0901'                                            
         BNE   DIOH21                                                           
         LA    R1,DMCBWS                                                        
         L     R5,DMCB3                                                         
         MVC   WRK+9(3),=C'RA='                                                 
         GOTO1 AHEXOUT,HEXWS,(R5),WRK+12,4,=C'MIX'                              
DIOH21   LA    R5,IOAREA                                                        
         SR    R6,R6                                                            
         IC    R6,STIFKL                                                        
         B     DIOHX                                                            
*                                                                               
DIOH3    CLI   2(R4),3                                                          
         BNE   DIOH4                                                            
         CLI   0(R4),X'0A'         TEST GETREC FOR FIRST RECORD                 
         BNE   DIOH31                                                           
         CLC   PFMRID1(10),=C'A,00000001'                                       
         BNE   DIOH31                                                           
         LA    R1,DMCBWS           OUTPUT DISK ADDR OF FIRST RECORD             
         L     R5,DMCB3                                                         
         MVC   WRK+9(3),=C'RA='                                                 
         GOTO1 AHEXOUT,HEXWS,(R5),WRK+12,4,=C'MIX'                              
DIOH31   LA    R5,STIK                                                          
         SR    R6,R6                                                            
         IC    R6,STIKL                                                         
         B     DIOHX                                                            
DIOH4    DC    H'0'                                                             
*                                                                               
DIOHX    LA    R7,SLIK             R5=A(KEY/ADR) R6=L'KEY/ADR                   
         CR    R7,R5                                                            
         BE    *+10                                                             
         XC    SLIK,SLIK                                                        
         STC   R6,SLIKL                                                         
         BCTR  R6,R0                                                            
         EX    R6,DIOHMV                                                        
         B     DISKIOX                                                          
DIOHMV   MVC   SLIK(0),0(R5)                                                    
*                                                                               
DIOMISS  MVI   SLRI,0                                                           
         TM    DMCTL3,X'40'        DISKERROR                                    
         BZ    *+12                                                             
         MVI   HDRN,1                                                           
         B     DIOMISS1                                                         
         TM    DMCTL3,X'80'        EOF                                          
         BZ    *+12                                                             
         MVI   HDRN,2                                                           
         B     DIOMISS1                                                         
         TM    DMCTL3,X'10'        NOTFOUND                                     
         BZ    *+12                                                             
         MVI   HDRN,3                                                           
         B     DIOMISS1                                                         
         TM    DMCTL3,X'2C'        DUPKEYADD/PHYSLOCK/SECLOCK                   
         BZ    *+12                                                             
         MVI   HDRN,4                                                           
         B     DIOMISS1                                                         
         DC    H'0'                                                             
DIOMISS1 EQU   *                                                                
*                                                                               
DISKIOX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*KEYTBL  FOR EACH KEY THIS TABLE CONTAINS                             *         
*        CL2   FIRST TWO CHRS OF NAME                                 *         
*        XL1   NUMBER                                                 *         
*        XL1   X'01'=VALID FOR RECORD  X'02'=VALID FOR ELEMENT        *         
***********************************************************************         
KEYTBL   DS    0CL4                                                             
         DC    C'K,',AL1(01),X'01'                                              
         DC    C'A,',AL1(02),X'01'                                              
         DC    C'FI',AL1(03),X'03'                                              
         DC    C'NE',AL1(04),X'01'                                              
         DC    C'S,',AL1(05),X'02'                                              
         DC    C'I,',AL1(06),X'02'                                              
         DC    C'LA',AL1(07),X'03'                                              
         DC    C'F=',AL1(08),X'03'                                              
         DC    C'G,',AL1(09),X'01'                                              
         DC    C'G=',AL1(10),X'01'                                              
KEYTBLX  DC    X'00'                                                            
                                                                                
***********************************************************************         
*ACTNTBL FOR EACH ACTION THIS TABLE CONTAINS                          *         
*        CL7   ACTION NAME                                            *         
*        XL1   NUMBER                                                 *         
*        XL1   X'01'=VALID FOR RECORD  X'02'=VALID FOR ELEMENT        *         
***********************************************************************         
ACTNTBL  DS    0CL9                                                             
         DC    C'DISPLAY',AL1(01),X'03'                                         
         DC    C'CHANGE ',AL1(02),X'03'                                         
         DC    C'ADD    ',AL1(03),X'03'                                         
         DC    C'BROWSE ',AL1(04),X'01'                                         
         DC    C'REPEAT ',AL1(05),X'01'                                         
         DC    C'COPY   ',AL1(06),X'02'                                         
ACTNTBLX DC    X'00'                                                            
                                                                                
***********************************************************************         
*THIS TABLE TRANSLATES A COMMAND NO (DMCMDN) TO A COMMAND NAME        *         
***********************************************************************         
DMCMDS   DS    0CL6                                                             
         DC    CL6'DMADD '         1                                            
         DC    CL6'DMREAD'         2                                            
         DC    CL6'DMRSEQ'         3                                            
         DC    CL6'DMRDHI'         4                                            
         DC    CL6'DMDEL '         5                                            
         DC    CL6'DMWRT '         6                                            
         DC    CL6'DMRDIR'         7                                            
         DC    CL6'ADDREC'         8                                            
         DC    CL6'PUTREC'         9                                            
         DC    CL6'GETREC'         A                                            
         DC    CL6'ADFREC'         B                                            
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
*                                                                               
DLWRHDR  DC    C'---BYTES---'                                                   
DUPRHDR  DC    C'***BYTES***'                                                   
HLWRHDR  DC    C'----HEX----'                                                   
HUPRHDR  DC    C'****HEX****'                                                   
*                                                                               
DWDISP   DC    H'20'               DECIMAL DISPLAY CHR WIDTH                    
DNDISP   DC    H'14'               DECIMAL NUM OF DISPLAY LINES                 
HWDISP   DC    H'16'               HEX DISPLAY CHR WIDTH                        
HNDISP   DC    H'16'               HEX NUM OF DISPLAY LINES                     
*                                                                               
DLINLEN  DC    Y(PFMDL2OH-PFMDL1OH)                                             
HLINLEN  DC    Y(PFMHL2OH-PFMHL1OH)                                             
*                                                                               
DCOUNT   DC    C'0.1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6.7.8.9.'                      
         DC    C'0.1.2.3.4.5.6.7.8.9.0.'                                        
DCHARS   DC    C'0.2.4.6.8.0.2.4.6.8.0.2.4.6.8.0'                               
HCOUNT   DC    C'0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.0.1.2.3.4.5.6.7.8.'            
         DC    C'9.A.B.C.D.E.F.'                                                
HCHARS   DC    C'0.2.4.6.8.A.C.E.0.2.4.6.8.A.C.E.0.'                            
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
*THIS TABLE CONTAINS HEADER MSGS INDEXED BY HDRN                      *         
***********************************************************************         
OKMSGS   DS    0CL45                                                            
OK0      DC    CL45'PFM - ACTION COMPLETED'                                     
OK1      DC    CL45'PFM - ENTER NEW RECORD DATA'                                
OK2      DC    CL45'PFM - ENTER RECORD DATA CHANGES'                            
OK3      DC    CL45'PFM - ENTER ELEMENT DATA'                                   
OK4      DC    CL45'PFM - ELEMENT DISPLAYED HIT <ENTER> TO COPY'                
                                                                                
***********************************************************************         
*THIS TABLE CONTAINS ERROR MSGS INDEXED BY FERN                       *         
***********************************************************************         
ERRMSGS  DS    0CL20                                                            
ERR01    DC    CL20'MISSING FILE NAME'                                          
ERR02    DC    CL20'INVALID FILE NAME'                                          
ERR03    DC    CL20'MISSING RECORD ID'                                          
ERR04    DC    CL20'INVALID RECORD ID'                                          
ERR05    DC    CL20'SCAN SYNTAX'                                                
ERR06    DC    CL20'INVALID ELEMENT ID'                                         
ERR07    DC    CL20'INVALID HEXADECIMAL'                                        
ERR08    DC    CL20'ELEMENT NOT FOUND'                                          
ERR09    DC    CL20'DISK ADDR <> 8 CHRS'                                        
ERR10    DC    CL20'INVALID DISK ADDRESS'                                       
ERR11    DC    CL20'NO PREV REC FOR FILE'                                       
ERR12    DC    CL20'INVALID START'                                              
ERR13    DC    CL20'START NON-NUMERIC'                                          
ERR14    DC    CL20'INVALID END'                                                
ERR15    DC    CL20'END NON-NUMERIC'                                            
ERR16    DC    CL20'START > END'                                                
ERR17    DC    CL20'END > RECORD LENGTH'                                        
ERR18    DC    CL20'START > RECORD LEN'                                         
ERR19    DC    CL20'START MUST BE ZERO'                                         
ERR20    DC    CL20'MISSING END'                                                
ERR21    DC    CL20'END TOO BIG'                                                
ERR22    DC    CL20'END <> RECORD LEN-1'                                        
ERR23    DC    CL20'END TOO SMALL'                                              
ERR24    DC    CL20'CAN`T FIT ON SCREEN'                                        
ERR25    DC    CL20'INVALID ACTION NAME'                                        
ERR26    DC    CL20'ACTION NOT AVAILABLE'                                       
ERR27    DC    CL20'NOT DEFINED FOR FILE'                                       
ERR28    DC    CL20'INPUT TOO LONG'                                             
ERR29    DC    CL20'DISK ERROR'                                                 
ERR30    DC    CL20'EOF-NO RECORD FOUND'                                        
ERR31    DC    CL20'RECORD NOT FOUND'                                           
ERR32    DC    CL20'CONTENTION LOCKOUT'                                         
ERR33    DC    CL20'REC. ALREADY EXISTS'                                        
ERR34    DC    CL20'TERM DUE TO SIZE ERR'                                       
ERR35    DC    CL20'TERM DUE TO DISK ERR'                                       
ERR36    DC    CL20'TERM DUE TO EOF'                                            
ERR37    DC    CL20'TERM DUE TO N-FOUND'                                        
ERR38    DC    CL20'TERM DUE TO CONTENTN'                                       
ERR39    DC    CL20'CAN`T CHANGE KEY'                                           
ERR40    DC    CL20'CAN`T CHANGE DATA'                                          
ERR41    DC    CL20'CAN`T CHNG HEX&&CHAR'                                       
ERR42    DC    CL20'CAN`T CHANGE EL. LEN'                                       
ERR43    DC    CL20'INVALID EL. ID CODE'                                        
ERR44    DC    CL20'INVALID SYSTEM NAME'                                        
ERR45    DC    CL20'SYSTEM IS NOT OPENED'                                       
ERR46    DC    CL20'INVALID INPUT FIELD'                                        
ERR47    DC    CL20'CURSOR NOT ON KEY L1'                                       
ERR48    DC    CL20'2ND LINE ACTIVATED'                                         
ERR49    DC    CL20'   PFM INTERFACE'                                           
ERR50    DC    CL20'2ND LINE DEACTIVATED'                                       
ERR51    DC    CL20'PFM SCREEN CHANGED'                                         
ERR52    DC    CL20'1000 RECS RD HIT NTR'                                       
ERR53    DC    CL20'KEY TOO SHORT'                                              
                                                                                
***********************************************************************         
*THIS TABLE DEFINES THE DISPLAYABLE CHARACTERS                        *         
***********************************************************************         
DISPTBLU DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'406F6F6F6F6F6F6F6F6F4A4B4C4D4E4F'  40-4F                    
         DC    XL16'506F6F6F6F6F6F6F6F6F5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60616F6F6F6F6F6F6F6F6A6B6C6D6E6F'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F797A7B7C7D7E7F'  70-7F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  80-8F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F9F'  90-9F                    
         DC    XL16'6FA16F6F6F6F6F6F6F6F6F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C96F6F6F6F6F6F'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D96F6F6F6F6F6F'  D0-D1                    
         DC    XL16'E06FE2E3E4E5E6E7E8E96F6F6F6F6F6F'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
                                                                                
DISPTBLL DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'406F6F6F6F6F6F6F6F6F4A4B4C4D4E4F'  40-4F                    
         DC    XL16'506F6F6F6F6F6F6F6F6F5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60616F6F6F6F6F6F6F6F6A6B6C6D6E6F'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F797A7B7C7D7E7F'  70-7F                    
         DC    XL16'6F8182838485868788896F6F6F6F6F6F'  80-8F                    
         DC    XL16'6F9192939495969798996F6F6F6F6F9F'  90-9F                    
         DC    XL16'6FA1A2A3A4A5A6A7A8A96F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C96F6F6F6F6F6F'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D96F6F6F6F6F6F'  D0-D1                    
         DC    XL16'E06FE2E3E4E5E6E7E8E96F6F6F6F6F6F'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
                                                                                
SYSTBL   DS    0H                                                               
*&&UK                                                                           
         DC    X'04',C'L',AL2(MEDFILS-FILETBL,MEDACTS-PERMTBL)                  
         DC    X'05',C'L',AL2(MPLFILS-FILETBL,MPLACTS-PERMTBL)                  
         DC    X'06',C'L',AL2(ACCFILS-FILETBL,ACCACTS-PERMTBL)                  
         DC    X'07',C'L',AL2(FEEFILS-FILETBL,FEEACTS-PERMTBL)                  
         DC    X'09',C'L',AL2(MBAFILS-FILETBL,MBAACTS-PERMTBL)                  
         DC    X'0A',C'L',AL2(CONFILS-FILETBL,CONACTS-PERMTBL)                  
         DC    X'0E',C'L',AL2(PERFILS-FILETBL,PERACTS-PERMTBL)                  
*&&                                                                             
*&&US                                                                           
         DC    X'02',C'L',AL2(SPTFILS-FILETBL,SPTACTS-PERMTBL)                  
         DC    X'03',C'L',AL2(SPTFILS-FILETBL,SPTACTS-PERMTBL)                  
         DC    X'04',C'L',AL2(PRTFILS-FILETBL,PRTACTS-PERMTBL)                  
         DC    X'05',C'U',AL2(MPLFILS-FILETBL,MPLACTS-PERMTBL)                  
         DC    X'06',C'U',AL2(ACCFILS-FILETBL,ACCACTS-PERMTBL)                  
         DC    X'07',C'U',AL2(TALFILS-FILETBL,TALACTS-PERMTBL)                  
         DC    X'08',C'U',AL2(REPFILS-FILETBL,REPACTS-PERMTBL)                  
         DC    X'09',C'L',AL2(MBAFILS-FILETBL,MBAACTS-PERMTBL)                  
         DC    X'0A',C'L',AL2(CONFILS-FILETBL,CONACTS-PERMTBL)                  
         DC    X'0C',C'U',AL2(CPPFILS-FILETBL,CPPACTS-PERMTBL)                  
         DC    X'0D',C'U',AL2(TRFFILS-FILETBL,TRFACTS-PERMTBL)                  
         DC    X'0E',C'L',AL2(PERFILS-FILETBL,PERACTS-PERMTBL)                  
*&&                                                                             
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*FILETBL FOR EACH FILE THIS TABLE CONTAINS                            *         
*        CL8   NAME                00=END-OF-TABLE                    *         
*        XL1   NUMBER              80 BIT ON FOR SYNONYM              *         
*        XL1   TYPE ORGANISATION   01=SEQ,02=IS,03=DA,04=DAL          *         
*        XL1   TYPE LOGICAL        01=REQ,02=RCV,80=DUMMY SYSAVE      *         
*                                  40=BLKED RCVR                      *         
*        XL1   RECORD TYPE         01=FIX,02=V/L,03=V/L/ELM           *         
*        XL2   MAX REC LEN - 1                                        *         
*        XL1   KEY LEN                                                *         
*        XL1   CONTROL LENGTH                                         *         
*        XL1   SYS SAVE LEN                                           *         
*        XL1   START BYTE REC LEN  FF=NOT STORED IN RECORD            *         
*        XL1   1ST KEY BYTE NUM    FF=NO 1ST KEY SETTING              *         
*        XL1   KEY FILL CHARACTER                                     *         
***********************************************************************         
FILETBL  DS    0H                                                               
*&&US                                                                           
SPTFILS  DS    0H                                                               
         DC    C'SPTDIR  ',X'01020001',H'00017',X'0D0100FF',X'0C00'             
         DC    C'RECOVER ',X'02034202',H'13312',X'000000FF',X'0300'             
         DC    C'REQUEST ',X'03030102',H'00959',X'020000FF',X'0300'             
         DC    C'SPTFILE ',X'04040003',H'05972',X'0D03080D',X'FF00'             
         DC    C'STATION ',X'05020002',H'01023',X'0F01000F',X'0E00'             
         DC    C'DEMDIR  ',X'06020001',H'00022',X'120100FF',X'1100'             
         DC    C'PAVDIR  ',X'07020001',H'00022',X'120100FF',X'1100'             
         DC    C'UNTDIR  ',X'08020001',H'00024',X'140100FF',X'1300'             
         DC    C'UNTFILE ',X'09040003',H'05972',X'14030414',X'FF00'             
         DC    C'XSPDIR  ',X'0A020001',H'00039',X'200400FF',X'1F00'             
         DC    C'XSPFIL  ',X'0B040003',H'05972',X'20060420',X'FF00'             
         DC    C'NTIDIR  ',X'0C020001',H'00022',X'120100FF',X'1100'             
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'84',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'STA     ',X'85',XL11'00'                                       
         DC    C'DD      ',X'86',XL11'00'                                       
         DC    C'PD      ',X'87',XL11'00'                                       
SPTFILSX DC    X'00'                                                            
*                                                                               
TRFFILS  DS    0H                                                               
         DC    C'TRFDIR  ',X'01020001',H'00017',X'0D0100FF',X'0C00'             
         DC    C'TRFRCVR ',X'02034202',H'13312',X'000000FF',X'0300'             
         DC    C'TRFREQ  ',X'03030102',H'00959',X'020000FF',X'0300'             
         DC    C'TRFFILE ',X'04040003',H'03972',X'0D03080D',X'FF00'             
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'84',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
TRFFILSX DC    X'00'                                                            
*                                                                               
PRTFILS  DS    0H                                                               
         DC    C'PRTDIR  ',X'01020001',H'0030',X'190200FF',X'1800'              
         DC    C'PRECOVE ',X'02034202',H'9191',X'000000FF',X'0300'              
         DC    C'PREQUES ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'PRTFILE ',X'04040003',H'4000',X'19040419',X'FF00'              
         DC    C'PUBDIR  ',X'05020001',H'0030',X'190200FF',X'1800'              
         DC    C'PUBFILE ',X'06040003',H'4000',X'19040419',X'FF00'              
         DC    C'BDIR    ',X'85',XL11'00'                                       
         DC    C'BFIL    ',X'86',XL11'00'                                       
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'84',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'TDIR    ',X'81',XL11'00'                                       
         DC    C'TFIL    ',X'84',XL11'00'                                       
PRTFILSX DC    X'00'                                                            
*                                                                               
MPLFILS  DS    0H                                                               
         DC    C'MPLDIR  ',X'01020001',H'0039',X'200400FF',X'1F00'              
         DC    C'MPLFIL  ',X'02040003',H'1999',X'20060420',X'FF00'              
         DC    C'MPLREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'MPLRCV  ',X'04034202',H'8191',X'000000FF',X'0300'              
         DC    C'MPQDRA  ',X'05020001',H'0039',X'200400FF',X'1F00'              
         DC    C'MPQDRA  ',X'06020001',H'0039',X'200400FF',X'1F00'              
         DC    C'MPQFLA  ',X'07040003',H'1999',X'20060420',X'FF00'              
         DC    C'MPRDRA  ',X'08020001',H'0031',X'130900FF',X'1200'              
         DC    C'MPRFLA  ',X'09040003',H'1999',X'130B0413',X'FF00'              
         DC    C'CTFILE  ',X'0A020003',H'0999',X'19030019',X'1800'              
         DC    C'BUDDIR  ',X'0B020001',H'0039',X'200400FF',X'1F00'              
         DC    C'BUDFIL  ',X'0C040003',H'1999',X'20060420',X'FF00'              
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'RCV     ',X'84',XL11'00'                                       
         DC    C'QDIR    ',X'86',XL11'00'                                       
         DC    C'QDRA    ',X'86',XL11'00'                                       
         DC    C'QFIL    ',X'87',XL11'00'                                       
         DC    C'QFLA    ',X'87',XL11'00'                                       
         DC    C'RDIR    ',X'88',XL11'00'                                       
         DC    C'RDRA    ',X'88',XL11'00'                                       
         DC    C'RFIL    ',X'89',XL11'00'                                       
         DC    C'RFLA    ',X'89',XL11'00'                                       
         DC    C'CON     ',X'8A',XL11'00'                                       
MPLFILSX DC    X'00'                                                            
*                                                                               
ACCFILS  DS    0H                                                               
         DC    C'ACCOUNT ',X'01028003',H'1999',X'2A03042A',X'2940'              
         DC    C'ACCRCVR ',X'02034202',H'8191',X'000000FF',X'0300'              
         DC    C'ACCREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'ACCDAY  ',X'04030003',H'1999',X'02000000',X'0300'              
         DC    C'ACCWRK  ',X'05030001',H'3599',X'000000FF',X'0300'              
         DC    C'ACCDIR  ',X'06020001',H'0053',X'2A0800FF',X'2900'              
         DC    C'ACCMST  ',X'07040003',H'1999',X'2A0A042A',X'FF00'              
         DC    C'ACCARC  ',X'08040003',H'1999',X'2A0A042A',X'FF00'              
         DC    C'CTFILE  ',X'09020003',H'0999',X'19030019',X'1800'              
         DC    C'ACCHST  ',X'0A028003',H'1999',X'2A03042A',X'2940'              
         DC    C'ARC     ',X'88',XL11'00'                                       
         DC    C'CON     ',X'89',XL11'00'                                       
         DC    C'DAY     ',X'84',XL11'00'                                       
         DC    C'DIR     ',X'86',XL11'00'                                       
         DC    C'FIL     ',X'81',XL11'00'                                       
         DC    C'HST     ',X'8A',XL11'00'                                       
         DC    C'MST     ',X'87',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'WRK     ',X'85',XL11'00'                                       
ACCFILSX DC    X'00'                                                            
*                                                                               
TALFILS  DS    0H                                                               
         DC    C'TALDIR  ',X'01020001',H'00037',X'200200FF',X'1F00'             
         DC    C'TALFIL  ',X'02040003',H'03989',X'20040420',X'FF00'             
         DC    C'TALREQ  ',X'03030102',H'00959',X'020000FF',X'0300'             
         DC    C'TALRCV  ',X'04034202',H'13312',X'000000FF',X'0300'             
         DC    C'CTFILE  ',X'05020003',H'00999',X'19030019',X'1800'             
         DC    C'CHKDIR  ',X'06020001',H'00037',X'200200FF',X'1F00'             
         DC    C'CHKFIL  ',X'07040003',H'03989',X'20040420',X'FF00'             
         DC    C'CON     ',X'86',XL11'00'                                       
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'82',XL11'00'                                       
         DC    C'RCV     ',X'84',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
TALFILSX DC    X'00'                                                            
*                                                                               
REPFILS  DS    0H                                                               
         DC    C'REPDIR  ',X'01020001',H'0031',X'1B0100FF',X'1A00'              
         DC    C'REPRCVR ',X'02034202',H'8200',X'000000FF',X'0300'              
         DC    C'REPREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'REPFILE ',X'04040003',H'3975',X'1B03041B',X'FF00'              
         DC    C'REPWRK  ',X'05030001',H'3659',X'000000FF',X'0300'              
         DC    C'RRGNEW  ',X'06020001',H'0115',X'3000FFFF',X'2F00'              
         DC    C'ROIDIR  ',X'07020001',H'0035',X'1F0100FF',X'1A00'              
         DC    C'ROIFILE ',X'08040003',H'2000',X'1F03041F',X'FF00'              
         DC    C'REVIEW  ',X'09020003',H'2047',X'30010030',X'2F00'              
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'84',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
REPFILSX DC    X'00'                                                            
*                                                                               
MBAFILS  DS    0H                                                               
         DC    C'MBADIR  ',X'01020001',H'0043',X'200800FF',X'1F00'              
         DC    C'MBAFIL  ',X'02040003',H'1999',X'200A0420',X'FF00'              
         DC    C'MBAREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'MBARCV  ',X'04034202',H'8191',X'000000FF',X'0300'              
         DC    C'MBANDX  ',X'05020001',H'0043',X'200800FF',X'1F00'              
         DC    C'MBUDIR  ',X'06020001',H'0043',X'200800FF',X'1F00'              
         DC    C'MBUFIL  ',X'07040003',H'1999',X'200A0420',X'FF00'              
         DC    C'CTFILE  ',X'08020003',H'0999',X'19030019',X'1800'              
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'RCV     ',X'84',XL11'00'                                       
         DC    C'NDX     ',X'85',XL11'00'                                       
         DC    C'DU      ',X'86',XL11'00'                                       
         DC    C'FU      ',X'87',XL11'00'                                       
         DC    C'CON     ',X'88',XL11'00'                                       
MBAFILSX DC    X'00'                                                            
*                                                                               
CONFILS  DS    0H                                                               
         DC    C'CTFILE  ',X'01020003',H'04096',X'19030019',X'1800'             
         DC    C'CTRCVR  ',X'02034202',H'08191',X'000000FF',X'0300'             
         DC    C'CTREQ   ',X'03030102',H'00959',X'020000FF',X'0300'             
         DC    C'GENDIR  ',X'04020001',H'00039',X'200400FF',X'1F00'             
         DC    C'GENFIL  ',X'05040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'06040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'07040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'08040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'09040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0A040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0B040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0C040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0D040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0E040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0F040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'10040003',H'01999',X'20060420',X'FF00'             
         DC    C'PRTQ1   ',X'11030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ2   ',X'12030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ3   ',X'13030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ4   ',X'14030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ5   ',X'15030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ6   ',X'16030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ7   ',X'17030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ8   ',X'18030001',H'13679',X'000000FF',X'0300'             
         DC    C'EDCTA   ',X'19030001',H'18431',X'000000FF',X'0300'             
         DC    C'EDCTR   ',X'1A030001',H'18431',X'000000FF',X'0300'             
         DC    C'WRKF1   ',X'1B030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF2   ',X'1C030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF3   ',X'1D030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF4   ',X'1E030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF5   ',X'1F030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF6   ',X'20030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF7   ',X'21030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF8   ',X'22030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ9   ',X'23030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQA   ',X'24030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQB   ',X'25030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQC   ',X'26030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQD   ',X'27030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQE   ',X'28030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQF   ',X'29030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQG   ',X'2A030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF9   ',X'2B030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFA   ',X'2C030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFB   ',X'2D030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFC   ',X'2E030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFD   ',X'2F030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFE   ',X'30030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFF   ',X'31030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFG   ',X'32030001',H'13679',X'000000FF',X'0300'             
         DC    C'WKFILE  ',X'33030001',H'03659',X'000000FF',X'0300'             
         DC    C'FACWRK  ',X'34030001',H'06139',X'000000FF',X'0300'             
         DC    C'WRKZ1   ',X'35030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ2   ',X'36030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ3   ',X'37030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ4   ',X'38030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ5   ',X'39030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ6   ',X'3A030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ7   ',X'3B030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ8   ',X'3C030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZ9   ',X'3D030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZA   ',X'3E030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZB   ',X'3F030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZC   ',X'40030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZD   ',X'41030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZE   ',X'42030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZF   ',X'43030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKZG   ',X'44030001',H'13679',X'000000FF',X'0300'             
         DC    C'CON     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'81',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'GDIR    ',X'84',XL11'00'                                       
         DC    C'GFIL    ',X'85',XL11'00'                                       
CONFILSX DC    X'00'                                                            
*                                                                               
CPPFILS  DS    0H                                                               
         DC    C'CPFILE  ',X'01020003',H'0999',X'10030010',X'0F00'              
         DC    C'CPRCVR  ',X'02034202',H'8191',X'000000FF',X'0300'              
         DC    C'CPREQ   ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'FIL     ',X'81',11X'00'                                        
         DC    C'RCV     ',X'82',11X'00'                                        
         DC    C'REQ     ',X'83',11X'00'                                        
CPPFILSX DC    X'00'                                                            
*                                                                               
PERFILS  DS    0H                                                               
         DC    C'PERDIR  ',X'01020001',H'0041',X'240200FF',X'2300'              
         DC    C'PERRCV  ',X'02034202',H'8191',X'000000FF',X'0300'              
         DC    C'PERREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'PERFIL  ',X'04040003',H'1023',X'24040424',X'FF00'              
         DC    C'CTFILE  ',X'05020003',H'0999',X'19030019',X'1800'              
         DC    C'CON     ',X'85',XL11'00'                                       
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'84',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
PERFILSX DC    X'00'                                                            
*&&                                                                             
         EJECT                                                                  
*&&UK                                                                           
MEDFILS  DS    0H                                                               
         DC    C'MEDIR   ',X'01020001',H'0031',X'140800FF',X'1300'              
         DC    C'RECOVER ',X'02034202',H'8191',X'000000FF',X'0300'              
         DC    C'REQUEST ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'MEDFILE ',X'04040003',H'1999',X'140A0414',X'FF00'              
         DC    C'MEDWRK  ',X'05030001',H'3599',X'000000FF',X'0300'              
         DC    C'DMGDIR  ',X'06020001',H'0031',X'140800FF',X'1300'              
         DC    C'DMGFIL  ',X'07040003',H'3999',X'140A0414',X'FF00'              
         DC    C'DMNDIR  ',X'08020001',H'0031',X'140800FF',X'1300'              
         DC    C'DMNNEW  ',X'09040003',H'4999',X'140A0414',X'FF00'              
         DC    C'DMNOLD  ',X'0A040003',H'4999',X'140A0414',X'FF00'              
         DC    C'DMODIR  ',X'0B020001',H'0031',X'140800FF',X'1300'              
         DC    C'DMO1FL  ',X'0C040003',H'4999',X'140A0414',X'FF00'              
         DC    C'DMO2FL  ',X'0D040003',H'4999',X'140A0414',X'FF00'              
         DC    C'DMO3FL  ',X'0E040003',H'4999',X'140A0414',X'FF00'              
         DC    C'DMO4FL  ',X'0F040003',H'4999',X'140A0414',X'FF00'              
         DC    C'CTFILE  ',X'10020003',H'0999',X'19030019',X'1800'              
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'84',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'WRK     ',X'85',XL11'00'                                       
         DC    C'GD      ',X'86',XL11'00'                                       
         DC    C'GF      ',X'87',XL11'00'                                       
         DC    C'ND      ',X'88',XL11'00'                                       
         DC    C'NN      ',X'89',XL11'00'                                       
         DC    C'NO      ',X'8A',XL11'00'                                       
         DC    C'OD      ',X'8B',XL11'00'                                       
         DC    C'O1      ',X'8C',XL11'00'                                       
         DC    C'O2      ',X'8D',XL11'00'                                       
         DC    C'O3      ',X'8E',XL11'00'                                       
         DC    C'O4      ',X'8F',XL11'00'                                       
         DC    C'CON     ',X'90',XL11'00'                                       
MEDFILSX DC    X'00'                                                            
*                                                                               
MPLFILS  DS    0H                                                               
         DC    C'MPLDIR  ',X'01020001',H'0039',X'200400FF',X'1F00'              
         DC    C'MPLFIL  ',X'02040003',H'1999',X'20060420',X'FF00'              
         DC    C'MPLREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'MPLRCV  ',X'04034202',H'8191',X'000000FF',X'0300'              
         DC    C'MPLRCV**',X'05034202',H'8191',X'000000FF',X'0300'              
         DC    C'MPLRCV**',X'06034202',H'8191',X'000000FF',X'0300'              
         DC    C'MPLRCV**',X'07034202',H'8191',X'000000FF',X'0300'              
         DC    C'MPLRCV**',X'08034202',H'8191',X'000000FF',X'0300'              
         DC    C'MPLRCV**',X'09034202',H'8191',X'000000FF',X'0300'              
         DC    C'CTFILE  ',X'0A020003',H'0999',X'19030019',X'1800'              
         DC    C'CTFILE**',X'0B020003',H'0999',X'19030019',X'1800'              
         DC    C'CTFILE**',X'0C020003',H'0999',X'19030019',X'1800'              
         DC    C'CTFILE**',X'0D020003',H'0999',X'19030019',X'1800'              
         DC    C'CTFILE**',X'0E020003',H'0999',X'19030019',X'1800'              
         DC    C'MPCDRC  ',X'0F020001',H'0019',X'090700FF',X'1200'              
         DC    C'MPCFLC  ',X'10040003',H'2047',X'09090409',X'FF00'              
         DC    C'MPCDRS  ',X'11020001',H'0019',X'090700FF',X'1200'              
         DC    C'MPCFLS  ',X'12040003',H'2047',X'09090409',X'FF00'              
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'RCV     ',X'84',XL11'00'                                       
         DC    C'CON     ',X'8A',XL11'00'                                       
         DC    C'CRADC   ',X'8F',XL11'00'                                       
         DC    C'CRAFC   ',X'90',XL11'00'                                       
         DC    C'CRADS   ',X'91',XL11'00'                                       
         DC    C'CRAFS   ',X'92',XL11'00'                                       
MPLFILSX DC    X'00'                                                            
*                                                                               
ACCFILS  DS    0H                                                               
         DC    C'ACCOUNT ',X'01028003',H'1999',X'2A03042A',X'2940'              
         DC    C'ACCRCVR ',X'02034202',H'8191',X'000000FF',X'0300'              
         DC    C'ACCREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'ACCDAY  ',X'04030003',H'1099',X'02000000',X'0300'              
         DC    C'ACCWRK  ',X'05030001',H'3599',X'000000FF',X'0300'              
         DC    C'ACCDIR  ',X'06020001',H'0053',X'2A0800FF',X'2900'              
         DC    C'ACCMST  ',X'07040003',H'1999',X'2A0A042A',X'FF00'              
         DC    C'ACCARC  ',X'08040003',H'1999',X'2A0A042A',X'FF00'              
         DC    C'CTFILE  ',X'09020003',H'0999',X'19030019',X'1800'              
         DC    C'ARC     ',X'88',XL11'00'                                       
         DC    C'CON     ',X'89',XL11'00'                                       
         DC    C'DAY     ',X'84',XL11'00'                                       
         DC    C'DIR     ',X'86',XL11'00'                                       
         DC    C'FIL     ',X'81',XL11'00'                                       
         DC    C'MST     ',X'87',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'WRK     ',X'85',XL11'00'                                       
ACCFILSX DC    X'00'                                                            
*                                                                               
FEEFILS  DS    0H                                                               
         DC    C'FEEDIR  ',AL1(01,02,00,01),H'0039',X'200400FF',X'1F00'         
         DC    C'FEEFIL  ',AL1(02,04,00,03),H'3999',X'20060420',X'FF00'         
         DC    C'FEEREQ  ',AL1(03,03,01,02),H'0959',X'020000FF',X'0300'         
         DC    C'FEERCV  ',AL1(04,03,00,02),H'8191',X'000000FF',X'0300'         
         DC    C'CTFILE  ',AL1(05,02,00,03),H'0999',X'19030019',X'1800'         
         DC    C'CON     ',X'86',XL11'00'                                       
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'82',XL11'00'                                       
         DC    C'RCV     ',X'84',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
FEEFILSX DC    X'00'                                                            
*                                                                               
MBAFILS  DS    0H                                                               
         DC    C'MBADIR  ',X'01020001',H'0043',X'200800FF',X'1F00'              
         DC    C'MBAFIL  ',X'02040003',H'1999',X'200A0420',X'FF00'              
         DC    C'MBAREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'MBARCV  ',X'04030002',H'8191',X'000000FF',X'0300'              
         DC    C'MBANDX  ',X'05020001',H'0043',X'200800FF',X'1F00'              
         DC    C'MBUDIR  ',X'06020001',H'0043',X'200800FF',X'1F00'              
         DC    C'MBUFIL  ',X'07040003',H'1999',X'200A0420',X'FF00'              
         DC    C'CTFILE  ',X'08020003',H'0999',X'19030019',X'1800'              
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'RCV     ',X'84',XL11'00'                                       
         DC    C'NDX     ',X'85',XL11'00'                                       
         DC    C'DU      ',X'86',XL11'00'                                       
         DC    C'FU      ',X'87',XL11'00'                                       
         DC    C'CON     ',X'88',XL11'00'                                       
MBAFILSX DC    X'00'                                                            
*                                                                               
CONFILS  DS    0H                                                               
         DC    C'CTFILE  ',X'01020003',H'04096',X'19030019',X'1800'             
         DC    C'CTRCVR  ',X'02034202',H'08191',X'000000FF',X'0300'             
         DC    C'CTREQ   ',X'03030102',H'00959',X'020000FF',X'0300'             
         DC    C'GENDIR  ',X'04020001',H'00039',X'200400FF',X'1F00'             
         DC    C'GENFIL  ',X'05040003',H'01999',X'20060420',X'FF00'             
         DC    C'CRADIR  ',X'06020001',H'00016',X'080500FF',X'0700'             
         DC    C'CRAFIL  ',X'07040003',H'01999',X'08070408',X'FF00'             
         DC    C'GENFIL**',X'08040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'09040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0A040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0B040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0C040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0D040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0E040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'0F040003',H'01999',X'20060420',X'FF00'             
         DC    C'GENFIL**',X'10040003',H'01999',X'20060420',X'FF00'             
         DC    C'PRTQ1   ',X'11030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ2   ',X'12030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ3   ',X'13030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ4   ',X'14030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ5   ',X'15030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ6   ',X'16030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ7   ',X'17030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ8   ',X'18030001',H'13679',X'000000FF',X'0300'             
         DC    C'*DCTA   ',X'19030001',H'18431',X'000000FF',X'0300'             
         DC    C'*DCTR   ',X'1A030001',H'18431',X'000000FF',X'0300'             
         DC    C'WRKF1   ',X'1B030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF2   ',X'1C030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF3   ',X'1D030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF4   ',X'1E030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF5   ',X'1F030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF6   ',X'20030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF7   ',X'21030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF8   ',X'22030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQ9   ',X'23030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQA   ',X'24030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQB   ',X'25030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQC   ',X'26030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQD   ',X'27030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQE   ',X'28030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQF   ',X'29030001',H'13679',X'000000FF',X'0300'             
         DC    C'PRTQG   ',X'2A030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKF9   ',X'2B030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFA   ',X'2C030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFB   ',X'2D030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFC   ',X'2E030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFD   ',X'2F030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFE   ',X'30030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFF   ',X'31030001',H'13679',X'000000FF',X'0300'             
         DC    C'WRKFG   ',X'32030001',H'13679',X'000000FF',X'0300'             
         DC    C'WKFILE  ',X'33030001',H'03659',X'000000FF',X'0300'             
         DC    C'FACWRK  ',X'34030001',H'03659',X'000000FF',X'0300'             
         DC    C'CON     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'81',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
         DC    C'GDIR    ',X'84',XL11'00'                                       
         DC    C'GFIL    ',X'85',XL11'00'                                       
         DC    C'CDIR    ',X'86',XL11'00'                                       
         DC    C'CFIL    ',X'87',XL11'00'                                       
*                                                                               
CONFILSX DC    X'00'                                                            
*                                                                               
PERFILS  DS    0H                                                               
         DC    C'PERDIR  ',X'01020001',H'0041',X'240200FF',X'2300'              
         DC    C'PERRCV  ',X'02034202',H'8191',X'000000FF',X'0300'              
         DC    C'PERREQ  ',X'03030102',H'0959',X'020000FF',X'0300'              
         DC    C'PERFIL  ',X'04040003',H'1999',X'24040424',X'FF00'              
         DC    C'CTFILE  ',X'05020003',H'0999',X'19030019',X'1800'              
         DC    C'CON     ',X'85',XL11'00'                                       
         DC    C'DIR     ',X'81',XL11'00'                                       
         DC    C'FIL     ',X'84',XL11'00'                                       
         DC    C'RCV     ',X'82',XL11'00'                                       
         DC    C'REQ     ',X'83',XL11'00'                                       
PERFILSX DC    X'00'                                                            
*                                                                               
*&&                                                                             
FILETBLX DS    0X                                                               
         EJECT                                                                  
***********************************************************************         
*PERMTBL FOR EACH VALID REQUEST THIS TABLE CONTAINS                   *         
*        XL3   FILE NUM , KEY NUM , ACTION NUM                        *         
*        XL1   ASSOCIATED FILE FOR F&G ACTIONS (NULL IF NO FILE)      *         
*        XL3   STATUS 0 FIRST I/O INFO                                *         
*        XL3   STATUS 0 SECND I/O INFO                                *         
*        XL3   STATUS 1 FIRST I/O INFO                                *         
*        XL3   STATUS 1 SECND I/O INFO                                *         
*        I/O INFO CONTAINS                                            *         
*        XL1   I/O COMMAND NO                                         *         
*        XL1   SET UP KEY/ADR INSTRUCTION                             *         
*        XL1   SAVE KEY/ADR INSTRUCTION                               *         
*        SET UP VALUES                                                *         
*        0     NOTHING                                                *         
*        1     POINT DMCB3 TO STIK                                    *         
*        2     POINT DMCB3 TO SLIK                                    *         
*        3     SPECIAL                                                *         
*        SAVE VALUES                                                  *         
*        0     NOTHING                                                *         
*        1     SAVE ADR FROM DMCB3 (& DISPLAY IT)                     *         
*        2     SAVE KEY FROM RECORD                                   *         
*        3     SAVE KEY/ADR FROM STIK                                 *         
***********************************************************************         
PERMTBL  DS    0H                                                               
*&&US                                                                           
SPTACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' SPTDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' SPTDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' SPTDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' SPTDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' SPTDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' SPTDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' SPTDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' SPTDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' SPTDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' SPTDIR/F=/BRO            
         DC    X'010901',X'040401040A0102000000000000' SPTDIR/G,/DIS            
         DC    X'010904',X'040401040A0102000000000000' SPTDIR/G,/BRO            
         DC    X'010A04',X'040401040A0102000000000000' SPTDIR/G=/BRO            
         DC    X'020201',X'00070101000000000000000000' RECOVER/A,/DI            
         DC    X'020204',X'00070101000000000000000000' RECOVER/A,/BR            
         DC    X'020301',X'00030101000000000000000000' RECOVER/FI/DI            
         DC    X'020304',X'00030101000000000000000000' RECOVER/FI/BR            
         DC    X'020401',X'00030201000000000000000000' RECOVER/NE/DI            
         DC    X'020404',X'00030201000000000000000000' RECOVER/NE/BRO           
         DC    X'020804',X'00030101000000000000000000' RECOVER/F=/BRO           
         DC    X'030101',X'00070101000000000000000000' REQUEST/K,/DIS           
         DC    X'030103',X'00000000000000000000010001' REQUEST/K,/ADD           
         DC    X'030201',X'00070101000000000000000000' REQUEST/A,/DIS           
         DC    X'030202',X'00070101000000070200060201' REQUEST/A,/CHA           
         DC    X'030301',X'00030101000000000000000000' REQUEST/FI/DIS           
         DC    X'030304',X'00030101000000000000000000' REQUEST/FI/BRO           
         DC    X'030401',X'00030201000000000000000000' REQUEST/NE/DIS           
         DC    X'030404',X'00030201000000000000000000' REQUEST/NE/BRO           
         DC    X'030804',X'00030101000000000000000000' REQUEST/F=/BRO           
         DC    X'040103',X'00020103000000000000080201' SPTFILE/K,/ADD           
         DC    X'040105',X'000201000000000A0200080201' SPTFILE/K,/REP           
         DC    X'040201',X'000A0103000000000000000000' SPTFILE/A,/DIS           
         DC    X'040202',X'000A01030000000A0200090200' SPTFILE/A,/CHA           
         DC    X'050101',X'00040102000000000000000000' STATION/K,/DIS           
         DC    X'050102',X'00020102000000020200060200' STATION/K,/CHA           
         DC    X'050103',X'00020103000000000000010200' STATION/K,/ADD           
         DC    X'050104',X'00040102000000000000000000' STATION/K,/BRO           
         DC    X'050105',X'00020100000000020200010103' STATION/K,/REP           
         DC    X'050301',X'00040102000000000000000000' STATION/FI/DIS           
         DC    X'050304',X'00040102000000000000000000' STATION/FI/BRO           
         DC    X'050401',X'00020202030202000000000000' STATION/NE/DIS           
         DC    X'050404',X'00020202030202000000000000' STATION/NE/BRO           
         DC    X'050702',X'00020102000000020200060200' STATION/LA/CHA           
         DC    X'050804',X'00040102000000000000000000' STATION/F=/BRO           
         DC    X'060101',X'00040102000000000000000000' DEMDIR/K,/DIS            
         DC    X'060104',X'00040102000000000000000000' DEMDIR/K,/BRO            
         DC    X'060301',X'00040102000000000000000000' DEMDIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' DEMDIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' DEMDIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' DEMDIR/NE/BRO            
         DC    X'070101',X'00040102000000000000000000' PAVDIR/K,/DIS            
         DC    X'070104',X'00040102000000000000000000' PAVDIR/K,/BRO            
         DC    X'070301',X'00040102000000000000000000' PAVDIR/FI/DIS            
         DC    X'070304',X'00040102000000000000000000' PAVDIR/FI/BRO            
         DC    X'070401',X'00020202030202000000000000' PAVDIR/NE/DIS            
         DC    X'070404',X'00020202030202000000000000' PAVDIR/NE/BRO            
         DC    X'080101',X'00040102000000000000000000' UNTDIR/K,/DIS            
         DC    X'080102',X'00020102000000020200060200' UNTDIR/K,/CHA            
         DC    X'080103',X'00020103000000000000010200' UNTDIR/K,/ADD            
         DC    X'080104',X'00040102000000000000000000' UNTDIR/K,/BRO            
         DC    X'080301',X'00040102000000000000000000' UNTDIR/FI/DIS            
         DC    X'080304',X'00040102000000000000000000' UNTDIR/FI/BRO            
         DC    X'080401',X'00020202030202000000000000' UNTDIR/NE/DIS            
         DC    X'080404',X'00020202030202000000000000' UNTDIR/NE/BRO            
         DC    X'080702',X'00020102000000020200060200' UNTDIR/LA/CHA            
         DC    X'080804',X'00040102000000000000000000' UNTDIR/F=/BRO            
         DC    X'080901',X'090401040A0102000000000000' UNTDIR/G,/DIS            
         DC    X'080904',X'090401040A0102000000000000' UNTDIR/G,/BRO            
         DC    X'080A04',X'090401040A0102000000000000' UNTDIR/G=/BRO            
         DC    X'090103',X'00020103000000000000080201' UNTFILE/K,/ADD           
         DC    X'090105',X'000201000000000A0200080201' UNTFILE/K,/REP           
         DC    X'090201',X'000A0103000000000000000000' UNTFILE/A,/DIS           
         DC    X'090202',X'000A01030000000A0200090200' UNTFILE/A,/CHA           
         DC    X'0A0101',X'00040102000000000000000000' XSPDIR/K,/DIS            
         DC    X'0A0102',X'00020102000000020200060200' XSPDIR/K,/CHA            
         DC    X'0A0103',X'00020103000000000000010200' XSPDIR/K,/ADD            
         DC    X'0A0104',X'00040102000000000000000000' XSPDIR/K,/BRO            
         DC    X'0A0301',X'00040102000000000000000000' XSPDIR/FI/DIS            
         DC    X'0A0304',X'00040102000000000000000000' XSPDIR/FI/BRO            
         DC    X'0A0401',X'00020202030202000000000000' XSPDIR/NE/DIS            
         DC    X'0A0404',X'00020202030202000000000000' XSPDIR/NE/BRO            
         DC    X'0A0702',X'00020102000000020200060200' XSPDIR/LA/CHA            
         DC    X'0A0804',X'00040102000000000000000000' XSPDIR/F=/BRO            
         DC    X'0A0901',X'0B0401040A0102000000000000' XSPDIR/G,/DIS            
         DC    X'0A0904',X'0B0401040A0102000000000000' XSPDIR/G,/BRO            
         DC    X'0A0A04',X'0B0401040A0102000000000000' XSPDIR/G=/BRO            
         DC    X'0B0103',X'00020103000000000000080201' XSPFIL/K,/ADD            
         DC    X'0B0105',X'000201000000000A0200080201' XSPFIL/K,/REP            
         DC    X'0B0201',X'000A0103000000000000000000' XSPFIL/A,/DIS            
         DC    X'0B0202',X'000A01030000000A0200090200' XSPFIL/A,/CHA            
         DC    X'0C0101',X'00040102000000000000000000' NTIDIR/K,/DIS            
         DC    X'0C0104',X'00040102000000000000000000' NTIDIR/K,/BRO            
         DC    X'0C0301',X'00040102000000000000000000' NTIDIR/FI/DIS            
         DC    X'0C0304',X'00040102000000000000000000' NTIDIR/FI/BRO            
         DC    X'0C0401',X'00020202030202000000000000' NTIDIR/NE/DIS            
         DC    X'0C0404',X'00020202030202000000000000' NTIDIR/NE/BRO            
SPTACTSX DC    X'00'                                                            
*                                                                               
TRFACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' TRFDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' TRFDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' TRFDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' TRFDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' TRFDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' TRFDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' TRFDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' TRFDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' TRFDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' TRFDIR/F=/BRO            
         DC    X'010901',X'040401040A0102000000000000' TRFDIR/G,/DIS            
         DC    X'010904',X'040401040A0102000000000000' TRFDIR/G,/BRO            
         DC    X'010A04',X'040401040A0102000000000000' TRFDIR/G=/BRO            
         DC    X'020201',X'00070101000000000000000000' TRFRCVR/A,/DIS           
         DC    X'020204',X'00070101000000000000000000' TRFRCVR/A,/BRO           
         DC    X'020301',X'00030101000000000000000000' TRFRCVR/FI/DIS           
         DC    X'020304',X'00030101000000000000000000' TRFRCVR/FI/BRO           
         DC    X'020401',X'00030201000000000000000000' TRFRCVR/NE/DIS           
         DC    X'020404',X'00030201000000000000000000' TRFRCVR/NE/BRO           
         DC    X'020804',X'00030101000000000000000000' TRFRCVR/F=/BRO           
         DC    X'030101',X'00070101000000000000000000' TRFREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' TRFREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' TRFREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' TRFREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' TRFREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' TRFREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' TRFREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' TRFREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' TRFREQ/F=/BRO            
         DC    X'040103',X'00020103000000000000080201' TRFFILE/K,/ADD           
         DC    X'040105',X'000201000000000A0200080201' TRFFILE/K,/REP           
         DC    X'040201',X'000A0103000000000000000000' TRFFILE/A,/DIS           
         DC    X'040202',X'000A01030000000A0200090200' TRFFILE/A,/CHA           
TRFACTSX DC    X'00'                                                            
*                                                                               
PRTACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' PRTDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' PRTDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' PRTDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' PRTDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' PRTDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' PRTDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' PRTDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' PRTDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' PRTDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' PRTDIR/F=/BRO            
         DC    X'010901',X'040401040A0102000000000000' PRTDIR/G,/DIS            
         DC    X'010904',X'040401040A0102000000000000' PRTDIR/G,/BRO            
         DC    X'010A04',X'040401040A0102000000000000' PRTDIR/G=/BRO            
         DC    X'020201',X'00070101000000000000000000' PRTRCVR/A,/DIS           
         DC    X'020204',X'00070101000000000000000000' PRTRCVR/A,/BRO           
         DC    X'020301',X'00030101000000000000000000' PRTRCVR/FI/DIS           
         DC    X'020304',X'00030101000000000000000000' PRTRCVR/FI/BRO           
         DC    X'020401',X'00030201000000000000000000' PRTRCVR/NE/DIS           
         DC    X'020404',X'00030201000000000000000000' PRTRCVR/NE/BRO           
         DC    X'020804',X'00030101000000000000000000' PRTRCVR/F=/BRO           
         DC    X'030101',X'00070101000000000000000000' PREQUES/K,/DIS           
         DC    X'030103',X'00000000000000000000010001' PREQUES/K,/ADD           
         DC    X'030201',X'00070101000000000000000000' PREQUES/A,/DIS           
         DC    X'030202',X'00070101000000070200060201' PREQUES/A,/CHA           
         DC    X'030301',X'00030101000000000000000000' PREQUES/FI/DIS           
         DC    X'030304',X'00030101000000000000000000' PREQUES/FI/BRO           
         DC    X'030401',X'00030201000000000000000000' PREQUES/NE/DIS           
         DC    X'030404',X'00030201000000000000000000' PREQUES/NE/BRO           
         DC    X'030804',X'00030101000000000000000000' PREQUES/F=/BRO           
         DC    X'040103',X'00020103000000000000080201' PRTFILE/K,/ADD           
         DC    X'040105',X'000201000000000A0200080201' PRTFILE/K,/REP           
         DC    X'040201',X'000A0103000000000000000000' PRTFILE/A,/DIS           
         DC    X'040202',X'000A01030000000A0200090200' PRTFILE/A,/CHA           
         DC    X'050101',X'00040102000000000000000000' PUBDIR/K,/DIS            
         DC    X'050102',X'00020102000000020200060200' PUBDIR/K,/CHA            
         DC    X'050103',X'00020103000000000000010200' PUBDIR/K,/ADD            
         DC    X'050104',X'00040102000000000000000000' PUBDIR/K,/BRO            
         DC    X'050301',X'00040102000000000000000000' PUBDIR/FI/DIS            
         DC    X'050304',X'00040102000000000000000000' PUBDIR/FI/BRO            
         DC    X'050401',X'00020202030202000000000000' PUBDIR/NE/DIS            
         DC    X'050404',X'00020202030202000000000000' PUBDIR/NE/BRO            
         DC    X'050702',X'00020102000000020200060200' PUBDIR/LA/CHA            
         DC    X'050804',X'00040102000000000000000000' PUBDIR/F=/BRO            
         DC    X'050901',X'060401040A0102000000000000' PUBDIR/G,/DIS            
         DC    X'050904',X'060401040A0102000000000000' PUBDIR/G,/BRO            
         DC    X'050A04',X'060401040A0102000000000000' PUBDIR/G=/BRO            
         DC    X'060103',X'00020103000000000000080201' PUBFILE/K,/ADD           
         DC    X'060105',X'000201000000000A0200080201' PUBFILE/K,/REP           
         DC    X'060201',X'000A0103000000000000000000' PUBFILE/A,/DIS           
         DC    X'060202',X'000A01030000000A0200090200' PUBFILE/A,/CHA           
PRTACTSX DC    X'00'                                                            
*                                                                               
MPLACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' MPLDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' MPLDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' MPLDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' MPLDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' MPLDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' MPLDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' MPLDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' MPLDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' MPLDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' MPLDIR/F=/BRO            
         DC    X'010901',X'020401040A0102000000000000' MPLDIR/G,/DIS            
         DC    X'010904',X'020401040A0102000000000000' MPLDIR/G,/BRO            
         DC    X'010A04',X'020401040A0102000000000000' MPLDIR/G=/BRO            
         DC    X'020103',X'00020103000000000000080201' MPLFIL/K,/ADD            
         DC    X'020105',X'000201000000000A0200080201' MPLFIL/K,/REP            
         DC    X'020201',X'000A0103000000000000000000' MPLFIL/A,/DIS            
         DC    X'020202',X'000A01030000000A0200090200' MPLFIL/A,/CHA            
         DC    X'030101',X'00070101000000000000000000' MPLREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' MPLREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' MPLREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' MPLREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' MPLREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' MPLREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' MPLREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' MPLREQ/NE/BRO            
         DC    X'040201',X'00070101000000000000000000' MPLRCV/A,/DIS            
         DC    X'040204',X'00070101000000000000000000' MPLRCV/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' MPLRCV/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' MPLRCV/FI/BRO            
         DC    X'040401',X'00030201000000000000000000' MPLRCV/NE/DIS            
         DC    X'040404',X'00030201000000000000000000' MPLRCV/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' MPLRCV/F=/BRO            
         DC    X'0A0101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'0A0104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'0A0301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'0A0304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'0A0401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'0A0404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'0A0804',X'00040102000000000000000000' CTFILE/F=/BRO            
MPLACTSX DC    X'00'                                                            
*                                                                               
ACCACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' ACCOUNT/K,/DIS           
         DC    X'010102',X'00020102000000020200060200' ACCOUNT/K,/CHA           
         DC    X'010103',X'00020103000000000000010200' ACCOUNT/K,/ADD           
         DC    X'010104',X'00040102000000000000000000' ACCOUNT/K,/BRO           
         DC    X'010105',X'00020100000000020200010103' ACCOUNT/K,/REP           
         DC    X'010301',X'00040102000000000000000000' ACCOUNT/FI/DIS           
         DC    X'010304',X'00040102000000000000000000' ACCOUNT/FI/BRO           
         DC    X'010401',X'00020202030202000000000000' ACCOUNT/NE/DIS           
         DC    X'010404',X'00020202030202000000000000' ACCOUNT/NE/BRO           
         DC    X'010702',X'00020102000000020200060200' ACCOUNT/LA/CHA           
         DC    X'010804',X'00040102000000000000000000' ACCOUNT/F=/BRO           
         DC    X'020201',X'00070101000000000000000000' ACCRCVR/A,/DIS           
         DC    X'020204',X'00070101000000000000000000' ACCRCVR/A,/BRO           
         DC    X'020301',X'00030101000000000000000000' ACCRCVR/FI/DIS           
         DC    X'020304',X'00030101000000000000000000' ACCRCVR/FI/BRO           
         DC    X'020401',X'00030201000000000000000000' ACCRCVR/NE/DIS           
         DC    X'020404',X'00030201000000000000000000' ACCRCVR/NE/BRO           
         DC    X'020804',X'00030101000000000000000000' ACCRCVR/F=/BRO           
         DC    X'030101',X'00070101000000000000000000' ACCREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' ACCREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' ACCREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' ACCREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' ACCREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' ACCREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' ACCREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' ACCREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' ACCREQ/F=/BRO            
         DC    X'040201',X'00070101000000000000000000' ACCDAY/A,/DIS            
         DC    X'040202',X'00070101000000000000060201' ACCDAY/A,/CHA            
         DC    X'040204',X'00030101000000000000000000' ACCDAY/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' ACCDAY/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' ACCDAY/FI/BRO            
         DC    X'040401',X'00030101000000000000000000' ACCDAY/NE/DIS            
         DC    X'040404',X'00030101000000000000000000' ACCDAY/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' ACCREQ/F=/BRO            
         DC    X'050201',X'00020101000000000000000000' ACCWRK/A,/DIS            
         DC    X'060101',X'00040102000000000000000000' ACCDIR/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' ACCDIR/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' ACCDIR/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' ACCDIR/K,/BRO            
         DC    X'060105',X'00020100000000020200010103' ACCDIR/K,/REP            
         DC    X'060301',X'00040102000000000000000000' ACCDIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' ACCDIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' ACCDIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' ACCDIR/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' ACCDIR/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' ACCDIR/F=/BRO            
         DC    X'060901',X'070401040A0102000000000000' ACCDIR/G,/DIS            
         DC    X'060904',X'070401040A0102000000000000' ACCDIR/G,/BRO            
         DC    X'060A04',X'070401040A0102000000000000' ACCDIR/G=/BRO            
         DC    X'070103',X'00020103000000000000080201' ACCMST/K,/ADD            
         DC    X'070105',X'000201000000000A0200080201' ACCMST/K,/REP            
         DC    X'070201',X'000A0103000000000000000000' ACCMST/A,/DIS            
         DC    X'070202',X'000A01030000000A0200090200' ACCMST/A,/CHA            
         DC    X'080103',X'00020103000000000000080201' ACCARC/K,/ADD            
         DC    X'080105',X'000201000000000A0200080201' ACCARC/K,/REP            
         DC    X'080201',X'000A0103000000000000000000' ACCARC/A,/DIS            
         DC    X'080202',X'000A01030000000A0200090200' ACCARC/A,/CHA            
         DC    X'090101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'090104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'090301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'090304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'090401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'090404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'090804',X'00040102000000000000000000' CTFILE/F=/BRO            
         DC    X'0A0101',X'00040102000000000000000000' ACCHST/K,/DIS            
         DC    X'0A0102',X'00020102000000020200060200' ACCHST/K,/CHA            
         DC    X'0A0103',X'00020103000000000000010200' ACCHST/K,/ADD            
         DC    X'0A0104',X'00040102000000000000000000' ACCHST/K,/BRO            
         DC    X'0A0105',X'00020100000000020200010103' ACCHST/K,/REP            
         DC    X'0A0301',X'00040102000000000000000000' ACCHST/FI/DIS            
         DC    X'0A0304',X'00040102000000000000000000' ACCHST/FI/BRO            
         DC    X'0A0401',X'00020202030202000000000000' ACCHST/NE/DIS            
         DC    X'0A0404',X'00020202030202000000000000' ACCHST/NE/BRO            
         DC    X'0A0702',X'00020102000000020200060200' ACCHST/LA/CHA            
         DC    X'0A0804',X'00040102000000000000000000' ACCHST/F=/BRO            
ACCACTSX DC    X'00'                                                            
*                                                                               
TALACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' TALDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' TALDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' TALDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' TALDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' TALDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' TALDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' TALDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' TALDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' TALDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' TALDIR/F=/BRO            
         DC    X'010901',X'020401040A0102000000000000' TALDIR/G,/DIS            
         DC    X'010904',X'020401040A0102000000000000' TALDIR/G,/BRO            
         DC    X'010A04',X'020401040A0102000000000000' TALDIR/G=/BRO            
         DC    X'020103',X'00020103000000000000080201' TALFIL/K,/ADD            
         DC    X'020105',X'000201000000000A0200080201' TALFIL/K,/REP            
         DC    X'020201',X'000A0103000000000000000000' TALFIL/A,/DIS            
         DC    X'020202',X'000A01030000000A0200090200' TALFIL/A,/CHA            
         DC    X'030101',X'00070101000000000000000000' TALREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' TALREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' TALREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' TALREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' TALREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' TALREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' TALREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' TALREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' TALREQ/F=/BRO            
         DC    X'040201',X'00070101000000000000000000' TALRCV/A,/DIS            
         DC    X'040204',X'00070101000000000000000000' TALRCV/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' TALRCV/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' TALRCV/FI/BRO            
         DC    X'040401',X'00030201000000000000000000' TALRCV/NE/DIS            
         DC    X'040404',X'00030201000000000000000000' TALRCV/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' TALRCV/F=/BRO            
         DC    X'050101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'050104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'050301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'050304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'050401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'050404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'050804',X'00040102000000000000000000' CTFILE/F=/BRO            
         DC    X'060101',X'00040102000000000000000000' CHKDIR/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' CHKDIR/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' CHKDIR/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' CHKDIR/K,/BRO            
         DC    X'060301',X'00040102000000000000000000' CHKDIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' CHKDIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' CHKDIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' CHKDIR/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' CHKDIR/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' CHKDIR/F=/BRO            
         DC    X'060901',X'070401040A0102000000000000' CHKDIR/G,/DIS            
         DC    X'060904',X'070401040A0102000000000000' CHKDIR/G,/BRO            
         DC    X'060A04',X'070401040A0102000000000000' CHKDIR/G=/BRO            
         DC    X'070103',X'00020103000000000000080201' CHKFIL/K,/ADD            
         DC    X'070105',X'000201000000000A0200080201' CHKFIL/K,/REP            
         DC    X'070201',X'000A0103000000000000000000' CHKFIL/A,/DIS            
         DC    X'070202',X'000A01030000000A0200090200' CHKFIL/A,/CHA            
TALACTSX DC    X'00'                                                            
*                                                                               
REPACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' REPDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' REPDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' REPDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' REPDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' REPDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' REPDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' REPDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' REPDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' REPDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' REPDIR/F=/BRO            
         DC    X'010901',X'040401040A0102000000000000' REPDIR/G,/DIS            
         DC    X'010904',X'040401040A0102000000000000' REPDIR/G,/BRO            
         DC    X'010A04',X'040401040A0102000000000000' REPDIR/G=/BRO            
         DC    X'020201',X'00070101000000000000000000' REPRCVR/A,/DIS           
         DC    X'020204',X'00070101000000000000000000' REPRCVR/A,/BRO           
         DC    X'020301',X'00030101000000000000000000' REPRCVR/FI/DIS           
         DC    X'020304',X'00030101000000000000000000' REPRCVR/FI/BRO           
         DC    X'020401',X'00030201000000000000000000' REPRCVR/NE/DIS           
         DC    X'020404',X'00030201000000000000000000' REPRCVR/NE/BRO           
         DC    X'020804',X'00030101000000000000000000' REPRCVR/F=/BRO           
         DC    X'030101',X'00070101000000000000000000' REPREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' REPREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' REPREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' REPREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' REPREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' REPREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' REPREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' REPREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' REPREQ/F=/BRO            
         DC    X'040103',X'00020103000000000000080201' REPFILE/K,/ADD           
         DC    X'040105',X'000201000000000A0200080201' REPFILE/K,/REP           
         DC    X'040201',X'000A0103000000000000000000' REPFILE/A,/DIS           
         DC    X'040202',X'000A01030000000A0200090200' REPFILE/A,/CHA           
         DC    X'050201',X'00020101000000000000000000' REPWRK/A,/DIS            
         DC    X'060101',X'00040102000000000000000000' RRGNEW/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' RRGNEW/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' RRGNEW/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' RRGNEW/K,/BRO            
         DC    X'060301',X'00040102000000000000000000' RRGNEW/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' RRGNEW/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' RRGNEW/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' RRGNEW/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' RRGNEW/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' RRGNEW/F=/BRO            
         DC    X'070101',X'00040102000000000000000000' ROIDIR/K,/DIS            
         DC    X'070102',X'00020102000000020200060200' ROIDIR/K,/CHA            
         DC    X'070103',X'00020103000000000000010200' ROIDIR/K,/ADD            
         DC    X'070104',X'00040102000000000000000000' ROIDIR/K,/BRO            
         DC    X'070301',X'00040102000000000000000000' ROIDIR/FI/DIS            
         DC    X'070304',X'00040102000000000000000000' ROIDIR/FI/BRO            
         DC    X'070401',X'00020202030202000000000000' ROIDIR/NE/DIS            
         DC    X'070404',X'00020202030202000000000000' ROIDIR/NE/BRO            
         DC    X'070702',X'00020102000000020200060200' ROIDIR/LA/CHA            
         DC    X'070804',X'00040102000000000000000000' ROIDIR/F=/BRO            
         DC    X'070901',X'080401040A0102000000000000' ROIDIR/G,/DIS            
         DC    X'070904',X'080401040A0102000000000000' ROIDIR/G,/BRO            
         DC    X'070A04',X'080401040A0102000000000000' ROIDIR/G=/BRO            
         DC    X'080103',X'00020103000000000000080201' ROIFILE/K,/ADD           
         DC    X'080105',X'000201000000000A0200080201' ROIFILE/K,/REP           
         DC    X'080201',X'000A0103000000000000000000' ROIFILE/A,/DIS           
         DC    X'080202',X'000A01030000000A0200090200' ROIFILE/A,/CHA           
         DC    X'090101',X'00040102000000000000000000' RRGNEW/K,/DIS            
         DC    X'090102',X'00020102000000020200060200' REVIEW/K,/CHA            
         DC    X'090104',X'00040102000000000000000000' REVIEW/K,/BRO            
         DC    X'090301',X'00040102000000000000000000' REVIEW/FI/DIS            
         DC    X'090304',X'00040102000000000000000000' REVIEW/FI/BRO            
         DC    X'090401',X'00020202030202000000000000' REVIEW/NE/DIS            
         DC    X'090404',X'00020202030202000000000000' REVIEW/NE/BRO            
REPACTSX DC    X'00'                                                            
*                                                                               
MBAACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' MBADIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' MBADIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' MBADIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' MBADIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' MBADIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' MBADIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' MBADIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' MBADIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' MBADIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' MBADIR/F=/BRO            
         DC    X'010901',X'020401040A0102000000000000' MBADIR/G,/DIS            
         DC    X'010904',X'020401040A0102000000000000' MBADIR/G,/BRO            
         DC    X'010A04',X'020401040A0102000000000000' MBADIR/G=/BRO            
         DC    X'020103',X'00020103000000000000080201' MBAFIL/K,/ADD            
         DC    X'020105',X'000201000000000A0200080201' MBAFIL/K,/REP            
         DC    X'020201',X'000A0103000000000000000000' MBAFIL/A,/DIS            
         DC    X'020202',X'000A01030000000A0200090200' MBAFIL/A,/CHA            
         DC    X'030101',X'00070101000000000000000000' MBAREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' MBAREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' MBAREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' MBAREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' MBAREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' MBAREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' MBAREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' MBAREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' MBAREQ/F=/BRO            
         DC    X'040201',X'00070101000000000000000000' MBARCV/A,/DIS            
         DC    X'040204',X'00070101000000000000000000' MBARCV/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' MBARCV/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' MBARCV/FI/BRO            
         DC    X'040401',X'00030201000000000000000000' MBARCV/NE/DIS            
         DC    X'040404',X'00030201000000000000000000' MBARCV/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' MBARCV/F=/BRO            
         DC    X'050101',X'00040102000000000000000000' MBANDX/K,/DIS            
         DC    X'050104',X'00040102000000000000000000' MBANDX/K,/BRO            
         DC    X'050301',X'00040102000000000000000000' MBANDX/FI/DIS            
         DC    X'050304',X'00040102000000000000000000' MBANDX/FI/BRO            
         DC    X'050401',X'00020202030202000000000000' MBANDX/NE/DIS            
         DC    X'050404',X'00020202030202000000000000' MBANDX/NE/BRO            
         DC    X'050804',X'00040102000000000000000000' MBANDX/F=/BRO            
         DC    X'060101',X'00040102000000000000000000' MBUDIR/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' MBUDIR/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' MBUDIR/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' MBUDIR/K,/BRO            
         DC    X'060301',X'00040102000000000000000000' MBUDIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' MBUDIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' MBUDIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' MBUDIR/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' MBUDIR/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' MBUDIR/F=/BRO            
         DC    X'060901',X'070401040A0102000000000000' MBUDIR/G,/DIS            
         DC    X'060904',X'070401040A0102000000000000' MBUDIR/G,/BRO            
         DC    X'060A04',X'070401040A0102000000000000' MBUDIR/G=/BRO            
         DC    X'070103',X'00020103000000000000080201' MBUFIL/K,/ADD            
         DC    X'070105',X'000201000000000A0200080201' MBUFIL/K,/REP            
         DC    X'070201',X'000A0103000000000000000000' MBUFIL/A,/DIS            
         DC    X'070202',X'000A01030000000A0200090200' MBUFIL/A,/CHA            
         DC    X'080101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'080104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'080301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'080304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'080401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'080404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'080804',X'00040102000000000000000000' CTFILE/F=/BRO            
MBAACTSX DC    X'00'                                                            
*                                                                               
CONACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' CTFILE/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' CTFILE/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'010105',X'00020100000000020200010103' CTFILE/K,/REP            
         DC    X'010301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' CTFILE/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' CTFILE/F=/BRO            
         DC    X'020201',X'00070101000000000000000000' CTRCVR/A,/DIS            
         DC    X'020204',X'00070101000000000000000000' CTRCVR/A,/BRO            
         DC    X'020301',X'00030101000000000000000000' CTRCVR/FI/DIS            
         DC    X'020304',X'00030101000000000000000000' CTRCVR/FI/BRO            
         DC    X'020401',X'00030201000000000000000000' CTRCVR/NE/DIS            
         DC    X'020404',X'00030201000000000000000000' CTRCVR/NE/BRO            
         DC    X'020804',X'00030101000000000000000000' CTRCVR/F=/BRO            
         DC    X'030101',X'00070101000000000000000000' CTREQ/K,/DIS             
         DC    X'030103',X'00000000000000000000010001' CTREQ/K,/ADD             
         DC    X'030201',X'00070101000000000000000000' CTREQ/A,/DIS             
         DC    X'030202',X'00070101000000070200060201' CTREQ/A,/CHA             
         DC    X'030301',X'00030101000000000000000000' CTREQ/FI/DIS             
         DC    X'030304',X'00030101000000000000000000' CTREQ/FI/BRO             
         DC    X'030401',X'00030201000000000000000000' CTREQ/NE/DIS             
         DC    X'030404',X'00030201000000000000000000' CTREQ/NE/BRO             
         DC    X'030804',X'00030101000000000000000000' CTREQ/F=/BRO             
         DC    X'040101',X'00040102000000000000000000' GENDIR/K,/DIS            
         DC    X'040102',X'00020102000000020200060200' GENDIR/K,/CHA            
         DC    X'040103',X'00020103000000000000010200' GENDIR/K,/ADD            
         DC    X'040104',X'00040102000000000000000000' GENDIR/K,/BRO            
         DC    X'040301',X'00040102000000000000000000' GENDIR/FI/DIS            
         DC    X'040304',X'00040102000000000000000000' GENDIR/FI/BRO            
         DC    X'040401',X'00020202030202000000000000' GENDIR/NE/DIS            
         DC    X'040404',X'00020202030202000000000000' GENDIR/NE/BRO            
         DC    X'040702',X'00020102000000020200060200' GENDIR/LA/CHA            
         DC    X'040804',X'00040102000000000000000000' GENDIR/F=/BRO            
         DC    X'040901',X'050401040A0102000000000000' GENDIR/G,/DIS            
         DC    X'040904',X'050401040A0102000000000000' GENDIR/G,/BRO            
         DC    X'040A04',X'050401040A0102000000000000' GENDIR/G=/BRO            
         DC    X'050103',X'00020103000000000000080201' GENFIL/K,/ADD            
         DC    X'050105',X'000201000000000A0200080201' GENFIL/K,/REP            
         DC    X'050201',X'000A0103000000000000000000' GENFIL/A,/DIS            
         DC    X'050202',X'000A01030000000A0200090200' GENFIL/A,/CHA            
         DC    X'110201',X'00020101000000000000000000' PRTQ1/A,/DIS             
         DC    X'110202',X'00020101000000020200060201' PRTQ1/A,/CHA             
         DC    X'120201',X'00020101000000000000000000' PRTQ2/A,/DIS             
         DC    X'120202',X'00020101000000020200060201' PRTQ2/A,/CHA             
         DC    X'130201',X'00020101000000000000000000' PRTQ3/A,/DIS             
         DC    X'130202',X'00020101000000020200060201' PRTQ3/A,/CHA             
         DC    X'140201',X'00020101000000000000000000' PRTQ4/A,/DIS             
         DC    X'140202',X'00020101000000020200060201' PRTQ4/A,/CHA             
         DC    X'150201',X'00020101000000000000000000' PRTQ5/A,/DIS             
         DC    X'150202',X'00020101000000020200060201' PRTQ5/A,/CHA             
         DC    X'160201',X'00020101000000000000000000' PRTQ6/A,/DIS             
         DC    X'160202',X'00020101000000020200060201' PRTQ6/A,/CHA             
         DC    X'170201',X'00020101000000000000000000' PRTQ7/A,/DIS             
         DC    X'170202',X'00020101000000020200060201' PRTQ7/A,/CHA             
         DC    X'180201',X'00020101000000000000000000' PRTQ8/A,/DIS             
         DC    X'180202',X'00020101000000020200060201' PRTQ8/A,/CHA             
         DC    X'190201',X'00070101000000000000000000' EDCTA/A,/DIS             
         DC    X'190202',X'00070101000000070200060201' EDCTA/A,/CHA             
         DC    X'1A0201',X'00070101000000000000000000' EDCTR/A,/DIS             
         DC    X'1A0202',X'00070101000000070200060201' EDCTR/A,/CHA             
         DC    X'1B0201',X'00020101000000000000000000' WRKF1/A,/DIS             
         DC    X'1B0202',X'00020101000000020200060201' WRKF1/A,/CHA             
         DC    X'1C0201',X'00020101000000000000000000' WRKF2/A,/DIS             
         DC    X'1C0202',X'00020101000000020200060201' WRKF2/A,/CHA             
         DC    X'1D0201',X'00020101000000000000000000' WRKF3/A,/DIS             
         DC    X'1D0202',X'00020101000000020200060201' WRKF3/A,/CHA             
         DC    X'1E0201',X'00020101000000000000000000' WRKF4/A,/DIS             
         DC    X'1E0202',X'00020101000000020200060201' WRKF4/A,/CHA             
         DC    X'1F0201',X'00020101000000000000000000' WRKF5/A,/DIS             
         DC    X'1F0202',X'00020101000000020200060201' WRKF5/A,/CHA             
         DC    X'200201',X'00020101000000000000000000' WRKF6/A,/DIS             
         DC    X'200202',X'00020101000000020200060201' WRKF6/A,/CHA             
         DC    X'210201',X'00020101000000000000000000' WRKF7/A,/DIS             
         DC    X'210202',X'00020101000000020200060201' WRKF7/A,/CHA             
         DC    X'220201',X'00020101000000000000000000' WRKF8/A,/DIS             
         DC    X'220202',X'00020101000000020200060201' WRKF8/A,/CHA             
         DC    X'2B0201',X'00020101000000000000000000' WRKF9/A,/DIS             
         DC    X'2B0202',X'00020101000000020200060201' WRKF9/A,/CHA             
         DC    X'2C0201',X'00020101000000000000000000' WRKFA/A,/DIS             
         DC    X'2C0202',X'00020101000000020200060201' WRKFA/A,/CHA             
         DC    X'2D0201',X'00020101000000000000000000' WRKFB/A,/DIS             
         DC    X'2D0202',X'00020101000000020200060201' WRKFB/A,/CHA             
         DC    X'2E0201',X'00020101000000000000000000' WRKFC/A,/DIS             
         DC    X'2E0202',X'00020101000000020200060201' WRKFC/A,/CHA             
         DC    X'2F0201',X'00020101000000000000000000' WRKFD/A,/DIS             
         DC    X'2F0202',X'00020101000000020200060201' WRKFD/A,/CHA             
         DC    X'300201',X'00020101000000000000000000' WRKFE/A,/DIS             
         DC    X'300202',X'00020101000000020200060201' WRKFE/A,/CHA             
         DC    X'310201',X'00020101000000000000000000' WRKFF/A,/DIS             
         DC    X'310202',X'00020101000000020200060201' WRKFF/A,/CHA             
         DC    X'320201',X'00020101000000000000000000' WRKFG/A,/DIS             
         DC    X'320202',X'00020101000000020200060201' WRKFG/A,/CHA             
*                                                                               
         DC    X'330201',X'00020101000000000000000000' WKFILE/A,/DIS            
         DC    X'330202',X'00020101000000020200060201' WKFILE/A,/CHA            
         DC    X'340201',X'00020101000000000000000000' FACWRK/A,/DIS            
         DC    X'340202',X'00020101000000020200060201' FACWRK/A,/CHA            
*                                                                               
         DC    X'350201',X'00020101000000000000000000' WRKZ1/A,/DIS             
         DC    X'350202',X'00020101000000020200060201' WRKZ1/A,/CHA             
         DC    X'360201',X'00020101000000000000000000' WRKZ2/A,/DIS             
         DC    X'360202',X'00020101000000020200060201' WRKZ2/A,/CHA             
         DC    X'370201',X'00020101000000000000000000' WRKZ3/A,/DIS             
         DC    X'370202',X'00020101000000020200060201' WRKZ3/A,/CHA             
         DC    X'380201',X'00020101000000000000000000' WRKZ4/A,/DIS             
         DC    X'380202',X'00020101000000020200060201' WRKZ4/A,/CHA             
         DC    X'390201',X'00020101000000000000000000' WRKZ5/A,/DIS             
         DC    X'390202',X'00020101000000020200060201' WRKZ5/A,/CHA             
         DC    X'3A0201',X'00020101000000000000000000' WRKZ6/A,/DIS             
         DC    X'3A0202',X'00020101000000020200060201' WRKZ6/A,/CHA             
         DC    X'3B0201',X'00020101000000000000000000' WRKZ7/A,/DIS             
         DC    X'3B0202',X'00020101000000020200060201' WRKZ7/A,/CHA             
         DC    X'3C0201',X'00020101000000000000000000' WRKZ8/A,/DIS             
         DC    X'3C0202',X'00020101000000020200060201' WRKZ8/A,/CHA             
         DC    X'3D0201',X'00020101000000000000000000' WRKZ9/A,/DIS             
         DC    X'3D0202',X'00020101000000020200060201' WRKZ9/A,/CHA             
         DC    X'3E0201',X'00020101000000000000000000' WRKZA/A,/DIS             
         DC    X'3E0202',X'00020101000000020200060201' WRKZA/A,/CHA             
         DC    X'3F0201',X'00020101000000000000000000' WRKZB/A,/DIS             
         DC    X'3F0202',X'00020101000000020200060201' WRKZB/A,/CHA             
         DC    X'400201',X'00020101000000000000000000' WRKZC/A,/DIS             
         DC    X'400202',X'00020101000000020200060201' WRKZC/A,/CHA             
         DC    X'410201',X'00020101000000000000000000' WRKZD/A,/DIS             
         DC    X'410202',X'00020101000000020200060201' WRKZD/A,/CHA             
         DC    X'420201',X'00020101000000000000000000' WRKZE/A,/DIS             
         DC    X'420202',X'00020101000000020200060201' WRKZE/A,/CHA             
         DC    X'430201',X'00020101000000000000000000' WRKZF/A,/DIS             
         DC    X'430202',X'00020101000000020200060201' WRKZF/A,/CHA             
         DC    X'440201',X'00020101000000000000000000' WRKZG/A,/DIS             
         DC    X'440202',X'00020101000000020200060201' WRKZG/A,/CHA             
*                                                                               
         DC    X'230201',X'00020101000000000000000000' PRTQ9/A,/DIS             
         DC    X'230202',X'00020101000000020200060201' PRTQ9/A,/CHA             
         DC    X'240201',X'00020101000000000000000000' PRTQA/A,/DIS             
         DC    X'240202',X'00020101000000020200060201' PRTQA/A,/CHA             
         DC    X'250201',X'00020101000000000000000000' PRTQB/A,/DIS             
         DC    X'250202',X'00020101000000020200060201' PRTQB/A,/CHA             
         DC    X'260201',X'00020101000000000000000000' PRTQC/A,/DIS             
         DC    X'260202',X'00020101000000020200060201' PRTQC/A,/CHA             
         DC    X'270201',X'00020101000000000000000000' PRTQD/A,/DIS             
         DC    X'270202',X'00020101000000020200060201' PRTQD/A,/CHA             
         DC    X'280201',X'00020101000000000000000000' PRTQE/A,/DIS             
         DC    X'280202',X'00020101000000020200060201' PRTQE/A,/CHA             
         DC    X'290201',X'00020101000000000000000000' PRTQF/A,/DIS             
         DC    X'290202',X'00020101000000020200060201' PRTQF/A,/CHA             
         DC    X'2A0201',X'00020101000000000000000000' PRTQG/A,/DIS             
         DC    X'2A0202',X'00020101000000020200060201' PRTQG/A,/CHA             
CONACTSX DC    X'00'                                                            
*                                                                               
CPPACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' CPFILE/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' CPFILE/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' CPFILE/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' CPFILE/K,/BRO            
         DC    X'010105',X'00020100000000020200010103' CPFILE/K,/REP            
         DC    X'010301',X'00040102000000000000000000' CPFILE/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' CPFILE/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' CPFILE/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' CPFILE/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' CPFILE/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' CPFILE/F=/BRO            
         DC    X'020201',X'00070101000000000000000000' CPRCVR/A,/DIS            
         DC    X'020204',X'00070101000000000000000000' CPRCVR/A,/BRO            
         DC    X'020301',X'00030101000000000000000000' CPRCVR/FI/DIS            
         DC    X'020304',X'00030101000000000000000000' CPRCVR/FI/BRO            
         DC    X'020401',X'00030201000000000000000000' CPRCVR/NE/DIS            
         DC    X'020404',X'00030201000000000000000000' CPRCVR/NE/BRO            
         DC    X'020804',X'00030101000000000000000000' CPRCVR/F=/BRO            
         DC    X'030101',X'00070101000000000000000000' CPREQ/K,/DIS             
         DC    X'030103',X'00000000000000000000010001' CPREQ/K,/ADD             
         DC    X'030201',X'00070101000000000000000000' CPREQ/A,/DIS             
         DC    X'030202',X'00070101000000070200060201' CPREQ/A,/CHA             
         DC    X'030301',X'00030101000000000000000000' CPREQ/FI/DIS             
         DC    X'030304',X'00030101000000000000000000' CPREQ/FI/BRO             
         DC    X'030401',X'00030201000000000000000000' CPREQ/NE/DIS             
         DC    X'030404',X'00030201000000000000000000' CPREQ/NE/BRO             
         DC    X'030804',X'00030101000000000000000000' CPREQ/F=/BRO             
CPPACTSX DC    X'00'                                                            
*                                                                               
PERACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' PERDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' PERDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' PERDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' PERDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' PERDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' PERDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' PERDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' PERDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' PERDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' PERDIR/F=/BRO            
         DC    X'010901',X'040401040A0102000000000000' PERDIR/G,/DIS            
         DC    X'010904',X'040401040A0102000000000000' PERDIR/G,/BRO            
         DC    X'010A04',X'040401040A0102000000000000' PERDIR/G=/BRO            
         DC    X'020201',X'00070101000000000000000000' PERRCV/A,/DIS            
         DC    X'020204',X'00070101000000000000000000' PERRCV/A,/BRO            
         DC    X'020301',X'00030101000000000000000000' PERRCV/FI/DIS            
         DC    X'020304',X'00030101000000000000000000' PERRCV/FI/BRO            
         DC    X'020401',X'00030201000000000000000000' PERRCV/NE/DIS            
         DC    X'020404',X'00030201000000000000000000' PERRCV/NE/BRO            
         DC    X'020804',X'00030101000000000000000000' PERRCV/F=/BRO            
         DC    X'030101',X'00070101000000000000000000' PERREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' PERREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' PERREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' PERREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' PERREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' PERREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' PERREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' PERREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' PERREQ/F=/BRO            
         DC    X'040103',X'00020103000000000000080201' PERFIL/K,/ADD            
         DC    X'040105',X'000201000000000A0200080201' PERFIL/K,/REP            
         DC    X'040201',X'000A0103000000000000000000' PERFIL/A,/DIS            
         DC    X'040202',X'000A01030000000A0200090200' PERFIL/A,/CHA            
         DC    X'050101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'050104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'050301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'050304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'050401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'050404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'050804',X'00040102000000000000000000' CTFILE/F=/BRO            
PERACTSX DC    X'00'                                                            
*                                                                               
*&&                                                                             
         EJECT                                                                  
*&&UK                                                                           
MEDACTS  DC    X'010101',X'00040102000000000000000000' MEDIR/K,/DIS             
         DC    X'010102',X'00020102000000020200060200' MEDIR/K,/CHA             
         DC    X'010103',X'00020103000000000000010200' MEDIR/K,/ADD             
         DC    X'010104',X'00040102000000000000000000' MEDIR/K,/BRO             
         DC    X'010301',X'00040102000000000000000000' MEDIR/FI/DIS             
         DC    X'010304',X'00040102000000000000000000' MEDIR/FI/BRO             
         DC    X'010401',X'00020202030202000000000000' MEDIR/NE/DIS             
         DC    X'010404',X'00020202030202000000000000' MEDIR/NE/BRO             
         DC    X'010702',X'00020102000000020200060200' MEDIR/LA/CHA             
         DC    X'010804',X'00040102000000000000000000' MEDIR/F=/BRO             
         DC    X'010901',X'040401040A0102000000000000' MEDIR/G,/DIS             
         DC    X'010904',X'040401040A0102000000000000' MEDIR/G,/BRO             
         DC    X'010A04',X'040401040A0102000000000000' MEDIR/G=/BRO             
         DC    X'020201',X'00070101000000000000000000' RECOVER/A,/DIS           
         DC    X'020204',X'00070101000000000000000000' RECOVER/A,/BRO           
         DC    X'020301',X'00030101000000000000000000' RECOVER/FI/DIS           
         DC    X'020304',X'00030101000000000000000000' RECOVER/FI/BRO           
         DC    X'020401',X'00030201000000000000000000' RECOVER/NE/DIS           
         DC    X'020404',X'00030201000000000000000000' RECOVER/NE/BRO           
         DC    X'020804',X'00030101000000000000000000' RECOVER/F=/BRO           
         DC    X'030101',X'00070101000000000000000000' REQUEST/K,/DIS           
         DC    X'030103',X'00000000000000000000010001' REQUEST/K,/ADD           
         DC    X'030201',X'00070101000000000000000000' REQUEST/A,/DIS           
         DC    X'030202',X'00070101000000070200060201' REQUEST/A,/CHA           
         DC    X'030301',X'00030101000000000000000000' REQUEST/FI/DIS           
         DC    X'030304',X'00030101000000000000000000' REQUEST/FI/BRO           
         DC    X'030401',X'00030201000000000000000000' REQUEST/NE/DIS           
         DC    X'030404',X'00030201000000000000000000' REQUEST/NE/BRO           
         DC    X'030804',X'00030101000000000000000000' REQUEST/F=/BRO           
         DC    X'040103',X'00020103000000000000080201' MEDFILE/K,/ADD           
         DC    X'040105',X'000201000000000A0200080201' MEDFILE/K,/REP           
         DC    X'040201',X'000A0103000000000000000000' MEDFILE/A,/DIS           
         DC    X'040202',X'000A01030000000A0200090200' MEDFILE/A,/CHA           
         DC    X'050201',X'00020101000000000000000000' MEDWRK/A,/DIS            
         DC    X'060101',X'00040102000000000000000000' DMGDIR/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' DMGDIR/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' DMGDIR/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' DMGDIR/K,/BRO            
         DC    X'060301',X'00040102000000000000000000' DMGDIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' DMGDIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' DMGDIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' DMGDIR/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' DMGDIR/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' DMGDIR/F=/BRO            
         DC    X'060901',X'090401040A0102000000000000' DMGDIR/G,/DIS            
         DC    X'060904',X'090401040A0102000000000000' DMGDIR/G,/BRO            
         DC    X'060A04',X'090401040A0102000000000000' DMGDIR/G=/BRO            
         DC    X'070103',X'00020103000000000000080201' DMGFIL/K,/ADD            
         DC    X'070105',X'000201000000000A0200080201' DMGFIL/K,/REP            
         DC    X'070201',X'000A0103000000000000000000' DMGFIL/A,/DIS            
         DC    X'070202',X'000A01030000000A0200090200' DMGFIL/A,/CHA            
         DC    X'080101',X'00040102000000000000000000' DMNDIR/K,/DIS            
         DC    X'080102',X'00020102000000020200060200' DMNDIR/K,/CHA            
         DC    X'080103',X'00020103000000000000010200' DMNDIR/K,/ADD            
         DC    X'080104',X'00040102000000000000000000' DMNDIR/K,/BRO            
         DC    X'080301',X'00040102000000000000000000' DMNDIR/FI/DIS            
         DC    X'080304',X'00040102000000000000000000' DMNDIR/FI/BRO            
         DC    X'080401',X'00020202030202000000000000' DMNDIR/NE/DIS            
         DC    X'080404',X'00020202030202000000000000' DMNDIR/NE/BRO            
         DC    X'080702',X'00020102000000020200060200' DMNDIR/LA/CHA            
         DC    X'080804',X'00040102000000000000000000' DMNDIR/F=/BRO            
         DC    X'080901',X'090401040A0102000000000000' DMNDIR/G,/DIS            
         DC    X'080904',X'090401040A0102000000000000' DMNDIR/G,/BRO            
         DC    X'080A04',X'090401040A0102000000000000' DMNDIR/G=/BRO            
         DC    X'090103',X'00020103000000000000080201' DMNNEW/K,/ADD            
         DC    X'090105',X'000201000000000A0200080201' DMNNEW/K,/REP            
         DC    X'090201',X'000A0103000000000000000000' DMNNEW/A,/DIS            
         DC    X'090202',X'000A01030000000A0200090200' DMNNEW/A,/CHA            
         DC    X'0A0103',X'00020103000000000000080201' DMNOLD/K,/ADD            
         DC    X'0A0105',X'000201000000000A0200080201' DMNOLD/K,/REP            
         DC    X'0A0201',X'000A0103000000000000000000' DMNOLD/A,/DIS            
         DC    X'0A0202',X'000A01030000000A0200090200' DMNOLD/A,/CHA            
         DC    X'0B0101',X'00040102000000000000000000' DMODIR/K,/DIS            
         DC    X'0B0102',X'00020102000000020200060200' DMODIR/K,/CHA            
         DC    X'0B0103',X'00020103000000000000010200' DMODIR/K,/ADD            
         DC    X'0B0104',X'00040102000000000000000000' DMODIR/K,/BRO            
         DC    X'0B0301',X'00040102000000000000000000' DMODIR/FI/DIS            
         DC    X'0B0304',X'00040102000000000000000000' DMODIR/FI/BRO            
         DC    X'0B0401',X'00020202030202000000000000' DMODIR/NE/DIS            
         DC    X'0B0404',X'00020202030202000000000000' DMODIR/NE/BRO            
         DC    X'0B0702',X'00020102000000020200060200' DMODIR/LA/CHA            
         DC    X'0B0804',X'00040102000000000000000000' DMODIR/FI/BRO            
         DC    X'0B0901',X'0C0401040A0102000000000000' DMODIR/G,/DIS            
         DC    X'0B0904',X'0C0401040A0102000000000000' DMODIR/G,/BRO            
         DC    X'0B0A04',X'0C0401040A0102000000000000' DMODIR/G=/BRO            
         DC    X'0C0103',X'00020103000000000000080201' DMO1FL/K,/ADD            
         DC    X'0C0105',X'000201000000000A0200080201' DMO1FL/K,/REP            
         DC    X'0C0201',X'000A0103000000000000000000' DMO1FL/A,/DIS            
         DC    X'0C0202',X'000A01030000000A0200090200' DMO1FL/A,/CHA            
         DC    X'0D0103',X'00020103000000000000080201' DMO2FL/K,/ADD            
         DC    X'0D0105',X'000201000000000A0200080201' DMO2FL/K,/REP            
         DC    X'0D0201',X'000A0103000000000000000000' DMO2FL/A,/DIS            
         DC    X'0D0202',X'000A01030000000A0200090200' DMO2FL/A,/CHA            
         DC    X'0E0103',X'00020103000000000000080201' DMO3FL/K,/ADD            
         DC    X'0E0105',X'000201000000000A0200080201' DMO3FL/K,/REP            
         DC    X'0E0201',X'000A0103000000000000000000' DMO3FL/A,/DIS            
         DC    X'0E0202',X'000A01030000000A0200090200' DMO3FL/A,/CHA            
         DC    X'0F0103',X'00020103000000000000080201' DMO4FL/K,/ADD            
         DC    X'0F0105',X'000201000000000A0200080201' DMO4FL/K,/REP            
         DC    X'0F0201',X'000A0103000000000000000000' DMO4FL/A,/DIS            
         DC    X'0F0202',X'000A01030000000A0200090200' DMO4FL/A,/CHA            
         DC    X'100101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'100104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'100301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'100304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'100401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'100404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'100804',X'00040102000000000000000000' CTFILE/F=/BRO            
MEDACTSX DC    H'0'                                                             
*                                                                               
MPLACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' MPLDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' MPLDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' MPLDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' MPLDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' MPLDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' MPLDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' MPLDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' MPLDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' MPLDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' MPLDIR/F=/BRO            
         DC    X'010901',X'020401040A0102000000000000' MPLDIR/G,/DIS            
         DC    X'010904',X'020401040A0102000000000000' MPLDIR/G,/BRO            
         DC    X'010A04',X'020401040A0102000000000000' MPLDIR/G=/BRO            
         DC    X'020103',X'00020103000000000000080201' MPLFIL/K,/ADD            
         DC    X'020105',X'000201000000000A0200080201' MPLFIL/K,/REP            
         DC    X'020201',X'000A0103000000000000000000' MPLFIL/A,/DIS            
         DC    X'020202',X'000A01030000000A0200090200' MPLFIL/A,/CHA            
         DC    X'030101',X'00070101000000000000000000' MPLREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' MPLREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' MPLREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' MPLREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' MPLREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' MPLREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' MPLREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' MPLREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' MPLREQ/FI/BRO            
         DC    X'040201',X'00070101000000000000000000' MPLRCV/A,/DIS            
         DC    X'040204',X'00070101000000000000000000' MPLRCV/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' MPLRCV/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' MPLRCV/FI/BRO            
         DC    X'040401',X'00030201000000000000000000' MPLRCV/NE/DIS            
         DC    X'040404',X'00030201000000000000000000' MPLRCV/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' MPLRCV/F=/BRO            
         DC    X'0A0101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'0A0104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'0A0301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'0A0304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'0A0401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'0A0404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'0A0804',X'00040102000000000000000000' CTFILE/F=/BRO            
*                                                                               
         DC    X'0F0101',X'00040102000000000000000000' MPCDRC/K,/DIS            
         DC    X'0F0102',X'00020102000000020200060200' MPCDRC/K,/CHA            
         DC    X'0F0103',X'00020103000000000000010200' MPCDRC/K,/ADD            
         DC    X'0F0104',X'00040102000000000000000000' MPCDRC/K,/BRO            
         DC    X'0F0301',X'00040102000000000000000000' MPCDRC/FI/DIS            
         DC    X'0F0304',X'00040102000000000000000000' MPCDRC/FI/BRO            
         DC    X'0F0401',X'00020202030202000000000000' MPCDRC/NE/DIS            
         DC    X'0F0404',X'00020202030202000000000000' MPCDRC/NE/BRO            
         DC    X'0F0702',X'00020102000000020200060200' MPCDRC/LA/CHA            
         DC    X'0F0804',X'00040102000000000000000000' MPCDRC/F=/BRO            
         DC    X'0F0901',X'100401040A0102000000000000' MPCDRC/G,/DIS            
         DC    X'0F0904',X'100401040A0102000000000000' MPCDRC/G,/BRO            
         DC    X'0F0A04',X'100401040A0102000000000000' MPCDRC/G=/BRO            
         DC    X'100103',X'00020103000000000000080201' MPCFLC/K,/ADD            
         DC    X'100105',X'000201000000000A0200080201' MPCFLC/K,/REP            
         DC    X'100201',X'000A0103000000000000000000' MPCFLC/A,/DIS            
         DC    X'100202',X'000A01030000000A0200090200' MPCFLC/A,/CHA            
*                                                                               
         DC    X'110101',X'00040102000000000000000000' MPCDRS/K,/DIS            
         DC    X'110102',X'00020102000000020200060200' MPCDRS/K,/CHA            
         DC    X'110103',X'00020103000000000000010200' MPCDRS/K,/ADD            
         DC    X'110104',X'00040102000000000000000000' MPCDRS/K,/BRO            
         DC    X'110301',X'00040102000000000000000000' MPCDRS/FI/DIS            
         DC    X'110304',X'00040102000000000000000000' MPCDRS/FI/BRO            
         DC    X'110401',X'00020202030202000000000000' MPCDRS/NE/DIS            
         DC    X'110404',X'00020202030202000000000000' MPCDRS/NE/BRO            
         DC    X'110702',X'00020102000000020200060200' MPCDRS/LA/CHA            
         DC    X'110804',X'00040102000000000000000000' MPCDRS/F=/BRO            
         DC    X'110901',X'120401040A0102000000000000' MPCDRS/G,/DIS            
         DC    X'110904',X'120401040A0102000000000000' MPCDRS/G,/BRO            
         DC    X'110A04',X'120401040A0102000000000000' MPCDRS/G=/BRO            
         DC    X'120103',X'00020103000000000000080201' MPCFLS/K,/ADD            
         DC    X'120105',X'000201000000000A0200080201' MPCFLS/K,/REP            
         DC    X'120201',X'000A0103000000000000000000' MPCFLS/A,/DIS            
         DC    X'120202',X'000A01030000000A0200090200' MPCFLS/A,/CHA            
MPLACTSX DC    X'00'                                                            
*                                                                               
ACCACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' ACCOUNT/K,/DIS           
         DC    X'010102',X'00020102000000020200060200' ACCOUNT/K,/CHA           
         DC    X'010103',X'00020103000000000000010200' ACCOUNT/K,/ADD           
         DC    X'010104',X'00040102000000000000000000' ACCOUNT/K,/BRO           
         DC    X'010105',X'00020100000000020200010103' ACCOUNT/K,/REP           
         DC    X'010301',X'00040102000000000000000000' ACCOUNT/FI/DIS           
         DC    X'010304',X'00040102000000000000000000' ACCOUNT/FI/BRO           
         DC    X'010401',X'00020202030202000000000000' ACCOUNT/NE/DIS           
         DC    X'010404',X'00020202030202000000000000' ACCOUNT/NE/BRO           
         DC    X'010702',X'00020102000000020200060200' ACCOUNT/LA/CHA           
         DC    X'010804',X'00040102000000000000000000' ACCOUNT/F=/BRO           
         DC    X'020201',X'00070101000000000000000000' ACCRCVR/A,/DIS           
         DC    X'020204',X'00070101000000000000000000' ACCRCVR/A,/BRO           
         DC    X'020301',X'00030101000000000000000000' ACCRCVR/FI/DIS           
         DC    X'020304',X'00030101000000000000000000' ACCRCVR/FI/BRO           
         DC    X'020401',X'00030201000000000000000000' ACCRCVR/NE/DIS           
         DC    X'020404',X'00030201000000000000000000' ACCRCVR/NE/BRO           
         DC    X'020804',X'00030101000000000000000000' ACCRCVR/F=/BRO           
         DC    X'030101',X'00070101000000000000000000' ACCREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' ACCREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' ACCREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' ACCREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' ACCREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' ACCREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' ACCREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' ACCREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' ACCREQ/F=/BRO            
         DC    X'040201',X'00070101000000000000000000' ACCDAY/A,/DIS            
         DC    X'040202',X'00070101000000000000060201' ACCDAY/A,/CHA            
         DC    X'040204',X'00030101000000000000000000' ACCDAY/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' ACCDAY/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' ACCDAY/FI/BRO            
         DC    X'040401',X'00030101000000000000000000' ACCDAY/NE/DIS            
         DC    X'040404',X'00030101000000000000000000' ACCDAY/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' ACCDAY/F=/BRO            
         DC    X'050201',X'00020101000000000000000000' ACCWRK/A,/DIS            
         DC    X'060101',X'00040102000000000000000000' ACCDIR/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' ACCDIR/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' ACCDIR/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' ACCDIR/K,/BRO            
         DC    X'060105',X'00020100000000020200010103' ACCDIR/K,/REP            
         DC    X'060301',X'00040102000000000000000000' ACCDIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' ACCDIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' ACCDIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' ACCDIR/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' ACCDIR/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' ACCDIR/F=/BRO            
         DC    X'060901',X'070401040A0102000000000000' ACCDIR/G,/DIS            
         DC    X'060904',X'070401040A0102000000000000' ACCDIR/G,/BRO            
         DC    X'060A04',X'070401040A0102000000000000' ACCDIR/G=/BRO            
         DC    X'070103',X'00020103000000000000080201' ACCMST/K,/ADD            
         DC    X'070105',X'000201000000000A0200080201' ACCMST/K,/REP            
         DC    X'070201',X'000A0103000000000000000000' ACCMST/A,/DIS            
         DC    X'070202',X'000A01030000000A0200090200' ACCMST/A,/CHA            
         DC    X'080103',X'00020103000000000000080201' ACCARC/K,/ADD            
         DC    X'080105',X'000201000000000A0200080201' ACCARC/K,/REP            
         DC    X'080201',X'000A0103000000000000000000' ACCARC/A,/DIS            
         DC    X'080202',X'000A01030000000A0200090200' ACCARC/A,/CHA            
         DC    X'090101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'090104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'090301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'090304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'090401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'090404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'090804',X'00040102000000000000000000' CTFILE/F=/BRO            
ACCACTSX DC    X'00'                                                            
*                                                                               
FEEACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' FEEDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' FEEDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' FEEDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' FEEDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' FEEDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' FEEDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' FEEDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' FEEDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' FEEDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' FEEDIR/F=/BRO            
         DC    X'010901',X'020401040A0102000000000000' FEEDIR/G,/DIS            
         DC    X'010904',X'020401040A0102000000000000' FEEDIR/G,/BRO            
         DC    X'010A04',X'020401040A0102000000000000' FEEDIR/G=/BRO            
         DC    X'020103',X'00020103000000000000080201' FEEFIL/K,/ADD            
         DC    X'020105',X'000201000000000A0200080201' FEEFIL/K,/REP            
         DC    X'020201',X'000A0103000000000000000000' FEEFIL/A,/DIS            
         DC    X'020202',X'000A01030000000A0200090200' FEEFIL/A,/CHA            
         DC    X'030101',X'00070101000000000000000000' FEEREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' FEEREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' FEEREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' FEEREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' FEEREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' FEEREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' FEEREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' FEEREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' FEEREQ/F=/BRO            
         DC    X'040201',X'00070101000000000000000000' FEERCV/A,/DIS            
         DC    X'040204',X'00070101000000000000000000' FEERCV/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' FEERCV/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' FEERCV/FI/BRO            
         DC    X'040401',X'00030201000000000000000000' FEERCV/NE/DIS            
         DC    X'040404',X'00030201000000000000000000' FEERCV/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' FEERCV/F=/BRO            
         DC    X'050104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'050301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'050304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'050401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'050404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'050804',X'00040102000000000000000000' CTFILE/F=/BRO            
FEEACTSX DC    X'00'                                                            
*                                                                               
MBAACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' MBADIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' MBADIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' MBADIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' MBADIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' MBADIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' MBADIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' MBADIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' MBADIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' MBADIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' MBADIR/F=/BRO            
         DC    X'010901',X'020401040A0102000000000000' MBADIR/G,/DIS            
         DC    X'010904',X'020401040A0102000000000000' MBADIR/G,/BRO            
         DC    X'010A04',X'020401040A0102000000000000' MBADIR/G=/BRO            
         DC    X'020103',X'00020103000000000000080201' MBAFIL/K,/ADD            
         DC    X'020105',X'000201000000000A0200080201' MBAFIL/K,/REP            
         DC    X'020201',X'000A0103000000000000000000' MBAFIL/A,/DIS            
         DC    X'020202',X'000A01030000000A0200090200' MBAFIL/A,/CHA            
         DC    X'030101',X'00070101000000000000000000' MBAREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' MBAREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' MBAREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' MBAREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' MBAREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' MBAREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' MBAREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' MBAREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' MBAREQ/F=/BRO            
         DC    X'040201',X'00070101000000000000000000' MBARCV/A,/DIS            
         DC    X'040204',X'00070101000000000000000000' MBARCV/A,/BRO            
         DC    X'040301',X'00030101000000000000000000' MBARCV/FI/DIS            
         DC    X'040304',X'00030101000000000000000000' MBARCV/FI/BRO            
         DC    X'040401',X'00030201000000000000000000' MBARCV/NE/DIS            
         DC    X'040404',X'00030201000000000000000000' MBARCV/NE/BRO            
         DC    X'040804',X'00030101000000000000000000' MBARCV/F=/BRO            
         DC    X'050101',X'00040102000000000000000000' MBANDX/K,/DIS            
         DC    X'050104',X'00040102000000000000000000' MBANDX/K,/BRO            
         DC    X'050301',X'00040102000000000000000000' MBANDX/FI/DIS            
         DC    X'050304',X'00040102000000000000000000' MBANDX/FI/BRO            
         DC    X'050401',X'00020202030202000000000000' MBANDX/NE/DIS            
         DC    X'050404',X'00020202030202000000000000' MBANDX/NE/BRO            
         DC    X'050804',X'00040102000000000000000000' MBANDX/F=/BRO            
         DC    X'060101',X'00040102000000000000000000' MBUDIR/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' MBUDIR/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' MBUDIR/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' MBUDIR/K,/BRO            
         DC    X'060301',X'00040102000000000000000000' MBUDIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' MBUDIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' MBUDIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' MBUDIR/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' MBUDIR/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' MBUDIR/F=/BRO            
         DC    X'060901',X'070401040A0102000000000000' MBUDIR/G,/DIS            
         DC    X'060904',X'070401040A0102000000000000' MBUDIR/G,/BRO            
         DC    X'060A04',X'070401040A0102000000000000' MBUDIR/G=/BRO            
         DC    X'070103',X'00020103000000000000080201' MBUFIL/K,/ADD            
         DC    X'070105',X'000201000000000A0200080201' MBUFIL/K,/REP            
         DC    X'070201',X'000A0103000000000000000000' MBUFIL/A,/DIS            
         DC    X'070202',X'000A01030000000A0200090200' MBUFIL/A,/CHA            
         DC    X'080101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'080104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'080301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'080304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'080401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'080404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'080804',X'00040102000000000000000000' CTFILE/FI/BRO            
MBAACTSX DC    X'00'                                                            
*                                                                               
CONACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' CTFILE/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' CTFILE/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'010105',X'00020100000000020200010103' CTFILE/K,/REP            
         DC    X'010301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' CTFILE/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' CTFILE/F=/BRO            
         DC    X'020201',X'00070101000000000000000000' CTRCVR/A,/DIS            
         DC    X'020204',X'00070101000000000000000000' CTRCVR/A,/BRO            
         DC    X'020301',X'00030101000000000000000000' CTRCVR/FI/DIS            
         DC    X'020304',X'00030101000000000000000000' CTRCVR/FI/BRO            
         DC    X'020401',X'00030201000000000000000000' CTRCVR/NE/DIS            
         DC    X'020404',X'00030201000000000000000000' CTRCVR/NE/BRO            
         DC    X'020804',X'00030101000000000000000000' CTRCVR/F=/BRO            
         DC    X'030101',X'00070101000000000000000000' CTREQ/K,/DIS             
         DC    X'030103',X'00000000000000000000010001' CTREQ/K,/ADD             
         DC    X'030201',X'00070101000000000000000000' CTREQ/A,/DIS             
         DC    X'030202',X'00070101000000070200060201' CTREQ/A,/CHA             
         DC    X'030301',X'00030101000000000000000000' CTREQ/FI/DIS             
         DC    X'030304',X'00030101000000000000000000' CTREQ/FI/BRO             
         DC    X'030401',X'00030201000000000000000000' CTREQ/NE/DIS             
         DC    X'030404',X'00030201000000000000000000' CTREQ/NE/BRO             
         DC    X'030804',X'00030101000000000000000000' CTREQ/F=/BRO             
         DC    X'040101',X'00040102000000000000000000' GENDIR/K,/DIS            
         DC    X'040102',X'00020102000000020200060200' GENDIR/K,/CHA            
         DC    X'040103',X'00020103000000000000010200' GENDIR/K,/ADD            
         DC    X'040104',X'00040102000000000000000000' GENDIR/K,/BRO            
         DC    X'040301',X'00040102000000000000000000' GENDIR/FI/DIS            
         DC    X'040304',X'00040102000000000000000000' GENDIR/FI/BRO            
         DC    X'040401',X'00020202030202000000000000' GENDIR/NE/DIS            
         DC    X'040404',X'00020202030202000000000000' GENDIR/NE/BRO            
         DC    X'040702',X'00020102000000020200060200' GENDIR/LA/CHA            
         DC    X'040804',X'00040102000000000000000000' GENDIR/F=/BRO            
         DC    X'040901',X'050401040A0102000000000000' GENDIR/G,/DIS            
         DC    X'040904',X'050401040A0102000000000000' GENDIR/G,/BRO            
         DC    X'040A04',X'050401040A0102000000000000' GENDIR/G=/BRO            
         DC    X'050103',X'00020103000000000000080201' GENFIL/K,/ADD            
         DC    X'050105',X'000201000000000A0200080201' GENFIL/K,/REP            
         DC    X'050201',X'000A0103000000000000000000' GENFIL/A,/DIS            
         DC    X'050202',X'000A01030000000A0200090200' GENFIL/A,/CHA            
         DC    X'060101',X'00040102000000000000000000' CRADIR/K,/DIS            
         DC    X'060102',X'00020102000000020200060200' CRADIR/K,/CHA            
         DC    X'060103',X'00020103000000000000010200' CRADIR/K,/ADD            
         DC    X'060104',X'00040102000000000000000000' CRADIR/K,/BRO            
         DC    X'060301',X'00040102000000000000000000' CRADIR/FI/DIS            
         DC    X'060304',X'00040102000000000000000000' CRADIR/FI/BRO            
         DC    X'060401',X'00020202030202000000000000' CRADIR/NE/DIS            
         DC    X'060404',X'00020202030202000000000000' CRADIR/NE/BRO            
         DC    X'060702',X'00020102000000020200060200' CRADIR/LA/CHA            
         DC    X'060804',X'00040102000000000000000000' CRADIR/F=/BRO            
*                                                                               
         DC    X'060901',X'070401040A0102000000000000' CRADIR/G,/DIS            
         DC    X'060904',X'070401040A0102000000000000' CRADIR/G,/BRO            
         DC    X'060A04',X'070401040A0102000000000000' CRADIR/G=/BRO            
*                                                                               
         DC    X'070103',X'00020103000000000000080201' CRAFIL/K,/ADD            
         DC    X'070105',X'000201000000000A0200080201' CRAFIL/K,/REP            
         DC    X'070201',X'000A0103000000000000000000' CRAFIL/A,/DIS            
         DC    X'070202',X'000A01030000000A0200090200' CRAFIL/A,/CHA            
         DC    X'110201',X'00020101000000000000000000' PRTQ1/A,/DIS             
         DC    X'110202',X'00020101000000020200060201' PRTQ1/A,/CHA             
         DC    X'120201',X'00020101000000000000000000' PRTQ2/A,/DIS             
         DC    X'120202',X'00020101000000020200060201' PRTQ2/A,/CHA             
         DC    X'130201',X'00020101000000000000000000' PRTQ3/A,/DIS             
         DC    X'130202',X'00020101000000020200060201' PRTQ3/A,/CHA             
         DC    X'140201',X'00020101000000000000000000' PRTQ4/A,/DIS             
         DC    X'140202',X'00020101000000020200060201' PRTQ4/A,/CHA             
         DC    X'150201',X'00020101000000000000000000' PRTQ5/A,/DIS             
         DC    X'150202',X'00020101000000020200060201' PRTQ5/A,/CHA             
         DC    X'160201',X'00020101000000000000000000' PRTQ6/A,/DIS             
         DC    X'160202',X'00020101000000020200060201' PRTQ6/A,/CHA             
         DC    X'170201',X'00020101000000000000000000' PRTQ7/A,/DIS             
         DC    X'170202',X'00020101000000020200060201' PRTQ7/A,/CHA             
         DC    X'180201',X'00020101000000000000000000' PRTQ8/A,/DIS             
         DC    X'180202',X'00020101000000020200060201' PRTQ8/A,/CHA             
         DC    X'190201',X'00070101000000000000000000' EDCTA/A,/DIS             
         DC    X'190202',X'00070101000000070200060201' EDCTA/A,/CHA             
         DC    X'1A0201',X'00070101000000000000000000' EDCTR/A,/DIS             
         DC    X'1A0202',X'00070101000000070200060201' EDCTR/A,/CHA             
         DC    X'1B0201',X'00020101000000000000000000' WRKF1/A,/DIS             
         DC    X'1B0202',X'00020101000000020200060201' WRKF1/A,/CHA             
         DC    X'1C0201',X'00020101000000000000000000' WRKF2/A,/DIS             
         DC    X'1C0202',X'00020101000000020200060201' WRKF2/A,/CHA             
         DC    X'1D0201',X'00020101000000000000000000' WRKF3/A,/DIS             
         DC    X'1D0202',X'00020101000000020200060201' WRKF3/A,/CHA             
         DC    X'1E0201',X'00020101000000000000000000' WRKF4/A,/DIS             
         DC    X'1E0202',X'00020101000000020200060201' WRKF4/A,/CHA             
         DC    X'1F0201',X'00020101000000000000000000' WRKF5/A,/DIS             
         DC    X'1F0202',X'00020101000000020200060201' WRKF5/A,/CHA             
         DC    X'200201',X'00020101000000000000000000' WRKF6/A,/DIS             
         DC    X'200202',X'00020101000000020200060201' WRKF6/A,/CHA             
         DC    X'210201',X'00020101000000000000000000' WRKF7/A,/DIS             
         DC    X'210202',X'00020101000000020200060201' WRKF7/A,/CHA             
         DC    X'220201',X'00020101000000000000000000' WRKF8/A,/DIS             
         DC    X'220202',X'00020101000000020200060201' WRKF8/A,/CHA             
         DC    X'2B0201',X'00020101000000000000000000' WRKF9/A,/DIS             
         DC    X'2B0202',X'00020101000000020200060201' WRKF9/A,/CHA             
         DC    X'2C0201',X'00020101000000000000000000' WRKFA/A,/DIS             
         DC    X'2C0202',X'00020101000000020200060201' WRKFA/A,/CHA             
         DC    X'2D0201',X'00020101000000000000000000' WRKFB/A,/DIS             
         DC    X'2D0202',X'00020101000000020200060201' WRKFB/A,/CHA             
         DC    X'2E0201',X'00020101000000000000000000' WRKFC/A,/DIS             
         DC    X'2E0202',X'00020101000000020200060201' WRKFC/A,/CHA             
         DC    X'2F0201',X'00020101000000000000000000' WRKFD/A,/DIS             
         DC    X'2F0202',X'00020101000000020200060201' WRKFD/A,/CHA             
         DC    X'300201',X'00020101000000000000000000' WRKFE/A,/DIS             
         DC    X'300202',X'00020101000000020200060201' WRKFE/A,/CHA             
         DC    X'310201',X'00020101000000000000000000' WRKFF/A,/DIS             
         DC    X'310202',X'00020101000000020200060201' WRKFF/A,/CHA             
         DC    X'320201',X'00020101000000000000000000' WRKFG/A,/DIS             
         DC    X'320202',X'00020101000000020200060201' WRKFG/A,/CHA             
*                                                                               
         DC    X'330201',X'00020101000000000000000000' WKFILE/A,/DIS            
         DC    X'330202',X'00020101000000020200060201' WKFILE/A,/CHA            
         DC    X'340201',X'00020101000000000000000000' FACWRK/A,/DIS            
         DC    X'340202',X'00020101000000020200060201' FACWRK/A,/CHA            
*                                                                               
         DC    X'230201',X'00020101000000000000000000' PRTQ9/A,/DIS             
         DC    X'230202',X'00020101000000020200060201' PRTQ9/A,/CHA             
         DC    X'240201',X'00020101000000000000000000' PRTQA/A,/DIS             
         DC    X'240202',X'00020101000000020200060201' PRTQA/A,/CHA             
         DC    X'250201',X'00020101000000000000000000' PRTQB/A,/DIS             
         DC    X'250202',X'00020101000000020200060201' PRTQB/A,/CHA             
         DC    X'260201',X'00020101000000000000000000' PRTQC/A,/DIS             
         DC    X'260202',X'00020101000000020200060201' PRTQC/A,/CHA             
         DC    X'270201',X'00020101000000000000000000' PRTQD/A,/DIS             
         DC    X'270202',X'00020101000000020200060201' PRTQD/A,/CHA             
         DC    X'280201',X'00020101000000000000000000' PRTQE/A,/DIS             
         DC    X'280202',X'00020101000000020200060201' PRTQE/A,/CHA             
         DC    X'290201',X'00020101000000000000000000' PRTQF/A,/DIS             
         DC    X'290202',X'00020101000000020200060201' PRTQF/A,/CHA             
         DC    X'2A0201',X'00020101000000000000000000' PRTQG/A,/DIS             
         DC    X'2A0202',X'00020101000000020200060201' PRTQG/A,/CHA             
CONACTSX DC    X'00'                                                            
*                                                                               
PERACTS  DS    0H                                                               
         DC    X'010101',X'00040102000000000000000000' PERDIR/K,/DIS            
         DC    X'010102',X'00020102000000020200060200' PERDIR/K,/CHA            
         DC    X'010103',X'00020103000000000000010200' PERDIR/K,/ADD            
         DC    X'010104',X'00040102000000000000000000' PERDIR/K,/BRO            
         DC    X'010301',X'00040102000000000000000000' PERDIR/FI/DIS            
         DC    X'010304',X'00040102000000000000000000' PERDIR/FI/BRO            
         DC    X'010401',X'00020202030202000000000000' PERDIR/NE/DIS            
         DC    X'010404',X'00020202030202000000000000' PERDIR/NE/BRO            
         DC    X'010702',X'00020102000000020200060200' PERDIR/LA/CHA            
         DC    X'010804',X'00040102000000000000000000' PERDIR/F=/BRO            
         DC    X'010901',X'040401040A0102000000000000' PERDIR/G,/DIS            
         DC    X'010904',X'040401040A0102000000000000' PERDIR/G,/BRO            
         DC    X'010A04',X'040401040A0102000000000000' PERDIR/G=/BRO            
         DC    X'020201',X'00070101000000000000000000' PERRCV/A,/DIS            
         DC    X'020204',X'00070101000000000000000000' PERRCV/A,/BRO            
         DC    X'020301',X'00030101000000000000000000' PERRCV/FI/DIS            
         DC    X'020304',X'00030101000000000000000000' PERRCV/FI/BRO            
         DC    X'020401',X'00030201000000000000000000' PERRCV/NE/DIS            
         DC    X'020404',X'00030201000000000000000000' PERRCV/NE/BRO            
         DC    X'020804',X'00030101000000000000000000' PERRCV/F=/BRO            
         DC    X'030101',X'00070101000000000000000000' PERREQ/K,/DIS            
         DC    X'030103',X'00000000000000000000010001' PERREQ/K,/ADD            
         DC    X'030201',X'00070101000000000000000000' PERREQ/A,/DIS            
         DC    X'030202',X'00070101000000070200060201' PERREQ/A,/CHA            
         DC    X'030301',X'00030101000000000000000000' PERREQ/FI/DIS            
         DC    X'030304',X'00030101000000000000000000' PERREQ/FI/BRO            
         DC    X'030401',X'00030201000000000000000000' PERREQ/NE/DIS            
         DC    X'030404',X'00030201000000000000000000' PERREQ/NE/BRO            
         DC    X'030804',X'00030101000000000000000000' PERREQ/F=/BRO            
         DC    X'040103',X'00020103000000000000080201' PERFIL/K,/ADD            
         DC    X'040105',X'000201000000000A0200080201' PERFIL/K,/REP            
         DC    X'040201',X'000A0103000000000000000000' PERFIL/A,/DIS            
         DC    X'040202',X'000A01030000000A0200090200' PERFIL/A,/CHA            
         DC    X'050101',X'00040102000000000000000000' CTFILE/K,/DIS            
         DC    X'050104',X'00040102000000000000000000' CTFILE/K,/BRO            
         DC    X'050301',X'00040102000000000000000000' CTFILE/FI/DIS            
         DC    X'050304',X'00040102000000000000000000' CTFILE/FI/BRO            
         DC    X'050401',X'00020202030202000000000000' CTFILE/NE/DIS            
         DC    X'050404',X'00020202030202000000000000' CTFILE/NE/BRO            
         DC    X'050804',X'00040102000000000000000000' CTFILE/F=/BRO            
PERACTSX DC    X'00'                                                            
*                                                                               
*&&                                                                             
PERMTBLX DS    0C                                                               
         EJECT                                                                  
* GEPFMSAVE                                                                     
       ++INCLUDE GEPFMSAVE                                                      
         EJECT                                                                  
* GEPFMTEMP                                                                     
       ++INCLUDE GEPFMTEMP                                                      
         EJECT                                                                  
* DDGLPFMD                                                                      
       ++INCLUDE DDGLPFMD                                                       
         EJECT                                                                  
CALLOVD  DSECT           CALLOV PARAMETER LIST DSECT                            
OVPARM1  DS    0F                                                               
OVNO     DS    CL1       OVERLAY NUMBER                                         
OVLOADA  DS    CL3       LOAD ADDR                                              
OVPARM2  DS    0F                                                               
OVERR    DS    CL1       ERROR CODE                                             
OVTWAA   DS    CL3       A(TWA)                                                 
                                                                                
HEXOUTD  DSECT           HEXOUT PARAMETER LIST DSECT                            
HOAS     DS    F         A(SOURCE)                                              
HOAD     DS    F         A(DESTN)                                               
HOLS     DS    F         L'SOURCE                                               
HOAO     DS    F         A(OPTION)                                              
HOLD     DS    F         L'DESTN                                                
                                                                                
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DDDMCB                                                                        
       ++INCLUDE DDDMCB                                                         
* DDFLDIND                                                                      
       ++INCLUDE DDFLDIND                                                       
* DDFLDHDR                                                                      
       ++INCLUDE DDFLDHDR                                                       
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* FATIOB                                                                        
       ++INCLUDE FATIOB                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004GEPFM00   09/18/17'                                      
         END                                                                    
