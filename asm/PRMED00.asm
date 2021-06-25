*          DATA SET PRMED00    AT LEVEL 042 AS OF 05/01/02                      
*PHASE T41C00A,+0                                                               
*INCLUDE GETINS                                                                 
*INCLUDE PPBYOUT                                                                
*INCLUDE PUBOUT                                                                 
*INCLUDE PUBEDIT                                                                
*INCLUDE PRINTIO                                                                
*INCLUDE PRNTVAL                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T41C00A - NEW PRINT SPOOL CONTROLLER'                           
         SPACE 3                                                                
* ORGANIZATION OF MEMORY: USED BY ENTIRE NEW PRINT SYSTEM                       
*                                                                               
*      24000 BYTES                     TWA                                      
*      ---------------                ---------------                           
* R8-> ' SPOOLD      '           RA-> ' SYSTEM TWA  '                           
*      ' (3144 BYTES)'                '             '                           
*      '             '                '             '                           
*      '-------------'                '-------------'                           
* RC-> ' GEND        '                ' APPLICATION '                           
*      ' (2792 BYTES)'                '        TWA  '                           
*      '             '                '             '                           
*      '-------------'                '-------------'                           
*      ' IO AREA     '                                                          
*      ' (2000 BYTES)'                                                          
*      '             '                                                          
*      '-------------'                                                          
* R9-> ' PRNTSYSD    '                                                          
*      ' (2000 BYTES)'                                                          
*      '             '                                                          
*      '-------------'                                                          
* R7-> ' PRNTBLOK    '                                                          
*      ' (4000 BYTES)'                                                          
*      '             '                                                          
*      '-------------'                                                          
*      ' APPLICATION '                                                          
*      '   COMMON    '                                                          
*      '  (I/0 1)    '                                                          
*      ' (4000 BYTES)                                                           
*      '-------------'                                                          
*      '  (W/S 1)    '                                                          
*      ' (4000 BYTES)                                                           
*      '-------------'                                                          
*      ' LOCAL W/S   '                                                          
*      '  (W/S2,3)   '                                                          
*      ' (1000 BYTES '                                                          
*      '       EACH) '                                                          
*      '-------------'                                                          
         EJECT                                                                  
T41C00A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 3000,**T41C00A,R6,RR=R2                                          
         ST    R2,RELO                                                          
         LR    R5,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
         L     RF,=F'24000'        CLEAR SELECTED STORAGE                       
         XCEF                                                                   
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         LA    R9,IO                                                            
         AH    R9,=H'2000'         GRABBING 1 2000 BYTE I/O AREA                
         LA    R9,8(R9)            NEED SPACE FOR 1 8BYTE LABEL                 
         USING PRNTSYSD,R9                                                      
         ST    R5,SYSPARMS                                                      
         LA    R7,PRTSYSDX                                                      
         LA    R7,8(R7)                                                         
         USING PRNTBLOK,R7                                                      
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    R9,ASYSD                                                         
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         L     R5,SYSPARMS                                                      
         L     R2,8(R5)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
         B     EXIT                THEN WE'RE THROUGH                           
         EJECT                                                                  
*              INITIALIZE SYSTEM DEPENDENT VALUES                               
*                                                                               
SYSINIT  NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PRBLKEND         ADDRESSES OF WORKING STORAGE                 
         LA    R2,8(R2)                                                         
         ST    R2,PBAIO1                                                        
         LA    R2,4008(R2)                                                      
         ST    R2,PBAWS1                                                        
         LA    R2,4008(R2)                                                      
         ST    R2,PBAWS2                                                        
         LA    R2,1008(R2)                                                      
         ST    R2,PBAWS3                                                        
         LA    R2,IO                                                            
         ST    R2,PBAIO                                                         
         L     R1,=V(PRINTIO)       **** THIS IS TEMPORARY                      
         A     R1,RELO              **** PRINTIO AND PRNTVAL WILL               
         ST    R1,PBPRTIO           **** BECOME CORE RESIDENT MODULES           
         L     R1,=V(PRNTVAL)       **** AND WILL BE LOADED IN                  
         A     R1,RELO                                                          
         ST    R1,PBPRTVAL                                                      
         MVC   PBPRINT,VPRINT      *** PRINTIO NEEDS TO GET THIS                
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
*                                  SET SYSTEM DEPENDENT VALUES                  
         MVI   SYSTEM,C'P'         NETWORK                                      
         MVI   FRSTLAST,C'N'       ONLY CALL REPORT MODULES ONCE                
         MVI   MAXIOS,1            USES 1 I/O AREA                              
         MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         L     R1,=V(DUMMY)        END OF SYSTEM BASE                           
         A     R1,RELO                                                          
         ST    R1,SYSDUMMY                                                      
         MVC   GETUSER,VALIAGY      ROUTINE TO GET AGY NAME,ADDR                
         MVC   SYSFIL,=C'PRTFILE '                                              
         MVC   SYSDIR,=C'PRTDIR  '                                              
         MVC   LKEY,=H'25'         SET VALUES FOR UNIT FILE                     
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'33'                                                  
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         MVC   LWORK,=F'24000'     WE TOOK 24000 BYTES IN NMOD                  
         MVC   RCPROG(2),=C'NP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9041C00'   PRESET FOR SYSTEM CALLOVS                
         L     R1,=A(RECACT)       RECORD/ACTION DIRECTORY                      
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         B     EXIT                                                             
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
EXIT     XIT1                                                                   
         SPACE 1                                                                
         PRINT GEN                                                              
         GETEL (R5),DATADISP,ELCODE                                             
         PRINT NOGEN                                                            
         EJECT                                                                  
         SPACE                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
         SPACE 1                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING PRNTSYSD,R9                                                      
         LA    R7,PRTSYSDX                                                      
         LA    R7,8(R7)                                                         
         USING PRNTBLOK,R7                                                      
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VFLD                                                             
         B     VAGY                                                             
         B     VMED                                                             
         B     VCLT                                                             
         B     VPRD                                                             
         B     VEST                                                             
         B     VDIV                                                             
         B     VREG                                                             
         B     VDST                                                             
         B     VSTD                START                                        
         B     VEND                END                                          
         B     VDTYP               DATE TYPE                                    
*        B     VDOPT               DOLLAR OPTION                                
*        B     VPUB                                                             
*        B     VZON                                                             
*        B     VEDT                                                             
*        B     VINS                                                             
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
*              GETFLD ROUTINE - EXTRACT DATA FROM SCREEN FIELD                  
         SPACE 3                                                                
*              INPUTS              R2=A(FIELD HEADER)                           
*                                  FTERMFLG=1 FIELD IS OPTIONAL                 
*              ARGUMENTS           1 (R5) MAXIMUM NUMERIC VALUE                 
*                                         DEFAULT IS 255                        
*              LOCAL               R4=MAX NUMERIC VALUE                         
*              OUTPUTS             FLDH  CONTAINS FIELD HEADER                  
*                                  FLD   FIELD DATA SPACE FILLED                
*                                  R0    BINARY VALUE IF FIELD NUMERIC          
*                                  R1    FIELD LENGTH                           
*                                  CONDITION CODE ZERO IF R1=0                  
         SPACE 1                                                                
VFLD     L     R5,0(R1)            GET A(FULLWORD)                              
         LA    R4,255              SET R4 TO DEFAULT IN CASE R5=0               
         LTR   R5,R5               IF ZERO THEN                                 
         BZ    *+8                                                              
         L     R4,0(R5)            GET ACTUAL VALUE IN R4                       
         MVC   FLDH,0(R2)                                                       
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         ZIC   R1,FLDH                                                          
         SH    R1,=H'9'                                                         
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BNO   *+8                                                              
         SH    R1,=H'8'            (SUBTRACT 8 MORE)                            
         EX    R1,FLDMOVE                                                       
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GTFERRCK                                                         
         SPACE 1                                                                
GETFLD4  LR    R3,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  R3,0                                                             
         MVC   WORK(6),=6X'F0'     TEST FOR NUMERIC FIELD                       
         EX    R3,MOVEZONE                                                      
         CLC   WORK(6),=6X'F0'                                                  
         BNE   GETFLDX                                                          
         EX    R3,FLDPACK                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
         CR    R0,R4               CK IF GT SPECIFIED MAXIMUM                   
         BH    GTNOTNUM                                                         
         B     GETFLDX                                                          
         SPACE 1                                                                
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         B     GETFLDX                                                          
         SPACE 1                                                                
GTFERRCK CLI   FTERMFLG,1          IS THIS OK?                                  
         BNE   MISSERR                                                          
         XC    FLD,FLD                                                          
         B     GETFLDX                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
GETFLDX  LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         SPACE 1                                                                
FLDMOVE  MVC   FLD(0),8(R2)                                                     
         SPACE 1                                                                
FLDPACK  PACK  DUB,FLD(0)                                                       
         SPACE 1                                                                
MOVEZONE MVZ   WORK(0),8(R2)                                                    
         EJECT                                                                  
**************************************                                          
* VALIDATE AGENCY                                                               
*                                                                               
* OUTPUT  USERNAME,USERADDR,PBEFFAGY,PBEFAGNM,PBEFAGAD                          
*         PBSELAGY(IF AGY NOT=ALL)                                              
*                                                                               
*                                                                               
VAGY     DS    0H                                                               
*                                                                               
         L     R2,ATWA                                                          
         USING TWADSEC,R2                                                       
         MVC   PBSELAGY,TWAAGY                                                  
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING PAGYKEY,R4                                                       
         MVC   PAGYKAGY,TWAAGY                                                  
         MVI   PAGYKRCD,X'01'                                                   
         GOTO1 HIGH                                                             
         B     VAG3                                                             
VAG2     GOTO1 SEQ                                                              
*                                                                               
VAG3     CLC   PAGYKAGY,KEYSAVE   *TEST FOR FUTURE ACROSS AGY READS             
         BNE   NOAGY              *                                             
         CLI   PAGYKRCD,X'01'     *                                             
         BNE   VAG2               *                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         L     R5,PBAIO                                                         
         USING PAGYELEM,R5                                                      
         BAS   RE,GETEL                                                         
         BNE   NOAGY                                                            
         SPACE 1                                                                
         MVC   USERNAME,PAGYNAME                                                
         MVC   USERADDR,PAGYADDR                                                
         MVC   PBEFFAGY,PAGYKAGY                                                
*                                                                               
         MVC   PBACOM,ACOMFACS     SET COMFACS                                  
         B     EXIT                                                             
*                                                                               
NOAGY    MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
******************************************                                      
*                                                                               
* VALIDATE MEDIA CODE *                                                         
*                                                                               
* OUTPUT: PBSELMED,PBEFMDNM,PBEFFMED                                            
*                                                                               
*                                                                               
VMED     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BZ    *+10                                                             
         MVC   PBSELMED,FLD                                                     
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING PAGYKEY,R4                                                       
         MVC   PAGYKAGY,PBSELAGY                                                
         MVC   PAGYKMED,FLD        MEDIA                                        
         MVI   PAGYKRCD,X'01'                                                   
*                                                                               
         GOTO1 HIGH                                                             
         B     VMED1                                                            
VMEDSEQ  GOTO1 SEQ                                                              
*                                                                               
VMED1    CLC   PAGYKAGY,KEYSAVE                                                 
         BNE   INVMED                                                           
         CLI   PAGYKRCD,X'01'                                                   
         BNE   VMEDSEQ                                                          
         OC    PBSELMED,PBSELMED                                                
         BZ    VMED2                                                            
         CLC   PBSELMED,PAGYKMED                                                
         BNE   VMEDSEQ                                                          
VMED2    GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         L     R5,PBAIO                                                         
         USING PAGYELEM,R5                                                      
         BAS   RE,GETEL                                                         
         BNE   INVMED                                                           
*                                                                               
         MVC   PBEFMDNM,PAGYMED                                                 
         MVC   PBEFFMED,PAGYKMED                                                
         B     EXIT                                                             
         DROP  R4,R5                                                            
*                                                                               
INVMED   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
***************************************                                         
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
* OUTPUT: PBEFFFCLT,PBSELCLT,PBECLTNM,PBEFFOFC                                  
*                                                                               
*                                                                               
VCLT     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BZ    VCLTX                                                            
         MVC   PBSELCLT,FLD                                                     
*                                                                               
         XC    KEY,KEY             GET CLIENT RECORD                            
         LA    R4,KEY                                                           
         USING PCLTKEY,R4                                                       
         MVC   PCLTKAGY,PBSELAGY                                                
         MVC   PCLTKMED,PBSELMED                                                
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,PBSELCLT                                                
         GOTO1 HIGH                                                             
         B     VCLT4                                                            
CLTSEQ   GOTO1 SEQ                                                              
*                                                                               
VCLT4    CLC   PCLTKAGY,KEYSAVE                                                 
         BNE   INVCLT                                                           
         CLI   PBSELMED,0                                                       
         BE    *+14                                                             
         CLC   PCLTKMED,PBSELMED                                                
         BNE   INVCLT                                                           
         CLI   PCLTKRCD,X'02'                                                   
         BNE   CLTSEQ                                                           
         OC    PBSELCLT,PBSELCLT                                                
         BZ    VCLT2                                                            
         CLC   PCLTKCLT,PBSELCLT                                                
         BNE   CLTSEQ                                                           
VCLT2    GOTO1 GETREC                                                           
         L     R5,PBAIO                                                         
         USING PCLTELEM,R5                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVCLT                                                           
*                                                                               
         MVC   PBEFFCLT,PCLTKCLT                                                
         MVC   PBECLTNM,PCLTNAME                                                
         MVC   PBEFFOFC,PCLTOFF    CLIENT OFFICE                                
VCLTX    B     EXIT                                                             
INVCLT   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
******************************************                                      
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
* OUTPUT: PBSELPRD,PBEFFPRD,PBEFPRNM,PBEFFDIV                                   
*                                                                               
*                                                                               
VPRD     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BZ    *+10                                                             
         MVC   PBSELPRD,FLD                                                     
*                                                                               
         XC    KEY,KEY             GET PRODUCT RECORD                           
         LA    R4,KEY                                                           
         USING PPRDKEY,R4                                                       
         MVC   PPRDKAGY,PBSELAGY                                                
         MVC   PPRDKMED,PBSELMED                                                
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,PBSELCLT                                                
         MVC   PPRDKPRD,PBSELPRD                                                
         GOTO1 HIGH                                                             
         B     VPRD4                                                            
PRDSEQ   GOTO1 SEQ                                                              
*                                                                               
VPRD4    CLC   PPRDKAGY,KEYSAVE                                                 
         BNE   INVPRD                                                           
         CLI   PBSELMED,0                                                       
         BE    *+14                                                             
         CLC   PPRDKMED,PBSELMED                                                
         BNE   INVPRD                                                           
         CLI   PPRDKRCD,X'06'                                                   
         BNE   PRDSEQ                                                           
         OC    PBSELCLT,PBSELCLT                                                
         BZ    *+14                                                             
         CLC   PPRDKCLT,PBSELCLT                                                
         BNE   PRDSEQ                                                           
         OC    PBSELPRD,PBSELPRD                                                
         BZ    VPRD6                                                            
         CLC   PPRDKPRD,PBSELPRD                                                
         BNE   PRDSEQ                                                           
VPRD6    GOTO1 GETREC                                                           
         L     R5,PBAIO                                                         
         USING PPRDELEM,R5                                                      
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVPRD                                                           
         MVC   PBEFFPRD,PPRDKPRD                                                
         MVC   PBEFPRNM,PPRDNAME                                                
         MVC   PBEFFDIV,PPRDDIV                                                 
VPRDX    B     EXIT                                                             
*                                                                               
INVPRD   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************                                 
*                                                                               
* VALIDATE ESTIMATE                                                             
*                                                                               
* OUTPUT: PBSELEST,PBEFFEST,PBEFESNM,PBEFESTR,PBEFEEND                          
*                                                                               
*                                                                               
VEST     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BZ    EST2                                                             
         STH   R0,HALF             CONTAINS BINARY AFTER GETFLD                 
         MVC   PBSELEST,HALF                                                    
         CLC   FLD(3),=C'ALL'                                                   
         BNE   EST2                                                             
         XC    FLD,FLD                                                          
*                                                                               
EST2     XC    KEY,KEY             GET ESTIMATE RECORD                          
         LA    R4,KEY                                                           
         USING PESTKEY,R4                                                       
         MVC   PESTKAGY,PBSELAGY                                                
         MVC   PESTKMED,PBSELMED                                                
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,PBSELCLT                                                
         MVC   PESTKPRD,PBSELPRD                                                
         MVC   PESTKEST,PBSELEST                                                
         GOTO1 HIGH                                                             
         B     VEST4                                                            
ESTSEQ   GOTO1 SEQ                                                              
*                                                                               
VEST4    CLC   PESTKAGY,KEYSAVE                                                 
         BNE   INVEST                                                           
         CLI   PBSELMED,0                                                       
         BE    *+14                                                             
         CLC   PESTKMED,PBSELMED                                                
         BNE   INVEST                                                           
         CLI   PESTKRCD,X'07'                                                   
         BNE   ESTSEQ                                                           
         OC    PBSELCLT,PBSELCLT                                                
         BZ    *+14                                                             
         CLC   PESTKCLT,PBSELCLT                                                
         BNE   ESTSEQ                                                           
         OC    PBSELPRD,PBSELPRD                                                
         BZ    *+14                                                             
         CLC   PESTKPRD,PBSELPRD                                                
         BNE   PRDSEQ                                                           
         OC    PBSELEST,PBSELEST                                                
         BZ    VEST6                                                            
         CLC   PESTKEST,PBSELEST                                                
         BNE   PRDSEQ                                                           
VEST6    GOTO1 GETREC                                                           
         L     R5,PBAIO                                                         
         USING PESTELEM,R5                                                      
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVEST                                                           
         MVC   PBEFFEST,PESTKEST                                                
         MVC   PBEFESNM,PESTNAME                                                
         CLI   FLD,0               IF NOT FOR SPECIFIC EST                      
         BE    ESTX                DONT FILL IN EST STRT/END                    
         MVC   PBEFESTR,PESTST     EST START(YYMMDD)                            
         MVC   PBEFEEND,PESTEND    EST END                                      
ESTX     B     EXIT                                                             
*                                                                               
INVEST   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
******************************************************                          
*                                                                               
* VALIDATE DIVISION                                                             
*                                                                               
* OUTPUT: PBSELDIV,PBEFDVNM,PBEFFDIV                                            
*                                                                               
*                                                                               
*                                                                               
VDIV     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         MVC   PBSELDIV,FLD                                                     
*                                                                               
         XC    KEY,KEY             GET DIVISION RECORD                          
         LA    R4,KEY                                                           
         USING PDIVKEY,R4                                                       
         MVC   PDIVKAGY,PBSELAGY                                                
         MVC   PDIVKMED,PBSELMED                                                
         MVI   PDIVKRCD,X'03'                                                   
         MVC   PDIVKCLT,PBSELCLT                                                
         MVC   PDIVKDIV,PBSELDIV                                                
         GOTO1 HIGH                                                             
         B     VDIV4                                                            
DIVSEQ   GOTO1 SEQ                                                              
*                                                                               
VDIV4    CLC   PDIVKAGY,KEYSAVE                                                 
         BNE   INVDIV                                                           
         CLI   PBSELMED,0                                                       
         BE    *+14                                                             
         CLC   PDIVKMED,PBSELMED                                                
         BNE   INVDIV                                                           
         CLI   PDIVKRCD,X'03'                                                   
         BNE   DIVSEQ                                                           
         OC    PBSELCLT,PBSELCLT                                                
         BZ    *+14                                                             
         CLC   PDIVKCLT,PBSELCLT                                                
         BNE   DIVSEQ                                                           
         OC    PBSELDIV,PBSELDIV                                                
         BZ    VDIV6                                                            
         CLC   PDIVKDIV,PBSELDIV                                                
         BNE   DIVSEQ                                                           
VDIV6    GOTO1 GETREC                                                           
         L     R5,PBAIO                                                         
         USING PDIVELEM,R5                                                      
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVDIV                                                           
         MVC   PBEFDVNM,PDIVNAME                                                
         MVC   PBEFFDIV,PDIVKDIV                                                
         B     EXIT                                                             
INVDIV   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
*******************************************                                     
*                                                                               
* VALIDATE REGION                                                               
*                                                                               
* OUTPUT: PBSELREG,PBEFFREG,PBEFRGNM                                            
*                                                                               
*                                                                               
VREG     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         MVC   PBSELREG,FLD             IF REGION=NNN                           
         OC    PBSELDIV,PBSELDIV           THEN DIVISION MUST = NNN             
         BNZ   VREG5                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VREG5    XC    KEY,KEY             GET REGION RECORD                            
         LA    R4,KEY                                                           
         USING PREGKEY,R4                                                       
         MVC   PREGKAGY,PBSELAGY                                                
         MVC   PREGKMED,PBSELMED                                                
         MVI   PREGKRCD,X'04'                                                   
         MVC   PREGKCLT,PBSELCLT                                                
         MVC   PREGKDIV,PBSELDIV                                                
         MVC   PREGKREG,PBSELREG                                                
         GOTO1 HIGH                                                             
         B     VREG6                                                            
REGSEQ   GOTO1 SEQ                                                              
*                                                                               
VREG6    CLC   PREGKAGY,KEYSAVE                                                 
         BNE   INVREG                                                           
         CLI   PBSELMED,0                                                       
         BE    *+14                                                             
         CLC   PREGKMED,PBSELMED                                                
         BNE   INVREG                                                           
         CLI   PREGKRCD,X'04'                                                   
         BNE   REGSEQ                                                           
         OC    PBSELCLT,PBSELCLT                                                
         BZ    *+14                                                             
         CLC   PREGKCLT,PBSELCLT                                                
         BNE   REGSEQ                                                           
         OC    PBSELDIV,PBSELDIV                                                
         BZ    *+14                                                             
         CLC   PREGKDIV,PBSELDIV                                                
         BNE   REGSEQ                                                           
         OC    PBSELREG,PBSELREG                                                
         BZ    VREG8                                                            
         CLC   PREGKREG,PBSELREG                                                
         BNE   REGSEQ                                                           
VREG8    GOTO1 GETREC                                                           
         L     R5,PBAIO                                                         
         USING PREGELEM,R5                                                      
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVREG                                                           
         MVC   PBEFFREG,PREGKREG                                                
         MVC   PBEFRGNM,PREGNAME                                                
         B     EXIT                                                             
INVREG   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
**************************************************                              
*                                                                               
* VALIDATE DISTRICT                                                             
*                                                                               
* OUTPUT: PBEFFDST,PBEDSTNM,PBSELDST                                            
*                                                                               
*                                                                               
VDST     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BZ    EXIT                                                             
         MVC   PBSELDST,FLD             IF DISTRICT=NNN                         
         OC    PBSELREG,PBSELREG           THEN REGION MUST = NNN               
         BNZ   VDST5                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VDST5    XC    KEY,KEY             GET DISTRICT RECORD                          
         LA    R4,KEY                                                           
         USING PDSTKEY,R4                                                       
         MVC   PDSTKAGY,PBSELAGY                                                
         MVC   PDSTKMED,PBSELMED                                                
         MVI   PDSTKRCD,X'05'                                                   
         MVC   PDSTKCLT,PBSELCLT                                                
         MVC   PDSTKDIV,PBSELDIV                                                
         MVC   PDSTKREG,PBSELREG                                                
         MVC   PDSTKDST,PBSELDST                                                
         GOTO1 HIGH                                                             
         B     VDST6                                                            
DSTSEQ   GOTO1 SEQ                                                              
*                                                                               
VDST6    CLC   PDSTKAGY,KEYSAVE                                                 
         BNE   INVDST                                                           
         CLI   PBSELMED,0                                                       
         BE    *+14                                                             
         CLC   PDSTKMED,PBSELMED                                                
         BNE   INVDST                                                           
         CLI   PDSTKRCD,X'05'                                                   
         BNE   DSTSEQ                                                           
         OC    PBSELCLT,PBSELCLT                                                
         BZ    *+14                                                             
         CLC   PDSTKCLT,PBSELCLT                                                
         BNE   DSTSEQ                                                           
         OC    PBSELDIV,PBSELDIV                                                
         BZ    *+14                                                             
         CLC   PDSTKDIV,PBSELDIV                                                
         BNE   DSTSEQ                                                           
         OC    PBSELREG,PBSELREG                                                
         BZ    VDST8                                                            
         CLC   PDSTKREG,PBSELREG                                                
         BNE   DSTSEQ                                                           
         OC    PBSELDST,PBSELDST                                                
         BZ    VDST8                                                            
         CLC   PDSTKDST,PBSELDST                                                
         BNE   DSTSEQ                                                           
VDST8    GOTO1 GETREC                                                           
         L     R5,PBAIO                                                         
         USING PDSTELEM,R5                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   INVREG                                                           
         MVC   PBEFFDST,PDSTKDST                                                
         MVC   PBEDSTNM,PDSTNAME                                                
         B     EXIT                                                             
INVDST   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         DROP  R4,R5                                                            
         EJECT                                                                  
****************************************                                        
* VALIDATE START DATE                                                           
*                                                                               
* OUTPUT: PBSELSTR(YMD),PBSELST2(YYMMDD)                                        
*                                                                               
*                                                                               
VSTD     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BNZ   VSTD5                                                            
         CLI   PBSELEST,0                                                       
         BE    DATERR2                   IF NO EST/DATE REQUIRED                
         CLC   PBSELEST(3),=C'ALL'      IF EST=ALL/DATE REQUIRED                
         BE    DATERR2                                                          
         B     VSTDX                                                            
VSTD5    GOTO1 DATVAL,DMCB,FLD,PBSELST2             (YYMMDD)                    
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,PBSELST2,(3,PBSELSTR)    (YMD)                       
         OC    PBSELEST,PBSELEST                                                
         BZ    VSTDX               IF NO EST/THEN FINISHED                      
         CLC   PBEFEEND,PBSELST2      EST END/SEL START                         
         BNL   VSTD7                                                            
****     MVI   ERROR,                 EST END BEFORE SEL START                  
         B     TRAPERR                                                          
VSTD7    CLC   PBSELST2,PBEFESTR      SEL START/EST START                       
         BNL   VSTDX                                                            
*****    MVI   ERROR,                 SEL START BEFORE EST START                
         B     TRAPERR                                                          
VSTDX    B     EXIT                                                             
         EJECT                                                                  
****************************************                                        
* VALIDATE END  DATE                                                            
*                                                                               
* OUTPUT: PBSELEND(YMD),PBSELEN2(YYMMDD)                                        
*                                                                               
*                                                                               
VEND     DS    0H                                                               
         GOTO1 VGETFLD                                                          
         BNZ   VEND5                                                            
         CLI   PBSELEST,0                                                       
         BE    DATERR2                   IF NO EST/DATE REQUIRED                
         CLC   PBSELEST(3),=C'ALL'      IF EST=ALL/DATE REQUIRED                
         BE    DATERR2                                                          
         B     VENDX                                                            
*                                                                               
VEND5    GOTO1 DATVAL,DMCB,FLD,PBSELEN2             (YYMMDD)                    
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         CLC   PBSELEN2,PBSELST2    SEL END/SEL START                           
         BNL   VEND6                                                            
***      MVI   ERROR                END DATE BEFORE START DATE                  
         B     TRAPERR                                                          
VEND6    GOTO1 DATCON,DMCB,PBSELEN2,(3,PBSELEND)    (YMD)                       
         OC    PBSELEST,PBSELEST                                                
         BZ    VENDX                    IF NO EST/FINISHED                      
         CLC   PBEFESTR,PBSELEN2           EST START/SEL END                    
         BNH   VEND7                                                            
*        MVI   ERROR,                      EST START AFTER SEL END              
         B     TRAPERR                                                          
VEND7    CLC   PBSELST2,PBEFESTR           SEL START/EST START                  
         BNL   VENDX                                                            
*****    MVI   ERROR,                      SEL START BEFORE EST START           
         B     TRAPERR                                                          
VENDX    B     EXIT                                                             
*                                                                               
DATERR   DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
DATERR2  DS    0H                                                               
****     MVI   ERROR           NO EST / DATES REQUIRED                          
         B     TRAPERR                                                          
         EJECT                                                                  
******************************************                                      
*                                                                               
* VALIDATE DATE TYPE                                                            
*                                                                               
*                                                                               
*                                                                               
VDTYP    DS    0H                                                               
         MVI   PBDATTYP,C'I'       INSERTION DATE (DEFAULT)                     
         GOTO1 VGETFLD                                                          
         BZ    DTYPX                                                            
         CLI   FLD,C'I'            INSERTION                                    
         BE    DTYPX                                                            
         CLI   FLD,C'P'                                                         
         BNE   *+12                                                             
         MVI   PBDATTYP,C'P'       PAYABLE                                      
         B     DTYPX                                                            
         CLI   FLD,C'B'           BILLABLE                                      
         BNE   *+12                                                             
         MVI   PBDATTYP,C'B'                                                    
         B     DTYPX                                                            
         CLI   FLD,C'C'            CLOSING                                      
         BNE   *+12                                                             
         MVI   PBDATTYP,C'C'                                                    
         B     DTYPX                                                            
         CLI   FLD,C'S'            ON SALE DATE                                 
         BNE   DTYPERR                                                          
         MVI   PBDATTYP,C'S'                                                    
DTYPX    B     EXIT                                                             
*                                                                               
DTYPERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 3                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'USERP   ',AL1(01),X'0031'                                
         DC    X'01',C'NEWPRINT',AL1(02),X'0031'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 1                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
*                                                                               
*                                                                               
         DC    X'03',AL1(01,01),X'F2510000C0',C'    '  USERP    MAINT           
         DC    X'03',AL1(01,10),X'E2510051F8',C'USRP'           LIST            
         DC    X'03',AL1(02,12),X'F352005238',C'NPRT'  NEWPRINT                 
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 1                                                                
*              PRNTINCLS                                                        
*    AND REQUIRED PRINT RECORDS                                                 
       ++INCLUDE PRNTINCLS                                                      
       ++INCLUDE PAGYREC                                                        
       ++INCLUDE PCLTREC                                                        
       ++INCLUDE PDIVREC                                                        
       ++INCLUDE PREGREC                                                        
       ++INCLUDE PDSTREC                                                        
       ++INCLUDE PESTREC                                                        
       ++INCLUDE PPRDREC                                                        
       ++INCLUDE PBUYREC                                                        
TWADSEC  DSECT                                                                  
       ++INCLUDE DDGENCTWAD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042PRMED00   05/01/02'                                      
         END                                                                    
