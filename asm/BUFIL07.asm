*          DATA SET BUFIL07    AT LEVEL 020 AS OF 05/01/02                      
*PHASE T50207A                                                                  
         TITLE 'T50207 - BUDGET CONTROL LFM - DATA ENTRY SCREEN'                
T50207   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI07**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
ENTRY1   GOTO1 VSETADD                                                          
*                                                                               
ENTRY2   LA    R2,CONACTH          CHECK ACTIONS                                
         MVI   ERROR,INVRCACT                                                   
         CLI   ACTNUM,ACTDIS       TEST FOR DISPLAY                             
         BE    *+12                                                             
         CLI   ACTNUM,ACTOVER      TEST FOR OVERRIDE                            
         BNE   TRAPERR                                                          
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    ENTRY4                                                           
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    ENTRY6                                                           
         B     ENTRYX                                                           
*                                                                               
ENTRY4   CLC   TWALREC,RECNUM      TEST FIRST TIME FOR SCREEN                   
         BE    *+10                                                             
         XC    SVCTLVAL,SVCTLVAL   YES-CLEAR SAVED CONTROL VALUES               
         GOTO1 VUPKEY              UPDATE KEY FIELD TABLE                       
         OI    CONACTH+6,X'81'     XMIT ACTION MODIFIED FOR RE-ENTRY            
         BAS   RE,VALHED           VALIDATE HEADLINE FIELDS                     
         MVC   MYACT,ACTNUM        SET ACTION FROM GENCON ACTION                
         CLC   CTLVALS(CTLVALLN),SVCTLVAL TEST FOR CONTROL VALUE CHANGE         
         BE    *+8                                                              
         MVI   MYACT,ACTDIS        FORCE DIS FOR CONTROL VALUE CHANGE           
         MVC   SVCTLVAL,CTLVALS    UPDATE SAVED CONTROL VALUES                  
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),OUTKEY                                              
         OI    WHENOK,X'01'        BYPASS GENCON MAINTENANCE                    
         B     ENTRYX                                                           
*                                                                               
ENTRY6   BAS   RE,GENMON           BUILD MONTH TABLE                            
         BAS   RE,GETDATA          GET RECORD VALUES                            
         BAS   RE,ADJDIS           ADJUST RECD VALS TO SCR PRECISION            
         CLI   MYACT,ACTDIS        TEST FOR DISPLAY                             
         BE    ENTRY8                                                           
*                                                                               
         BAS   RE,EDVAL            EDIT VALUES                                  
         GOTO1 TOTAL,INPVALS       RE-DISPLAY TOTAL LINE                        
         BAS   RE,OVER             TEST OVERRIDES AND UPDATE RECORDS            
         MVC   CONHEAD(L'OVERMSG),OVERMSG                                       
         CLI   OVERSW,YES          TEST IF OVERRIDES INPUT                      
         BE    *+14                YES                                          
         MVC   CONHEAD(L'NOOVERS),NOOVERS                                       
         B     ENTRY7                                                           
*                                                                               
         TM    PLANIND,BUPLNDAT    TEST IF PLAN HAD DATA RECORDS                
         BO    *+8                 YES                                          
         BAS   RE,UPPLAN           UPDATE PLAN TURNING ON INDICATOR             
*                                                                               
ENTRY7   LA    R2,ENTCLTH          POSITION CURSOR                              
         OI    6(R2),X'80'         XMIT CURSOR FIELD                            
         ST    R2,ACURFORC         CURSOR OVERRIDE                              
         B     ENTRYX                                                           
*                                                                               
ENTRY8   BAS   RE,PERDIS           DISPLAY PERIODS                              
         BAS   RE,DISVAL           DISPLAY VALUES                               
         CLI   ACTIVITY,0          TEST FOR DISPLAYING ACTIVITY                 
         BE    *+12                NO                                           
         BAS   RE,ACTVDIS          YES                                          
         B     ENTRY9                                                           
*                                                                               
ENTRY9   GOTO1 TOTAL,RECADJ        DISPLAY TOTALS LINE                          
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
         LA    R2,CONACTH          POINT CURSOR AT FIRST                        
         CLI   ACTNUM,ACTOVER      TEST FOR FORCED DISPLAY                      
         BNE   *+14                                                             
         MVC   CONHEAD+L'DISMSG+1(L'EXTRAMSG),EXTRAMSG                          
         LA    R2,ENTVAL1H         CURSOR AT FIRST VALUE FIELD                  
         OI    6(R2),X'80'                                                      
         ST    R2,ACURFORC         SET CURSOR POSITION                          
*                                                                               
ENTRYX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE HEADLINE KEY FIELDS                                   
*                                                                               
VALHED   NTR1                                                                   
         GOTO1 VVALCLT,PARAS,ENTCLTH,0                                          
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVC   SVCLTVAL,CLTVALS    SAVE CLIENT RECORD VALUES                    
         MVC   ENTCLN(L'CLTNAM),CLTNAM                                          
         OI    ENTCLNH+6,X'80'     XMIT                                         
*                                                                               
VALHED2  GOTO1 VVALPRD,PARAS,ENTPRDH,0                                          
         MVC   SVPRDVAL,PRDVALS    SAVE AWAY PRODUCT VALUES                     
         MVC   ENTPRN(L'PRDNAM),PRDNAM                                          
         OI    ENTPRNH+6,X'80'     XMIT                                         
*                                                                               
VALHED4  GOTO1 VVALPLAN,PARAS,ENTPLAH,0                                         
*                                                                               
         MVC   SVPLNVAL,PLANVALS   SAVE PLAN VALUES                             
         MVC   STMON,PLANST        SET SCREEN START/END = PLAN                  
         MVC   ENDMON,PLANEND      START/END YM                                 
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVI   NEWPLAN,NO                                                       
         CLC   SVPLNKEY,NDLVKEY    TEST FOR CHANGE IN PLAN                      
         BE    *+8                                                              
         MVI   NEWPLAN,YES         YES                                          
         MVC   SVPLNKEY,NDLVKEY    EXTRACT PLAN'S REGULAR KEY                   
         XC    ENTPLN,ENTPLN                                                    
         OI    ENTPLNH+6,X'80'                                                  
         MVC   ENTPLN(L'PLANNAM),PLANNAM                                        
         GOTO1 VPEROUT,PARAS,(1,PLANST),WORK                                    
         LA    R1,ENTPLN+L'PLANNAM+1                                            
         MVC   0(13,R1),WORK                                                    
*                                                                               
VALHED5  MVI   SVNSNAPS,0          SAVE PLAN SNAPSHOT LIST                      
         XC    SVSNPLST,SVSNPLST                                                
         MVI   ELCODE,BUSNELQ                                                   
         L     R4,NDIOA                                                         
         BAS   RE,GETEL                                                         
         BNE   VALHED6             NO SNAPSHOT ELEMENT                          
         USING BUSND,R6                                                         
         ZIC   R1,BUSNLEN                                                       
         SH    R1,=Y(BUSNAPS-BUSND+1)                                           
         EX    R1,*+8              EXTRACT SNAPSHOT LIST                        
         B     *+10                                                             
         MVC   SVSNPLST(0),BUSNAPS                                              
*                                                                               
         LA    R1,1(R1)            RESTORE SNAPSHOT LIST LENGTH                 
         SR    R0,R0                                                            
         LA    RE,L'BUSNAPS                                                     
         DR    R0,RE               R1=N'SNAPSHOTS                               
         STC   R1,SVNSNAPS                                                      
*                                                                               
VALHED6  LA    R2,ENTOUTH          VALIDATE OUTLINE                             
         BAS   RE,OUTPF            HANDLE PF KEYS FOR OUTLINES                  
         BE    VALHED7                                                          
         GOTO1 VGETFLD,PARAS,(X'FF',(R2))                                       
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    SPERR                                                            
         MVC   OUTCODE,FLD                                                      
         GOTO1 VFINDOUT,PARAS,OUTCODE,NDIOA                                     
         BNE   SPERR                                                            
*                                                                               
         GOTO1 VTRACE                                                           
         GOTO1 VGETVAL                                                          
         L     R3,NDLEVPTR                                                      
         MVC   OUTKEY,NDLVKEY      SAVE OUTLINE'S DIRECTORY KEY                 
         MVI   ERROR,PARERR                                                     
         CLI   OUTLEV,MAXOUTS      BOTTOM LEVEL HAS NO CHILDREN                 
         BE    *+18                                                             
         LA    R3,NDLVTABL(R3)     BUMP TO NEXT LEVEL ENTRY                     
         OC    NDLVNOD,NDLVNOD     TEST IF OUTLINE IS A PARENT                  
         BNZ   SPERR               YES-SO STOP INPUT                            
         MVI   ERROR,ROWFERR                                                    
         MVI   ELCODE,BUPOLELQ     LOOK FOR ROW FORMULA                         
         BAS   RE,GETEL                                                         
         BE    SPERR               YES-NO INPUT FOR ROW FORMULA OUTLINE         
*                                                                               
VALHED7  MVC   SVOUTVAL,OUTVALS                                                 
         MVC   SVNKEY,NODKEY                                                    
         MVC   ENTOUTN,OUTNAME                                                  
         OI    ENTOUTNH+6,X'80'                                                 
         GOTO1 VUPKEY              RE-DO THE KEY TABLE UPDATE                   
*                                                                               
VALHED8  LA    R2,ENTDTH           VALIDATE DATA TYPE                           
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    SPERR                                                            
         LA    R4,KEY              MAKE KEY ADDRESSABLE                         
         MVC   BUKEY,SVPLNKEY      START WITH PLAN KEY                          
         MVI   BUDSUB,BUDSUBQ                                                   
         MVC   BUDTYP,FLD                                                       
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(L'BUKEY),KEYSAVE                                             
         BNE   SPERR                                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ERROR,COLFERR                                                    
         MVI   ELCODE,BUPOLELQ     SEARCH FOR FORMULA ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    SPERR               FOUND-NO INPUT                               
*                                                                               
         BAS   RE,GETDT            GET DATA TYPE RECORD VALUES                  
         MVC   DTYP,DTCODE         SET DATA TYPE IN CONTROL VALUES              
*                                                                               
         XC    ENTDTD,ENTDTD                                                    
         MVC   ENTDTD(L'DTHEAD),DTHEAD                                          
         LA    RE,ENTDTD+L'DTHEAD+1                                             
         MVC   0(L'DTSCEXP,RE),DTSCEXP DISPLAY SCALE                            
         LA    RE,L'DTSCEXP+1(RE)                                               
         CLI   DTDEC,0             TEST FOR ANY DECIMAL PLACES                  
         BE    *+14                NO                                           
         MVI   0(RE),C'.'          DECIMAL PLACES AS '.999' ETC.                
         MVC   1(1,RE),DTDECEXP                                                 
         OI    ENTDTDH+6,X'80'                                                  
*                                                                               
VALHED10 MVC   SCALE,DTSC          SET DEFAULT CONTROL VALUES                   
         MVC   DECIMAL,DTDEC                                                    
         MVI   PERIOD,C'M'                                                      
         BAS   RE,OPTVAL           VALIDATE OPTIONS FIELD                       
         CLI   STMON,0             TEST MISSING THE YEAR                        
         BNE   VALHED12            NO                                           
         MVI   ERROR,YEARERR                                                    
         L     R2,FADDR            SET CURSOR POSITION FOR ERROR                
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         B     SPERR                                                            
*                                                                               
VALHED12 ZIC   RE,SCALE                                                         
         ZIC   RF,DECIMAL                                                       
         SR    RE,RF               SCALE-DECIMAL                                
         STH   RE,SCRSHIFT         SET SCREEN INTEGER SHIFT                     
         STC   RE,SCRPREC                                                       
         BNM   VALHEDX                                                          
         LPR   RE,RE               FORCE NEGATIVE DIFFERENCE POSITIVE           
         STC   RE,SCRPREC                                                       
         OI    SCRPREC,X'80'       TURN ON NEGATIVE EXPONENT BIT                
*                                                                               
VALHEDX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS PF KEYS FOR OUTLINES                                   
* ON EXIT, CC=EQ IF OUTLINE DETERMINED BY PF KEY, CC=NEQ TO EDIT                
*          OUTLINE FIELD                                                        
*                                                                               
OUTPF    NTR1                                                                   
         MVC   NODCOMM(4),=C'LSEQ'                                              
         CLI   THISPF,PF1          TEST FOR PF1                                 
         BE    OUTPF1              YES                                          
         CLI   NEWPLAN,YES         TEST IF PLAN HAS CHANGED                     
         BE    OUTPFN              YES-IGNORE PF KEYS AND EDIT FIELD            
*                                                                               
         CLI   THISPF,PF8          TEST FOR PF8                                 
         BE    OUTPF2                                                           
         MVC   NODCOMM(4),=C'BSEQ'                                              
         CLI   THISPF,PF7                                                       
         BE    OUTPF2                                                           
         B     OUTPFN                                                           
*                                                                               
* POSITION TO FIRST OUTLINE IN PLAN                                             
*                                                                               
OUTPF1   LA    R1,OUTPFHK                                                       
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   RECFIND,YES         TEST IF FOUND FIRST RECORD                   
         BE    OUTPFY              YES                                          
         CLI   NDERR,0             TEST FOR ERROR                               
         BE    OUTPF4              NO-CONTINUE                                  
         B     OUTPFN              YES-NO MORE OUTLINES IN PLAN                 
*                                                                               
* RE-READ LAST OUTLINE ON SCREEN                                                
*                                                                               
OUTPF2   MVC   NODKEY,SVNKEY                                                    
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FIND PREVIOUS OR NEXT OUTLINE                                                 
*                                                                               
OUTPF4   MVI   NDSQBACK,3          READ UNTIL PLAN ENCOUNTERED                  
         CLI   NODCOMM,C'B'        TEST IF READING BACKWARDS                    
         BNE   *+8                                                              
         MVI   NDSKIP,YES                                                       
         LA    R1,OUTPFHK                                                       
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,NODCOMM,NODKEY,0                             
         CLI   RECFIND,YES         TEST IF NEW OUTLINE FOUND                    
         BE    OUTPFY              YES                                          
*                                                                               
* SCROLL FAILED-RE-READ LAST OUTLINE OR EXIT WITH ERROR                         
*                                                                               
OUTPF6   CLI   THISPF,PF1                                                       
         BNE   *+12                                                             
         MVI   ERROR,NOAVLERR                                                   
         B     TRAPERR                                                          
*                                                                               
         MVC   NODKEY,SVNKEY                                                    
         LA    R1,OUTPFHK                                                       
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    SVCTLVAL,SVCTLVAL   FORCE DISPLAY MODE                           
         B     OUTPFY                                                           
*                                                                               
OUTPFY   CR    RB,RB               FORCE CC=EQ                                  
         B     OUTPFX                                                           
*                                                                               
OUTPFN   LTR   RB,RB               FORCE CC=NEQ                                 
*                                                                               
OUTPFX   B     XIT                                                              
         SPACE 2                                                                
* HOOK ROUTINE TO PROCESS OUTLINE RECORDS                                       
*                                                                               
OUTPFHK  ST    RE,SAVERE                                                        
         CLI   NDMODE,NDPROC                                                    
         BNER  RE                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCOUT                                                  
         BZ    OUTPFHKX                                                         
*                                                                               
         GOTO1 VGETVAL                                                          
         L     R3,NDLEVPTR                                                      
         CLI   OUTLEV,MAXOUTS      BOTTOM LEVEL HAS NO CHILDREN                 
         BE    *+18                                                             
         LA    R3,NDLVTABL(R3)     POINT TO NEXT LEVEL                          
         OC    NDLVNOD,NDLVNOD     TEST IF OUTLINE IS A PARENT                  
         BNZ   OUTPFHKX            YES                                          
         MVI   ELCODE,BUPOLELQ     SEARCH FOR POLISH FORMULA ELEM               
         BAS   RE,GETEL                                                         
         BE    OUTPFHKX            SKIP OUTLINE THAT HAS FORMULA                
*                                                                               
OUTPFHK2 L     R3,NDLEVPTR                                                      
         MVC   OUTKEY,NDLVKEY      SAVE OUTLINE'S REGULAR KEY                   
         MVC   DUB,OUTCODE                                                      
         OC    DUB,SPACES                                                       
         MVC   ENTOUT,DUB          XMIT BACK OUTLINE CODE                       
         OI    ENTOUTH+6,X'80'                                                  
         MVI   RECFIND,YES         SET OUTLINE FOUND SWITCH                     
         MVI   NDMODE,NDEND        FORCE NODIO TO STOP                          
         MVC   CONACT,=C'OVERRIDE'                                              
         OI    CONACTH+6,X'80'     FORCE ACTION 'OVERRIDE' NEXT                 
         MVI   ACTNUM,ACTOVER                                                   
*                                                                               
OUTPFHKX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT OPTIONS FIELD (KEY=PARAMETER VALUE,...)                   
*                                                                               
OPTVAL   NTR1                                                                   
         LA    R2,ENTOPTH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0            TEST FOR NO DATA IN FIELD                    
         BE    OPTVALX                                                          
         XC    FLAST,FLAST         START EDIT AT BEGINNING OF FIELD             
*                                                                               
OPTVAL2  XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUALS        LOOK FOR EQUALS SIGN                         
         GOTO1 VFVAL                                                            
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0                                                         
         BNE   OPTVAL4                                                          
         CLI   FSTOP,X'FF'         TEST FOR EOF                                 
         BE    OPTVALX             YES                                          
         B     OPTVALR                                                          
*                                                                               
OPTVAL4  CLI   FLDH+5,L'OPTNAME    VALIDATE THE KEYWORD                         
         BH    OPTVALR                                                          
         LA    R0,OPTIONS                                                       
         LA    R3,OPTTAB                                                        
         USING OPTTABD,R3                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
OPTVAL6  CLC   FLDH+5(1),OPTMINL   TEST FOR MINIMUM LENGTH FOR THIS KEY         
         BL    OPTVAL7             NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),OPTNAME                                                   
         BE    OPTVAL8             VALID KEYWORD                                
OPTVAL7  LA    R3,OPTTABL(R3)                                                   
         BCT   R0,OPTVAL6                                                       
         B     OPTVALR                                                          
*                                                                               
OPTVAL8  XC    FTERM,FTERM         EXTRACT PARAMETER VALUE                      
         MVI   FTERM,COMMA                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    OPTVALR             NO                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,OPTROUT        GET DISP TO VALIDATION ROUTINE               
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         B     OPTVAL2                                                          
*                                                                               
OPTVALX  B     XIT                                                              
         SPACE 2                                                                
OPTVALR  B     SPERR                                                            
         DROP  R3                                                               
         EJECT                                                                  
* FORMAT OPTIONS PARAMETER VALIDATION ROUTINES                                  
*                                                                               
OPTSCA   ST    RE,SAVERE                                                        
         CLI   FLDH+5,MAXSCALE+2                                                
         BH    OPTVALR                                                          
         CLI   FLD,LPAREN          TEST FOR LEFT PAREN STARTING FIELD           
         BNE   OPTSCA2                                                          
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'1'                                                         
         BZ    OPTVALR                                                          
         STC   R1,FLDH+5           REMOVE LEFT PAREN AND ADJUST LEN             
         MVC   DUB,FLD+1                                                        
         MVC   FLD(L'DUB),DUB                                                   
*                                                                               
OPTSCA2  ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,FLD(R1)                                                       
         CLI   0(RE),RPAREN                                                     
         BNE   OPTSCA4                                                          
         LTR   R1,R1               TEST FOR SOMETHING BESIDES RPAREN            
         BZ    OPTVALR                                                          
         STC   R1,FLDH+5                                                        
         MVI   0(RE),C' '                                                       
*                                                                               
OPTSCA4  CLI   FLDH+5,MAXSCALE                                                  
         BH    OPTVALR                                                          
         CLI   FLDH+5,1                                                         
         BNE   OPTSCA5                                                          
         CLI   FLD,C'1'            TEST FOR UNITS                               
         BNE   OPTSCA5             NO                                           
         MVI   SCALE,0             YES-SET SCALE TO ZERO                        
         B     OPTSCAX                                                          
*                                                                               
OPTSCA5  ZIC   R0,FLDH+5                                                        
         LA    RE,FLD                                                           
         CLI   0(RE),C'0'          TEST FOR A ZERO                              
         BNE   OPTVALR                                                          
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
         MVC   SCALE,FLDH+5        SET SCALE                                    
*                                                                               
OPTSCAX  B     OPTX                                                             
         SPACE 2                                                                
OPTRANGE ST    RE,SAVERE           VALIDATE PERIOD RANGE                        
         CLI   FLDH+5,1                                                         
         BNE   OPTVALR                                                          
         MVC   PERIOD,FLD          SET PERIOD OPTION                            
         CLI   FLD,C'M'            TEST FOR MONTHLY PERIODS                     
         BE    OPTX                                                             
         CLI   FLD,C'Q'            TEST FOR QUARTERLY PERIODS                   
         BE    OPTX                                                             
         B     OPTVALR                                                          
         SPACE 2                                                                
OPTDEC   ST    RE,SAVERE           VALIDATE N'DECIMAL PLACES                    
         CLI   FLDH+5,1                                                         
         BH    OPTVALR                                                          
         MVI   ERROR,NOTNUM                                                     
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BZ    OPTVALR                                                          
         MVI   ERROR,INVALID                                                    
         CVB   R0,DUB                                                           
         CH    R0,=Y(MAXDEC)                                                    
         BH    OPTVALR                                                          
         STC   R0,DECIMAL                                                       
         B     OPTX                                                             
         SPACE 2                                                                
OPTACT   ST    RE,SAVERE                                                        
         CLI   FLDH+5,4            TEST IF OPTION G.T. 4 BYTES                  
         BH    OPTVALR                                                          
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         MVI   ACTIVITY,C'D'       SET DATA ACTIVITY                            
         EX    R1,DATACOMP                                                      
         BE    OPTX                                                             
         MVI   ACTIVITY,C'S'                                                    
         EX    R1,SNAPCOMP                                                      
         BE    OPTX                                                             
         B     OPTVALR                                                          
         SPACE 1                                                                
DATACOMP CLC   FLD(0),=C'DATA'                                                  
SNAPCOMP CLC   FLD(0),=C'SNAP'                                                  
         SPACE 2                                                                
OPTYEAR  ST    RE,SAVERE                                                        
         CLI   FLDH+5,2            TEST FOR 1 OR 2 DIGIT NUMBER                 
         BH    OPTVALR                                                          
         MVI   ERROR,NOTNUM                                                     
         TM    FLDH+4,X'08'                                                     
         BZ    OPTVALR                                                          
         CLI   STMON,0             TEST OPEN-ENDED PLAN                         
         BE    OPTYEAR2            YES                                          
         MVI   ERROR,INVALID                                                    
         MVC   XTRA(20),=C'YEAR DEFINED BY PLAN'                                
         B     OPTVALR                                                          
*                                                                               
OPTYEAR2 CVB   R1,DUB                                                           
         CHI   R1,80               TREAT YEARS BEFORE 1980 AS > Y2K             
         BNL   *+8                                                              
         AHI   R1,100                                                           
         STC   R1,YEAR                                                          
         STC   R1,STMON            SET START MONTH'S YEAR                       
         CLC   STMON+1(1),ENDMON+1 TEST ST MON GT END MON                       
         BNH   *+8                                                              
         LA    R1,1(R1)            YES-THEN IT GOES INTO NEXT YEAR              
         STC   R1,ENDMON           SET END YEAR                                 
         B     OPTX                                                             
         SPACE 2                                                                
OPTSNAP  NTR1                      VALIDATE SNAPSHOT DATE                       
         GOTO1 DATVAL,PARAS,FLD,DUB                                             
         MVI   ERROR,INVDATE                                                    
         OC    0(4,R1),0(R1)       TEST FOR ERROR                               
         BZ    OPTVALR                                                          
         CLC   FLDH+5(1),3(R1)     TEST THAT DATE MAKES UP WHOLE FIELD          
         BNE   OPTVALR                                                          
         GOTO1 DATCON,PARAS,DUB,(3,SNAP)                                        
*                                                                               
OPTSNAP2 MVI   ERROR,SNAPERR                                                    
         SR    R0,R0                                                            
         ICM   R0,1,SVNSNAPS       GET N'SNAPSHOTS                              
         BZ    OPTVALR                                                          
         LA    RE,SVSNPLST         RE=A(SNAPSHOT LIST)                          
*                                                                               
OPTSNAP3 CLC   SNAP,0(RE)          TEST FOR MATCH ON SNAPSHOT DATE              
         BE    OPTSNAPX                                                         
         LA    RE,L'BUSNAPS(RE)                                                 
         BCT   R0,OPTSNAP3                                                      
         B     OPTVALR                                                          
*                                                                               
OPTSNAPX MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         SPACE 2                                                                
OPTX     L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO'                                                    
         EJECT                                                                  
* SUB-ROUTINE TO GET VALUES FROM DATA TYPE RECORD                               
*                                                                               
GETDT    NTR1                                                                   
         L     R4,AIO                                                           
         USING BURECD,R4                                                        
         XC    DTVALS(DTVALLN),DTVALS CLEAR DATA TYPE VALUES                    
         MVC   DTCODE,BUDTYP       DATA TYPE CODE                               
         LA    R3,WORK             R3=OUTPUT POINTER                            
         MVC   WORK(40),SPACES                                                  
         MVI   ELCODE,BUDHELQ      FIRST HEADING                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDHD,R6                                                         
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,MOVEHEAD                                                      
         LA    R3,2(R1,R3)         POINT R3 TO POSITION FOR 2ND HEAD            
*                                                                               
         SR    R0,R0                                                            
*                                                                               
GETDT2   IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETDT3              YES                                          
         CLI   0(R6),BUDHELQ       TEST FOR HEADING ELEMENT                     
         BNE   GETDT2                                                           
*                                                                               
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,MOVEHEAD                                                      
*                                                                               
GETDT3   GOTO1 SQUASHER,DMCB,WORK,40                                            
         MVC   DTHEAD,WORK         SET COMPRESSED HEADLINE                      
         B     GETDT4                                                           
*                                                                               
MOVEHEAD MVC   0(0,R3),BUDHEAD                                                  
*                                                                               
GETDT4   MVI   ELCODE,BUDTELQ      GET DATA TYPE ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDTD,R6                                                         
         MVC   DTEX,BUDTEX                                                      
*                                                                               
GETDT5   MVC   DTCOL,BUDTCOL                                                    
         MVC   DTSC,BUDTSC                                                      
         MVC   DUB,SPACES                                                       
         MVI   DUB,LPAREN                                                       
         LA    RE,DUB+1            RE=OUTPUT POINTER                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DTSC           R0=EXPONENT                                  
         BNZ   GETDT6                                                           
         MVI   0(RE),C'1'          SET UNITS AS DEFAULT                         
         LA    RE,1(RE)                                                         
         B     GETDT7                                                           
*                                                                               
GETDT6   MVI   0(RE),C'0'          ONE ZERO FOR EACH PLACE                      
         LA    RE,1(RE)                                                         
         BCT   R0,*-8                                                           
*                                                                               
GETDT7   MVI   0(RE),RPAREN                                                     
         MVC   DTSCEXP,DUB         MOVE TO OUTPUT SCALE AREA                    
*                                                                               
GETDT8   CLI   BUDTDEC,0           TEST FOR ANY DECIMAL PLACES                  
         BE    GETDTX                                                           
         MVC   DTDEC,BUDTDEC                                                    
         ZIC   R0,DTDEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DTDECEXP,DUB+7(1)                                                
*                                                                               
GETDTX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A PLAN MONTH TABLE                                       
*                                                                               
GENMON   ST    RE,SAVERE                                                        
         LA    RE,MONTAB           RE=A(MONTH TABLE ENTRY)                      
         MVC   0(2,RE),STMON       FIRST ENTRY IS SCREEN START MONTH            
         MVC   HALF,STMON          HALF=NEXT MONTH WORK AREA                    
         ZIC   R1,STMON+1         R1=NEXT MONTH NUMBER                          
         MVI   BYTE,12             12 MONTHS FOR ALL FISCAL YEARS               
         CLI   CLTTYPE,10          EXCEPT TYPE 10 (444)                         
         BNE   *+8                                                              
         MVI   BYTE,13                                                          
         LA    RE,2(RE)            POINT RE AT NEXT ENTRY                       
*                                                                               
GENMON2  LA    R1,1(R1)            INCREMENT MONTH                              
         CLM   R1,1,BYTE           TEST IF PAST YEAR END                        
         BNH   GENMON3             NO                                           
         LA    R1,1                RESET MONTH TO ONE                           
         ZIC   RF,HALF                                                          
         LA    RF,1(RF)            INCREMENT YEAR                               
         STC   RF,HALF                                                          
*                                                                               
GENMON3  STC   R1,HALF+1           SET NEW MONTH                                
         CLC   HALF,ENDMON         TEST IF PAST PLAN END                        
         BH    GENMON4             YES-TABLE IS DONE                            
         MVC   0(2,RE),HALF                                                     
         LA    RE,2(RE)                                                         
         B     GENMON2                                                          
*                                                                               
GENMON4  LA    RF,MONTAB                                                        
         SR    RE,RF               RE=L'TABLE                                   
         SRL   RE,1                COMPUTES N'TABLE ENTRIES                     
         STC   RE,NMONTHS                                                       
         SRDL  RE,32                                                            
         LA    RF,2(RF)            FIND N'QUARTERS                              
         D     RE,=F'3'                                                         
         STC   RF,NQUARTRS                                                      
*                                                                               
GENMONX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO OUTPUT PLAN PERIODS TO SCREEN                                  
*                                                                               
PERDIS   NTR1                                                                   
         GOTO1 VCLEARF,PARAS,(1,ENTPER1H),ENTPFH                                
         LA    R2,ENTPER1H                                                      
         CLI   PERIOD,C'Q'         TEST FOR QUARTERLY PERIODS                   
         BE    PERDIS4                                                          
         LA    R3,MONTAB           R3=A(MONTH TABLE ENTRY)                      
         ZIC   R4,NMONTHS                                                       
*                                                                               
PERDIS2  GOTO1 VPEROUT,PARAS,0(R3),WORK                                         
         MVC   8(L'ENTPER1,R2),WORK OUTPUT TO SCREEN                            
         BAS   RE,NEXTFLD          NEXT PERIOD FIELD                            
         LA    R3,L'MONTAB(R3)     NEXT MONTH                                   
         BCT   R4,PERDIS2                                                       
         B     PERDISX                                                          
*                                                                               
PERDIS4  ZIC   R1,NQUARTRS         DISPLAY QUARTERS                             
         MVI   HALF,C'Q'                                                        
         LA    R3,1                R3=QUARTER NUMBER                            
*                                                                               
PERDIS6  CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HALF+1(1),DUB+7(1)                                               
         MVC   8(L'HALF,R2),HALF                                                
         BAS   RE,NEXTFLD                                                       
         LA    R3,1(R3)                                                         
         BCT   R1,PERDIS6                                                       
*                                                                               
PERDISX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO GET THE DATA FOR THE OUTLINE/DATA TYPE                         
*                                                                               
GETDATA  NTR1                                                                   
         XC    SVACTIV,SVACTIV     CLEAR ACTIVITY ELEMENT LIST                  
         ZIC   R5,NMONTHS                                                       
         LA    R3,RECVALS          INITIALIZE RECORD VALUES TO ZERO             
         MVC   0(1,R3),SCRPREC                                                  
         ZAP   1(6,R3),=P'0'                                                    
         LA    R3,L'RECVALS(R3)                                                 
         BCT   R5,*-16                                                          
*                                                                               
GETDATA2 ZIC   R5,NMONTHS          R5=MONTH COUNTER                             
         LA    R2,MONTAB           R2=MONTH TABLE POINTER                       
         LA    R3,RECVALS          R3=RECORD VALUE TABLE POINTER                
         LA    R7,SVACTIV          R7=A(ACTIVITY ELEMENT)                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,OUTKEY        START WITH OUTLINE DIRECTORY KEY             
         MVI   BUVSUB,BUVSUBQ                                                   
         MVC   BUVDTYP,DTCODE      DATA TYPE                                    
         MVC   BUVPER,0(R2)        GET FIRST MONTH                              
         GOTO1 HIGH                                                             
         B     GETDATA4                                                         
*                                                                               
GETDATA3 LA    R4,KEY              MAKE KEY ADDRESSABLE                         
         GOTO1 SEQ                                                              
*                                                                               
GETDATA4 CLC   BUKEY(BUVPER-BUKEY),KEYSAVE TEST SAME OUTLINE/DATATYPE           
         BNE   GETDATAX            NO-ALL DONE                                  
GETDATA5 CLC   BUVPER,0(R2)        FIND SLOT FOR RECORD                         
         BE    GETDATA6                                                         
         LA    R2,L'MONTAB(R2)                                                  
         LA    R3,L'RECVALS(R3)                                                 
         LA    R7,BUACTLNQ(R7)     NEXT ACTIVITY ELEMENT                        
         BCT   R5,GETDATA5                                                      
         B     GETDATAX            OUT OF PLAN RANGE                            
*                                                                               
GETDATA6 GOTO1 GETREC                                                           
         BAS   RE,GETNUM           EXTRACT VALUE FROM RECORD                    
         B     GETDATA3                                                         
*                                                                               
GETDATAX B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO EXTRACT THE VALUE FROM DATA VALUE RECORD                       
* S/R CALLED FROM GETDATA                                                       
*                                                                               
GETNUM   ST    RE,SAVERE                                                        
         L     R4,AIO                                                           
         LA    R6,BUFRSTEL                                                      
         USING BUDAD,R6                                                         
         SR    R1,R1                                                            
*                                                                               
GETNUM1  CLI   0(R6),0             TEST FOR EOR                                 
         BNE   GETNUM2                                                          
*                                                                               
         MVI   0(R3),X'82'         NO CURRENT DATA ELEMENT                      
         ZAP   1(6,R3),=P'0'       RETURN ZERO                                  
         LA    R6,BUFRSTEL                                                      
         B     GETNUM5                                                          
*                                                                               
GETNUM2  CLI   0(R6),BUACTELQ      TEST FOR ACTIVITY ELEMENT                    
         BNE   *+14                                                             
         MVC   0(BUACTLNQ,R7),0(R6) SAVE ACTIVITY ELEMENT                       
         B     GETNUM3                                                          
*                                                                               
         CLI   0(R6),BUDAELQ       TEST FOR DATA ELEMENT                        
         BNE   *+14                                                             
         OC    BUDADATE,BUDADATE   TEST FOR CURRENT DATA ELEMENT                
         BZ    GETNUM4                                                          
*                                                                               
GETNUM3  IC    R1,BUDALEN                                                       
         AR    R6,R1                                                            
         B     GETNUM1                                                          
*                                                                               
GETNUM4  MVC   0(L'RECVALS,R3),BUDAPREC EXTRACT NUMBER                          
*                                                                               
GETNUM5  OC    SNAP,SNAP           TEST FOR SNAPSHOT DATE FILTER                
         BZ    GETNUMX             NO                                           
         MVC   WORK(3),SNAP                                                     
         XC    WORK(3),EFFS        COMPLEMENT SNAPSHOT DATE                     
*                                                                               
GETNUM6  IC    R1,BUDALEN                                                       
         AR    R6,R1                                                            
         CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETNUM8                                                          
         CLI   0(R6),BUDAELQ                                                    
         BNE   GETNUM6                                                          
         CLC   WORK(3),BUDADATE                                                 
         BNE   GETNUM6             TRY NEXT ELEMENT                             
         MVC   0(L'RECVALS,R3),BUDAPREC                                         
         B     GETNUMX                                                          
*                                                                               
GETNUM8  ZAP   1(6,R3),=P'0'                                                    
*                                                                               
GETNUMX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ADJUST RECORD VALUES TO SCREEN PRECISION.  FOR                 
* QUARTERLY DISPLAY, SUM THE MONTHS IN EACH QUARTER.                            
*                                                                               
* ADJUSTMENT SHIFT = SHIFT(RECORD VALUE) - SHIFT(SCREEN PRECISION)              
*                                                                               
ADJDIS   NTR1                                                                   
         ZIC   R0,NMONTHS          R0=PERIOD COUNTER                            
         LA    R2,RECVALS          R2=A(RECORD VALUES OR INPUT)                 
         LA    R3,RECADJ           R3=A(ADJUSTED VALUES OR OUTPUT)              
*                                                                               
ADJDIS2  MVC   0(L'RECADJ,R3),0(R2) RECORD VALUE TO ADJUSTED AREA               
         MVC   0(1,R3),SCRPREC                                                  
*                                                                               
         ZIC   RE,0(R2)            CONVERT RECORD PRECISION TO INTEGER          
         TM    0(R2),X'80'         SHIFT AMOUNT                                 
         BZ    ADJDIS3                                                          
         SLL   RE,24+1                                                          
         SRL   RE,24+1                                                          
         LNR   RE,RE                                                            
*                                                                               
ADJDIS3  LH    RF,SCRSHIFT         RF=SCREEN PRECISION INTEGER SHIFT            
         SR    RE,RF               RE=ADJUSTMENT SHIFT TO SCR PRECISION         
         BZ    ADJDIS4             NONE NEEDED                                  
         SRP   1(6,R3),0(RE),5                                                  
*                                                                               
ADJDIS4  LA    R2,L'RECVALS(R2)    NEXT RECORD VALUE                            
         LA    R3,L'RECADJ(R3)     NEXT ADJUSTED VALUE POSITION                 
         BCT   R0,ADJDIS2                                                       
*                                                                               
ADJDIS6  CLI   PERIOD,C'Q'         TEST FOR QUARTERLY DISPLAY                   
         BNE   ADJDISX             NO                                           
*                                                                               
         ZIC   R0,NQUARTRS         R0=N'QUARTERS IN PLAN                        
         LA    R3,RECADJ           R3=A(QUARTERLY VALUE)                        
         LA    RE,RECADJ           RE=A(MONTHLY VALUE)                          
*                                                                               
ADJDIS7  LA    RF,3                MONTHS IN QUARTER                            
         MVC   SUM(1),SCRPREC      INITIALIZE SUM                               
         ZAP   SUM+1(6),=P'0'                                                   
*                                                                               
         AP    SUM+1(6),1(6,RE)    SUM THE MONTHS IN QUARTER                    
         LA    RE,L'RECADJ(RE)                                                  
         BCT   RF,*-10                                                          
*                                                                               
ADJDIS8  MVC   0(L'RECADJ,R3),SUM                                               
         LA    R3,L'RECADJ(R3)     NEXT QUARTER'S POSITION                      
         BCT   R0,ADJDIS7                                                       
*                                                                               
ADJDISX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY DATA VALUES ON SCREEN                                  
*                                                                               
DISVAL   NTR1                                                                   
         GOTO1 VCLEARF,PARAS,ENTVAL1H,ENTLAST                                   
*                                                                               
         BAS   RE,SETEBLK                                                       
*                                                                               
         LA    R2,ENTVAL1H         R2=SCREEN POINTER                            
         LA    R3,RECADJ           R3=DATA POINTER                              
         ZIC   R4,NMONTHS          R4=N'PERIODS                                 
         CLI   PERIOD,C'M'         TEST FOR MONTHLY PERIODS                     
         BE    DISVAL2                                                          
         ZIC   R4,NQUARTRS                                                      
*                                                                               
DISVAL2  ST    R3,EBAIN            A(INPUT VALUE)                               
         MVC   EBSCIN,0(R3)        INPUT SCALE                                  
         LA    RF,8(R2)            SET OUTPUT ADDRESS                           
         ST    RF,EBAOUT                                                        
         GOTO1 EDITOR,PARAS,EBLOCK                                              
         BAS   RE,NEXTFLD          NEXT VALUE FIELD                             
         LA    R3,L'RECVALS(R3)                                                 
         BCT   R4,DISVAL2                                                       
*                                                                               
DISVALX  B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO COMPUTE AND DISPLAY TOTALS LINE                                
*                                                                               
* AT ENTRY, R1=A(VALUES TO BE TOTALLED)                                         
*                                                                               
TOTAL    NTR1                                                                   
         LR    R3,R1               R3=A(VALUES                                  
         XC    ENTTOT,ENTTOT       CLEAR TOTALS FIELD                           
         MVC   ENTTOT(6),=C'TOTAL='                                             
         SR    R2,R2               R2=PERIOD COUNTER                            
         IC    R2,NMONTHS                                                       
         CLI   PERIOD,C'M'                                                      
         BE    *+8                                                              
         IC    R2,NQUARTRS                                                      
         MVC   SUM(1),SCRPREC      INITIALIZE SUM                               
         ZAP   SUM+1(6),=P'0'                                                   
*                                                                               
TOTAL1   AP    SUM+1(6),1(6,R3)    FIND THE TOTAL                               
         LA    R3,7(R3)                                                         
         BCT   R2,TOTAL1                                                        
*                                                                               
TOTAL2   BAS   RE,SETEBLK                                                       
         LA    RF,SUM                                                           
         ST    RF,EBAIN                                                         
         LA    RF,ENTTOT+6                                                      
         ST    RF,EBAOUT                                                        
         MVC   EBSCIN,SUM                                                       
         GOTO1 EDITOR,PARAS,EBLOCK                                              
         OI    ENTTOTH+6,X'80'                                                  
*                                                                               
TOTALX   B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO INITIALIZE EBLOCK                                              
*                                                                               
SETEBLK  XC    EBLOCK,EBLOCK       PRESET EBLOCK                                
         MVI   EBLIN,7             7-BYTE SCALED NUMBER                         
         MVI   EBTIN,C'S'                                                       
         MVI   EBLOUT,L'ENTVAL1                                                 
         MVI   EBOPT,EMINUS+EZERO  SET MINUS=Y,ZERO=NOBLANK                     
         MVI   EBALIGN,C'L'        LEFT ALIGN OUTPUT                            
         MVC   EBDECS,DECIMAL      OUTPUT DECIMAL PLACES                        
         MVC   EBSCOUT,SCALE       OUTPUT SCALE                                 
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY ACTIVITY DATA ON SCREEN                                
*                                                                               
ACTVDIS  NTR1                                                                   
         CLI   PERIOD,C'M'         TEST FOR MONTHLY PERIODS                     
         BNE   ACTVDISX            NO-SO SKIP DISPLAY                           
         LA    R2,ENTACT1H         R2=A(ACTIVITY FIELD HEADER)                  
         ZIC   R3,NMONTHS          R3=K'MONTHS                                  
         LA    R4,SVACTIV          R4=A(ACTIVITY ELEMENT)                       
         USING BUACTD,R4                                                        
*                                                                               
ACTVDIS2 LA    R6,BUACTLUP         R6=ACTIVITY DATA POINTER                     
         CLI   ACTIVITY,C'D'       TEST IF DATA ACTIVITY WANTED                 
         BE    *+8                 YES                                          
         LA    R6,BUACTLMT         NO-MAINTNENACE ACTIVITY                      
         OC    0(3,R6),0(R6)       TEST IF ANY DATA PRESENT                     
         BZ    ACTVDIS4            NO                                           
*                                                                               
         LA    R0,ACTENTS          R0=TABLE ENTRY COUNTER                       
         LA    RE,ACTTAB           RE=A(TABLE)                                  
         CLC   3(1,R6),0(RE)       MATCH ON ACTIVITY CODE                       
         BE    ACTVDIS3                                                         
         LA    RE,L'ACTTAB(RE)                                                  
         BCT   R0,*-14                                                          
         B     ACTVDIS4                                                         
*                                                                               
ACTVDIS3 MVC   8(8,R2),1(RE)       EXTRACT EXPANDED NAME                        
         GOTO1 DATCON,DMCB,(3,(R6)),(8,17(R2))                                  
*                                                                               
ACTVDIS4 LA    R4,BUACTLNQ(R4)     NEXT ACTIVITY ELEMENT                        
         BAS   RE,NEXTFLD                                                       
         BCT   R3,ACTVDIS2                                                      
*                                                                               
ACTVDISX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT VALUE FIELDS                                              
*                                                                               
EDVAL    NTR1                                                                   
         LA    R2,ENTVAL1H         R2=A(FIELD HEADER)                           
         LA    R3,INPVALS          R3=A(INPUT VALUE)                            
         SR    R4,R4               R4=N'FIELDS                                  
         IC    R4,NMONTHS                                                       
         CLI   PERIOD,C'M'         TEST FOR MONTHLY PERIOD                      
         BE    *+8                 YES                                          
         IC    R4,NQUARTRS                                                      
*                                                                               
EDVAL2   GOTO1 VGETFLD,PARAS,(R2)                                               
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    SPERR                                                            
         ZIC   R0,FLDH+5                                                        
         GOTO1 VBURNEM,DMCB,((R0),FLD),DUB                                      
         CLI   0(R1),0             TEST FOR ERROR                               
         BNE   EDVALR              YES                                          
*                                                                               
         MVC   0(1,R3),DUB         EXTRACT SCALE                                
         ZAP   1(6,R3),DUB+1(7)    EXTRACT INTEGER                              
         TM    DUB,X'80'           TEST IF DECIMAL PLACES INPUT                 
         BZ    EDVAL4                                                           
         MVI   ERROR,DECERR                                                     
         NI    DUB,X'FF'-X'80'     TEST IF MORE DECIMAL PLACES                  
         CLC   DUB(1),DECIMAL      INPUT THAN DISPLAYED ON SCREEN               
         BH    SPERR                                                            
*                                                                               
EDVAL4   BAS   RE,ADJINP           ADJUST INPUT NUMBER TO SCREEN PREC           
         LA    R3,L'INPVALS(R3)                                                 
         BAS   RE,NEXTFLD          NEXT VALUE FIELD                             
         BCT   R4,EDVAL2                                                        
*                                                                               
EDVALX   B     XIT                                                              
*                                                                               
EDVALR   MVI   ERROR,INVALID                                                    
         L     R2,FADDR                                                         
         LA    R2,8(R2)            POINT TO DATA                                
         ZIC   RE,4(R1)            GET INDEX TO ERROR BYTE                      
         LA    R2,0(RE,R2)                                                      
         STCM  R2,7,FLAST          SET FLAST AS POINTER TO IT                   
         B     SPERR                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO ADJUST INPUT NUMBER TO SCREEN PRECISION (CALLED                
* BY EDVAL). AT ENTRY, R3 POINTS TO INPUT NUMBER                                
*                                                                               
* ADJUSTMENT SHIFT = SHIFT(INPUT NUMBER) - SHIFT(DECIMAL)                       
*                                                                               
ADJINP   STM   RE,RF,WORK                                                       
         ZIC   RE,0(R3)            GET INPUT NUMBER'S SCALE                     
         TM    0(R3),X'80'         CONVERT TO INTEGER SHIFT                     
         BZ    ADJINP2                                                          
         SLL   RE,24+1                                                          
         SRL   RE,24+1                                                          
         LNR   RE,RE                                                            
*                                                                               
ADJINP2  ZIC   RF,DECIMAL          RF=SHIFT FOR SCR DECIMAL PLACES              
         LNR   RF,RF                                                            
         SR    RE,RF               RE=ADJUSTMENT SHIFT                          
         BZ    *+10                                                             
         SRP   1(6,R3),0(RE),5                                                  
         MVC   0(1,R3),SCRPREC     SET SCREEN PRECISION                         
*                                                                               
ADJINPX  LM    RE,RF,WORK                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO LOOK FOR OVERRIDES AND UPDATE DATA VALUES RECORDS              
* AT ENTRY, INPVALS AND RECADJ HAVE SAME SCALE                                  
*                                                                               
OVER     NTR1                                                                   
         ZIC   R0,NMONTHS          R0=PERIOD COUNTER                            
         CLI   PERIOD,C'M'                                                      
         BE    *+10                                                             
         ZIC   R0,NQUARTRS                                                      
         LA    R2,MONTAB           R2=A(MONTH TABLE ENTRY)                      
         LA    R3,INPVALS          R3=A(INPUT VALUE)                            
         LA    R5,RECADJ           R5=A(ADJUSTED RECORD VALUE)                  
         MVI   OVERSW,NO           INITIALIZE OVERRIDE SWITCH                   
*                                                                               
OVER2    CP    1(6,R3),1(6,R5)     TEST INPUT VALUE=RECORD VALUE                
         BE    OVER6                                                            
         MVI   OVERSW,YES          SET OVERRIDES INPUT                          
         CLI   PERIOD,C'Q'         TEST FOR QUARTERLY PERIOD                    
         BNE   *+8                 NO                                           
         BAS   RE,DIVIDE           YES-SO DIVIDE INPUT NUMBER BY 3              
*                                                                               
OVER3    GOTO1 BUILD,PARAS,0                                                    
         LA    R4,1                R4=PERIOD COUNTER                            
         CLI   PERIOD,C'M'                                                      
         BE    *+8                                                              
         LA    R4,3                SET FOR A QUARTER                            
*                                                                               
OVER4    XC    BUPBLOCK,BUPBLOCK   CALL BUPPER TO DO UPDATE                     
         LA    RE,BUPBLOCK                                                      
         USING BUPBLKD,RE                                                       
         MVC   BUPADMGR,DATAMGR                                                 
         MVC   BUPAHELO,HELLO                                                   
         MVC   BUPAREC,AIO                                                      
         MVI   BUPORIG,BUACTINP                                                 
         OC    SNAP,SNAP           TEST FOR OVERRIDING SNAPSHOT                 
         BZ    *+8                                                              
         MVI   BUPORIG,BUACTSNO                                                 
         MVC   BUPBDATE,BTODAY                                                  
*                                                                               
OVER5    MVI   BYTE,BUPPUT         SET BUPPER ACTION                            
         GOTO1 VBUPPER,DMCB,(BYTE,BUPBLOCK)                                     
*                                                                               
         LA    R2,L'MONTAB(R2)     NEXT MONTH IN PERIOD                         
         BCT   R4,*+8                                                           
         B     OVER8               ALL DONE FOR QUARTER                         
*                                                                               
         SR    R6,R6               NO REMAINDER TO BE APPLIED                   
         CH    R4,=H'1'            TEST FOR LAST PERIOD IN QUARTER              
         BNE   *+8                                                              
         LA    R6,REMAIN           YES-POINT TO REMAINDER                       
         GOTO1 BUILD,PARAS,(R6)                                                 
         B     OVER4                                                            
*                                                                               
* INPUT AND RECORD VALUES EQUAL-POINT TO NEXT MONTH TABLE ENTRY                 
*                                                                               
OVER6    LA    R2,L'MONTAB(R2)     NEXT MONTH                                   
         CLI   PERIOD,C'M'         TEST FOR MONTHLY PERIODS                     
         BE    *+8                                                              
         LA    R2,L'MONTAB*2(R2)   BUMP 2 MORE MONTHS FOR QUARTER               
*                                                                               
OVER8    LA    R3,L'INPVALS(R3)                                                 
         LA    R5,L'RECADJ(R5)                                                  
         BCT   R0,OVER2                                                         
*                                                                               
OVERX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A RECORD FOR BUPPER(CALLED BY OVER)                      
* AT ENTRY, R2 = A(YEAR/MONTH FOR RECORD KEY)                                   
*           R3 = A(DATA VALUE)                                                  
*           P1 = A(REMAINDER TO BE ADDED TO VALUE) OR A(0)                      
*                                                                               
BUILD    NTR1                                                                   
         L     R5,0(R1)            R5=A(REMAINDER)                              
         L     R4,AIO1             CONSTRUCT RECORD AT AIO1                     
         LR    RE,R4                                                            
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         LA    R4,4(R4)            AT AIO+4                                     
         USING BURECD,R4                                                        
         MVC   BUKEY,OUTKEY        START WITH OUTLINE'S KEY                     
         MVI   BUVSUB,BUVSUBQ                                                   
         MVC   BUVDTYP,DTCODE      DATA TYPE                                    
         MVC   BUVPER,0(R2)        YEAR/MONTH                                   
         MVC   BURLEN,DATADISP                                                  
*                                                                               
         XC    ELEM,ELEM           BUILD A DATA ELEMENT                         
         LA    R6,ELEM             CONTAINING OVERRIDE VALUE                    
         USING BUDAD,R6                                                         
         MVI   BUDAEL,BUDAELQ                                                   
         MVI   BUDALEN,BUDALNQ                                                  
         OC    SNAP,SNAP           TEST FOR SNAPSHOT DATE                       
         BZ    BUILD2              NO-OVERRIDING CURRENT DATA                   
         MVC   BUDADATE,SNAP       SET SNAPSHOT DATE                            
         XC    BUDADATE,EFFS       AND COMPLEMENT IT                            
*                                                                               
BUILD2   MVC   BUDAPREC(7),0(R3)   OVERRIDE VALUE                               
         LTR   R5,R5               TEST FOR ANY REMAINDER                       
         BZ    *+10                                                             
         AP    BUDATA,1(6,R5)      YES-ADD REMAINDER TO VALUE                   
         ST    R4,AIO                                                           
         GOTO1 ADDELEM                                                          
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         SR    R1,R1               BUILD A DUMMY WORKER HEADER                  
         ICM   R1,3,BURLEN                                                      
         LA    R1,4(R1)                                                         
         L     RF,AIO                                                           
         STH   R1,0(RF)                                                         
*                                                                               
BUILDX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DIVIDE INPUT VALUE BY 3 (CALLED BY OVER FOR                    
* QUARTERLY PERIODS).  AT ENTRY, R3 POINTS TO DIVIDEND.  ON EXIT                
* R3 POINTS TO QUOTIENT ADJUSTED TO SCREEN PRECISION AND REMAIN                 
* CONTAINS REMAINDER                                                            
*                                                                               
DIVIDE   ST    RE,SAVERE                                                        
         MVI   DIVISOR,X'00'       INITIALIZE DIVISOR OF 3                      
         ZAP   DIVISOR+1(6),=P'3'                                               
         MVC   REMAIN,0(R3)        INITIALIZE REMAINDER W DIVIDEND              
         CLI   0(R3),X'00'         TEST IF DIVIDEND IS INTEGER                  
         BE    DIVIDE4             YES                                          
*                                                                               
         ZIC   RE,0(R3)                                                         
         TM    0(R3),X'80'         TEST IF DIVIDEND HAS DEC. PLACES             
         BO    DIVIDE2             YES                                          
         SRP   1(6,R3),0(RE),0     CONVERT DIVIDEND TO INTEGER                  
         MVI   0(R3),X'00'         SET SCALE TO INTEGER                         
         B     DIVIDE4                                                          
*                                                                               
DIVIDE2  SLL   RE,24+1             REMOVE NEGATIVE EXPONENT BIT                 
         SRL   RE,24+1                                                          
         SRP   DIVISOR+1(6),0(RE),0 ADJUST DIVISOR TO DIVIDEND'S SCALE          
         MVC   DIVISOR(1),0(R3)                                                 
*                                                                               
DIVIDE4  CP    1(6,R3),=P'0'       TEST FOR ZERO DIVIDEND                       
         BE    DIVIDE6             YES                                          
         ZAP   WORK(16),1(6,R3)    PUT DIVIDEND IN WORK AREA                    
         SR    RE,RE                                                            
         TM    0(R3),X'80'                                                      
         BZ    DIVIDE5                                                          
         ZIC   RE,0(R3)            DIVIDEND*10 TO N'DECIMAL PLCS                
         SLL   RE,24+1                                                          
         SRL   RE,24+1                                                          
*                                                                               
DIVIDE5  LA    RE,1(RE)            DIVIDEND X 10 FOR ROUNDING                   
         SRP   WORK(16),0(RE),0                                                 
         DP    WORK(16),DIVISOR+1(6)                                            
         SRP   WORK(10),64-1,5     ROUNDED DIVIDE                               
         ZAP   1(6,R3),WORK(10)                                                 
*                                  ADJUST QUOTIENT TO SCREEN PRECISION          
DIVIDE6  ZIC   RE,0(R3)            GET VALUE'S INTEGER SHIFT                    
         TM    0(R3),X'80'                                                      
         BZ    DIVIDE7                                                          
         SLL   RE,24+1                                                          
         SRL   RE,24+1                                                          
         LNR   RE,RE                                                            
*                                                                               
DIVIDE7  LH    RF,SCRSHIFT         RF=SCREEN PRECISION INTEGER SHIFT            
         SR    RE,RF               RE=ADJUSTMENT SHIFT                          
         BZ    DIVIDE8                                                          
         SRP   1(6,R3),0(RE),5     R3=A(ADJUSTED QUOTIENT)                      
         MVC   0(1,R3),SCRPREC     SET SCREEN PRECISION                         
*                                                                               
DIVIDE8  MVC   WORK(7),0(R3)       MOVE ADJUSTED QUOTIENT TO WORK               
         MP    WORK+1(6),=P'3'     QUOTIENT*3                                   
         SP    REMAIN+1(6),WORK+1(6) COMPUTE REMAINDER                          
*                                                                               
DIVIDEX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE PLAN RECORD                                             
*                                                                               
UPPLAN   NTR1                                                                   
         GOTO1 VSETKEY             BUILD PLAN KEY                               
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,NDIOA                                                         
         MVI   ELCODE,BUPLNELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPLND,R6                                                        
         OI    BUPLNIND,BUPLNDAT   SET PLAN HAS DATA RECORDS                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         OI    6(R2),X'40'         SLAVE MODE ERROR EXIT                        
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 SAVEUWKA                                                         
         L     RD,AWORK            UNWIND OUT TO USER                           
         B     ENTRYX                                                           
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
* BUMP TO NEXT FIELD ON LOWER SCREEN                                            
*                                                                               
NEXTFLD  ST    RE,FULL                                                          
         LA    RE,3                RE=COUNTER                                   
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   RE,*-6                                                           
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
EFFS     DC    8X'FF'                                                           
DISMSG   DC    C'DATA DISPLAYED'                                                
EXTRAMSG DC    C'- NOW ENTER OVERRIDES'                                         
OVERMSG  DC    C'OVERRIDES COMPLETED'                                           
NOOVERS  DC    C'NO OVERRIDES HAVE BEEN INPUT'                                  
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* TABLE OF FORMAT OPTIONS (COVERED BY OPTTABD)                                  
*                                                                               
OPTTAB   DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'RANGE',AL1(1),AL2(OPTRANGE-T50207)                           
         DC    AL1(L'PERIOD),AL2(PERIOD-SYSD)                                   
*                                                                               
         DC    CL8'SCALE',AL1(2),AL2(OPTSCA-T50207)                             
         DC    AL1(L'SCALE),AL2(SCALE-SYSD)                                     
*                                                                               
         DC    CL8'DECIMAL',AL1(1),AL2(OPTDEC-T50207)                           
         DC    AL1(L'DECIMAL),AL2(DECIMAL-SYSD)                                 
*                                                                               
         DC    CL8'SNAP',AL1(2),AL2(OPTSNAP-T50207)                             
         DC    AL1(L'SNAP),AL2(SNAP-SYSD)                                       
*                                                                               
         DC    CL8'ACTIVITY',AL1(1),AL2(OPTACT-T50207)                          
         DC    AL1(L'ACTIVITY),AL2(ACTIVITY-SYSD)                               
*                                                                               
         DC    CL8'YEAR',AL1(1),AL2(OPTYEAR-T50207)                             
         DC    AL1(L'YEAR),AL2(YEAR-SYSD)                                       
*                                                                               
OPTIONS  EQU   (*-OPTTAB)/L'OPTTAB                                              
         SPACE 2                                                                
* TABLE OF ACTIVITY CODES AND EXPANDED NAMES                                    
*                                                                               
ACTTAB   DS    0CL9                                                             
         DC    C'C',CL8'COPY'                                                   
         DC    C'E',CL8'EXTRACT'                                                
         DC    C'I',CL8'INPUT'                                                  
         DC    C'T',CL8'TRANSFER'                                               
         DC    C'S',CL8'SNAPSHOT'                                               
         DC    C'D',CL8'SNP DEL'                                                
         DC    C'O',CL8'SNP OVER'                                               
ACTENTS  EQU   (*-ACTTAB)/L'ACTTAB                                              
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF7D                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
*                                                                               
MYACT    DS    X                   INTERNAL ACTION                              
OVERSW   DS    C                   OVERRIDES HAVE BEEN INPUT (Y/N)              
NEWPLAN  DS    C                   Y=PLAN HAS CHANGED                           
RECFIND  DS    C                   Y=FOUND RECORD TO SCROLL TO                  
NODCOMM  DS    CL8                                                              
*                                                                               
ACTIVITY DS    C                   ACTIVITY OPTION D=DATA,S=SNAPSHOT            
YEAR     DS    X                   YEAR FOR OPEN-ENDED PLANS                    
*                                                                               
SCRPREC  DS    X                   SCREEN DISPLAY PRECISION                     
SCRSHIFT DS    H                   SCREEN PRECISION INTEGER SHIFT AMT           
SUM      DS    CL7                                                              
DIVISOR  DS    CL7                                                              
REMAIN   DS    CL7                 REMAINDER                                    
*                                                                               
NMONTHS  DS    X                   N'MONTHS IN PLAN                             
NQUARTRS DS    X                   N'QUARTERS                                   
MONTAB   DS    13XL2               PLAN MONTH TABLE                             
*                                                                               
RECVALS  DS    13CL7               RECORD VALUES                                
RECADJ   DS    13CL7               ADJUSTED RECORD VALUES                       
*                                                                               
INPVALS  DS    13CL7               INPUT VALUES                                 
*                                                                               
DTVALS   DS    0C                  DATA TYPE RECORD VALUES                      
DTCODE   DS    CL(L'BUDTYP)                                                     
DTHEAD   DS    CL28                HEADING 1+2 (SPACE PADDED)                   
DTEX     DS    X                   EXTRACT TYPE                                 
DTCOL    DS    X                   COLUMN WIDTH                                 
DTSC     DS    X                   SCALE                                        
DTSCEXP  DS    CL(MAXSCALE+2)      OUTPUT SCALE EXPRESSION                      
DTDEC    DS    X                   N'DECIMAL PLACES                             
DTDECEXP DS    C                   OUTPUT DECIMAL PLACES                        
DTVALLN  EQU   *-DTVALS            L'DATA TYPE RECORD VALUES                    
*                                                                               
CTLVALS  DS    0C                  THIS TIME CONTROL VALUES                     
OUTKEY   DS    CL(L'BUKEY)         OUTLINE KEY                                  
DTYP     DS    CL(L'BUDTYP)        DATA TYPE                                    
SCALE    DS    X                   SCALE                                        
DECIMAL  DS    X                   DECIMAL PLACES                               
PERIOD   DS    C                   PERIOD OPTION (M=MONTH,Q=QUARTER)            
STMON    DS    XL2                 START YEAR/MONTH FOR SCREEN                  
ENDMON   DS    XL2                 END YEAR/MONTH FOR SCREEN                    
SNAP     DS    XL3                 SNAPSHOT DATE (YMD)                          
CTLVALLN EQU   *-CTLVALS           CONTROL VALUES LENGTH                        
*                                                                               
       ++INCLUDE DDEBLOCK                                                       
*                                                                               
         DS    0D                                                               
BUPBLOCK DS    CL(BUPBLKL)         BUPPER BLOCK AREA                            
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         ORG   TWA1USER            SAVE AREA                                    
SVCLTVAL DS    CL(L'CLTVALS)       CLIENT VALUES SAVE AREA                      
SVPRDVAL DS    CL(L'PRDVALS)       PRODUCT VALUES SAVE AREA                     
SVPLNVAL DS    CL(L'PLANVALS)      PLAN VALUES SAVE AREA                        
SVPLNKEY DS    CL(L'BUKEY)         PLAN KEY                                     
SVOUTVAL DS    CL(L'OUTVALS)       OUTLINE VALUES SAVE AREA                     
*                                                                               
SVCTLVAL DS    0CL(CTLVALLN)       SAVED CONTROL VALUES                         
SVOUTKEY DS    CL(L'BUKEY)         OUTLINE KEY                                  
SVDTYP   DS    CL(L'BUDTYP)        DATA TYPE                                    
SVSC     DS    X                   SCALE                                        
SVDEC    DS    X                   N'DECIMAL PLACES                             
SVPERIOD DS    C                   PERIOD OPTION (M=MONTH,Q=QUARTER)            
SVSTMON  DS    XL2                 START YEAR/MONTH FOR SCREEN                  
SVENDMON DS    XL2                 END YEAR/MONTH FOR SCREEN                    
SVSNAP   DS    XL3                 SNAPSHOT DATE (YMD)                          
*                                                                               
SVNSNAPS DS    X                   N'PLAN SNAPSHOTS                             
SVSNPLST DS    XL255               SNAPSHOT LIST                                
SVACTIV  DS    CL(13*BUACTLNQ)     ACTIVITY ELEMENT LIST                        
         SPACE 2                                                                
* DSECT TO COVER FORMAT OPTIONS TABLE                                           
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                                                              
OPTMINL  DS    X                                                                
OPTROUT  DS    AL2                 DISPLACEMENT TO PARAMETER ROUTINE            
OPTPLEN  DS    X                   L'PARAMETER DATA FIELD                       
OPTDATA  DS    AL2                 DISPLACEMENT TO DATA FIELD                   
OPTTABL  EQU   *-OPTTABD                                                        
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
LPAREN   EQU   C'('                                                             
RPAREN   EQU   C')'                                                             
COMMA    EQU   C','                                                             
EMINUS   EQU   X'40'               MINUS=YES                                    
EZERO    EQU   X'20'               ZERO=NOBLANK                                 
MAXSCALE EQU   6                                                                
MAXDEC   EQU   5                   MAXIMUM DECIMAL PLACES                       
PF1      EQU   1                                                                
PF7      EQU   7                                                                
PF8      EQU   8                                                                
         SPACE 2                                                                
* BUPPERD                                                                       
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020BUFIL07   05/01/02'                                      
         END                                                                    
