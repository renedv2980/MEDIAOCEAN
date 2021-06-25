*          DATA SET BUFIL08    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T50208A                                                                  
         TITLE 'T50208 - BUDGET CONTROL LFM - TEXT RECORD'                      
T50208   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI08**,RA,RR=R2                                              
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
         GOTO1 VSETADD                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DREC                                                             
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    DREC                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     LA    RE,SVVALS           CLEAR SAVE AREA VALUES                       
         LA    RF,SVVALN                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         GOTO1 VUPKEY                                                           
*                                                                               
         LA    R2,TXLCLTH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BE    *+12                                                             
         BAS   RE,CLRNAME          CLEAR PROTECTED NAME FIELDS                  
         LA    R2,TEXCLTH          VALIDATE CLIENT                              
         GOTO1 VVALCLT,PARAS,(R2),0                                             
*                                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
*                                                                               
         MVC   SVCLTVAL,CLTVALS    SAVE CLIENT RECORD VALUES                    
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   SVNKEY,NODKEY       SET NODAL KEY                                
         MVC   SVDIRKEY,NDLVKEY    AND ITS DIRECTORY                            
         MVC   SVORGLEV,NDLEV      SAVE POTENTIAL LIST LEVEL                    
         CLI   ACTNUM,ACTLIST      TEST FOR ACTION LIST                         
         BE    VKEY2               YES-SKIP DISPLAY OF CLIENT NAME              
         MVC   TEXCLN(L'CLTNAM),CLTNAM                                          
*                                                                               
* EDIT AND VALIDATE PRODUCT                                                     
*                                                                               
VKEY2    LA    R2,TEXPRDH          VALIDATE PRODUCT                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,TXLPRDH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   VKEY3                                                            
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   ACTNUM,ACTREP       PRODUCT IS REQUIRED FOR REPORT               
         BE    SPERR                                                            
*                                                                               
         LA    R6,TEXPERH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R6,TXLOUTH                                                       
         GOTO1 CHKEM,DMCB,(R2),(R6)                                             
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX               YES-ALL DONE                                 
         B     VKEY12              SKIP TO CODE EDIT                            
*                                                                               
VKEY3    GOTO1 VVALPRD,PARAS,(R2),0                                             
*                                                                               
         L     R4,NDIOA                                                         
         MVC   SVPRDVAL,PRDVALS    SAVE AWAY PRODUCT VALUES                     
         MVC   SVNKEY,NODKEY                                                    
         L     R3,NDLEVPTR                                                      
         MVC   SVDIRKEY,NDLVKEY                                                 
         MVC   SVORGLEV,NDLEV                                                   
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEY4                                                            
         MVC   TEXPRN(L'PRDNAM),PRDNAM                                          
*                                                                               
* EDIT AND VALIDATE PLAN CODE                                                   
*                                                                               
VKEY4    LA    R2,TEXPLAH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,TXLPLAH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BNE   VKEY5                                                            
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   ACTNUM,ACTREP                                                    
         BE    SPERR                                                            
*                                                                               
         LA    R6,TEXPERH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R6,TXLOUTH                                                       
         GOTO1 CHKEM,DMCB,(R2),(R6)                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEYX                                                            
         B     VKEY12              SKIP TO CODE EDIT                            
*                                                                               
VKEY5    GOTO1 VVALPLAN,PARAS,(R2),0                                            
         L     R4,NDIOA                                                         
         MVC   SVNKEY,NODKEY       SAVE NODAL KEY                               
         MVC   SVPLNVAL,PLANVALS   SAVE PLAN VALUES                             
         L     R3,NDLEVPTR                                                      
         MVC   SVDIRKEY,NDLVKEY    EXTRACT PLAN'S REGULAR KEY                   
         MVC   SVPLNKEY,NDLVKEY                                                 
         MVC   SVORGLEV,NDLEV                                                   
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEY6                                                            
         MVC   TEXPLN(L'PLANNAM),PLANNAM                                        
         CLI   ACTNUM,ACTREP       TEST FOR REPORT                              
         BNE   VKEY6                                                            
         GOTO1 FORCEM,PARAS,TEXOUTH,TEXPERH                                     
         B     VKEY12              SKIP OUTLINE-PERIOD                          
*                                                                               
* EDIT OUTLINE                                                                  
*                                                                               
VKEY6    LA    R2,TEXOUTH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,TXLOUTH                                                       
         GOTO1 VGETFLD,PARAS,(X'FF',(R2))                                       
         CLI   FLDH+5,0                                                         
         BNE   VKEY7                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEYX                                                            
         GOTO1 CHKEM,DMCB,(R2),TEXPERH                                          
         B     VKEY12              SKIP TO CODE EDIT                            
*                                                                               
VKEY7    MVC   OUTCODE,FLD                                                      
         GOTO1 VFINDOUT,PARAS,OUTCODE,NDIOA                                     
         BNE   SPERR                                                            
         GOTO1 VTRACE                                                           
         GOTO1 VGETVAL                                                          
         MVC   SVOUTVAL,OUTVALS                                                 
         MVC   SVNKEY,NODKEY                                                    
         L     R3,NDLEVPTR                                                      
         MVC   SVDIRKEY,NDLVKEY                                                 
         MVC   SVORGLEV,NDLEV                                                   
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEYX                                                            
         MVC   TEXOUTN,OUTNAME                                                  
*                                                                               
* EDIT AND VALIDATE DATA TYPE                                                   
*                                                                               
VKEY8    LA    R2,TEXDTH                                                        
         GOTO1 VGETFLD,DMCB,(R2)                                                
         CLI   FLDH+5,0                                                         
         BE    VKEY10                                                           
         MVC   DATATYPE,FLD                                                     
*                                                                               
VKEY9    XC    KEY,KEY             VALIDATE DATA TYPE                           
         LA    R4,KEY              MAKE KEY ADDRESSABLE                         
         MVC   BUKEY,SVPLNKEY      START WITH PLAN KEY                          
         MVI   BUDSUB,BUDSUBQ                                                   
         MVC   BUDTYP,DATATYPE                                                  
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(L'BUKEY),KEYSAVE                                             
         BNE   SPERR                                                            
         GOTO1 GETREC                                                           
         BAS   RE,DTOUT            DISPLAY DATA TYPE NAME                       
*                                                                               
* EDIT PERIOD                                                                   
*                                                                               
VKEY10   LA    R2,TEXPERH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    VKEY12                                                           
         XC    DMCB+8(4),DMCB+8                                                 
         MVC   DMCB+8(1),PLANST+1  FISCAL YEAR START MONTH                      
         MVC   DMCB+9(1),CLTTYPE   FISCAL MONTH TYPE                            
         GOTO1 VMONVAL,DMCB,FLD,PLANST                                          
         MVI   ERROR,INVDATE                                                    
         OC    4(4,R1),4(R1)       TEST FOR MONVAL ERROR                        
         BZ    SPERR               YES                                          
         MVC   PERIOD,4(R1)        EXTRACT PERIOD RETURNED BY MONVAL            
         OC    DATATYPE,DATATYPE   TEST IF DATA TYPE INPUT                      
         BNZ   VKEY12                                                           
         MVI   ERROR,MISSING                                                    
         LA    R2,TEXDTH           SET UP ERROR AT DATA TYPE                    
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         B     SPERR                                                            
*                                                                               
* EDIT CODE FIELD                                                               
*                                                                               
VKEY12   LA    R2,TEXCODEH                                                      
         GOTO1 VGETFLD,PARAS,(R2)                                               
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BNE   *+16                                                             
         CLI   ACTNUM,ACTREP       CODE IS OPTIONAL FOR REPORT                  
         BE    VKEYX                                                            
         B     SPERR                                                            
*                                                                               
         MVI   ERROR,NOTALPHA                                                   
         TM    FLDH+4,X'04'        TEST ALPHA INPUT                             
         BZ    SPERR                                                            
         MVC   CODE,FLD                                                         
*                                                                               
* CONSTRUCT KEY                                                                 
*                                                                               
VKEY15   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BUKEY,SVDIRKEY      SET LEVEL'S DIRECTORY KEY                    
         MVI   BUTSUB,BUTSUBQ      TEXT SUB-RECORD                              
         MVC   BUTDTYP,DATATYPE                                                 
         MVC   BUTPER,PERIOD                                                    
         MVC   BUTCODE,CODE                                                     
*                                                                               
VKEYX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*        DISPLAY KEY                                                            
*                                                                               
DKEY     LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   CLTVALS,SVCLTVAL    RESTORE CLIENT VALUES                        
         MVC   PRDVALS,SVPRDVAL    RESTORE PRODUCT VALUES                       
         MVC   PLANVALS,SVPLNVAL   RESTORE PLAN VALUES                          
         MVC   OUTVALS,SVOUTVAL    RESTORE OUTLINE VALUES                       
*                                                                               
         ZIC   R1,SELLISTN         INDEX INTO LIST DIRECTORY                    
         MH    R1,=H'6'            LENGTH OF LIST DIRECTORY ENTRY               
         LA    R1,LISTDIR(R1)                                                   
         CLI   0(R1),C'R'          TEST FOR SELECT FOR RESTORE                  
         BNE   DKEY1                                                            
         MVI   ERROR,INVACT        PREVENT THEM                                 
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
DKEY1    XC    DUB,DUB                                                          
         MVC   DUB(L'CLTCODE),CLTCODE                                           
         OC    DUB,SPACES                                                       
         MVC   TEXCLT,DUB                                                       
         OI    TEXCLTH+6,X'80'                                                  
*                                                                               
         MVC   TEXCLN,CLTNAM                                                    
         OI    TEXCLNH+6,X'80'                                                  
*                                                                               
DKEY2    OC    PRDCODE,PRDCODE     TEST FOR PRODUCT                             
         BZ    DKEY4                                                            
         XC    DUB,DUB                                                          
         MVC   DUB(L'PRDCODE),PRDCODE                                           
         OC    DUB,SPACES                                                       
         MVC   TEXPRD,DUB                                                       
         OI    TEXPRDH+6,X'80'                                                  
*                                                                               
         MVC   TEXPRN,PRDNAM                                                    
         OI    TEXPRNH+6,X'80'                                                  
*                                                                               
DKEY4    OC    PLANCODE,PLANCODE   TEST FOR PLAN CODE                           
         BZ    DKEY6                                                            
         MVC   DUB,PLANCODE        DISPLAY PLAN CODE                            
         OC    DUB,SPACES                                                       
         MVC   TEXPLA,DUB                                                       
         OI    TEXPLAH+6,X'80'                                                  
         MVC   TEXPLN,PLANNAM      AND NAME                                     
         OI    TEXPLNH+6,X'80'                                                  
*                                                                               
DKEY6    OC    OUTCODE,OUTCODE     TEST FOR OUTLINE CODE                        
         BZ    DKEY8                                                            
         MVC   DUB,OUTCODE                                                      
         OC    DUB,SPACES                                                       
         MVC   TEXOUT,DUB                                                       
         OI    TEXOUTH+6,X'80'                                                  
         MVC   TEXOUTN,OUTNAME                                                  
         OI    TEXOUTNH+6,X'80'                                                 
*                                                                               
DKEY8    OC    BUTDTYP,BUTDTYP     TEST FOR DATA TYPE                           
         BZ    DKEY10                                                           
         MVC   TEXDT,BUTDTYP                                                    
         OI    TEXDTH+6,X'80'                                                   
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   SAVEAIO,AIO                                                      
         MVC   BUKEY,SVPLNKEY                                                   
         MVI   BUDSUB,BUDSUBQ                                                   
         MVC   BUDTYP,TEXDT                                                     
         GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE                                                    
         BNE   DKEY9                                                            
         MVC   AIO,AIO2            READ DATA TYPE RECORD INTO IO2               
         GOTO1 GETREC                                                           
         BAS   RE,DTOUT                                                         
         OI    TEXDTNH+6,X'80'                                                  
*                                                                               
DKEY9    MVC   KEY,SAVEKEY                                                      
         MVC   AIO,SAVEAIO                                                      
*                                                                               
DKEY10   OC    BUTPER,BUTPER       TEST FOR PERIOD                              
         BZ    DKEY12              NO                                           
         BAS   RE,PEROUT                                                        
         MVC   TEXPER,WORK                                                      
         OI    TEXPERH+6,X'80'     XMIT PERIOD FIELD                            
*                                                                               
DKEY12   MVC   TEXCODE,BUTCODE                                                  
         OI    TEXCODEH+6,X'80'                                                 
         GOTO1 VUPKEY                                                           
*                                                                               
DKEYX    B     XIT                                                              
         EJECT                                                                  
* VALIDATE TEXT RECORD                                                          
*                                                                               
VREC     L     R4,AIO                                                           
         USING BURECD,R4                                                        
         MVC   SVGENKEY,KEY        SAVE ORIGINAL KEY AND DMWORK TO              
         MVC   SVDMWORK,DMWORK     PROTECT MYSELF FROM GENCON                   
         MVC   CLTVALS,SVCLTVAL    RESTORE CLIENT VALUES                        
         MVC   PRDVALS,SVPRDVAL    RESTORE PRODUCT VALUES                       
         MVC   PLANVALS,SVPLNVAL   RESTORE PLAN VALUES                          
         MVC   OUTVALS,SVOUTVAL    RESTORE OUTLINE VALUES                       
*                                                                               
         CLI   ACTNUM,ACTADD       TEST IF ADDING                               
         BE    VREC2               YES                                          
         MVI   ELCODE,BUTXTELQ     NO-DELETE TEXT ELEMENTS                      
         GOTO1 REMELEM                                                          
         B     VREC4                                                            
*                                                                               
VREC2    LR    RE,R4               CLEAR IO AREA FOR ADD                        
         L     RF,SIZEIO                                                        
         XCEF                                                                   
*                                                                               
         MVC   BUKEY,SVDIRKEY      INITIALIZE KEY W LEVEL'S KEY                 
         MVI   BUTSUB,BUTSUBQ                                                   
         MVC   BUTDTYP,DATATYPE                                                 
         MVC   BUTPER,PERIOD                                                    
         MVC   BUTCODE,CODE                                                     
         MVC   BURLEN,DATADISP                                                  
*                                                                               
* EDIT TEXT FIELDS                                                              
*                                                                               
VREC4    LA    R2,TEXTEX1H         R2=A(TEXT FIELD HEADER)                      
         LA    R0,TEXPFH                                                        
         ST    R0,ASCREND                                                       
         LA    R3,1                INITIALIZE LINE COUNTER                      
         MVI   INPUTSW,NO                                                       
         LA    R6,ELEM                                                          
         USING BUTXTD,R6                                                        
*                                                                               
VREC6    ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    VREC8               NO                                           
         XC    ELEM,ELEM                                                        
         MVI   BUTXTEL,BUTXTELQ                                                 
         STC   R3,BUTXTSEQ                                                      
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUTEXT(0),FLD                                                    
         LA    R1,BUTEXT-BUTXTD+1(R1) SET ELEMENT LENGTH                        
         STC   R1,BUTXTLEN                                                      
         GOTO1 ADDELEM                                                          
         MVI   INPUTSW,YES                                                      
*                                                                               
VREC8    ZIC   RE,0(R2)                                                         
         AR    R2,RE               BUMP TO NEXT FIELD                           
         LA    R3,1(R3)            BUMP LINE SEQUENCE NUMBER                    
         C     R2,ASCREND          TEST FOR MORE FIELDS                         
         BL    VREC6                                                            
         CLI   INPUTSW,YES         TEST FOR SOME TEXT                           
         BE    VRECX               YES                                          
         MVI   ERROR,MISSING                                                    
         LA    R2,TEXTEX1H                                                      
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         B     SPERR                                                            
*                                                                               
VRECX    MVC   KEY,SVGENKEY        RESTORE ORIGINAL KEY                         
         MVC   DMWORK,SVDMWORK     AND DMWORK                                   
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY TEXT RECORD                                                           
*                                                                               
DREC     GOTO1 VCLEARF,DMCB,TEXTEX1H,TEXPFH                                     
         L     R4,AIO                                                           
         USING BURECD,R4                                                        
*                                                                               
DREC4    LA    R2,TEXTEX1H         R2=A(TEXT FIELD HEADER)                      
         LA    R6,BUFRSTEL-BUKEY(R4)                                            
         USING BUTXTD,R6                                                        
         LA    R3,1                R3=LINE SEQUENCE NUMBER                      
         LA    RF,10               RF=SCREEN LINE COUNTER                       
*                                                                               
DREC6    CLI   0(R6),0             TEST FOR EOR                                 
         BE    DREC10              YES-ALL DONE                                 
         CLI   0(R6),BUTXTELQ                                                   
         BE    DREC8                                                            
*                                                                               
DREC7    ZIC   R0,BUTXTLEN                                                      
         AR    R6,R0                                                            
         B     DREC6                                                            
*                                                                               
DREC8    CLM   R3,1,BUTXTSEQ       TEST FOR RIGHT LINE SEQUENCE NO.             
         BE    DREC9                                                            
         BAS   RE,NEXTFLD          BUMP TO NEXT FIELD                           
         LA    R3,1(R3)            BUMP LINE SEQUENCE NUMBER                    
         BCT   RF,DREC8                                                         
         B     DREC10                                                           
*                                                                               
DREC9    ZIC   R1,BUTXTLEN                                                      
         SH    R1,=Y(BUTEXT-BUTXTD+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BUTEXT                                                   
         BAS   RE,NEXTFLD          BUMP TO NEXT FIELD                           
         LA    R3,1(R3)            INCREMENT LINE SEQUENCE                      
         BCT   RF,DREC7            NEXT ELEMENT                                 
*                                                                               
DREC10   CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DRECX                                                            
         MVC   TEXDA(3),=C'DA='    DISPLAY DISK ADDRESS                         
         GOTO1 HEXOUT,DMCB,DMDSKADD,TEXDA+3,4,=C'TOG'                           
         MVC   TEXDA+12(5),=C'NODE='                                            
         GOTO1 HEXOUT,DMCB,BUKNODE,TEXDA+17,4,=C'TOG'                           
         OI    TEXDAH+6,X'80'      XMIT                                         
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
* LIST TEXT RECORDS                                                             
*                                                                               
LIST     LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         OC    KEY,KEY             TEST FOR FIRST TIME                          
         BNZ   LIST4               NO                                           
*                                                                               
         ZIC   R3,SVORGLEV         GET LIST LEVEL                               
         CH    R3,=H'3'            TEST IF ABOVE PLAN                           
         BH    *+8                 YES                                          
         LA    R3,1                NO-START AT CLIENT                           
         STC   R3,SVLSTLEV         SAVE START LEVEL                             
         MH    R3,=Y(NDLVTABL)                                                  
         LA    R3,NDLVTAB(R3)      POINT TO LEVEL TABLE ENTRY                   
         USING NDLVTABD,R3                                                      
*                                                                               
LIST1    MVC   BUKEY,NDLVKEY       EXTRACT DIRECTORY KEY                        
         MVI   BUTSUB,BUTSUBQ                                                   
         GOTO1 HIGH                                                             
         B     LIST4                                                            
*                                                                               
LIST2    LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
LIST4    CLC   KEY(BUTDTYP-BUKEY),KEYSAVE TEST FOR SAME LEVEL                   
         BE    LIST6               YES                                          
*                                                                               
         CLI   SVLSTLEV,3          TEST IF OUTLINE LIST                         
         BH    XIT                 YES                                          
         CLC   SVLSTLEV,SVORGLEV   TEST IF ORIGINAL LEVEL REACHED               
         BE    XIT                 YES-ALL DONE                                 
         ZIC   R3,SVLSTLEV                                                      
         LA    R3,1(R3)            INCREMENT LIST LEVEL                         
         STC   R3,SVLSTLEV                                                      
         MH    R3,=Y(NDLVTABL)                                                  
         LA    R3,NDLVTAB(R3)                                                   
         B     LIST1               START BY READING HIGH                        
*                                                                               
LIST6    GOTO1 GETREC                                                           
         MVC   LISTAR,SPACES                                                    
         LA    RF,DISLIST          RF=A(OUTLINE TEXT LIST ROUTINE)              
         CLI   SVLSTLEV,3                                                       
         BH    *+8                                                              
         LA    RF,DISHIGH          RF=A(HIGH LEVEL TEXT ROUTINE)                
         BASR  RE,RF                                                            
*                                                                               
         GOTO1 LISTMON                                                          
         B     LIST2                                                            
         DROP  R3                                                               
*                                                                               
LISTHD   DC    C'CODE DATATYPE  PERIOD  TEXT  '                                 
HIGHHD   DC    C'CLIENT PRODUCT PLAN CODE TEXT  '                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY TEXT FOR OUTLINE LIST                                  
*                                                                               
DISLIST  NTR1                                                                   
         MVC   TXLHED(L'LISTHD),LISTHD                                          
         OI    TXLHEDH+6,X'80'                                                  
         LA    R3,LISTAR                                                        
         USING LSTLIND,R3                                                       
         L     R4,AIO                                                           
         MVC   LSTCODE,BUTCODE                                                  
         OC    BUTDTYP,BUTDTYP                                                  
         BZ    *+10                                                             
         MVC   LSTDTYP,BUTDTYP                                                  
         OC    BUTPER,BUTPER                                                    
         BZ    *+14                                                             
         BAS   RE,PEROUT                                                        
         MVC   LSTPER,WORK                                                      
*                                                                               
         MVI   ELCODE,BUTXTELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUTXTD,R6                                                        
         LA    R0,L'LSTTEXT-1                                                   
         ZIC   R1,BUTXTLEN                                                      
         SH    R1,=Y(BUTEXT-BUTXTD+1)                                           
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTTEXT(0),BUTEXT                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY TEXT FOR HIGH LEVEL LIST                               
*                                                                               
DISHIGH  NTR1                                                                   
         MVC   TXLHED(L'HIGHHD),HIGHHD                                          
         OI    TXLHEDH+6,X'80'                                                  
         LA    R3,LISTAR                                                        
         USING LSTLIND,R3                                                       
         L     R4,AIO                                                           
         MVC   LSTCLT,CLTCODE                                                   
         OC    LSTCLT,SPACES                                                    
         CLI   SVLSTLEV,1          TEST FOR CLIENT                              
         BE    DISHIGH2                                                         
*                                                                               
         MVC   LSTPRD,PRDCODE                                                   
         OC    LSTPRD,SPACES                                                    
         CLI   SVLSTLEV,2          TEST FOR PRODUCT                             
         BE    DISHIGH2                                                         
*                                                                               
         MVC   LSTPLAN,PLANCODE                                                 
         OC    LSTPLAN,SPACES                                                   
*                                                                               
DISHIGH2 MVC   LSTCODE2,BUTCODE                                                 
         MVI   ELCODE,BUTXTELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUTXTD,R6                                                        
         LA    R0,L'LSTTEXT2-1                                                  
         ZIC   R1,BUTXTLEN                                                      
         SH    R1,=Y(BUTEXT-BUTXTD+1)                                           
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTTEXT2(0),BUTEXT                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PREP     LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   AIO,AIO3            READ TEXT RECORDS INTO IO3                   
         MVI   NDUPDTSW,NO                                                      
         MVC   SAVERDSW,NDREREAD   SAVE RE-READ SWITCH VALUE                    
         MVI   NDREREAD,YES        SET TO RE-READ CLIENT AND PRODUCT            
         LA    R1,PREP2                                                         
         ST    R1,NDHOOK                                                        
*                                                                               
PREP1    GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BNE   PREPX                                                            
         GOTO1 (RF),DMCB,NODBLKD,=C'LSEQ',NODKEY,0                              
*                                                                               
PREPX    MVC   NDREREAD,SAVERDSW   RESTORE RE-READ SWITCH                       
         B     XIT                                                              
         SPACE 2                                                                
* NODIO HOOK ROUTINE                                                            
*                                                                               
PREP2    NTR1                                                                   
         CLI   NDMODE,NDVAL        TEST FOR HIGHER LEVEL RECORD PASSED          
         BNE   PREP3                                                            
         CLI   NDLEV,1             TEST FOR CLIENT                              
         BE    PREP4                                                            
         CLI   NDLEV,2             TEST FOR PRODUCT                             
         BE    PREP4                                                            
         B     PREP10                                                           
*                                                                               
PREP3    CLI   NDMODE,NDPROC                                                    
         BNE   PREP10                                                           
*                                                                               
PREP4    GOTO1 VGETVAL                                                          
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,NDLVKEY       EXTRACT DIRECTORY KEY                        
         MVI   BUTSUB,BUTSUBQ      READ TEXT RECORDS                            
*                                                                               
PREP5    GOTO1 HIGH                                                             
         B     PREP8                                                            
*                                                                               
PREP6    GOTO1 SEQ                                                              
*                                                                               
PREP8    CLC   BUKEY(BUTDTYP-BUKEY),KEYSAVE                                     
         BNE   PREP10                                                           
         OC    CODE,CODE           TEST FOR CODE FILTER                         
         BZ    *+14                                                             
         CLC   BUTCODE,CODE        APPLY CODE FILTER                            
         BNE   PREP6                                                            
         GOTO1 GETREC                                                           
         BAS   RE,DISREP           PRINT OUTPUT FOR TEXT RECORD                 
         B     PREP6                                                            
*                                                                               
PREP10   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT OUTPUT FOR A TEXT RECORD                                 
*                                                                               
DISREP   NTR1                                                                   
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVC   PRTCLT,CLTCODE                                                   
         OC    PRTCLT,SPACES                                                    
         CLI   NDLEV,1             TEST FOR CLIENT                              
         BE    DISREP2                                                          
*                                                                               
         MVC   PRTPRD,PRDCODE                                                   
         OC    PRTPRD,SPACES                                                    
         CLI   NDLEV,2             TEST FOR PRODUCT                             
         BE    DISREP2                                                          
*                                                                               
         MVC   PRTPLAN,PLANCODE                                                 
         OC    PRTPLAN,SPACES                                                   
         CLI   NDLEV,3             TEST FOR PLAN                                
         BE    DISREP2                                                          
*                                                                               
         MVC   PRTOUT,OUTCODE                                                   
         OC    PRTOUT,SPACES                                                    
         OC    BUTDTYP,BUTDTYP                                                  
         BZ    DISREP2                                                          
         MVC   PRTDTYP,BUTDTYP                                                  
*                                                                               
         OC    BUTPER,BUTPER                                                    
         BZ    DISREP2                                                          
         BAS   RE,PEROUT                                                        
         MVC   PRTPER,WORK                                                      
*                                                                               
DISREP2  MVC   PRTCODE,BUTCODE                                                  
         L     R4,AIO                                                           
         LA    R6,BUFRSTEL-BUKEY(R4) R6=A(TEXT ELEMENT)                         
         LA    R3,1                R3=LINE SEQUENCE NUMBER                      
         LA    R7,10               R7=LOOP COUNTER                              
*                                                                               
DISREP4  CLI   0(R6),0             TEST FOR EOR                                 
         BE    DISREP8                                                          
         CLI   0(R6),BUTXTELQ      TEST FOR TEXT ELEMENT                        
         BE    DISREP6                                                          
*                                                                               
DISREP5  ZIC   R0,BUTXTLEN                                                      
         AR    R6,R0                                                            
         B     DISREP4                                                          
*                                                                               
DISREP6  CLM   R3,1,BUTXTSEQ                                                    
         BE    DISREP7                                                          
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R3,1(R3)                                                         
         BCT   R7,DISREP6                                                       
         B     DISREP8                                                          
*                                                                               
DISREP7  ZIC   R1,BUTXTLEN                                                      
         SH    R1,=Y(BUTEXT-BUTXTD+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTTEXT(0),BUTEXT                                                
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R3,1(R3)                                                         
         BCT   R7,DISREP5                                                       
*                                                                               
DISREP8  GOTO1 SPOOL,PARAS,(R8)                                                 
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE DETAILS                                             
*                                                                               
HOOK     NTR1                                                                   
         MVC   H4+10(3),CLTCODE                                                 
         OC    H4+10(3),SPACES                                                  
         MVC   H4+15(L'CLTNAM),CLTNAM                                           
*                                                                               
         MVC   H5+10(3),PRDCODE                                                 
         OC    H5+10(3),SPACES                                                  
         MVC   H5+15(L'PRDNAM),PRDNAM                                           
*                                                                               
         MVC   H6+10(3),PLANCODE                                                
         OC    H6+10(3),SPACES                                                  
         MVC   H6+15(L'PLANNAM),PLANNAM                                         
*                                                                               
         ICM   R3,15,ABOX                                                       
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
         MVI   BOXROWS+7,C'T'      SET UP FOR BOXES                             
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   0(R2),C'L'                                                       
         MVI   8(R2),C'C'                                                       
         MVI   16(R2),C'C'                                                      
         MVI   22(R2),C'C'                                                      
         MVI   32(R2),C'C'                                                      
         MVI   42(R2),C'C'                                                      
         MVI   50(R2),C'C'                                                      
         MVI   55(R2),C'C'                                                      
         MVI   112(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHECK IF A SERIES OF FIELDS HAVE NO INPUT                      
*                                                                               
* AT ENTRY,  P1 = A(FIRST EMPTY FIELD)                                          
*            P2 = A(LAST FIELD TO CHECK)                                        
*                                                                               
CHKEM    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LR    R4,R2               R4=A(FIRST EMPTY FIELD)                      
         B     CHKEM3              BUMP TO NEXT UNPROTECTED FIELD               
*                                                                               
CHKEM2   GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   CHKEMR              NO-DEAL WITH ERROR                           
*                                                                               
CHKEM3   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *-10                YES                                          
         CR    R2,R3               TEST IF PAST CHECK LIMIT                     
         BNH   CHKEM2              NO-SO HAVE A LOOK                            
*                                                                               
CHKEMX   B     XIT                                                              
*                                                                               
CHKEMR   MVI   ERROR,MISSING                                                    
         ST    R4,FADDR                                                         
         LA    R4,8(R4)                                                         
         STCM  R4,7,FLAST                                                       
         B     SPERR                                                            
         EJECT                                                                  
* SUB-ROUTINE TO FORCE A SERIES OF FIELDS TO BE EMPTY                           
*                                                                               
* AT ENTRY, P1 = A(FIRST FIELD TO BE CHECKED)                                   
*           P2 = A(LAST FIELD TO BE CHECKED)                                    
*                                                                               
FORCEM   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   ERROR,INVALID                                                    
*                                                                               
FORCEM2  GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    FORCEM3                                                          
         MVC   XTRA(11),=C'ERASE FIELD'                                         
         B     SPERR                                                            
*                                                                               
FORCEM3  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         CR    R2,R3                                                            
         BNH   FORCEM2                                                          
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR PROTECTED NAME FIELDS AT START OF VALKEY                 
*                                                                               
CLRNAME  NTR1                                                                   
         LA    RE,NAMETAB          RE=TABLE POINTER                             
         LA    R0,NAMES            R0=COUNTER                                   
*                                                                               
CLRNAME2 SR    R2,R2                                                            
         ICM   R2,3,0(RE)          RE=FIELD HEADER DISPLACEMENT                 
         A     R2,ATWA             R2=A(FIELD HEADER)                           
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'                                                         
         TM    1(R2),X'02'         TEST FOR EXTENDED HEADER                     
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         BCTR  R1,0                FOR EXECUTE                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
CLRNAME4 LA    RE,L'NAMETAB(RE)                                                 
         BCT   R0,CLRNAME2                                                      
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY DATA TYPE HEADINGS ON SCREEN                           
*                                                                               
DTOUT    NTR1                                                                   
         L     R4,AIO                                                           
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
DTOUT2   IC    R0,1(R6)            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST FOR EOR                                 
         BE    DTOUT4              YES                                          
         CLI   0(R6),BUDHELQ       TEST FOR HEADING ELEMENT                     
         BNE   DTOUT2                                                           
*                                                                               
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,MOVEHEAD                                                      
*                                                                               
DTOUT4   GOTO1 SQUASHER,DMCB,WORK,40                                            
         MVC   TEXDTN,WORK         SET COMPRESSED HEADLINE                      
         MVC   SVDTNAME,WORK                                                    
*                                                                               
DTOUTX   B     XIT                                                              
         SPACE 2                                                                
MOVEHEAD MVC   0(0,R3),BUDHEAD                                                  
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY THE PERIOD                                             
* AT ENTRY, R4 POINTS TO KEY.  ON EXIT, WORK CONTAINS DISPLAY PERIOD            
*                                                                               
PEROUT   NTR1                                                                   
         USING BURECD,R4                                                        
         MVC   WORK,SPACES                                                      
         CLI   CLTTYPE,10          TEST FOR 444 FISCAL YEAR                     
         BE    PEROUT2                                                          
*                                                                               
PEROUT1  MVC   FULL(2),BUTSTART    GET START MONTH                              
         MVI   FULL+2,X'01'                                                     
         GOTO1 DATCON,PARAS,(3,FULL),(6,DUB)                                    
         MVC   WORK(3),DUB         EXTRACT START MONTH                          
         CLC   BUTSTART,BUTEND     TEST START MONTH=END MONTH                   
         BE    PEROUTX                                                          
*                                                                               
         MVC   FULL(2),BUTEND                                                   
         GOTO1 DATCON,(R1),(3,FULL),(6,DUB)                                     
         MVI   WORK+3,DASH                                                      
         MVC   WORK+4(3),DUB       END MONTH                                    
         B     PEROUTX                                                          
*                                                                               
PEROUT2  ZIC   R1,BUTSTART+1                                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK,C'P'                                                        
         UNPK  WORK+1(2),DUB+6(2)                                               
         CLC   BUTSTART,BUTEND     TEST START PERIOD=END PERIOD                 
         BE    PEROUTX                                                          
*                                                                               
         ZIC   R1,BUTEND+1                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   WORK+3,DASH                                                      
         MVI   WORK+4,C'P'                                                      
         UNPK  WORK+5(2),DUB+6(2)                                               
*                                                                               
PEROUTX  B     XIT                                                              
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,AIO),0,0                       
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
NEXTFLD  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS OF PROTECTED 'NAME' FIELDS                             
*                                                                               
NAMETAB  DS    0XL2                                                             
         DC    AL2(TEXCLNH-T502FFD)                                             
         DC    AL2(TEXPRNH-T502FFD)                                             
         DC    AL2(TEXPLNH-T502FFD)                                             
         DC    AL2(TEXOUTNH-T502FFD)                                            
         DC    AL2(TEXDTNH-T502FFD)                                             
NAMES    EQU   (*-NAMETAB)/L'NAMETAB                                            
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,49,C'TEXT REPORT'                                             
         SSPEC H2,49,C'-----------'                                             
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'PLAN'                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
         SSPEC H9,2,C'CLIENT'                                                   
         SSPEC H9,10,C'PRODUCT'                                                 
         SSPEC H9,18,C'PLAN'                                                    
         SSPEC H9,24,C'OUTLINE'                                                 
         SSPEC H9,34,C'DATATYPE'                                                
         SSPEC H9,44,C'PERIOD'                                                  
         SSPEC H9,52,C'CODE'                                                    
         SSPEC H9,57,C'TEXT'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF8D                                                       
         EJECT                                                                  
* DSECT TO COVER LIST SCREEN                                                    
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILE8D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
ASCREND  DS    A                                                                
*                                                                               
DATATYPE DS    CL(L'BUDTYP)        DATA TYPE CODE                               
PERIOD   DS    0CL(L'BUTPER)                                                    
START    DS    XL(L'BUTSTART)      START Y/M                                    
END      DS    XL(L'BUTEND)        END Y/M                                      
CODE     DS    CL(L'BUTCODE)       TEXT CODE                                    
*                                                                               
INPUTSW  DS    C                                                                
*                                                                               
SAVEKEY  DS    CL(L'KEY)                                                        
SAVEAIO  DS    XL4                                                              
SAVERDSW DS    C                   SAVED REREAD SWITCH VALUE                    
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         ORG   TWA1USER            SAVE AREA                                    
SVVALS   DS    0C                  KEY FIELDS SAVE AREA                         
SVCLTVAL DS    CL(L'CLTVALS)       CLIENT VALUES SAVE AREA                      
SVPRDVAL DS    CL(L'PRDVALS)       PRODUCT VALUES SAVE AREA                     
SVPLNVAL DS    CL(L'PLANVALS)      PLAN VALUES SAVE AREA                        
SVOUTVAL DS    CL(L'OUTVALS)       OUTLINE VALUES SAVE AREA                     
SVDTNAME DS    CL(L'TEXDTN)        DATA TYPE NAME SAVE                          
SVPLNKEY DS    CL(L'BUKEY)         PLAN DIRECTORY KEY                           
SVVALN   EQU   *-SVVALS            L'KEY FIELDS SAVE AREA                       
SVDIRKEY DS    CL(L'BUKEY)         DIRECTORY KEY FOR NODAL LEVEL                
SVGENKEY DS    XL(L'KEY)           ORIGINAL KEY AND DMWORK                      
SVDMWORK DS    12D                                                              
SVORGLEV DS    X                   ORIGINAL LIST LEVEL                          
SVLSTLEV DS    X                   LIST LEVEL                                   
         SPACE 2                                                                
* DSECT TO COVER LIST DISPLAY LINE                                              
*                                                                               
LSTLIND  DSECT                                                                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSTCODE  DS    CL2                                                              
         DS    CL3                                                              
LSTDTYP  DS    CL8                 DATA TYPE CODE                               
         DS    CL2                                                              
LSTPER   DS    CL7                                                              
         DS    C                                                                
LSTTEXT  DS    CL45                                                             
         ORG   LSTLIN                                                           
LSTCLT   DS    CL3                 CLIENT CODE                                  
         DS    CL4                                                              
LSTPRD   DS    CL3                 PRODUCT CODE                                 
         DS    CL5                                                              
LSTPLAN  DS    CL3                 PLAN CODE                                    
         DS    CL2                                                              
LSTCODE2 DS    CL2                 TEXT CODE                                    
         DS    CL3                                                              
LSTTEXT2 DS    CL40                                                             
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    C                                                                
PRTCLT   DS    CL3                 CLIENT                                       
         DS    CL5                                                              
PRTPRD   DS    CL3                 PRODUCT                                      
         DS    CL5                                                              
PRTPLAN  DS    CL3                 PLAN                                         
         DS    CL3                                                              
PRTOUT   DS    CL8                 OUTLINE                                      
         DS    CL2                                                              
PRTDTYP  DS    CL8                 DATA TYPE                                    
         DS    CL2                                                              
PRTPER   DS    CL7                 PERIOD                                       
         DS    C                                                                
PRTCODE  DS    CL2                 TEXT CODE                                    
         DS    CL3                                                              
PRTTEXT  DS    CL56                TEXT                                         
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009BUFIL08   05/01/02'                                      
         END                                                                    
