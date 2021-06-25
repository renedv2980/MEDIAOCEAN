*          DATA SET BUFIL06    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T50206A,*                                                                
         TITLE 'T50206 - BUDGET CONTROL LFM - DATA TYPE RECORD'                 
T50206   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI06**,RA,RR=R2                                              
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
         CLI   MODE,RECDEL                                                      
         BE    DREC                                                             
         CLI   MODE,RECREST                                                     
         BE    DREC                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     GOTO1 VUPKEY                                                           
         LA    R2,DTYCLTH          VALIDATE CLIENT                              
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,DTLCLTH                                                       
         GOTO1 VVALCLT,PARAS,(R2),0                                             
*                                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
*                                                                               
         MVC   SVCLTVAL,CLTVALS    SAVE CLIENT RECORD VALUES                    
         CLI   ACTNUM,ACTLIST      TEST FOR ACTION LIST                         
         BE    VKEY2               YES-SKIP DISPLAY OF CLIENT NAME              
*                                                                               
         XC    DTYCLN,DTYCLN       CLEAR CLIENT NAME                            
         MVC   DTYCLN(L'CLTNAM),CLTNAM                                          
         OI    DTYCLNH+6,X'80'     XMIT                                         
*                                                                               
* EDIT AND VALIDATE PRODUCT                                                     
*                                                                               
VKEY2    LA    R2,DTYPRDH          VALIDATE PRODUCT                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,DTLPRDH                                                       
*                                                                               
         SR    R0,R0                                                            
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+8                                                              
         ICM   R0,8,MISSPARM                                                    
         GOTO1 VVALPRD,PARAS,(R2),(R0)                                          
*                                                                               
         OC    PRDCODE,PRDCODE     TEST FOR 'ALL' PRODUCTS                      
         BNZ   VKEY3               NO                                           
         GOTO1 VGETFLD,PARAS,DTYPLAH                                            
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0            TEST FOR 'ALL' PLANS                         
         BNE   SPERR               NO-FIELD MUST BE EMPTY                       
         MVI   ERROR,NOWERR                                                     
         TM    WHEN,X'38'          TEST REQUEST MADE SOON OR OVERN              
         BNZ   VKEYX               YES                                          
*                                                                               
         LA    R2,CONWHENH                                                      
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         B     SPERR                                                            
*                                                                               
VKEY3    L     R4,NDIOA                                                         
         MVC   SVPRDVAL,PRDVALS    SAVE AWAY PRODUCT VALUES                     
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEY4                                                            
*                                                                               
         XC    DTYPRN,DTYPRN       CLEAR PRODUCT NAME                           
         MVC   DTYPRN(L'PRDNAM),PRDNAM                                          
         OI    DTYPRNH+6,X'80'     XMIT                                         
*                                                                               
* EDIT AND VALIDATE PLAN CODE                                                   
*                                                                               
VKEY4    LA    R2,DTYPLAH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,DTLPLAH                                                       
         SR    R0,R0                                                            
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+8                                                              
         ICM   R0,8,MISSPARM                                                    
         GOTO1 VVALPLAN,PARAS,(R2),(R0)                                         
*                                                                               
         OC    PLANCODE,PLANCODE   TEST FOR 'ALL' PLANS                         
         BNZ   VKEY5                                                            
         MVI   ERROR,NOWERR                                                     
         TM    WHEN,X'38'          TEST FOR SOON OR OVERNIGHT REQUEST           
         BNZ   VKEYX                                                            
         LA    R2,CONWHENH                                                      
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         B     SPERR                                                            
*                                                                               
VKEY5    L     R4,NDIOA                                                         
         MVC   SVNKEY,NODKEY       SAVE NODAL KEY                               
         MVC   SVPLNVAL,PLANVALS   SAVE PLAN VALUES                             
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   SVPLNKEY,NDLVKEY    EXTRACT PLAN'S REGULAR KEY                   
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX                                                            
*                                                                               
VKEY6    XC    DTYPLN,DTYPLN                                                    
         MVC   DTYPLN(L'PLANNAM),PLANNAM                                        
         OI    DTYPLNH+6,X'80'                                                  
         CLI   ACTNUM,ACTREP       TEST FOR REPORT                              
         BE    VKEYX               YES-ALL DONE WITH EDIT                       
*                                                                               
* EDIT AND VALIDATE DATA TYPE                                                   
*                                                                               
VKEY8    LA    R2,DTYCODH                                                       
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   DATATYPE,FLD                                                     
         MVC   LENDT,FLDH+5                                                     
*                                                                               
VKEY10   XC    KEY,KEY             VALIDATE DATA TYPE                           
         LA    R4,KEY              MAKE KEY ADDRESSABLE                         
         MVC   KEY(L'BUKEY),SVPLNKEY START WITH PLAN KEY                        
         MVI   BUDSUB,BUDSUBQ                                                   
         MVC   BUDTYP,DATATYPE                                                  
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VKEYX                                                            
         CLI   ACTNUM,ACTREST      TEST FOR RESTORE                             
         BE    VKEYX                                                            
*                                                                               
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(L'BUKEY),KEYSAVE                                             
         BNE   TRAPERR                                                          
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BNE   VKEYX                                                            
         MVI   ERROR,DELERR                                                     
         TM    PLANIND,BUPLNDAT    TEST IF PLAN HAS DATA RECORDS                
         BO    TRAPERR                                                          
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
*                                                                               
         ZIC   R1,SELLISTN         INDEX INTO LIST DIRECTORY                    
         MH    R1,=H'6'            LENGTH OF LIST DIRECTORY ENTRY               
         LA    R1,LISTDIR(R1)                                                   
         CLI   0(R1),C'D'          TEST FOR SELECT FOR DELETE                   
         BE    *+12                                                             
         CLI   0(R1),C'R'          TEST FOR SELECT FOR RESTORE                  
         BNE   DKEY2                                                            
         MVI   ERROR,INVACT        PREVENT THEM                                 
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
DKEY2    XC    DUB,DUB                                                          
         MVC   DUB(L'CLTCODE),CLTCODE                                           
         OC    DUB,SPACES                                                       
         MVC   DTYCLT,DUB                                                       
         OI    DTYCLTH+6,X'80'                                                  
*                                                                               
         MVC   DTYCLN,CLTNAM                                                    
         OI    DTYCLNH+6,X'80'                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(L'PRDCODE),PRDCODE                                           
         OC    DUB,SPACES                                                       
         MVC   DTYPRD,DUB                                                       
         OI    DTYPRDH+6,X'80'                                                  
*                                                                               
         MVC   DTYPRN,PRDNAM                                                    
         OI    DTYPRNH+6,X'80'                                                  
*                                                                               
         MVC   DUB,PLANCODE        DISPLAY PLAN CODE                            
         OC    DUB,SPACES                                                       
         MVC   DTYPLA,DUB                                                       
         OI    DTYPLAH+6,X'80'                                                  
         MVC   DTYPLN,PLANNAM      AND NAME                                     
         OI    DTYPLNH+6,X'80'                                                  
*                                                                               
         MVC   DTYCOD,BUDTYP       SHOW DATA TYPE                               
         OI    DTYCODH+6,X'80'                                                  
         GOTO1 VUPKEY                                                           
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE DATA TYPE RECORD                                                     
*                                                                               
VREC     L     R4,AIO                                                           
         USING BURECD,R4                                                        
*                                                                               
         MVC   SVGENKEY,KEY        SAVE KEY AND DMWORK TO                       
         MVC   SVDMWORK,DMWORK     PROTECT ME AGAINST GENCON                    
*                                                                               
         MVC   CLTVALS,SVCLTVAL    RESTORE CLIENT VALUES                        
         MVC   PRDVALS,SVPRDVAL    RESTORE PRODUCT VALUES                       
         MVC   PLANVALS,SVPLNVAL   RESTORE PLAN VALUES                          
*                                                                               
         CLI   ACTNUM,ACTADD       TEST IF ADDING                               
         BE    VREC1                                                            
         MVI   ELCODE,BUDTELQ                                                   
         GOTO1 REMELEM                                                          
         B     VREC2                                                            
*                                                                               
VREC1    LR    RE,R4               CLEAR IO AREA FOR ADD                        
         L     RF,SIZEIO                                                        
         XCEF                                                                   
*                                                                               
         MVC   BUKEY,SVPLNKEY      INITIALIZE KEY W MASTER                      
         MVI   BUDSUB,BUDSUBQ                                                   
         MVC   BUDTYP,DATATYPE                                                  
         MVC   BURLEN,DATADISP                                                  
*                                                                               
* EDIT DATA TYPE HEADINGS                                                       
*                                                                               
VREC2    MVI   ELCODE,BUDHELQ      DELETE DATA TYPE HEADING EL                  
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING BUDHD,R6                                                         
         LA    R2,DTYHD1H          EDIT HEADING                                 
         GOTO1 VGETFLD,DMCB,(R2)                                                
         CLI   FLDH+5,0                                                         
         BNE   VREC3                                                            
*                                                                               
         MVI   EMPTYSW,YES                                                      
         MVC   FLD(L'DATATYPE),DATATYPE                                         
         MVC   FLDH+5(1),LENDT                                                  
         MVC   8(L'DATATYPE,R2),DATATYPE                                        
         OI    6(R2),X'80'         XMIT DEFAULT BACK TO USER                    
*                                                                               
VREC3    MVI   BUDHEL,BUDHELQ      BUILD HEADING ELEMENT                        
         MVI   BUDHNUM,1                                                        
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,MOVEHEAD                                                      
         LA    R1,BUDHEAD-BUDHD+1(R1)                                           
         STC   R1,BUDHLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
VREC4    LA    R2,DTYHD2H                                                       
         GOTO1 VGETFLD,DMCB,(R2)                                                
         CLI   FLDH+5,0                                                         
         BE    VREC5                                                            
         MVI   ERROR,INVALID                                                    
         CLI   EMPTYSW,YES         TEST IF FIRST FIELD EMPTY                    
         BE    TRAPERR                                                          
         MVI   BUDHNUM,2                                                        
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,MOVEHEAD                                                      
         LA    R1,BUDHEAD-BUDHD+1(R1)                                           
         STC   R1,BUDHLEN                                                       
         GOTO1 ADDELEM                                                          
         B     VREC5                                                            
*                                                                               
MOVEHEAD MVC   BUDHEAD(0),FLD                                                   
*                                                                               
* EDIT STACK FORMAT NAME                                                        
*                                                                               
VREC5    MVI   ELCODE,BUSTELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,DTYSTAKH                                                      
         GOTO1 VGETFLD,DMCB,(R2)                                                
         CLI   FLDH+5,0            TEST IF ANY INPUT                            
         BE    VREC6               NO                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING BUSTD,R6                                                         
         MVI   BUSTEL,BUSTELQ                                                   
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BUSTNAME(0),FLD                                                  
         LA    R1,BUSTNAME-BUSTD+1(R1)                                          
         STC   R1,BUSTLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
* EDIT EXTRACT TYPE                                                             
*                                                                               
VREC6    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,BUDTELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   ELCODE,BUINELQ      DELETE SCREEN INPUT EL                       
         GOTO1 REMELEM                                                          
         MVI   ELCODE,BUDOPELQ     DELETE DATA TYPE OPERAND EL                  
         GOTO1 REMELEM                                                          
         MVI   ELCODE,BUPOLELQ     DELETE POLISH ELEMENT                        
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING BUDTD,R6                                                         
         MVI   BUDTEL,BUDTELQ                                                   
         MVI   BUDTLEN,BUDTLNQ                                                  
*                                                                               
         MVI   BUDTEX,0                                                         
         LA    R2,DTYEXH                                                        
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    VREC8               NOT EXTRACTABLE                              
         MVI   ERROR,INVALID                                                    
         LA    R0,EXTNTR                                                        
         LA    RE,EXTTAB                                                        
         USING EXTTABD,RE                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
VREC7    CLC   FLDH+5(1),EXTMINL                                                
         BL    *+12                                                             
         EX    R1,EXTCOMP                                                       
         BE    VREC7A                                                           
         LA    RE,EXTTABL(RE)                                                   
         BCT   R0,VREC7                                                         
         B     TRAPERR                                                          
*                                                                               
VREC7A   MVC   BUDTEX,EXTEQU       GET EXTRACT TYPE EQUATE                      
         MVC   EXTRTYPE,EXTNAME    GET EXTRACT TYPE NAME                        
         B     VREC8                                                            
*                                                                               
EXTCOMP  CLC   FLD(0),EXTNAME                                                   
         DROP  RE                                                               
*                                                                               
* EDIT COLUMN WIDTH                                                             
*                                                                               
VREC8    MVI   BUDTCOL,COLLEN      SET DEFAULT COLUMN LENGTH                    
         LA    R2,DTYCOLH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    VREC10                                                           
         MVI   ERROR,NOTNUM                                                     
         TM    FLDH+4,X'08'        TEST NUMERIC VALUE                           
         BZ    TRAPERR                                                          
         LTR   R0,R0                                                            
         BZ    TRAPERR                                                          
         CH    R0,=Y(MAXCOL)                                                    
         BH    TRAPERR                                                          
         STC   R0,BUDTCOL                                                       
*                                                                               
* EDIT SCALE FIELD                                                              
*                                                                               
VREC10   MVI   BUDTSC,0            DEFAULT IS UNITS                             
         LA    R2,DTYSCAH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    VREC14                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   FLD,LPAREN          TEST FOR LEFT PAREN STARTING FLD             
         BNE   VREC11                                                           
         ZIC   R1,FLDH+5           YES                                          
         SH    R1,=H'1'                                                         
         BZ    TRAPERR                                                          
         STC   R1,FLDH+5           REMOVE IT FROM STRING                        
         MVC   DUB,FLD+1           AND ADJUST LENGTH                            
         MVC   FLD(L'DUB),DUB                                                   
*                                                                               
VREC11   ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,FLD(R1)                                                       
         CLI   0(RE),C')'          TEST IF LAST BYTE IS RIGHT PAREN             
         BNE   VREC12                                                           
         LTR   R1,R1                                                            
         BZ    TRAPERR                                                          
         STC   R1,FLDH+5           YES-REMOVE AND ADJUST LEN                    
         MVI   0(RE),C' '                                                       
*                                                                               
VREC12   CLI   FLDH+5,MAXSCALE     TEST FOR MAXIMUM SCALE                       
         BH    TRAPERR                                                          
         ZIC   R0,FLDH+5                                                        
         LA    RE,FLD                                                           
         CLI   0(RE),C'0'          TEST FOR ZERO                                
         BNE   TRAPERR                                                          
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
         MVC   BUDTSC,FLDH+5       SET SCALE BYTE                               
         B     VREC14                                                           
*                                                                               
* EDIT DECIMAL PLACES                                                           
*                                                                               
VREC14   MVI   BUDTDEC,0                                                        
         LA    R2,DTYDECH                                                       
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    VREC15                                                           
         MVI   ERROR,NOTNUM                                                     
         TM    FLDH+4,X'08'                                                     
         BZ    TRAPERR                                                          
         CH    R0,=Y(MAXDEC)       TEST MAXIMUM DECIMAL PLACES                  
         BH    TRAPERR                                                          
         STC   R0,BUDTDEC                                                       
*                                                                               
* EDIT FORMAT OPTIONS                                                           
*                                                                               
VREC15   XC    BUDTFORM,BUDTFORM                                                
         BAS   RE,OPTED                                                         
         GOTO1 ADDELEM             ADD THE DATA TYPE ELEMENT                    
         MVI   EXSW,NO                                                          
         CLI   BUDTEX,0                                                         
         BE    VREC20                                                           
         MVI   EXSW,YES            NOTE-EXTRACT DATA PRESENT                    
*                                                                               
* EDIT FORMULA FIELD                                                            
*                                                                               
VREC20   BAS   RE,SCRIN                                                         
         BNE   VREC25              NO FORMULA INPUT                             
         LA    R2,DTYFO1H                                                       
         CLI   EXSW,YES                                                         
         BE    TRAPERR                                                          
*                                                                               
         GOTO1 VALFORM,DMCB,(2,(R2)),ELEM                                       
         BNE   SPERR                                                            
         GOTO1 ADDELEM                                                          
*                                                                               
VREC25   MVC   KEY,SVGENKEY        RESTORE KEY TO PRESERVE DA                   
         MVC   DMWORK,SVDMWORK     RESTORE ORIGINAL DMWORK FOR PUT              
*                                                                               
VRECX    B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* DISPLAY DATA TYPE RECORD                                                      
*                                                                               
DREC     GOTO1 VCLEARF,DMCB,DTYHD1H,DTYLAST                                     
         BAS   RE,GETDT            GET DATA TYPE VALUES                         
         L     R4,AIO                                                           
*                                                                               
DREC2    MVC   DTYHD1,DTHEAD1      GET FIRST HEADING                            
         CLI   DTLHEAD2,0          TEST FOR SECOND HEADING                      
         BE    *+10                                                             
         MVC   DTYHD2,DTHEAD2      YES-MOVE TO SCREEN                           
         MVC   DTYSTAK,DTSTNAME    STACK FORMAT NAME                            
*                                                                               
         CLI   DTEX,0              TEST FOR EXTRACT TYPE                        
         BE    *+10                NONE                                         
         MVC   DTYEX(L'DTEXNAME),DTEXNAME SHOW EXTRACT TYPE NAME                
*                                                                               
         CLI   DTCOL,0                                                          
         BE    DREC4                                                            
         ZIC   R0,DTCOL                                                         
         EDIT  (R0),(2,DTYCOL),ALIGN=LEFT                                       
*                                                                               
DREC4    CLI   DTSC,0              TEST FOR ANY SCALE                           
         BE    *+10                                                             
         MVC   DTYSCA,DTSCEXP      MOVE SCALE EXPRESSION TO SCREEN              
         CLI   DTDEC,0             TEST FOR ANY DECIMAL PLACES                  
         BE    *+10                                                             
         MVC   DTYDEC,DTDECEXP                                                  
         CLI   DTFORM,0            TEST FOR ANY FORMAT OPTIONS                  
         BE    DREC6               NO                                           
         GOTO1 DISOPT,PARAS,DTYOPTH                                             
*                                                                               
DREC6    MVI   ELCODE,BUPOLELQ     SEARCH FOR POLISH STRING ELEMENT             
         BAS   RE,GETEL                                                         
         BNE   DREC8                                                            
         GOTO1 DISSCR,PARAS,DTYFO1H,2                                           
*                                                                               
DREC8    CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DRECX                                                            
         MVC   DTYDA(3),=C'DA='    DISPLAY DISK ADDRESS                         
         GOTO1 HEXOUT,DMCB,DMDSKADD,DTYDA+3,4,=C'TOG'                           
         OI    DTYDAH+6,X'80'      XMIT                                         
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
* LIST DATA TYPE RECORDS                                                        
*                                                                               
LIST     LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         OC    KEY,KEY             TEST FOR FIRST TIME                          
         BNZ   LIST4               NO                                           
         MVC   KEY(L'SVPLNKEY),SVPLNKEY                                         
         MVI   BUDSUB,BUDSUBQ                                                   
*                                                                               
LIST1    GOTO1 HIGH                                                             
         B     LIST4                                                            
*                                                                               
LIST2    GOTO1 SEQ                                                              
*                                                                               
LIST4    CLC   KEY(BUDTYP-BUKEY),KEYSAVE TEST FOR SAME PLAN                     
         BNE   XIT                 NO-ALL DONE                                  
         GOTO1 GETREC                                                           
         BAS   RE,GETDT            GET DATA TYPE VALUES                         
         MVC   DTLHED(L'LISTHD),LISTHD                                          
         OI    DTLHEDH+6,X'80'                                                  
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING LSTLIND,R3                                                       
         MVC   LSTDTYP,DTCODE      EXTRACT DATA TYPE CODE                       
         LA    R0,DTHEADL                                                       
         GOTO1 SQUASHER,DMCB,DTHEAD1,(R0)                                       
         MVC   LSTNAME,DTHEAD1                                                  
         MVC   LSTSCA,DTSCEXP                                                   
         CLI   DTDEC,0             TEST FOR ZERO DECIMAL PLACES                 
         BE    *+14                                                             
         MVI   LSTPOINT,C'.'                                                    
         MVC   LSTDEC,DTDECEXP     N'DECIMAL PLACES                             
*                                                                               
         GOTO1 LISTMON                                                          
         B     LIST2                                                            
         DROP  R3                                                               
*                                                                               
LISTHD   DC    C'CODE       HEADING 1/2                   SCALE     DECX        
               IMAL'                                                            
         EJECT                                                                  
* PRINT DATA TYPE REPORT                                                        
*                                                                               
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVC   AIO,AIO3            READ DATA TYPE RECORD INTO IO3               
*                                                                               
         GOTO1 VSETKEY                                                          
         MVI   NDUPDTSW,NO                                                      
         LA    R1,PREP2                                                         
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BNE   PREPX                                                            
*                                                                               
         GOTO1 (RF),DMCB,NODBLKD,=C'LSEQ',NODKEY,0                              
*                                                                               
PREPX    B     XIT                                                              
         SPACE 2                                                                
* HOOK TO PROCESS RECORDS FROM NODIO                                            
*                                                                               
PREP2    NTR1                                                                   
         CLI   NDMODE,NDFRST       TEST FOR FIRST TIME FOR LEVEL                
         BNE   PREP4                                                            
         CLI   NDLEV,3             TEST FOR PLANS                               
         BNE   *+8                                                              
         MVI   NDSKIP,YES          SKIP ANYTHING BELOW PLAN                     
         B     PREPX                                                            
*                                                                               
PREP4    CLI   NDMODE,NDLAST       TEST LAST TIME FOR LEVEL                     
         BNE   PREP6                                                            
         MVI   NDSKIP,NO           TURN OFF SKIP SWITCH                         
         B     PREPX                                                            
*                                                                               
PREP6    CLI   NDMODE,NDPROC                                                    
         BNE   PREPX                                                            
         GOTO1 VGETVAL                                                          
         CLI   NDLEV,3             TEST FOR PLAN                                
         BNE   PREPX                                                            
*                                                                               
         MVI   FORCEHED,YES        BREAK PAGE FOR PLAN                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   BUKEY,NDLVKEY       EXTRACT PLAN'S DIRECTORY KEY                 
         MVI   BUDSUB,BUDSUBQ      NOW READ DATA TYPES FOR PLAN                 
*                                                                               
PREP8    GOTO1 HIGH                                                             
         B     PREP10                                                           
*                                                                               
PREP9    GOTO1 SEQ                                                              
*                                                                               
PREP10   CLC   BUKEY(BUDTYP-BUKEY),KEYSAVE TEST FOR SAME PLAN                   
         BNE   PREPX                                                            
         GOTO1 GETREC                                                           
         BAS   RE,GETDT            GET DATA TYPE RECORD VALUES                  
         BAS   RE,DISREP           OUTPUT REPORT LINE FOR RECORD                
         GOTO1 SPOOL,PARAS,(R8)    PRINT REPORT LINES                           
         GOTO1 (RF),PARAS,(R8)     SKIP A LINE AFTER                            
         B     PREP9                                                            
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
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   0(R2),C'L'                                                       
         MVI   11(R2),C'C'                                                      
         MVI   35(R2),C'C'                                                      
         MVI   45(R2),C'C'                                                      
         MVI   66(R2),C'C'                                                      
         MVI   123(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY REPORT DETAIL FOR A DATA TYPE RECORD                   
*                                                                               
DISREP   NTR1                                                                   
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVC   PRTCODE,DTCODE                                                   
         MVC   PRTHED1,DTHEAD1                                                  
         MVC   PRTHED2,DTHEAD2                                                  
         CLC   DTSTNAME,SPACES     TEST FOR STACK FORMAT NAME                   
         BE    DISREP1             NO                                           
         MVC   PRTSTAK(6),=C'STACK='                                            
         MVC   PRTSTAK+6(L'DTSTNAME),DTSTNAME                                   
*                                                                               
DISREP1  CLI   DTEX,0              TEST FOR EXTRACT TYPE                        
         BE    *+10                                                             
         MVC   PRTEXT,DTEXNAME                                                  
*                                                                               
         LA    R0,10               DEFAULT COL WIDTH IS 10                      
         CLI   DTCOL,0                                                          
         BE    *+10                                                             
         SR    R0,R0                                                            
         IC    R0,DTCOL                                                         
         EDIT  (R0),(2,PRTCOL),ALIGN=LEFT                                       
*                                                                               
         MVI   PRTSCA-1,SLASH      SEPARATE COLUMN WIDTH AND SCALE              
         CLI   DTSC,0              TEST FOR SCALE                               
         BE    *+14                                                             
         MVC   PRTSCA,DTSCEXP      SCALE EXPRESSION                             
         B     *+10                                                             
         MVC   PRTSCA(3),=C'(1)'   DEFAULT IS INTEGER                           
*                                                                               
         MVI   PRTPOINT-1,SLASH    SEPARATE SCALE AND DECIMAL W SLASH           
         CLI   DTDEC,0                                                          
         BE    *+14                                                             
         MVI   PRTPOINT,C'.'                                                    
         MVC   PRTDEC,DTDECEXP                                                  
*                                                                               
DISREP2  L     R6,AIO                                                           
         AH    R6,DATADISP         R6=A(ELEMENT)                                
         LA    RE,PRTFORM1                                                      
*                                                                               
DISREP4  CLI   0(R6),0             TEST FOR EOR                                 
         BE    DISREP8                                                          
         CLI   0(R6),BUINELQ       TEST FOR SCREEN INPUT ELEMENT                
         BE    DISREP6                                                          
*                                                                               
DISREP5  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DISREP4                                                          
*                                                                               
         USING BUIND,R6                                                         
DISREP6  ZIC   R1,BUINLEN                                                       
         SH    R1,=Y(BUINPUT-BUIND+1)                                           
         EX    R1,MOVEOUT                                                       
         LA    RE,132(RE)          NEXT PRINT LINE                              
         B     DISREP5                                                          
*                                                                               
DISREP8  CLI   DTFORM,0            TEST FOR FORMAT OPTIONS                      
         BE    DISREPX                                                          
*                                                                               
         XC    FLDH,FLDH                                                        
         MVI   FLDH,L'FLDH+L'FLD                                                
         MVC   FLD,SPACES                                                       
         GOTO1 DISOPT,PARAS,FLDH                                                
*                                                                               
DISREP9  LA    R0,L'FLD                                                         
         LA    R3,L'PRTOPT                                                      
         GOTO1 CHOPPER,DMCB,((R0),FLD),((R3),AIO3),(C'P',4),0                   
         LA    R3,PRTOPT           R3=A(PRINT POSITION)                         
         L     R1,AIO3             R1=A(CHOPPED OUTPUT)                         
         ICM   RF,15,DMCB+8        RF=N'OUTPUT LINES                            
         BZ    DISREPX                                                          
*                                                                               
DISREP10 MVC   0(L'PRTOPT,R3),0(R1)                                             
         LA    R3,L'P(R3)                                                       
         LA    R1,L'P(R1)                                                       
         BCT   RF,DISREP10                                                      
*                                                                               
DISREPX  B     XIT                                                              
*                                                                               
MOVEOUT  MVC   0(0,RE),BUINPUT                                                  
         DROP  R2,R6                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EDIT FORMAT OPTIONS (KEY=PARAMETER VALUE,...)                  
*                                                                               
OPTED    NTR1                                                                   
         USING BUDTD,R6                                                         
         LA    R2,DTYOPTH          TEST FOR NO DATA IN FIELD                    
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    OPTEDX                                                           
         XC    FLAST,FLAST         START EDIT AT BEGINNING OF FIELD             
*                                                                               
OPTED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUALS        LOOK FOR EQUALS SIGN                         
         GOTO1 VFVAL                                                            
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0                                                         
         BNE   OPTED4                                                           
         CLI   FSTOP,X'FF'         TEST FOR EOF                                 
         BE    OPTEDX              YES                                          
         B     OPTEDR                                                           
*                                                                               
OPTED4   CLI   FLDH+5,L'OPTNAME    VALIDATE THE KEYWORD                         
         BH    OPTEDR                                                           
         LA    R0,OPTIONS                                                       
         LA    R3,OPTTAB                                                        
         USING OPTTABD,R3                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
*                                                                               
OPTED6   CLC   FLDH+5(1),OPTMINL   TEST FOR MINIMUM LENGTH FOR THIS KEY         
         BL    OPTED6A             NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),OPTNAME                                                   
         BE    OPTED7              VALID KEYWORD                                
OPTED6A  LA    R3,OPTTABL(R3)                                                   
         BCT   R0,OPTED6                                                        
         B     OPTEDR                                                           
*                                                                               
OPTED7   XC    FTERM,FTERM         EXTRACT PARAMETER VALUE                      
         MVI   FTERM,COMMA                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BE    OPTEDR              NO                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,OPTROUT        GET DISP TO VALIDATION ROUTINE               
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         B     OPTED2                                                           
*                                                                               
OPTEDX   B     XIT                                                              
         SPACE 2                                                                
OPTEDR   B     SPERR                                                            
         DROP  R3                                                               
         EJECT                                                                  
* FORMAT OPTIONS PARAMETER VALIDATION ROUTINES                                  
*                                                                               
OPTALIGN ST    RE,SAVERE                                                        
         CLI   FLDH+5,4                                                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,LEFTCOMP                                                      
         BNE   OPTEDR                                                           
         OI    BUDTFORM,BUDTLEFT   TURN ON LEFT ALIGNMENT                       
         B     OPTX                                                             
         SPACE 1                                                                
LEFTCOMP CLC   FLD(0),=C'LEFT'                                                  
         SPACE 2                                                                
OPTFLOAT ST    RE,SAVERE                                                        
         CLI   FLDH+5,1                                                         
         BNE   OPTEDR                                                           
         CLI   FLD,CURRENCY        TEST FOR CURRENCY SYMBOL                     
         BNE   OPTEDR                                                           
         OI    BUDTFORM,BUDTCURS                                                
         B     OPTX                                                             
         SPACE 2                                                                
OPTCOMMA ST    RE,SAVERE                                                        
         CLI   FLDH+5,3                                                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OI    BUDTFORM,BUDTCOMS                                                
         EX    R1,YESCOMP                                                       
         BE    OPTX                                                             
         NI    BUDTFORM,X'FF'-BUDTCOMS                                          
         EX    R1,NOCOMP                                                        
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
OPTMINUS ST    RE,SAVERE                                                        
         CLI   FLDH+5,3                                                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OI    BUDTFORM,BUDTNEGS                                                
         EX    R1,YESCOMP                                                       
         BE    OPTX                                                             
         NI    BUDTFORM,X'FF'-BUDTNEGS                                          
         EX    R1,NOCOMP                                                        
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
OPTZERO  ST    RE,SAVERE           ZERO=NOBLANK OPTION                          
         CLI   FLDH+5,3                                                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OI    BUDTFORM,BUDTZERO                                                
         EX    R1,YESCOMP                                                       
         BE    OPTX                                                             
         NI    BUDTFORM,X'FF'-BUDTZERO                                          
         EX    R1,NOCOMP                                                        
         BE    OPTX                                                             
         B     OPTEDR                                                           
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
         XC    DTVALS(DTVALLN),DTVALS CLEAR DATA TYPE VALUES                    
         MVC   DTCODE,BUDTYP       DATA TYPE CODE                               
         MVC   DTHEAD1(DTHEADL),SPACES CLEAR HEADINGS TO SPACES                 
         MVI   ELCODE,BUDHELQ      FIRST HEADING                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDHD,R6                                                         
         ZIC   R1,BUDHLEN                                                       
         SH    R1,=Y(BUDHEAD-BUDHD+1)                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTHEAD1(0),BUDHEAD                                               
         LA    R1,1(R1)                                                         
         STC   R1,DTLHEAD1         SAVE L'HEADING 1                             
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
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTHEAD2(0),BUDHEAD                                               
         LA    R1,1(R1)                                                         
         STC   R1,DTLHEAD2                                                      
*                                                                               
GETDT3   MVC   DTSTNAME,SPACES     CLEAR STACK FORMAT NAME                      
         MVI   ELCODE,BUSTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   GETDT4              NONE FOUND                                   
         USING BUSTD,R6                                                         
         ZIC   R1,BUSTLEN                                                       
         SH    R1,=Y(BUSTNAME-BUSTD+1)                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTSTNAME(0),BUSTNAME                                             
*                                                                               
GETDT4   MVI   ELCODE,BUDTELQ      GET DATA TYPE ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUDTD,R6                                                         
         CLI   BUDTEX,0            TEST FOR EXTRACT TYPE                        
         BE    GETDT6              NONE                                         
*                                                                               
         MVC   DTEX,BUDTEX                                                      
         LA    R0,EXTNTR                                                        
         LA    RE,EXTTAB                                                        
         USING EXTTABD,RE                                                       
         CLC   DTEX,EXTEQU         TEST FOR EXTRACT TYPE EQUATE                 
         BE    *+14                                                             
         LA    RE,EXTTABL(RE)                                                   
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   DTEXNAME,EXTNAME    GET EXTRACT TYPE NAME                        
*                                                                               
         DROP  RE                                                               
*                                                                               
GETDT6   MVC   DTCOL,BUDTCOL                                                    
         CLI   BUDTSC,0            TEST FOR ANY SCALE                           
         BE    GETDT8                                                           
         MVC   DTSC,BUDTSC                                                      
         MVC   DUB,SPACES                                                       
         MVI   DUB,LPAREN                                                       
         LA    RE,DUB+1                                                         
         ZIC   R0,DTSC                                                          
         MVI   0(RE),C'0'          ONE ZERO FOR EACH PLACE                      
         LA    RE,1(RE)                                                         
         BCT   R0,*-8                                                           
         MVI   0(RE),RPAREN                                                     
         MVC   DTSCEXP,DUB         MOVE TO OUTPUT SCALE AREA                    
*                                                                               
GETDT8   CLI   BUDTDEC,0           TEST FOR ANY DECIMAL PLACES                  
         BE    GETDT10                                                          
         MVC   DTDEC,BUDTDEC                                                    
         ZIC   R0,DTDEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DTDECEXP,DUB+7(1)                                                
*                                                                               
GETDT10  MVC   DTFORM,BUDTFORM     EXTRACT FORMAT OPTIONS                       
*                                                                               
GETDTX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY OUTPUT FORMAT OPTIONS                                  
*                                                                               
* AT ENTRY, P1=A(OUTPUT FIELD HEADER)                                           
*                                                                               
DISOPT   NTR1                                                                   
         L     R4,0(R1)            R4=A(OUTPUT FLD HEADER)                      
         L     R3,AIO3                                                          
         MVI   0(R3),C' '                                                       
         MVC   1(255,R3),0(R3)     CLEAR 256 BYTES                              
         LA    R0,OPTIONS          R0=OPTION COUNTER                            
         LA    RE,OPTTAB           RE=A(OPTION TABLE)                           
         USING OPTTABD,RE                                                       
         SR    R1,R1                                                            
         SR    R2,R2               R2=N'OPTIONS TO OUTPUT                       
*                                                                               
DISOPT2  IC    R1,OPTEQU           GET EQUATED BIT SETTING                      
         EX    R1,TESTOPT                                                       
         BZ    DISOPT4             OPTION NOT SET                               
*                                                                               
         MVC   0(L'OPTNAME,R3),OPTNAME 1ST HALF IS OPTION NAME                  
         MVC   10(L'OPTVAL,R3),OPTVAL  2ND HALF IS OPTION VALUE                 
         LA    R2,1(R2)            INCREMENT OPTION COUNT                       
         LA    R3,20(R3)           NEXT UNSCAN BOLCK                            
*                                                                               
DISOPT4  LA    RE,OPTTABL(RE)      NEXT OPTION ENTRY                            
         BCT   R0,DISOPT2                                                       
*                                                                               
         GOTO1 UNSCAN,DMCB,((R2),AIO3),(R4),0                                   
*                                                                               
DISOPTX  B     XIT                                                              
*                                                                               
TESTOPT  TM    DTFORM,0                                                         
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHECK SCREEN INPUT IN FORMULA FIELDS                           
*                                                                               
SCRIN    NTR1                                                                   
         LA    R3,1                R3=FIELD SEQUENCE NUMBER                     
         XC    FTERM,FTERM                                                      
         MVI   EMPTYSW,NO          SET EMPTY LINE SWITCH TO NO                  
         MVI   INPUTSW,NO          SET INPUT SWITCH TO NO                       
         LA    R0,2                R0=N'SCREEN FIELDS                           
         LA    R2,DTYFO1H          R2=A(SCREEN FIELD HEADER)                    
*                                                                               
SCRIN2   ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR INPUT IN FIELD                      
         BE    SCRIN4              NO                                           
         MVI   ERROR,INVALID                                                    
         CLI   EMPTYSW,YES         TEST IF BLANK FIELD PRECEDES IT              
         BE    TRAPERR             YES                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING BUIND,R6                                                         
         MVI   BUINEL,BUINELQ                                                   
         STC   R3,BUINSEQ                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,MOVEIN                                                        
         LA    R1,BUINPUT-BUIND+1(R1)                                           
         STC   R1,BUINLEN          SET ELEMENT LENGTH                           
         MVI   INPUTSW,YES         FOUND SCREEN INPUT                           
         GOTO1 ADDELEM                                                          
         B     SCRIN5                                                           
*                                                                               
SCRIN4   MVI   EMPTYSW,YES         FOUND BLANK LINE                             
*                                                                               
SCRIN5   ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO NEXT FIELD HEADER                   
         LA    R3,1(R3)                                                         
         BCT   R0,SCRIN2                                                        
*                                                                               
         CLI   INPUTSW,YES         SET CC ON EXIT                               
         B     XIT                                                              
         SPACE 2                                                                
MOVEIN   MVC   BUINPUT(0),FLD                                                   
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY SCREEN INPUT ELEMENT DATA                              
*        P1 = A(FIRST FIELD HEADER)                                             
*        P2 = N'FIELDS                                                          
*                                                                               
DISSCR   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     R6,AIO                                                           
         AH    R6,DATADISP         R6=ELEMENT POINTER                           
         SR    R0,R0                                                            
*                                                                               
DISSCR2  CLI   0(R6),0             TEST FOR EOR                                 
         BE    DISSCRX                                                          
         CLI   0(R6),BUINELQ       TEST FOR SCREEN INPUT ELEMENT                
         BE    DISSCR4                                                          
*                                                                               
DISSCR3  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DISSCR2                                                          
*                                                                               
         USING BUIND,R6                                                         
DISSCR4  ZIC   R1,BUINLEN                                                       
         SH    R1,=Y(BUINPUT-BUIND+1)                                           
         EX    R1,MOVESCR                                                       
         ZIC   RE,0(R2)            GET FIELD LENGTH                             
         AR    R2,RE               POINT TO NEXT FIELD                          
         BCT   R3,DISSCR3                                                       
*                                                                               
DISSCRX  B     XIT                                                              
         SPACE 2                                                                
MOVESCR  MVC   8(0,R2),BUINPUT                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FORMULA & BUILD POLISH STRING                        *         
***********************************************************************         
         SPACE 1                                                                
VALFORM  NTR1  WORK=(R8,VALWORKX-VALWORKD)                                      
         USING VALWORKD,R8         R8=A(LOCAL W/S)                              
         MVI   ERROR,0                                                          
         ST    R1,VALPARMA                                                      
         MVC   VALPARMS(VALPARML),0(R1)                                         
         MVI   VALOPNUM,0                                                       
         MVI   VALOPNDN,0                                                       
         MVI   VALPAREN,0                                                       
         L     R3,VALPAFST                                                      
         ST    R3,FADDR            SET FIELD ADDRESS                            
         ZIC   R4,VALPNUMF                                                      
*                                                                               
VALF2    LA    RE,VALDELIM         BUILD TRANSLATE TABLE                        
         XC    VALTRTOP,VALTRTOP                                                
VALF4    SR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    VALF10                                                           
         LA    RF,VALTRTOP(RF)                                                  
         MVC   0(1,RF),1(RE)                                                    
         LA    RE,L'VALDELIM(RE)                                                
         B     VALF4                                                            
*                                                                               
VALF10   LA    R7,VALINTER         R7=A(INTERMEDIATE STRING)                    
         MVI   VALNEXTS,VALOPND+VALOSTR+VALPARO                                 
*                                                                               
VALF12   CLI   0(R3),0             TEST END OF TWA                              
         BE    VALF24                                                           
         TM    1(R3),X'20'         TEST PROTECTED FIELD                         
         BNZ   VALF22                                                           
         SR    R5,R5                                                            
         ICM   R5,1,5(R3)          R5=INPUT LENGTH OF THIS FIELD                
         BZ    VALF20                                                           
         ST    R3,FADDR            SET FIELD ADDRESS                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   VALINPUT(0),8(R3)   EXTRACT INPUT FIELD INTO W/S                 
         LA    R6,VALINPUT(R5)                                                  
         MVI   0(R6),X'FF'         SET END OF LINE INDICATOR                    
         LA    R6,VALINPUT         R6=A(EXTRACTED INPUT FIELD)                  
*                                                                               
VALF14   STCM  R6,7,FLAST          SAVE CURRENT INPUT POINTER                   
         EX    R5,*+8              FIND NEXT DELIMITER                          
         B     *+10                                                             
         TRT   0(0,R6),VALTRTOP                                                 
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NO DELIMITERS FOUND                   
         STC   R2,VALOP            SET DELIMITER INDEX VALUE                    
         CR    R6,R1               TEST FIRST CHARACTER IS DELIMITER            
         BE    VALF16              YES - MUST BE AN OPERATION                   
         TM    VALNEXTS,VALOPND    TEST OPERAND EXPECTED                        
         BZ    VALFE1              NO - ERROR                                   
*                                                                               
VALF15   SR    R1,R6               R1=OPERAND LENGTH                            
         BAS   RE,VALDTA           VALIDATE DATA EXPRESSION                     
         BNE   VALFE2              ERROR IF BAD DATA VALUE                      
         SR    R5,R1               DECREMENT STRING LENGTH                      
         AR    R6,R1               POINT TO OPERATION                           
         LR    R1,R6                                                            
         MVC   0(1,R7),VALOPNUM    MOVE OPERAND TO OUTPUT STRING                
         LA    R7,1(R7)                                                         
         MVI   VALNEXTS,VALOPTN                                                 
         CLI   VALPAREN,0                                                       
         BE    *+8                                                              
         OI    VALNEXTS,VALPARC                                                 
         CLC   0(1,R1),VALDOPND    TEST END OF SPECIAL OPERAND                  
         BE    VALFOP4             YES - IGNORE                                 
*                                                                               
VALF16   ZIC   RF,VALOP            RF=OPERATION INDEX VALUE                     
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALFOP              01=+                                         
         B     VALFOP              02=-                                         
         B     VALFOP              03=*                                         
         B     VALFOP              04=/                                         
         B     VALFOPP             05=(                                         
         B     VALFCLP             06=)                                         
         B     VALFLIT             07='   (START OF SPECIAL OPERAND)            
         B     *                   08=N/D                                       
         B     *                   09=N/D                                       
         B     VALF20              10=FF  (END OF INPUT STRING)                 
*                                                                               
VALFOP   TM    VALNEXTS,VALOPTN    TEST OPERATION EXPECTED                      
         BZ    VALFE4                                                           
         MVI   VALNEXTS,VALOPND+VALOSTR+VALPARO                                 
         B     VALFOP2                                                          
*                                                                               
VALFOPP  TM    VALNEXTS,VALPARO    TEST OPEN PARENS EXPECTED                    
         BZ    VALFE5                                                           
         IC    R0,VALPAREN         INCREMENT PAREN LEVEL                        
         AH    R0,=H'1'                                                         
         STC   R0,VALPAREN                                                      
         MVI   VALNEXTS,VALOPND+VALOSTR+VALPARO                                 
         B     VALFOP2                                                          
*                                                                               
VALFCLP  TM    VALNEXTS,VALPARC    TEST CLOSE PARENS EXPECTED                   
         BZ    VALFE6                                                           
         MVI   VALNEXTS,VALOPTN                                                 
         IC    R0,VALPAREN         DECREMENT PAREN LEVEL                        
         SH    R0,=H'1'                                                         
         STC   R0,VALPAREN                                                      
         BZ    *+8                                                              
         OI    VALNEXTS,VALPARC                                                 
         B     VALFOP2                                                          
*                                                                               
VALFLIT  TM    VALNEXTS,VALOSTR    TEST SPECIAL LITERAL EXPECTED                
         BZ    VALFE7                                                           
         LA    R1,1(R1)            LOCATE END OF SPECIAL LITERAL                
VALFLIT2 CLC   0(1,R1),VALDEOIS    TEST END OF INPUT STRING                     
         BE    VALFE8                                                           
         CLC   0(1,R1),VALDOPND    TEST END OF SPECIAL OPERAND                  
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     VALFLIT2                                                         
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
         B     VALF15              PROCESS OPERAND                              
*                                                                               
VALFOP2  MVC   0(1,R7),0(R1)       MOVE OPERATION TO OUTPUT STRING              
         LA    R7,1(R7)                                                         
VALFOP4  LA    R6,1(R6)            BUMP INPUT POINTER                           
         BCTR  R5,0                DECREMENT STRING LENGTH                      
         B     VALF14                                                           
*                                                                               
VALF20   BCT   R4,VALF22           DECREMENT INPUT FIELD COUNT                  
         B     VALF24                                                           
VALF22   ZIC   RE,0(R3)            BUMP TO NEXT TWA FIELD                       
         AR    R3,RE                                                            
         B     VALF12                                                           
*                                                                               
VALF24   ICM   R6,7,FLAST                                                       
         TM    VALNEXTS,VALOPND                                                 
         BNZ   VALFE9                                                           
         CLI   VALPAREN,0                                                       
         BNE   VALFE3                                                           
         MVI   0(R7),C'='          SET END-OF-FORMULA                           
         MVI   1(R7),X'FF'                                                      
*                                                                               
         XC    VALOPSAV,VALOPSAV   CONVERT STRING TO POLISH FORMAT              
         MVI   VALOP,0                                                          
         LA    R2,VALINTER         R2=A(INTERMEDIATE STRING)                    
         L     R3,VALPAOUT         R3=A(OUTPUT STRING)                          
         USING BUPOLD,R3                                                        
         MVI   BUPOLEL,BUPOLELQ                                                 
         LA    R3,BUPOLISH                                                      
         SR    R1,R1               R1=CURRENT PAREN LEVEL                       
*                                                                               
VALF26   CLI   0(R2),X'FF'         TEST END-OF-STRING                           
         BE    VALF60                                                           
         CLI   0(R2),C'='          TEST END-OF-FORMULA                          
         BE    VALF34                                                           
*                                                                               
         CLI   0(R2),X'40'         DEAL WITH OPERANDS                           
         BH    VALF28                                                           
         CLI   VALOP,0             TEST IF OPERATION SAVED                      
         BE    VALF36              NO - OUTPUT OPERAND                          
         MVC   0(1,R3),0(R2)       YES - OUTPUT OPERAND/OPERATION               
         LA    R3,1(R3)                                                         
         MVC   0(1,R3),VALOP                                                    
         MVI   VALOP,0                                                          
         B     VALF38                                                           
*                                                                               
VALF28   CLC   0(1,R2),VALDPARO    DEAL WITH OPEN PAREN                         
         BNE   VALF30                                                           
         LA    RE,VALOPSAV(R1)     RE=A(SAVE OPERATION STACK ENTRY)             
         LA    R1,1(R1)            BUMP PAREN LEVEL                             
         CLI   VALOP,0             TEST IF OPERATION SAVED                      
         BE    *+10                                                             
         MVC   0(1,RE),VALOP       YES - SAVE IN STACK                          
         MVI   VALOP,0                                                          
         B     VALF40                                                           
*                                                                               
VALF30   CLC   0(1,R2),VALDPARC    DEAL WITH CLOSE PAREN                        
         BNE   VALF32                                                           
         BCTR  R1,0                DECREMENT PAREN LEVEL                        
         LA    RE,VALOPSAV(R1)     RE=A(SAVED OPERATION STACK ENTRY)            
         CLI   0(RE),0             TEST IF OPERATION SAVED                      
         BE    VALF40                                                           
         MVC   0(1,R3),0(RE)       YES - OUTPUT SAVED OPERATION                 
         MVI   0(RE),0                                                          
         B     VALF38                                                           
*                                                                               
VALF32   CLI   VALOP,0             TEST IF OPERATION SAVED                      
         BE    *+14                                                             
         MVC   0(1,R3),VALOP       YES - OUTPUT SAVED OPERATION                 
         LA    R3,1(R3)                                                         
         MVC   VALOP,0(R2)         SAVE OPERATION IN TEMPORARY SAVE             
         B     VALF40                                                           
*                                                                               
VALF34   CLI   VALOP,0             TEST IF OPERATION SAVED                      
         BE    *+14                                                             
         MVC   0(1,R3),VALOP       YES - OUTPUT SAVED OPERATION                 
         LA    R3,1(R3)                                                         
*                                                                               
VALF36   MVC   0(1,R3),0(R2)       MOVE TO OUTPUT STRING & BUMP PTRS            
VALF38   LA    R3,1(R3)                                                         
VALF40   LA    R2,1(R2)                                                         
         B     VALF26                                                           
*                                                                               
VALF60   LR    R0,R3               SET LENGTH OF POLISH ELEMENT                 
         L     R3,VALPAOUT                                                      
         LA    R3,0(R3)                                                         
         SR    R0,R3                                                            
         STC   R0,BUPOLEN                                                       
         MVI   ERROR,0             SET NO ERROR                                 
*                                                                               
VALFX    CLI   ERROR,0             SET CC=EQ IF OK                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LITERAL OR DATA VALUE                           *         
*                                                                     *         
* NTRY - R6=A(LITERAL OR DATA VALUE)                                  *         
*        R1=L'VALUE                                                   *         
*                                                                     *         
* EXIT - VALOPNUM SET TO OPERAND NUMBER                               *         
***********************************************************************         
         SPACE 1                                                                
VALDTA   NTR1  ,                                                                
         MVI   ERROR,0                                                          
         XC    VALWORK,VALWORK                                                  
         LA    R2,VALWORK                                                       
         USING BUDOPCTL,R2                                                      
*                                                                               
         LR    R3,R1               CHECK FOR LITERAL VALUE                      
         GOTO1 VBURNEM,DMCB,((R3),(R6)),VALDUB                                  
         CLI   0(R1),0             TEST VALID                                   
         BNE   VALDTA2                                                          
         MVC   BUDOPCTL,VALDUB     YES - SET VALUE                              
         CLI   BUDOPCTL,X'85'      TEST FOR MORE THAN 5 DECIMALS                
         BH    VALDTAN                                                          
         OI    BUDOPCTL,BUDOPCNS                                                
         ZAP   BUDOPCON,VALDUB+1(7)                                             
         B     VALDTAY                                                          
*                                                                               
VALDTA2  MVI   BUDOPCTL,BUDOPDTA   SET DATA TYPE OPERAND                        
         XC    ELEM,ELEM                                                        
         LA    R0,8(R3)                                                         
         STC   R0,ELEM                                                          
         STC   R3,ELEM+5                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),0(R6)                                                  
         GOTO1 SCANNER,DMCB,ELEM,(5,SCANBLK),C'6B7E6BFF'                        
         CLI   4(R1),0                                                          
         BE    VALDTAN                                                          
         CLI   4(R1),4                                                          
         BH    VALDTAN                                                          
         LA    R5,SCANBLK                                                       
         MVC   SCANNUM,4(R1)                                                    
         MVI   FNDX,1                                                           
*                                                                               
         LA    R4,KEY              VALIDATE DATA TYPE                           
         USING BURECD,R4                                                        
         MVC   BUKEY,SVPLNKEY                                                   
         MVI   BUDSUB,BUDSUBQ                                                   
         MVC   BUDTYP,12(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE       TEST DATA TYPE RECORD FOUND                  
         BNE   VALDTAN                                                          
         MVC   BUDOPTYP,BUDTYP                                                  
         BAS   RE,BUMPSCAN                                                      
*                                                                               
         MVC   WORK(4),PLANST      VALIDATE PERIOD                              
         SR    R0,R0                                                            
         ICM   R0,8,PLANST+1                                                    
         ICM   R0,4,CLTTYPE                                                     
         ICM   R0,1,YESPARM        ALLOW CROSS-FISCAL YEARS                     
         GOTO1 VMONVAL,DMCB,12(R5),WORK,(R0)                                    
         OC    4(4,R1),4(R1)       TEST FOR ERRORS                              
         BNZ   *+16                                                             
         CLI   SCANNUM,3                                                        
         BE    VALDTAN                                                          
         B     VALDTA4                                                          
         MVC   BUDOPST(4),4(R1)                                                 
         BAS   RE,BUMPSCAN                                                      
*                                                                               
VALDTA4  DS    0H                  VALIDATE ASAT DATE                           
         GOTO1 DATVAL,DMCB,(0,12(R5)),WORK                                      
         OC    0(4,R1),0(R1)       TEST FOR ERRORS                              
         BNZ   *+16                                                             
         CLI   SCANNUM,2                                                        
         BE    VALDTAN                                                          
         B     VALDTA6                                                          
         CLC   0(1,R5),3(R1)       CHECK VALIDATION LENGTH VS INPUT             
         BNE   VALDTAN                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,BUDOPAS)  EBCDIC TO BINARY               
         BAS   RE,BUMPSCAN                                                      
*                                                                               
VALDTA6  DS    0H                  VALIDATE RELATIVE LEVEL NUMBER               
         GOTO1 VBURNEM,DMCB,(0(R5),12(R5)),VALDUB                               
         CLI   0(R1),0             TEST FOR ERRORS                              
         BNE   VALDTAN                                                          
         CLI   VALDUB,0            TEST INTEGER SCALE                           
         BNE   VALDTAN                                                          
         CP    VALDUB,=P'6'        TEST +6 THRU -6                              
         BH    VALDTAN                                                          
         CP    VALDUB,=P'-6'                                                    
         BL    VALDTAN                                                          
         CVB   R0,VALDUB                                                        
         STC   R0,BUDOPLEV         SET RELATIVE LEVEL NUMBER                    
*                                                                               
VALDTAY  MVI   FNDX,0                                                           
         LA    RE,VALOPNDS         TEST OPERAND ALREADY DEFINED                 
         SR    R1,R1                                                            
         ICM   R1,1,VALOPNDN       R1=N'OPERANDS SO FAR                         
         BZ    VALDTAY4                                                         
VALDTAY2 CLC   1(L'VALOPNDS-1,RE),BUDOPCTL                                      
         BNE   *+14                                                             
         MVC   VALOPNUM,0(RE)                                                   
         B     VALDTAX                                                          
         LA    RE,L'VALOPNDS(RE)                                                
         BCT   R1,VALDTAY2                                                      
*                                                                               
VALDTAY4 IC    R1,VALOPNDN         CREATE NEW OPERAND ENTRY                     
         LA    R1,1(R1)                                                         
         STC   R1,VALOPNDN                                                      
         STC   R1,0(RE)                                                         
         STC   R1,VALOPNUM                                                      
         MVC   1(L'VALOPNDS-1,RE),BUDOPCTL                                      
*                                                                               
         LA    R2,ELEM                                                          
         USING BUDOPD,R2           BUILD & ADD OPERAND ELEMENT                  
         MVI   BUDOPEL,BUDOPELQ                                                 
         MVC   BUDOPN,0(RE)                                                     
         MVC   BUDOPCTL,1(RE)                                                   
         TM    BUDOPCTL,BUDOPCNS   TEST CONSTANT                                
         BZ    VALDTAY6            NO                                           
         MVC   BUDOPCON,2(RE)                                                   
         MVI   BUDOPLEN,BUDOPL1Q                                                
         B     VALDTAY8                                                         
*                                                                               
VALDTAY6 MVC   BUDOPTYP(L'VALOPNDS-2),2(RE)                                     
         MVI   BUDOPLEN,BUDOPL2Q                                                
*                                                                               
VALDTAY8 GOTO1 ADDELEM             ADD OPERAND ELEMENT TO RECORD                
         B     VALDTAX                                                          
*                                                                               
VALDTAN  MVI   ERROR,INVALID       SET INVALID OPERAND                          
         B     VALDTAX                                                          
*                                                                               
VALDTAX  B     VALFX                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
* ROUTINE TO BUMP TO NEXT SCANNER BLOCK ENTRY                                   
*                                                                               
BUMPSCAN ZIC   R1,SCANNUM                                                       
         SH    R1,=H'1'                                                         
         BZ    VALDTAY                                                          
         STC   R1,SCANNUM                                                       
         IC    R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,L'SCANBLK(R5)                                                 
         BR    RE                                                               
         EJECT                                                                  
* FORMULA VALIDATION ERRORS                                                     
*                                                                               
VALFE1   LA    R1,VALERR1          OPERAND NOT EXPECTED                         
         LA    RE,L'VALERR1                                                     
         B     VALFEX                                                           
VALFE2   LA    R1,VALERR2          INVALID DATA EXPRESSION                      
         LA    RE,L'VALERR2                                                     
         B     VALFEX                                                           
VALFE3   LA    R1,VALERR3          CLOSE PARENTHESIS MISSING                    
         LA    RE,L'VALERR3                                                     
         B     VALFEX                                                           
VALFE4   LA    R1,VALERR4          OPERATION NOT EXPECTED                       
         LA    RE,L'VALERR4                                                     
         B     VALFEX                                                           
VALFE5   LA    R1,VALERR5          OPEN PARENS NOT EXPECTED                     
         LA    RE,L'VALERR5                                                     
         B     VALFEX                                                           
VALFE6   LA    R1,VALERR6          CLOSE PARENS NOT EXPECTED                    
         LA    RE,L'VALERR6                                                     
         B     VALFEX                                                           
VALFE7   LA    R1,VALERR7          SPECIAL LITERAL NOT EXPECTED                 
         LA    RE,L'VALERR7                                                     
         B     VALFEX                                                           
VALFE8   LA    R1,VALERR8          END OF SPECIAL LITERAL EXPECTED              
         LA    RE,L'VALERR8                                                     
         B     VALFEX                                                           
VALFE9   LA    R1,VALERR9          OPERAND EXPECTED                             
         LA    RE,L'VALERR9                                                     
         B     VALFEX                                                           
         SPACE 1                                                                
VALFEX   MVI   WORK,C' '           BUILD MESSAGE IN WORK                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(9),=C'**ERROR**'                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+10(0),0(R1)                                                 
         MVI   ERROR,SUPPLIED                                                   
         LA    R0,VALINPUT         SET CURSOR POSITION                          
         SR    R6,R0                                                            
         A     R6,FADDR                                                         
         LA    R6,8(R6)                                                         
         STCM  R6,7,FLAST                                                       
         B     SPERR                                                            
         DROP  R8                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         OI    6(R2),X'80'         SLAVED MODE EXIT                             
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 SAVEUWKA                                                         
         L     RD,AWORK                                                         
         B     XIT                                                              
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
MISSPARM DC    C'M'                                                             
YESPARM  DC    C'Y'                                                             
         SPACE 2                                                                
* TABLE OF EXTRACT TYPES (COVERED BY EXTTABD)                                   
*                                                                               
* BYTES 0-7 = EXTRACT TYPE NAME                                                 
* BYTE  8   = MINIMUM LENGTH                                                    
* BYTE  9   = EXTRACT TYPE NUMBER                                               
*                                                                               
EXTTAB   DS    0CL(EXTTABL)                                                     
       ++INCLUDE BUFIL06TAB                                                     
EXTNTR   EQU   (*-EXTTAB)/L'EXTTAB                                              
         SPACE 2                                                                
* TABLE OF FORMAT OPTIONS (COVERED BY OPTTABD)                                  
*                                                                               
OPTTAB   DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'ALIGN',AL1(1),AL1(BUDTLEFT),AL2(OPTALIGN-T50206)             
         DC    CL8'LEFT'                                                        
*                                                                               
         DC    CL8'FLOAT',AL1(1),AL1(BUDTCURS),AL2(OPTFLOAT-T50206)             
         DC    AL1(CURRENCY),CL7' '                                             
*                                                                               
         DC    CL8'COMMAS',AL1(1),AL1(BUDTCOMS),AL2(OPTCOMMA-T50206)            
         DC    CL8'YES'                                                         
*                                                                               
         DC    CL8'MINUS',AL1(1),AL1(BUDTNEGS),AL2(OPTMINUS-T50206)             
         DC    CL8'YES'                                                         
*                                                                               
         DC    CL8'ZERO',AL1(1),AL1(BUDTZERO),AL2(OPTZERO-T50206)               
         DC    CL8'YES'                                                         
*                                                                               
OPTIONS  EQU   (*-OPTTAB)/L'OPTTAB                                              
         SPACE 2                                                                
VALDELIM DS    0XL2                                                             
         DC    X'4E',AL1(01)       '+'   -  ADD                                 
         DC    X'60',AL1(02)       '-'   -  SUBTRACT                            
         DC    X'5C',AL1(03)       '*'   -  MULTIPLY                            
         DC    X'61',AL1(04)       '/'   -  DIVIDE                              
VALDPARO DC    X'4D',AL1(05)       '('   -  OPEN PARENS                         
VALDPARC DC    X'5D',AL1(06)       ')'   -  CLOSE PARENS                        
VALDOPND DC    X'7D',AL1(07)       START/END OF SPECIAL OPERAND                 
VALDEOIS DC    X'FF',AL1(10)       END OF TWA INPUT FIELD                       
         DC    AL1(0)                                                           
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
VALERR1  DC    C'OPERAND NOT EXPECTED'                                          
VALERR2  DC    C'INVALID DATA EXPRESSION'                                       
VALERR3  DC    C'CLOSE PARENTHESIS MISSING'                                     
VALERR4  DC    C'OPERATION NOT EXPECTED'                                        
VALERR5  DC    C'OPEN PARENTHESIS NOT EXPECTED'                                 
VALERR6  DC    C'CLOSE PARENTHESIS NOT EXPECTED'                                
VALERR7  DC    C'SPECIAL OPERAND NOT EXPECTED'                                  
VALERR8  DC    C'SPECIAL OPERAND END MISSING'                                   
VALERR9  DC    C'OPERAND MISSING'                                               
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,47,C'DATA TYPE REPORT'                                        
         SSPEC H2,47,C'----------------'                                        
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'PLAN'                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
         SSPEC H9,2,C'CODES'                                                    
         SSPEC H9,13,C'DESCRIPTION'                                             
         SSPEC H9,37,C'EXTRACT'                                                 
         SSPEC H10,38,C'TYPE'                                                   
         SSPEC H9,47,C'WIDTH/SCALE/DECIMALS'                                    
         SSPEC H10,47,C'FORMAT OPTIONS'                                         
         SSPEC H9,68,C'FORMULA'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF6D                                                       
         EJECT                                                                  
* DSECT TO COVER LIST SCREEN                                                    
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILE6D                                                       
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
DATATYPE DS    CL(L'BUDTYP)        DATA TYPE CODE                               
EXTRTYPE DS    CL(L'BUDTYP)        EXTRACT DATA TYPE CODE                       
LENDT    DS    X                   L'DATA TYPE CODE                             
*                                                                               
EMPTYSW  DS    C                                                                
INPUTSW  DS    C                                                                
EXSW     DS    C                   Y=EXTRACT TYPE PRESENT                       
*                                                                               
DTVALS   DS    0C                  DATA TYPE RECORD VALUES                      
DTCODE   DS    CL(L'BUDTYP)                                                     
DTLHEAD1 DS    X                   L'HEADING 1                                  
DTLHEAD2 DS    X                   L'HEADING 2                                  
DTHEAD1  DS    CL(L'DTYHD1)        HEADING 1 (SPACE PADDED)                     
DTSEP    DS    C                   SEPARATOR BYTE                               
DTHEAD2  DS    CL(L'DTYHD2)        HEADING 2 (SPACE PADDED)                     
DTHEADL  EQU   *-DTHEAD1           L'HEADING FIELDS                             
DTSTNAME DS    CL(L'DTYSTAK)       STACK FORMAT NAME                            
DTEX     DS    X                   EXTRACT TYPE                                 
DTEXNAME DS    CL(L'EXTNAME)       EXTRACT TYPE NAME                            
DTCOL    DS    X                   COLUMN WIDTH                                 
DTSC     DS    X                   SCALE                                        
DTSCEXP  DS    CL(L'DTYSCA)        OUTPUT SCALE EXPRESSION                      
DTDEC    DS    X                   N'DECIMAL PLACES                             
DTDECEXP DS    C                   OUTPUT DECIMAL PLACES                        
DTFORM   DS    XL(L'BUDTFORM)      FORMAT OPTION BIT SETTINGS                   
DTVALLN  EQU   *-DTVALS            L'DATA TYPE RECORD VALUES                    
*                                                                               
SCANNUM  DS    X                   N'SCAN BLOCK ENTRIES                         
SCANBLK  DS    5XL32               FORMULA DATA VALIDATION BLOCK                
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
*                                                                               
         ORG   TWA1USER            SAVE AREA                                    
SVCLTVAL DS    CL(L'CLTVALS)       CLIENT VALUES SAVE AREA                      
SVPRDVAL DS    CL(L'PRDVALS)       PRODUCT VALUES SAVE AREA                     
SVPLNVAL DS    CL(L'PLANVALS)      PLAN VALUES SAVE AREA                        
SVPLNKEY DS    CL(L'BUKEY)         PLAN KEY                                     
SVGENKEY DS    XL(L'KEY)           SAVED GENCON KEY W DISK ADDRESS              
SVDMWORK DS    12D                 ORIGINAL DMWORK (FOR PUT)                    
         SPACE 2                                                                
* DSECT TO COVER LIST DISPLAY LINE                                              
*                                                                               
LSTLIND  DSECT                                                                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSTDTYP  DS    CL8                 DATA TYPE CODE                               
         DS    CL3                                                              
LSTNAME  DS    CL28                HEADINGS                                     
         DS    CL2                                                              
LSTSCA   DS    CL8                 SCALE                                        
         DS    CL2                                                              
LSTPOINT DS    C                   DECIMAL POINT                                
LSTDEC   DS    C                   DECIMAL PLACES                               
         SPACE 2                                                                
* DSECT TO COVER PRINT OUTPUT LINE                                              
*                                                                               
PRTD     DSECT                                                                  
         DS    C                                                                
PRTCODE  DS    CL8                 DATA TYPE CODE                               
         DS    CL3                                                              
PRTHED1  DS    CL20                HEADING 1                                    
         DS    CL4                                                              
PRTEXT   DS    CL8                 EXTRACT TYPE                                 
         DS    CL2                                                              
PRTCOL   DS    CL2                 COLUMN WIDTH                                 
         DS    CL2                                                              
PRTSCA   DS    CL7                 SCALE                                        
         DS    CL3                                                              
PRTPOINT DS    C                   DECIMAL POINT                                
PRTDEC   DS    CL1                 N'DECIMAL PLACES                             
         DS    CL4                                                              
         DS    C                                                                
PRTFORM1 DS    CL56                FORMULA                                      
         ORG   PRTHED1+132                                                      
PRTHED2  DS    CL20                HEADING 2                                    
         ORG   PRTCOL+132                                                       
PRTOPT   DS    CL20                FORMAT OPTIONS                               
         ORG   PRTFORM1+132                                                     
PRTFORM2 DS    CL56                FORMULA-LINE 2                               
         ORG   PRTHED1+264                                                      
PRTSTAK  DS    CL26                STACK FORMAT NAME                            
         SPACE 2                                                                
* DSECT TO COVER EXTRACT TYPE TABLE                                             
*                                                                               
EXTTABD  DSECT                                                                  
EXTNAME  DS    CL8                 EXTRACT TYPE NAME                            
EXTMINL  DS    X                   MINIMUM LENGTH FOR NAME COMPARE              
EXTEQU   DS    X                   EXTRACT TYPE EQUATED VALUE                   
EXTTABL  EQU   *-EXTTABD           TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* DSECT TO COVER FORMAT OPTIONS TABLE                                           
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                                                              
OPTMINL  DS    X                                                                
OPTEQU   DS    X                   OPTION BIT SETTING EQUATE                    
OPTROUT  DS    AL2                 DISPLACEMENT TO PARAMETER ROUTINE            
OPTVAL   DS    CL8                 PARAMETER VALUE                              
OPTTABL  EQU   *-OPTTABD                                                        
         SPACE 2                                                                
* VALFORM S/R LOCAL W/S                                                         
*                                                                               
VALWORKD DSECT                                                                  
VALPARMA DS    A                   A(INPUT PARAMETER LIST)                      
*                                                                               
VALPARMS DS    0F                  ** INPUT PARAMETER LIST **                   
VALPNUMF DS    0X                  N'TWA INPUT FIELDS                           
VALPAFST DS    A                   A(FIRST TWA INPUT FIELD)                     
VALPAOUT DS    A                   A(OUTPUT STRING)                             
VALPARML EQU   *-VALPARMS                                                       
*                                                                               
VALPAREN DS    X                   PARENTHESIS LEVEL                            
VALDUB   DS    D                                                                
VALNEXTS DS    X                   VALID NEXT TIME BITS                         
VALOPND  EQU   X'80'                                                            
VALOSTR  EQU   X'40'                                                            
VALOPTN  EQU   X'20'                                                            
VALPARO  EQU   X'10'                                                            
VALPARC  EQU   X'08'                                                            
VALOPNUM DS    X                   OPERAND NUMBER                               
VALOP    DS    X                   OPERATION CODE                               
VALOPSAV DS    CL32                OPERATION SAVE STACK                         
VALWORK  DS    XL64                                                             
VALINPUT DS    XL81                INPUT STRING                                 
VALINTER DS    CL256               INTERMEDIATE STRING                          
VALTRTOP DS    XL256               TRANSLATE TABLE                              
VALOPNDN DS    X                   N'OPERANDS IN STACK                          
VALOPNDS DS    32XL18              OPERAND STACK                                
VALWORKX EQU   *                                                                
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
LPAREN   EQU   C'('                                                             
RPAREN   EQU   C')'                                                             
COMMA    EQU   C','                                                             
COLLEN   EQU   10                  DEFAULT COLUMN LENGTH                        
MAXCOL   EQU   132                 MAXIMUM COLUMN WIDTH                         
MAXSCALE EQU   5                   MAXIMUM SCALE                                
MAXDEC   EQU   5                                                                
CURRENCY EQU   X'5B'               DOLLAR OR POUND SIGN                         
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019BUFIL06   05/01/02'                                      
         END                                                                    
