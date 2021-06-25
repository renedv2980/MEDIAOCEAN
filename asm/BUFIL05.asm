*          DATA SET BUFIL05    AT LEVEL 027 AS OF 05/01/02                      
*PHASE T50205A                                                                  
*INCLUDE BUEDRULE                                                               
*INCLUDE PUBVAL                                                                 
*====================================================================           
* 7/20/01 DELETED LMBUFIL05, RMBUFIL05, AND MADE THIS A LINK BOOK               
* MHER                                                                          
*====================================================================           
         TITLE 'T50205 - BUDGET CONTROL LFM - OUTLINE ADD/DIS/CHA'              
T50205   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI05**,RA,RR=R2                                              
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
         L     RE,=V(EDRULES)                                                   
         AR    RE,R2                                                            
         ST    RE,VEDRULES                                                      
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
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     GOTO1 VUPKEY                                                           
         LA    R2,OUTCLTH          VALIDATE CLIENT                              
         GOTO1 VVALCLT,PARAS,(R2),0                                             
         MVC   SVCLTVAL,CLTVALS    SAVE CLIENT RECORD VALUES                    
         XC    OUTCLN,OUTCLN       CLEAR CLIENT NAME                            
         MVC   OUTCLN(L'CLTNAM),CLTNAM                                          
         OI    OUTCLNH+6,X'80'     XMIT                                         
*                                                                               
* EDIT AND VALIDATE PRODUCT                                                     
*                                                                               
VKEY2    LA    R2,OUTPRDH          VALIDATE PRODUCT                             
         SR    R0,R0                                                            
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+8                                                              
         ICM   R0,8,MISSPARM                                                    
         GOTO1 VVALPRD,PARAS,(R2),(R0)                                          
*                                                                               
         OC    PRDCODE,PRDCODE     TEST FOR 'ALL' PRODUCTS                      
         BNZ   VKEY3                                                            
         GOTO1 VGETFLD,PARAS,OUTPLAH                                            
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0            TEST FOR 'ALL' PLANS                         
         BNE   SPERR                                                            
         TM    WHEN,X'38'          TEST FOR SOON OR OVERNIGHT REQUEST           
         BZ    WHENERR             NO-EXIT W AN ERROR                           
         B     VKEYX                                                            
*                                                                               
VKEY3    MVC   SVPRDVAL,PRDVALS    SAVE AWAY PRODUCT VALUES                     
         XC    OUTPRN,OUTPRN       CLEAR PRODUCT NAME                           
         MVC   OUTPRN(L'PRDNAM),PRDNAM                                          
         OI    OUTPRNH+6,X'80'     XMIT                                         
*                                                                               
* EDIT AND VALIDATE PLAN CODE                                                   
*                                                                               
VKEY4    LA    R2,OUTPLAH                                                       
         SR    R0,R0                                                            
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+8                                                              
         ICM   R0,8,MISSPARM                                                    
         GOTO1 VVALPLAN,PARAS,(R2),(R0)                                         
         OC    PLANCODE,PLANCODE   TEST FOR 'ALL' PLANS                         
         BNZ   VKEY5                                                            
         TM    WHEN,X'38'          TEST FOR SOON OR OVERNIGHT REQUEST           
         BZ    WHENERR                                                          
         B     VKEYX                                                            
*                                                                               
VKEY5    MVC   SVPARKEY,NODKEY     SAVE NODAL KEY                               
         MVC   SVPLNKEY,NODKEY                                                  
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)     R3=A(FIRST OUTLINE LEVEL)                    
         MVC   NEXTNODE,NDLVNOD    EXTRACT NODE ESTABLISHED BY PLAN             
         MVI   NEXTLEV,1           INITIALIZE NEXT OUTLINE LEVEL TO 1           
         CLI   ACTNUM,ACTADD       TEST FOR ACTION ADD                          
         BNE   VKEY6                                                            
*                                                                               
         MVI   ERROR,LIMERR                                                     
         SR    R1,R1                                                            
         ICM   R1,3,PLANCNT        TEST FOR BLOWING OUTLINE LIMIT               
         LA    R1,1(R1)                                                         
         CH    R1,=Y(OUTLIMIT)                                                  
         BH    TRAPERR                                                          
         STCM  R1,3,PLANCNT                                                     
*                                                                               
VKEY6    MVC   SVPLNVAL,PLANVALS   SAVE PLAN VALUES                             
         XC    OUTPLN,OUTPLN                                                    
         MVC   OUTPLN(L'PLANNAM),PLANNAM                                        
         LA    R2,OUTPLN+L'PLANNAM+1 R2=OUTPUT POINTER                          
         MVC   0(9,R2),=C'OUTLINES='                                            
         LA    R2,9(R2)                                                         
         SR    R0,R0               SHOW OUTLINE COUNT                           
         ICM   R0,3,PLANCNT                                                     
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BNE   *+6                                                              
         BCTR  R0,0                YES-DECREMENT COUNT                          
         EDIT  (R0),(4,(R2)),ALIGN=LEFT                                         
         OI    OUTPLNH+6,X'80'                                                  
         CLI   ACTNUM,ACTREP                                                    
         BE    VKEYX                                                            
*                                                                               
* EDIT AND VALIDATE ATTACH TO POINT                                             
*                                                                               
VKEY8    XC    OUTATN,OUTATN       CLEAR ATTACH TO NAME FIELD                   
         OI    OUTATNH+6,X'80'      AND XMIT                                    
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKEY10                                                           
         LA    R2,OUTATTH                                                       
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         CLI   FLDH+5,0                                                         
         BE    VKEY10                                                           
*                                                                               
         MVC   PARCODE,FLD         EXTRACT PARENT'S CODE                        
         GOTO1 VFINDOUT,PARAS,PARCODE,NDIOA                                     
         BNE   TRAPERR             INVALID PARENT                               
         GOTO1 VTRACE              GET NODAL KEY                                
         MVC   SVPARKEY,NODKEY     SAVE NODAL KEY                               
*                                                                               
         GOTO1 VGETVAL                                                          
         MVC   OUTATN(L'OUTNAME),OUTNAME  EXTRACT PARENT OUTLINE'S NAME         
         MVI   ERROR,LEVERR                                                     
         CLI   OUTLEV,MAXOUTS      TEST AT LOWEST LEVEL                         
         BE    TRAPERR             YES-CANNOT ATTACH TO IT                      
*                                                                               
         L     R3,NDLEVPTR                                                      
         LA    R3,NDLVTABL(R3)     NEXT LEVEL TABLE ENTRY                       
         MVC   NEXTNODE,NDLVNOD    EXTRACT NODE ESTABLISHED BY PARENT           
         ZIC   R1,OUTLEV                                                        
         LA    R1,1(R1)            INCREMENT OUTLINE LEVEL                      
         STC   R1,NEXTLEV          SET NEXT OUTLINE LEVEL                       
*                                                                               
* EDIT ADD BEFORE POINT                                                         
*                                                                               
VKEY10   CLI   ACTNUM,ACTADD                                                    
         BNE   VKEY12                                                           
         LA    R2,OUTADDH                                                       
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         CLI   FLDH+5,0                                                         
         BE    VKEY12                                                           
         MVC   BEFORE,FLD          EXTRACT BEFORE OUTLINE CODE                  
*                                                                               
         GOTO1 VFINDOUT,PARAS,BEFORE,AIO                                        
         BNE   TRAPERR                                                          
         MVI   ERROR,NOTCHILD                                                   
         L     R4,AIO              MAKE THE RECORD ADDRESSABLE                  
         USING BURECD,R4                                                        
         CLC   BUKNODE,NEXTNODE    TEST THAT RECORD IS CHILD                    
         BNE   TRAPERR                                                          
*                                                                               
* EDIT AND VALIDATE OUTLINE CODE                                                
*                                                                               
VKEY12   LA    R2,OUTCODH                                                       
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    TRAPERR                                                          
*                                                                               
         MVC   OUTCODE,FLD                                                      
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BNE   VKEY14              NO                                           
*                                                                               
         BAS   RE,TSTAN            CHECK FOR ALPHANUMERIC CODE                  
*                                                                               
         GOTO1 VFINDOUT,PARAS,OUTCODE,NDIOA                                     
         BNE   *+12                                                             
         MVI   ERROR,DUPCDERR      OUTLINE CODE NOT UNIQUE FOR PLAN             
         B     TRAPERR                                                          
         GOTO1 CONCAT,OUTCODE      GENERATE NODAL KEY FOR OUTLINE               
         MVC   SVNKEY,NODKEY       SAVE NODAL KEY                               
         OI    WHENOK,X'01'        BYPASS GENCON MAINTENANCE                    
         B     VKEYX                                                            
*                                                                               
VKEY14   MVI   BYTE,0                                                           
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    *+16                                                             
         CLI   ACTNUM,ACTREST      TEST FOR RESTORE                             
         BNE   VKEY15                                                           
         MVI   BYTE,X'08'          SET TO PASS DELETES                          
         OI    WHENOK,X'01'        BYPASS GENCON FOR DELETE/RESTORE             
*                                                                               
VKEY15   GOTO1 VFINDOUT,PARAS,(BYTE,OUTCODE),NDIOA                              
         BNE   TRAPERR                                                          
         CLI   ACTNUM,ACTREST                                                   
         BNE   *+8                                                              
         MVI   NDDELRSW,YES        READ DELETES IN NODIO                        
         GOTO1 VTRACE              GET NODAL KEY AND READ THROUGH NODIO         
         GOTO1 VGETVAL                                                          
         MVC   SVOUTKEY,NODKEY     SAVE START OUTLINE NODAL KEY                 
         MVC   SVNKEY,NODKEY       SAVE RECORD'S NODAL KEY                      
         B     VKEYX                                                            
*                                                                               
VKEYX    B     XIT                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC OUTLINE CODE                            
*                                                                               
TSTAN    ST    RE,SAVERE                                                        
         LA    R1,OUTCODE                                                       
         ZIC   RE,FLDH+5           GET FIELD LENGTH                             
         NI    FLDH+4,X'F0'                                                     
         OI    FLDH+4,X'0C'                                                     
         MVI   ERROR,INVALID                                                    
         TM    PLANIND,BUPLNCOD    TEST CODE RESERVATION ENABLED                
         BZ    TSTAN1                                                           
         CLI   OUTCODE,C'X'        YES-NO CODES STARTING WITH 'X'               
         BE    TRAPERR                                                          
*                                                                               
TSTAN1   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    TRAPERR                                                          
         B     TSTAN2                                                           
         NI    FLDH+4,X'FF'-X'08'                                               
         CLI   0(R1),C'A'                                                       
         BL    TRAPERR                                                          
         CLI   0(R1),C'I'                                                       
         BNH   TSTAN2              CHARACTER IS BETWEEN A-I                     
         CLI   0(R1),C'J'                                                       
         BL    TRAPERR                                                          
         CLI   0(R1),C'R'                                                       
         BNH   TSTAN2                                                           
         CLI   0(R1),C'S'                                                       
         BL    TRAPERR                                                          
         CLI   0(R1),C'Z'                                                       
         BNH   TSTAN2                                                           
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   RE,TSTAN1                                                        
*&&UK                                                                           
         TM    FLDH+4,X'08'                                                     
         BNZ   TRAPERR                                                          
*&&                                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*        DISPLAY KEY                                                            
*                                                                               
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
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
         MVC   OUTCLT,DUB                                                       
         OI    OUTCLTH+6,X'80'                                                  
*                                                                               
         MVC   OUTCLN,CLTNAM                                                    
         OI    OUTCLNH+6,X'80'                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(L'PRDCODE),PRDCODE                                           
         OC    DUB,SPACES                                                       
         MVC   OUTPRD,DUB                                                       
         OI    OUTPRDH+6,X'80'                                                  
*                                                                               
         MVC   OUTPRN,PRDNAM                                                    
         OI    OUTPRNH+6,X'80'                                                  
*                                                                               
         MVC   DUB(L'PLANCODE),PLANCODE    PLAN CODE                            
         OC    DUB,SPACES                                                       
         MVC   OUTPLA,DUB                                                       
         OI    OUTPLAH+6,X'80'                                                  
*                                                                               
         MVC   OUTPLN(L'PLANNAM),PLANNAM                                        
         LA    R2,OUTPLN+L'PLANNAM+1 R2=OUTPUT POINTER                          
         MVC   0(9,R2),=C'OUTLINES='                                            
         LA    R2,9(R2)                                                         
         SR    R0,R0               SHOW OUTLINE COUNT                           
         ICM   R0,3,PLANCNT                                                     
         EDIT  (R0),(4,(R2)),ALIGN=LEFT                                         
         OI    OUTPLNH+6,X'80'                                                  
*                                                                               
         MVC   DUB,BUKCODE         EXTRACT OUTLINE CODE                         
         OC    DUB,SPACES                                                       
         MVC   OUTCOD,DUB                                                       
         OI    OUTCODH+6,X'80'                                                  
*                                                                               
         GOTO1 VTRACE              RE-READ FOR SELECT FOR CHANGE                
         MVC   SVNKEY,NODKEY       RESUME LIST AT SELECTED RECORD               
         GOTO1 VUPKEY                                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE OUTLINE RECORD                                                       
*                                                                               
VREC     L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVC   CLTVALS,SVCLTVAL    RESTORE CLIENT VALUES                        
         MVC   PRDVALS,SVPRDVAL    RESTORE PRODUCT VALUES                       
         MVC   PLANVALS,SVPLNVAL   RESTORE PLAN VALUES                          
*                                                                               
         CLI   ACTNUM,ACTADD       TEST IF ADDING                               
         BE    VREC1                                                            
         GOTO1 VGETVAL             EXTRACT OUTLINE VALUES BEFORE CHANGE         
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    VREC17                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    VREC18                                                           
         MVI   ELCODE,BUOUTELQ                                                  
         GOTO1 REMELEM                                                          
         GOTO1 PASSKEY,PARAS,OLDPASS                                            
         B     VREC2                                                            
*                                                                               
VREC1    LR    RE,R4               CLEAR IO AREA FOR ADD                        
         L     RF,SIZEIO                                                        
         XCEF                                                                   
*                                                                               
         MVC   BUKEY,NDKEY         INITIALIZE KEY W MASTER                      
         MVC   BURLEN,DATADISP                                                  
         OI    BURCTYP,BUKCOUT     OUTLINE RECORD INDICATOR                     
*                                                                               
         XC    ELEM(BUOUTLNQ),ELEM                                              
         LA    R6,ELEM             INITIALIZE OUTLINE DESCRIPTION ELEM          
         USING BUOUTD,R6                                                        
         MVI   BUOUTEL,BUOUTELQ                                                 
         MVI   BUOUTLEN,BUOUTLNQ                                                
*                                                                               
VREC2    LA    R6,ELEM             POINT TO ELEMENT                             
         LA    R2,OUTNAMH          EDIT OUTLINE NAME                            
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVC   BUOUTNAM,FLD                                                     
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   VREC3                                                            
         MVC   BUOUTNAM(L'OUTCODE),OUTCODE                                      
         OC    BUOUTNAM,SPACES     DEFAULT TO OUTLINE CODE                      
         MVC   8(L'BUOUTNAM,R2),BUOUTNAM                                        
         OI    6(R2),X'80'         XMIT DEFAULT BACK TO USER                    
*                                                                               
VREC3    MVC   SAVOUTEL,ELEM       SAVE OUTLINE ELEMENT                         
*                                                                               
VREC4    MVI   ELCODE,BURULELQ     DELETE RULES ELEMENTS                        
         GOTO1 REMELEM                                                          
         MVI   ELCODE,BUINELQ      DELETE SCREEN INPUT ELEMENTS                 
         GOTO1 REMELEM                                                          
         MVI   FIELDSW,C'R'                                                     
         BAS   RE,SCRIN            CHECK SCREEN INPUT                           
         BNE   VREC6               NO INPUT                                     
         GOTO1 VEDRULES,DMCB,GEND,OUTRUH,OUTRU4H                                
         MVI   RULESW,YES          SET SOME RULES                               
         L     R2,AIO2                                                          
         LA    R2,1000(R2)         POINT TO RULES ELEMENTS                      
         USING BURULD,R2                                                        
*                                                                               
* ADD RULES ELEMENTS TO RECORD                                                  
*                                                                               
VREC5    CLI   0(R2),0             TEST FOR EOL                                 
         BE    VREC6                                                            
         XC    ELEM,ELEM                                                        
         ZIC   R1,BURULEN                                                       
         LR    R0,R1               SAVE ELEMENT LENGTH IN R0                    
         BCTR  R1,0                                                             
         EX    R1,MOVEEL                                                        
         GOTO1 ADDELEM                                                          
         AR    R2,R0               POINT TO NEXT ELEMENT POSITION               
         B     VREC5                                                            
         DROP  R2                                                               
*                                                                               
* EDIT FORMULA FIELD                                                            
*                                                                               
VREC6    MVI   ELCODE,BUROPELQ     DELETE ROW OPERAND AND                       
         GOTO1 REMELEM                                                          
         MVI   ELCODE,BUPOLELQ     POLISH FORMULA ELEMENTS                      
         GOTO1 REMELEM                                                          
         MVI   FIELDSW,C'F'                                                     
         BAS   RE,SCRIN                                                         
         BNE   VREC10              NO INPUT                                     
*                                                                               
         LA    R2,OUTFOH                                                        
         ST    R2,FADDR                                                         
         BAS   RE,SCAN                                                          
*                                                                               
         LA    R2,OUTFO2H                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR SOME INPUT                          
         BNE   VREC7                                                            
         MVI   ERROR,MISODERR      TEST FOR OPERATION ENDING                    
         CLI   LASTSTOP,X'FF'      LAST LINE                                    
         BE    VREC8               OPERAND ENDED LINE                           
         B     TRAPERR                                                          
*                                                                               
VREC7    BAS   RE,SCAN                                                          
         MVI   ERROR,MISOPERR                                                   
         CLI   LASTSTOP,X'FF'      TEST IF LAST LINE ENDED IN OPERATION         
         BNE   SPERR                                                            
*                                                                               
VREC8    MVI   ERROR,RUFOERR                                                    
         LA    R2,OUTFOH                                                        
         CLI   RULESW,YES                                                       
         BE    TRAPERR                                                          
         MVI   ERROR,FOLEVERR                                                   
         CLI   ACTNUM,ACTADD       TEST FOR ADD                                 
         BE    VREC9                                                            
         CLI   OUTLEV,MAXOUTS      TEST AT LOWEST LEVEL                         
         BE    VREC9               YES-OUTLINE CANNOT HAVE CHILDREN             
         LA    R3,NDLVTAB-NODBLKD(R5)                                           
         ZIC   R1,NDLEV                                                         
         LA    R1,1(R1)            NEXT LEVEL AFTER OUTLINE                     
         MH    R1,=Y(NDLVTABL)                                                  
         LA    R3,0(R1,R3)                                                      
         USING NDLVTABD,R3                                                      
         OC    NDLVNOD,NDLVNOD     TEST IF OUTLINE HAS CHILDREN                 
         BNZ   TRAPERR                                                          
*                                                                               
* ADD ROW OPERAND AND POLISH FORMULA ELEMENTS TO RECORD                         
*                                                                               
VREC9    L     R2,AIO3             POINT TO OPERAND ELEMENT LIST                
         ZIC   R0,NOPER            R0=N'ELEMENTS                                
         MVI   ELCODE,BUROPELQ                                                  
VREC9A   XC    ELEM,ELEM                                                        
         MVC   ELEM(OPLISTL),0(R2)                                              
         GOTO1 ADDELEM                                                          
         LA    R2,OPLISTL(R2)                                                   
         BCT   R0,VREC9A                                                        
         BAS   RE,POLISH           BUILD AND ADD POLISH ELEMENT                 
*                                                                               
VREC10   XC    ELEM,ELEM                                                        
         MVC   ELEM(L'SAVOUTEL),SAVOUTEL RESTORE OUTLINE ELEMENT                
*                                                                               
         MVI   BUOUTBEF,0                                                       
         MVI   BUOUTAFT,0                                                       
         XC    BUOUTIND,BUOUTIND                                                
         GOTO1 OPTED,PARAS,OUTCONH,OPTTAB  EDIT PRINT CONTROL OPTIONS           
         GOTO1 ADDELEM                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    VREC20                                                           
*                                                                               
* I/O ROUTINE FOR PUT                                                           
*                                                                               
VREC15   MVI   IOOPT,YES                                                        
         GOTO1 VCHAACTV                                                         
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',SVNKEY,0                             
         CLI   NDERR,0                                                          
         BE    VRECX                                                            
         LA    R2,OUTCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* PROCESSING AND I/O ROUTINE FOR DELETE                                         
*                                                                               
VREC17   LA    R2,OUTCODH                                                       
         L     R3,NDLEVPTR                                                      
*                                                                               
         MVI   ERROR,DELERR                                                     
         CLI   OUTLEV,MAXOUTS      TEST AT LOWEST LEVEL                         
         BE    *+18                YES-OUTLINE CANNOT HAVE CHILDREN             
         LA    R3,NDLVTABL(R3)     NEXT LEVEL AFTER OUTLINE                     
         OC    NDLVNOD,NDLVNOD     TEST IF RECORD IS PARENT                     
         BNZ   TRAPERR             YES-DO NOT ALLOW DELETE                      
*                                                                               
         MVI   ERROR,DELDTERR                                                   
         L     R3,NDLEVPTR         RESTORE LEVEL POINTER                        
         XC    KEY,KEY                                                          
         LA    R4,KEY              MAKE KEY ADDRESSABLE                         
         MVC   BUKEY(BUSUBKEY-BUKEY),NDLVKEY EXTRACT NODAL KEY                  
         MVI   BUVSUB,BUVSUBQ      READ FOR DATA RECORDS                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   BUKEY(BUVDTYP-BUKEY),KEYSAVE                                     
         BNE   VREC17A             NO DATA RECORDS FOUND                        
         CLI   THISPF,PF9          TEST PF9=OK TO DELETE OUTLINE                
         BE    VREC17A                                                          
         OI    CONACTH+6,X'81'     ALLOW RE-ENTRY WITH PF9                      
         B     TRAPERR             FOUND A DATA RECORD FOR OUTLINE              
*                                                                               
VREC17A  L     R4,NDIOA            RESTORE R4                                   
*                                                                               
         GOTO1 PASSKEY,PARAS,OLDPASS                                            
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'DELETE',SVNKEY,0                          
         CLI   NDERR,0                                                          
         BE    VREC17B                                                          
         GOTO1 VNODERR                                                          
*                                                                               
VREC17B  GOTO1 DELPTR,PARAS,OLDPASS                                             
         MVC   CONHEAD(L'DELMSG),DELMSG                                         
         OI    6(R2),X'81'         XMIT BACK MODIFIED TO PREVENT                
         B     VREC30              DOUBLE DELETION                              
*                                                                               
* PROCESSING AND I/O ROUTINE FOR RESTORE                                        
*                                                                               
VREC18   LA    R2,OUTCODH                                                       
         MVI   ERROR,LIMERR                                                     
         SR    R1,R1               TEST IF RESTORING OUTLINE WILL               
         ICM   R1,3,PLANCNT        BLOW PLAN OUTLINE LIMIT                      
         LA    R1,1(R1)                                                         
         CH    R1,=Y(OUTLIMIT)                                                  
         BH    TRAPERR                                                          
*                                                                               
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'RES',SVNKEY,0                             
         CLI   NDERR,0                                                          
         BE    VREC18A                                                          
         GOTO1 VNODERR                                                          
*                                                                               
* RE-READ RESTORED RECORD THROUGH NODIO AND DEAL WITH POINTERS                  
*                                                                               
VREC18A  GOTO1 VNODIO,(R1),NODBLKD,=C'READ',SVNKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PASSKEY,PARAS,NEWPASS                                            
         GOTO1 NEWPTR,(R1),NEWPASS                                              
         MVC   CONHEAD(L'RESMSG),RESMSG                                         
         B     VREC30                                                           
*                                                                               
* I/O ROUTINE FOR ADD (READ TO CHECK FOR DUPLICATES.  THEN ADD                  
* OUTLINE TO END OF LIST UNLESS USER GIVES ADD BEFORE POINT.                    
*                                                                               
VREC20   MVI   IOOPT,YES                                                        
         BAS   RE,POINTEL          ADD PASSIVE POINTER ELEMENT                  
         GOTO1 VADDACTV                                                         
         MVC   NODKEY,SVNKEY                                                    
         MVC   NODKEYSV,SVNKEY                                                  
*                                                                               
         XC    NDHOOK,NDHOOK       CHECK FOR DUPLICATE KEYS                     
         MVI   NDDELRSW,YES        RETURN DELETED RECORDS                       
         MVC   NDIOA,AIO2          GET RECORD INTO IO2                          
         MVC   NDIOA2,AIO3                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         MVI   NDDELRSW,0          CLEAR PASS DELETE SWITCH                     
         CLI   NDERR,NDRNFERR      TEST FOR RNF                                 
         BE    VREC23              YES-PROCEED WITH ADD                         
         CLI   NDERR,0                                                          
         BE    VREC22              RECORD FOUND-DUPLICATE                       
         LA    R2,OUTCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC22   L     R4,NDIOA                                                         
         MVI   ERROR,DELEXIST                                                   
         LA    R2,OUTCODH                                                       
         XC    KEY,KEY             READ THE DIRECTORY POINTER                   
         MVC   KEY(L'BUKEY),BUKEY                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 READ                                                             
         TM    KEY+(BUKCSTA-BUKEY),X'80'                                        
         BO    *+8                 RECORD IS DELETED                            
         MVI   ERROR,RECEXIST                                                   
         B     TRAPERR                                                          
*                                                                               
VREC23   OC    BEFORE,BEFORE       TEST FOR ADD BEFORE POINT                    
         BZ    VREC24              NO-ADD TO END OF LIST                        
         MVC   NODKEY,SVNKEY       INITIALIZE NODIO KEY                         
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2         GET RECORD INTO IO2                          
         LA    R0,L'BEFORE                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'B',=C'ADD'),NODKEY,((R0),BEFORE),X        
               0,0                                                              
         CLI   NDERR,0                                                          
         BE    VREC25                                                           
         LA    R2,OUTCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VREC24   MVC   NODKEY,SVNKEY                                                    
         MVC   NDIOA,AIO1          RESTORE I/O AREAS                            
         MVC   NDIOA2,AIO2                                                      
         GOTO1 VNODIO,DMCB,NODBLKD,(C'A',=C'ADD'),NODKEY,0,0                    
         CLI   NDERR,0                                                          
         BE    VREC25                                                           
         LA    R2,OUTCODH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
* ADD PASSIVE POINTER                                                           
*                                                                               
VREC25   GOTO1 PASSKEY,PARAS,NEWPASS                                            
         GOTO1 NEWPTR,(R1),NEWPASS                                              
*                                                                               
VREC27   L     R4,NDIOA                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),BUKEY  EXTRACT KEY FROM RECORD                      
         MVC   TWAKEYSV,KEY                                                     
         MVC   CONHEAD(L'ADDMSG),ADDMSG                                         
*                                                                               
* UPDATE PLAN RECORD FOR OUTLINE ADD, DELETE, OR RESTORE                        
*                                                                               
VREC30   GOTO1 VSETKEY             BUILD PLAN KEY                               
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT HERE ON NODIO ERROR               
         MVI   ELCODE,BUPLNELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPLND,R6                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,BUPLNCNT       R1=OUTLINE COUNT FOR PLAN                    
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    *+12                                                             
         LA    R1,1(R1)            INCREMENT COUNT                              
         B     *+6                                                              
*                                                                               
         BCTR  R1,0                DECREMENT COUNT FOR DELETE                   
*                                                                               
         STCM  R1,3,BUPLNCNT                                                    
         CLI   ACTNUM,ACTADD       TEST FOR ACTION ADD                          
         BNE   VREC32              SKIP SETTING LOWEST LEVEL                    
         CLC   BUPLNLOW,NEXTLEV    TEST IF NEW LOW LEVEL FOR PLAN               
         BH    *+10                                                             
         MVC   BUPLNLOW,NEXTLEV                                                 
*                                                                               
VREC32   GOTO1 VNODIO,DMCB,NODBLKD,=C'PUT',NODKEY,0                             
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* RE-READ OUTLINE FOR DELETES AND RESTORES AND DISPLAY IT-EXIT FOR ADD          
*                                                                               
VREC35   CLI   ACTNUM,ACTREST      TEST FOR RESTORE                             
         BE    *+16                                                             
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BNE   VRECX               NO                                           
         MVI   NDDELRSW,YES        NODIO SET TO READ DELETES                    
*                                                                               
         MVC   NODKEY,SVNKEY                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BNE   VRECX                                                            
         L     R3,NDLEVPTR                                                      
         MVC   DMDSKADD,NDLVDA                                                  
         B     DREC                NOW DISPLAY RECORD AND EXIT                  
*                                                                               
VRECX    B     XIT                                                              
         SPACE 2                                                                
MOVEEL   MVC   ELEM(0),0(R2)                                                    
         EJECT                                                                  
* DISPLAY OUTLINE RECORD                                                        
*                                                                               
DREC     GOTO1 VCLEARF,PARAS,OUTATTH,OUTCODH                                    
         XC    OUTATN,OUTATN       CLEAR ATTACH TO NAME FIELD                   
         OI    OUTATNH+6,X'80'     XMIT                                         
         MVC   DSKADDR,DMDSKADD    SAVE DA FOR LATER DISPLAY                    
         CLI   NDLEV,4             TEST FOR FIRST LEVEL OUTLINE                 
         BE    DREC2               YES-PARENT IS PLAN                           
*                                                                               
         L     R3,NDLEVPTR         R3=A(LEVEL TABLE ENTRY)                      
         SH    R3,=Y(NDLVTABL)     BACK UP 1 ENTRY                              
         MVC   PARCODE,NDLVCOD     EXTRACT PARENT'S CODE                        
         MVC   OUTATN(8),NDLVCOD   CODE/NAME                                    
*                                                                               
         LA    R0,8                R0=COUNTER                                   
         LA    R2,OUTATN+7         R2=A(LAST CHARACTER)                         
         CLI   0(R2),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   R0,*-10                                                          
         LA    R2,1(R2)                                                         
         MVI   0(R2),SLASH                                                      
         LA    R2,1(R2)            R2=OUTPUT POINTER                            
*                                                                               
DREC1    GOTO1 VFINDOUT,PARAS,PARCODE,AIO2                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2             R4=A(PARENT OUTLINE)                         
         MVI   ELCODE,BUOUTELQ                                                  
         BAS   RE,GETEL            GET OUTLINE ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUOUTD,R6                                                        
         MVC   0(20,R2),BUOUTNAM                                                
*                                                                               
DREC2    GOTO1 VCLEARF,DMCB,OUTNAMH,OUTLAST                                     
         GOTO1 VGETVAL             GET OUTLINE RECORD VALUES                    
         L     R4,AIO                                                           
         MVC   OUTNAM,OUTNAME                                                   
         MVI   ELCODE,BURULELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DREC4                                                            
         GOTO1 DISSCR,PARAS,OUTRUH,4                                            
*                                                                               
DREC4    MVI   ELCODE,BUROPELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DREC6                                                            
         GOTO1 DISSCR,PARAS,OUTFOH,2                                            
*                                                                               
DREC6    GOTO1 DISOPT,PARAS,OUTCONH                                             
*                                                                               
DREC8    CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DRECX                                                            
         MVC   OUTDA(3),=C'DA='    DISPLAY DISK ADDRESS                         
         GOTO1 HEXOUT,DMCB,DSKADDR,OUTDA+3,4,=C'TOG'                            
         MVC   OUTDA+12(5),=C'NODE='  AND NODE                                  
         GOTO1 (RF),DMCB,BUKNODE,OUTDA+17,4,=C'TOG'                             
         OI    OUTDAH+6,X'80'      XMIT                                         
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
* PRINT REPORT                                                                  
*                                                                               
PREP     DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 VSETKEY                                                          
         MVI   NDUPDTSW,NO                                                      
         LA    R1,PREP2            SET IN-LINE HOOK                             
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,NODBLKD,=C'HIGH',NODKEY,0                            
         CLI   NDERR,0                                                          
         BNE   PREPX                                                            
*                                                                               
         GOTO1 (RF),DMCB,NODBLKD,=C'LSEQ',NODKEY,0                              
         B     PREPX                                                            
         SPACE 2                                                                
* HOOK ROUTINE TO PROCESS RECORDS AND PRINT AN OUTPUT LINE                      
*                                                                               
PREP2    NTR1                                                                   
         CLI   NDMODE,NDPROC                                                    
         BNE   PREPX                                                            
*                                                                               
         GOTO1 VGETVAL                                                          
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCPLN     TEST FOR PLAN                                
         BZ    *+12                                                             
         MVI   FORCEHED,YES        BREAK PAGE FOR NEW PLAN                      
         B     PREPX                                                            
*                                                                               
PREP4    TM    BURCTYP,BUKCOUT     TEST FOR OUTLINE                             
         BZ    PREPX               NO                                           
*                                                                               
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRTD,R3                                                          
         CLI   OUTLEV,1            TEST FOR FIRST LEVEL OUTLINE                 
         BNE   PREP5                                                            
         GOTO1 SPOOL,PARAS,(R8)    SKIP A LINE BEFORE PRINTING                  
*                                                                               
PREP5    ZIC   R1,OUTLEV           GET OUTLINE LEVEL                            
         BCTR  R1,0                DEVELOP INDEX INTO PRINT FIELD               
         LA    RE,PRTCODE(R1)                                                   
         MVC   0(L'OUTCODE,RE),OUTCODE EXTRACT OUTLINE CODE                     
         OC    0(L'OUTCODE,RE),SPACES SPACE FILL FIELD                          
         LA    RE,PRTDESC(R1)      INDEX INTO DESCRIPTION                       
         MVC   0(L'OUTNAME,RE),OUTNAME                                          
*                                                                               
PREP6    LA    R6,BUFRSTEL         R6=ELEMENT POINTER                           
         SR    R0,R0                                                            
         LA    RE,PRTRUFO          RE=OUTPUT POINTER                            
*                                                                               
PREP7    CLI   0(R6),0             TEST FOR EOR                                 
         BE    PREP12                                                           
         CLI   0(R6),BUINELQ       TEST FOR SCREEN INPUT ELEMENT                
         BE    PREP10                                                           
*                                                                               
PREP8    IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PREP7                                                            
*                                                                               
         USING BUIND,R6                                                         
PREP10   ZIC   R1,BUINLEN                                                       
         SH    R1,=Y(BUINPUT-BUIND+1)                                           
         EX    R1,MOVEPRT                                                       
         LA    RE,L'P(RE)          NEXT PRINT LINE                              
         B     PREP8                                                            
*                                                                               
PREP12   XC    FLDH,FLDH           PRINT CONTROL OPTIONS                        
         MVI   FLDH,L'FLDH+L'FLD                                                
         MVC   FLD,SPACES                                                       
         GOTO1 DISOPT,PARAS,FLDH                                                
         CLI   FLD,C' '            TEST FOR ANY OUTPUT                          
         BE    PREP15                                                           
*                                                                               
PREP13   LA    R0,L'FLD                                                         
         LA    R2,L'PRTCON                                                      
         GOTO1 CHOPPER,DMCB,((R0),FLD),((R2),AIO3),(C'P',4),0                   
         LA    RE,PRTCON           RE=A(PRINT POSITION)                         
         L     R1,AIO3                                                          
         ICM   RF,15,DMCB+8                                                     
         BZ    PREP15                                                           
*                                                                               
PREP14   MVC   0(L'PRTCON,RE),0(R1)                                             
         LA    RE,132(RE)                                                       
         LA    R1,132(R1)                                                       
         BCT   RF,PREP14                                                        
*                                                                               
PREP15   GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 (RF),(R1),(R8)                                                   
*                                                                               
PREPX    B     XIT                                                              
         SPACE 2                                                                
MOVEPRT  MVC   0(0,RE),BUINPUT                                                  
         EJECT                                                                  
* HEADLINE PRINTING ROUTINE HOOK                                                
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
         MVI   14(R2),C'C'                                                      
         MVI   40(R2),C'C'                                                      
         MVI   97(R2),C'C'                                                      
         MVI   118(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CONCATENATE THE OUTLINE CODE TO ITS PARENT'S                   
* NODAL KEY.  AT ENTRY, R1 POINTS TO OUTLINE CODE.  ON EXIT, NODKEY             
* AND NODKEYSV CONTAIN FULL NODAL KEY                                           
*                                                                               
CONCAT   ST    RE,SAVERE                                                        
         MVC   NODKEY,SVPARKEY                                                  
         LA    R0,L'NODKEY                                                      
         LA    RE,NODKEY+L'NODKEY-1                                             
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    CONCAT2                                                          
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
*                                                                               
CONCAT2  LA    RE,1(RE)            POSITION FOR DELIMITER                       
         MVC   0(1,RE),NDDELIM                                                  
         MVC   1(L'OUTCODE,RE),0(R1)                                            
         OC    NODKEY,SPACES                                                    
         MVC   NODKEYSV,NODKEY                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHECK SCREEN INPUT IN RULES OR FORMULA FIELDS                  
* AT ENTRY, FIELDSW = R (RULES) OR F (FORMULA)                                  
*                                                                               
SCRIN    NTR1                                                                   
         LA    R3,1                R3=FIELD SEQUENCE NUMBER                     
         XC    FTERM,FTERM                                                      
         MVI   EMPTYSW,NO          SET EMPTY LINE SWITCH TO NO                  
         MVI   INPUTSW,NO          SET INPUT SWITCH TO NO                       
         LA    R0,4                R0=N'SCREEN FIELDS                           
         LA    R2,OUTRUH           R2=A(SCREEN FIELD HEADER)                    
         CLI   FIELDSW,C'R'        TEST FOR RULES                               
         BE    *+12                                                             
         LA    R0,2                                                             
         LA    R2,OUTFOH                                                        
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
* SUB-ROUTINE TO SCAN A FORMULA FIELD LINE FOR OPERATION/OPERAND                
* PAIRS.  ASSUMES CALLER HAS CHECKED FIELD FOR SOME INPUT AND                   
* SET FADDR.                                                                    
*                                                                               
SCAN     NTR1                                                                   
         XC    FLAST,FLAST         EDIT FIELD FROM START                        
         XC    FTERM,FTERM                                                      
         MVC   FTERM(L'OPERS1),OPERS1 SCAN FOR OPERATIONS                       
*                                                                               
SCAN2    MVI   FZERO,YES                                                        
         GOTO1 VFVAL                                                            
         ZIC   R1,FNDX             INCREMENT FIELD NUMBER COUNT                 
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         CLI   FSTOP,X'FF'         TEST IF OPERATION STOPPED SCAN               
         BNE   SCAN3               YES                                          
         CLI   FLDH+5,0            TEST FOR EOF                                 
         BE    SCANX               YES                                          
         B     SCAN10              NO-LOOK FOR AN OPERAND                       
*                                                                               
SCAN3    CLI   FNDX,1              TEST FOR FIRST FIELD ON LINE                 
         BNE   SCAN8               NO                                           
         CLI   FLDH+5,0            TEST FOR ANYTHING BEFORE OPERATION           
         BE    SCAN6               NO                                           
*                                                                               
* FIRST FIELD - POTENTIAL OPERAND                                               
*                                                                               
         CLI   NOPER,0             TEST FOR FIRST OPERAND                       
         BNE   SCAN4               NO                                           
         MVI   OPERTN,PLUS         FIRST OPERAND ALWAYS IS ADDED                
         BAS   RE,OPERAND          VALIDATE POTENTIAL OPERAND                   
         BNE   SCANR                                                            
         MVC   OPERTN,FSTOP        SET NEXT OPERATION                           
         MVC   LASTSTOP,FSTOP      SAVE LAST STOP CHARACTER                     
         B     SCAN15              CONTINUE SCAN                                
*                                  FIRST FIELD ON 2ND TO 4TH LINE               
SCAN4    MVI   ERROR,OPNSTERR                                                   
         CLI   LASTSTOP,X'FF'      TEST IF LAST LINE ENDED IN OPERATION         
         BE    SCANR               NO-SO THIS LINE MUST START W OPERTN          
         BAS   RE,OPERAND                                                       
         BNE   SCANR                                                            
         MVC   OPERTN,FSTOP        SET NEXT OPERATION                           
         MVC   LASTSTOP,FSTOP                                                   
         B     SCAN15                                                           
*                                  FIRST FIELD IS AN OPERATION                  
SCAN6    MVI   ERROR,OPSTERR                                                    
         CLI   NOPER,0             TEST IF LOOKING FOR 1ST OPERAND              
         BE    SCANR                                                            
         MVI   ERROR,DBOPNERR                                                   
         CLI   LASTSTOP,X'FF'      TEST IF LAST LINE ENDED IN OPERAND           
         BNE   SCANR               NO-DOUBLE OPERATION                          
         MVC   OPERTN,FSTOP                                                     
         MVC   LASTSTOP,FSTOP                                                   
         B     SCAN15                                                           
*                                                                               
* FOUND OPERATION - 2ND TO NTH FIELD                                            
*                                                                               
SCAN8    MVI   ERROR,DBOPNERR                                                   
         CLI   FLDH+5,0            TEST FOR CONSECUTIVE OPERATIONS              
         BE    SCANR                                                            
         BAS   RE,OPERAND                                                       
         BNE   SCANR                                                            
         CLI   OPERTN,0            TEST IF OPERTN PRECEDES OR FOLLOWS           
         BE    *+14                FOLLOWS                                      
         MVC   OPERTN,FSTOP        PRECEDES-SO SET NEXT OPERATION               
         B     *+10                                                             
*                                                                               
         USING BUROPD,R3                                                        
         MVC   BUROPER,FSTOP       FOLLOWS-SO SET OPERTN=STOP CHAR              
*                                                                               
         MVC   LASTSTOP,FSTOP                                                   
         B     SCAN15              CONTINUE SCAN                                
*                                                                               
* OPERAND ONLY FOUND                                                            
*                                                                               
SCAN10   CLI   FNDX,1              TEST FOR FIRST FIELD                         
         BNE   SCAN12                                                           
         CLI   NOPER,0             TEST FOR FIRST LINE                          
         BNE   *+12                                                             
         MVI   OPERTN,PLUS         SET DEFAULT OPERATION OF PLUS                
         B     SCAN11                                                           
*                                                                               
         MVI   ERROR,MISOPERR                                                   
         CLI   LASTSTOP,X'FF'      TEST IF LAST LINE ENDED IN OPERATION         
         BE    SCANR               NO-SO MISSING AN OPERATION                   
*                                                                               
SCAN11   BAS   RE,OPERAND          FIRST FIELD PROCESSING                       
         BNE   SCANR                                                            
         MVC   LASTSTOP,FSTOP                                                   
         MVI   OPERTN,0                                                         
         B     SCAN15                                                           
*                                  OPERAND IN 2ND TO NTH FIELD                  
SCAN12   BAS   RE,OPERAND                                                       
         BNE   SCANR                                                            
         L     R3,AOPER                                                         
         CLI   OPERTN,0            TEST FOR PENDING OPERATION                   
         BNE   *+8                                                              
         MVI   BUROPER,PLUS        YES-SET DEFAULT OF ADD                       
         MVC   LASTSTOP,FSTOP                                                   
         MVI   OPERTN,0                                                         
         B     SCAN15                                                           
*                                                                               
SCAN15   B     SCAN2                                                            
*                                                                               
SCANX    B     XIT                                                              
*                                                                               
SCANR    B     SPERR                                                            
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE AN OPERAND AND TO ADD A ROW FROMULA                   
* ELEMENT TO OPERAND LIST                                                       
*                                                                               
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR INVALID OPERAND                              
*                                                                               
OPERAND  NTR1                                                                   
         CLI   NOPER,1             TEST ABOUT TO EDIT SECOND OPERAND            
         BNE   OPERAND1                                                         
*                                                                               
         ZIC   R0,FLDH+5                                                        
         GOTO1 VBURNEM,DMCB,((R0),FLD),DUB                                      
         CLI   0(R1),0             TEST FOR VALID NUMBER                        
         BNE   OPERAND1            NO                                           
         MVI   ERROR,DECERR                                                     
         CLI   DUB,X'85'           TEST FOR MORE THAN 5 DECIMAL PLACES          
         BH    OPERANDX                                                         
         MVI   CONFLAG,YES                                                      
         MVI   BYTE,BUROPCNS       SET CONSTANT FOUND                           
         B     OPERAND6                                                         
*                                                                               
OPERAND1 ZIC   R2,FLDH+5                                                        
         CLI   FLD,QUOTE           TEST FOR QUOTE MARK                          
         BNE   OPERAND4                                                         
*                                                                               
         MVI   ERROR,INVALID                                                    
         SH    R2,=H'1'                                                         
         BZ    OPERANDX                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+1        SHIFT INPUT OVER ONE POSITION                
*                                                                               
         LA    RE,FLD(R2)          POINT TO LAST BYTE                           
         CLI   0(RE),QUOTE         TEST QUOTE SIGN ENDS FIELD                   
         BNE   OPERAND2            NO-HAVE AN ERROR                             
         XC    0(2,RE),0(RE)       ZERO OUT DUPLICATE QUOTE MARKS               
         LTR   R2,R2               TEST FOR MISSING NUMBER                      
         BZ    OPERANDX            YES                                          
         STC   R2,FLDH+5           SET NEW INPUT LENGTH                         
         B     OPERAND4                                                         
*                                                                               
OPERAND2 MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'UNPAIRED),UNPAIRED                                        
         SR    R1,R1                                                            
         ICM   R1,7,FLAST                                                       
         AR    R1,R2               POINT AT LAST BYTE                           
         STCM  R1,7,FLAST                                                       
         B     OPERANDX                                                         
*                                                                               
OPERAND4 MVI   ERROR,LENERR                                                     
         CLI   FLDH+5,L'BUKCODE    TEST MAX LEN FOR OPERAND                     
         BH    OPERANDX                                                         
         MVI   BYTE,BUROPOUT                                                    
         GOTO1 VFINDOUT,PARAS,FLD,AIO2                                          
         BE    OPERAND5                                                         
*                                                                               
         MVI   ERROR,OUTERR                                                     
         CLI   NOPER,1             TEST EDITING SECOND OPERAND                  
         BNE   *+8                                                              
         MVI   ERROR,INVALID                                                    
         B     OPERANDX                                                         
*                                                                               
OPERAND5 CLI   NOPER,1                                                          
         BE    OPERAND6                                                         
         BL    OPERAND7                                                         
         CLI   CONFLAG,YES         TEST IF CONSTANT ALREADY FOUND               
         BNE   OPERAND7            NO                                           
         MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'AFTERMSG),AFTERMSG                                        
         B     OPERANDX                                                         
*                                                                               
OPERAND6 XC    FTERM,FTERM         SECOND OPERAND - RESET OPERATIONS            
         MVC   FTERM(L'OPERS2),OPERS2 TO SEARCH FOR                             
         TM    BYTE,BUROPOUT       TEST IF OUTLINE FOUND                        
         BZ    OPERAND7            NO                                           
         CLI   OPERTN,C'*'         TEST IF FIRST OPERATION IS MULTIPLY          
         BE    *+12                                                             
         CLI   OPERTN,C'/'         OR DIVIDE                                    
         BNE   OPERAND7                                                         
         MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'OPERMSG),OPERMSG                                          
         B     OPERANDX                                                         
*                                                                               
OPERAND7 L     R3,AIO3             INITIALIZE OPERAND LIST POINTER              
         CLI   NOPER,0             TEST FOR FIRST ENTRY                         
         BE    *+12                                                             
         L     R3,AOPER            GET POINTER TO LAST ENTRY                    
         LA    R3,BUROPL1Q(R3)     R3=A(NEW ENTRY)                              
         ST    R3,AOPER                                                         
*                                                                               
         ZIC   R1,NOPER            INCREMENT OPERAND COUNT                      
         LA    R1,1(R1)                                                         
         STC   R1,NOPER                                                         
         MVI   ERROR,NUMOPERR                                                   
         CLI   NOPER,MAXOPS        TEST FOR TOO MANY OPERANDS                   
         BH    OPERANDX                                                         
*                                                                               
         USING BUROPD,R3                                                        
         XC    0(BUROPL1Q+1,R3),0(R3) CLEAR ELEMENT AREA                        
         MVI   BUROPEL,BUROPELQ                                                 
         MVC   BUROPN,NOPER        SET OPERAND NUMBER                           
         TM    BYTE,BUROPOUT       TEST IF OUTLINE FOUND                        
         BZ    OPERAND8            NO                                           
*                                                                               
         MVI   BUROPLEN,BUROPL1Q   ELEMENT LENGTH FOR OUTLINE                   
         MVI   BUROPCTL,BUROPOUT   OPERAND IS OUTLINE CODE/OPER                 
         MVC   BUROPCOD,FLD                                                     
         MVC   BUROPER,OPERTN                                                   
         B     OPERAND9                                                         
*                                                                               
OPERAND8 TM    BYTE,BUROPCNS       TEST FOR CONSTANT FOUND                      
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   BUROPLEN,BUROPL2Q   ELEMENT LENGTH FOR CONSTANT                  
         MVC   BUROPCTL,DUB        SET EXPONENT INTO CONTROL BYTE               
         OI    BUROPCTL,BUROPCNS   SET FOR CONSTANT                             
         ZAP   BUROPCON,DUB+1(7)                                                
         MVC   BUROPER2,OPERTN                                                  
*                                                                               
OPERAND9 MVI   ERROR,0                                                          
*                                                                               
OPERANDX CLI   ERROR,0                                                          
         XIT1  REGS=(R3)           RETURN ENTRY POINTER TO USER                 
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CONSTRUCT A POLISH FORMULA ELEMENT FROM ROW OPERAND            
* ELEMENT LIST                                                                  
*                                                                               
POLISH   NTR1                                                                   
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING BUPOLD,R6                                                        
         MVI   BUPOLEL,BUPOLELQ                                                 
         LA    R2,BUPOLISH         R2=POLISH STRING POINTER                     
         L     R3,AIO3             R3=A(ROW OPERAND ELEMENT)                    
         USING BUROPD,R3                                                        
         ZIC   R0,NOPER            R0=N'ELEMENTS                                
         SR    RE,RE                                                            
*                                                                               
POLISH1  MVC   0(1,R2),BUROPN      PROCESS FIRST ELEMENT                        
         LA    R2,1(R2)                                                         
         SH    R0,=H'1'                                                         
         BZ    POLISH4                                                          
*                                                                               
POLISH2  LA    R3,BUROPL1Q(R3)     BUMP TO NEXT ELEMENT                         
         MVC   0(1,R2),BUROPN      SET OPERAND NUMBER                           
         MVC   1(1,R2),BUROPER     AND OPERATION                                
         TM    BUROPCTL,BUROPCNS   TEST FOR CONSTANT OPERAND                    
         BZ    *+10                                                             
         MVC   1(1,R2),BUROPER2    TAKE OPERATION FROM DIFFERENT PLACE          
         LA    R2,2(R2)                                                         
         BCT   R0,POLISH2                                                       
*                                                                               
POLISH4  MVI   0(R2),EQUALS        SET EQUALS AT END OF STRING                  
         LA    R2,1(R2)                                                         
         LA    RF,BUPOLISH                                                      
         SR    R2,RF               R2=L'POLISH STRING                           
         LA    R2,BUPOLISH-BUPOLD(R2)                                           
         STC   R2,BUPOLEN                                                       
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT PRINT CONTROL OPTIONS                                     
* AT ENTRY R6=A(OUTLINE ELEMENT)                                                
*                                                                               
* P1 = A(OPTION FIELD HEADER)                                                   
* P2 = A(OPTION TABLE)                                                          
*                                                                               
OPTED    NTR1                                                                   
         USING BUOUTD,R6                                                        
         LM    R2,R3,0(R1)         R2=A(OPTION FLDH),R3=A(OPTION TABLE)         
         ST    R3,AOPTTAB          SAVE A(OPTION TABLE)                         
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0                                                         
         BE    OPTEDX                                                           
         XC    FLAST,FLAST         START EDIT AT BEGINNING OF FIELD             
*                                                                               
OPTED2   XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'=,'     LOOK FOR EQUALS SIGN OR COMMA                
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
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         L     R3,AOPTTAB          R3=A(OPTION TABLE)                           
         USING OPTTABD,R3                                                       
*                                                                               
OPTED6   CLI   OPTNAME,X'FF'       TEST EOT                                     
         BE    OPTEDR                                                           
         CLC   FLDH+5(1),OPTMINL   TEST FOR MINIMUM LENGTH FOR THIS KEY         
         BL    OPTED6A             NO                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),OPTNAME                                                   
         BE    OPTED7              VALID KEYWORD                                
OPTED6A  LA    R3,OPTTABL(R3)                                                   
         B     OPTED6                                                           
*                                                                               
OPTED7   XC    FTERM,FTERM         EXTRACT PARAMETER VALUE                      
         MVI   FTERM,COMMA                                                      
         GOTO1 VFVAL                                                            
         CLI   FLDH+5,0            TEST FOR VALUE FOUND                         
         BNE   OPTED8              YES                                          
         TM    OPTCTL,KEYONLY      TEST FOR KEYWORD ONLY OPTION                 
         BO    OPTED8              YES-OK TO HAVE NO PARMETER VALUE             
         MVC   XTRA(12),=C'NO VALUE FOR'                                        
         MVC   XTRA+13(L'OPTNAME),OPTNAME                                       
         B     OPTEDR                                                           
*                                                                               
OPTED8   SR    RF,RF                                                            
         ICM   RF,3,OPTAED         GET DISP TO VALIDATION ROUTINE               
         A     RF,MYBASE                                                        
         BASR  RE,RF                                                            
         B     OPTED2                                                           
*                                                                               
OPTEDX   B     XIT                                                              
         SPACE 2                                                                
OPTEDR   B     SPERR                                                            
         EJECT                                                                  
* PRINT CONTROL OPTIONS VALUE EDITS                                             
*                                                                               
OPTNO    ST    RE,SAVERE           EDIT NO/YES                                  
         CLI   FLDH+5,3                                                         
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         OC    BUOUTIND(1),OPTEQU                                               
         EX    R1,NOCOMP                                                        
         BE    OPTX                                                             
         MVC   BYTE,OPTEQU         PREPARE 'AND' MASK FOR OPTION                
         XI    BYTE,X'FF'                                                       
         NC    BUOUTIND(1),BYTE    TURN OFF OPTION BIT                          
         EX    R1,YESCOMP                                                       
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
* EDIT EJECT VALUE (NO VALUE MEANS TO SET EJECT BIT)                            
*                                                                               
OPTEJECT ST    RE,SAVERE                                                        
         OI    BUOUTIND,BUOUTEJ    TURN ON EJECT BIT                            
         CLI   FLDH+5,0            TEST FOR NO VALUE                            
         BE    OPTX                YES-EXIT RIGHT NOW                           
         CLI   FLDH+5,3            TEST FOR MAX LEN OF 3                        
         BH    OPTEDR                                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BE    OPTX                                                             
         NI    BUOUTIND,X'FF'-BUOUTEJ                                           
         EX    R1,NOCOMP                                                        
         BE    OPTX                                                             
         B     OPTEDR                                                           
         SPACE 2                                                                
* EDIT SPACE BEFORE AND SPACE AFTER                                             
*                                                                               
OPTBEF   ST    RE,SAVERE                                                        
         LA    RE,BUOUTBEF                                                      
         B     OPTNUM1                                                          
*                                                                               
OPTAFT   ST    RE,SAVERE                                                        
         LA    RE,BUOUTAFT                                                      
*                                                                               
OPTNUM1  CLI   FLDH+5,1            TEST FOR 1 DIGIT                             
         BNE   OPTEDR                                                           
         MVI   ERROR,NOTNUM                                                     
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    OPTEDR                                                           
         MVI   ERROR,INVALID                                                    
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    OPTEDR                                                           
         CH    R1,=H'4'            MAXIMUM OF SPACE '4'                         
         BH    OPTEDR                                                           
         STC   R1,0(RE)                                                         
         B     OPTX                                                             
         SPACE 2                                                                
OPTX     L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
NOCOMP   CLC   FLD(0),=C'NO '                                                   
YESCOMP  CLC   FLD(0),=C'YES'                                                   
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY PRINT CONTROL OPTIONS                                  
* AT ENTRY, P1=A(OUTPUT AREA)                                                   
*                                                                               
DISOPT   NTR1                                                                   
         L     R6,0(R1)            R6=A(OUTPUT)                                 
         L     R4,AIO3                                                          
         MVI   0(R4),C' '                                                       
         MVC   1(255,R4),0(R4)                                                  
         LA    R3,OPTTAB           R3=A(OPTION TABLE)                           
         USING OPTTABD,R3                                                       
         SR    R5,R5               R5=N'ACTIVE OPTIONS                          
*                                                                               
DISOPT2  CLI   OPTNAME,X'FF'       TEST FOR EOT                                 
         BE    DISOPT8                                                          
         SR    R1,R1                                                            
         ICM   R1,1,OPTEQU                                                      
         BZ    DISOPT3             NOT A BIT SETTING OPTION                     
         EX    R1,TESTOPT          TEST IF OPTION ACTIVE                        
         BZ    DISOPT6             NO                                           
*                                                                               
         MVC   0(L'OPTNAME,R4),OPTNAME                                          
         TM    OPTCTL,KEYONLY      TEST FOR KEYWORD ONLY OPTION                 
         BO    *+10                                                             
         MVC   10(L'OPTPARM,R4),OPTPARM                                         
         B     DISOPT4                                                          
*                                                                               
DISOPT3  SR    RE,RE                                                            
         ICM   RE,3,OPTAVAL                                                     
         LA    RE,SYSD(RE)                                                      
         CLI   0(RE),0                                                          
         BE    DISOPT6                                                          
         MVC   0(L'OPTNAME,R4),OPTNAME                                          
         TM    OPTCTL,NUM1         TEST 1 BYTE NUMERIC VALUE                    
         BZ    DISOPT4                                                          
         ZIC   R0,0(RE)            GET VALUE                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  10(1,R4),DUB+7(1)                                                
*                                                                               
DISOPT4  LA    R5,1(R5)            BUMP OPTION COUNT                            
         LA    R4,20(R4)                                                        
*                                                                               
DISOPT6  LA    R3,OPTTABL(R3)                                                   
         B     DISOPT2                                                          
*                                                                               
DISOPT8  LTR   R5,R5               TEST IF ANY ACTIVE OPTIONS                   
         BZ    DISOPTX                                                          
         GOTO1 UNSCAN,DMCB,((R5),AIO3),(R6),0                                   
*                                                                               
DISOPTX  B     XIT                                                              
         SPACE 2                                                                
TESTOPT  TM    OUTIND,0                                                         
         SPACE 2                                                                
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO MAINTAIN PASSIVE POINTER ELEMENT                               
*                                                                               
POINTEL  NTR1                                                                   
         MVI   ELCODE,BUPTRELQ                                                  
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING BUPTRD,R6                                                        
         MVI   BUPTREL,BUPTRELQ                                                 
         MVI   BUPTRLEN,BUPTRLNQ                                                
         LA    R1,BUPOINT                                                       
         USING BUCRECD,R1                                                       
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVC   BUCSYS,BUKSYS                                                    
         MVC   BUCAGY,BUKAGY                                                    
         MVI   BUCRTYP,BUCRTYPQ                                                 
         MVC   BUCCLT,CLTCODE                                                   
         MVC   BUCPRD,PRDCODE                                                   
         MVC   BUCPLAN,PLANCODE                                                 
         MVC   BUCCODE,OUTCODE                                                  
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
         DROP  R1,R4                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO BUILD A PASSIVE POINTER KEY                                    
* AT ENTRY P1=A(OUTPUT)                                                         
*                                                                               
PASSKEY  NTR1                                                                   
         L     R2,0(R1)                                                         
         USING BUCRECD,R2                                                       
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         MVI   ELCODE,BUPTRELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUCKEY,BUPOINT-BUPTREL(R6) EXTRACT PASSIVE KEY                   
         MVC   BUCCTL,BURCTL       GET CONTROL VALUES FROM RECORD               
         LA    R3,NDLVTAB-NODBLKD(R5) R3=A(START OF LEVEL TABLE)                
         ZIC   R1,NDLEV                                                         
         MH    R1,=Y(NDLVTABL)     DEVELOP INDEX INTO LEVEL TABLE               
         LA    R3,0(R1,R3)         R3 POINTS TO LEVEL TABLE FOR OUTLINE         
         USING NDLVTABD,R3                                                      
         MVC   BUCDA,NDLVDA        EXTRACT DISK ADDRESS FOR LEVEL TABLE         
         CLC   BUCCODE,BUKCODE     TEST FOR MATCH ON OUTLINE CODES              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO HANDLE I/O ON NEW POINTER                                      
* AT ENTRY, P1=A(PASSIVE POINTER)                                               
*                                                                               
NEWPTR   NTR1                                                                   
         L     R2,0(R1)            R2=A(POINTER)                                
         XC    KEY,KEY                                                          
         MVC   KEY(PASSLEN),0(R2)                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUKEY),KEYSAVE                                             
         BE    NEWPTR2                                                          
         MVC   KEY(PASSLEN),0(R2)                                               
         GOTO1 ADD                                                              
         B     NEWPTRX                                                          
*                                                                               
NEWPTR2  MVC   KEY(PASSLEN),0(R2)                                               
         GOTO1 WRITE                                                            
*                                                                               
NEWPTRX  NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETES                        
         B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO DELETE A PASSIVE POINTER                                       
* AT ENTRY, P1 = A(POINTER)                                                     
*                                                                               
DELPTR   NTR1                                                                   
         L     R2,0(R1)                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'BUKEY),0(R2)                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUKEY),KEYSAVE                                             
         BNE   DELPTRX                                                          
         OI    KEY+(BUKCTL-BUKEY),X'80'                                         
         GOTO1 WRITE                                                            
*                                                                               
DELPTRX  B     XIT                                                              
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
BUMPU    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         CLI   0(R2),0             TEST FOR EOS                                 
         BER   RE                                                               
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    BUMPU                                                            
         LTR   RE,RE               SET CC=NEQ FOR ANOTHER FIELD                 
         BR    RE                                                               
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
WHENERR  LA    R2,CONWHENH                                                      
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         MVI   ERROR,NOWERR                                                     
         B     SPERR                                                            
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
ADDMSG   DC    C'OUTLINE RECORD ADDED TO FILE'                                  
DELMSG   DC    C'OUTLINE RECORD DELETED'                                        
RESMSG   DC    C'OUTLINE RECORD HAS BEEN RESTORED'                              
AFTERMSG DC    C'** ERROR - NUMBER SHOULD END FORMULA '                         
OPERMSG  DC    C'** ERROR - ONLY A NUMBER CAN FOLLOW ''*'' OR ''/'' '           
UNPAIRED DC    C'** ERROR - LOOKING FOR '' HERE'                                
OPERS1   DC    C'+-*/'                                                          
OPERS2   DC    C'+-'                                                            
MISSPARM DC    C'M'                                                             
         EJECT                                                                  
* TABLE OF PRINT CONTROL OPTIONS (COVERED BY OPTTABD)                           
*                                                                               
OPTTAB   DS    0CL(OPTTABL)                                                     
*                                                                               
         DC    CL8'SUM     ',AL1(1),AL1(0),AL1(BUOUTNSU)                        
         DC    AL2(OUTIND-SYSD),AL2(OPTNO-T50205),CL3'NO '                      
*                                                                               
         DC    CL8'PRINT   ',AL1(1),AL1(0),AL1(BUOUTNPR)                        
         DC    AL2(OUTIND-SYSD),AL2(OPTNO-T50205),CL3'NO '                      
*                                                                               
         DC    CL8'EJECT   ',AL1(1),AL1(KEYONLY),AL1(BUOUTEJ)                   
         DC    AL2(OUTIND-SYSD),AL2(OPTEJECT-T50205),CL3'YES'                   
*                                                                               
         DC    CL8'TOTAL   ',AL1(1),AL1(0),AL1(BUOUTNTO)                        
         DC    AL2(OUTIND-SYSD),AL2(OPTNO-T50205),CL3'NO '                      
*                                                                               
         DC    CL8'BEFORE  ',AL1(1),AL1(NUM1),AL1(0)                            
         DC    AL2(OUTBEF-SYSD),AL2(OPTBEF-T50205),CL3'   '                     
*                                                                               
         DC    CL8'AFTER   ',AL1(1),AL1(NUM1),AL1(0)                            
         DC    AL2(OUTAFT-SYSD),AL2(OPTAFT-T50205),CL3'   '                     
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* REPORT SPEC POOL                                                              
*                                                                               
HEDSPECS DS    0H                                                               
*&&US*&& SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
*&&UK*&& SSPEC H1,2,C'ABS - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,48,C'OUTLINE REPORT'                                          
         SSPEC H2,48,C'--------------'                                          
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,2,C'CLIENT'                                                   
         SSPEC H5,2,C'PRODUCT'                                                  
         SSPEC H6,2,C'PLAN'                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,100,PAGE                                                      
         SSPEC H9,2,C'CODES'                                                    
         SSPEC H9,16,C'DESCRIPTION'                                             
         SSPEC H9,42,C'RULES/FORMULA'                                           
         SSPEC H9,99,C'CONTROL OPTIONS'                                         
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF5D                                                       
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
VEDRULES DS    V                   V(EDRULES)                                   
BEFORE   DS    CL(L'BUKCODE)       'ADD BEFORE' CODE                            
NEXTLEV  DS    X                   NEXT OUTLINE LEVEL                           
NEXTNODE DS    XL4                 NODE ESTABLISHED BY PARENT                   
DSKADDR  DS    XL4                 SAVED DISK ADDRESS FOR DISPLAY               
PARCODE  DS    CL(L'BUKCODE)       PARENT RECORD CODE                           
OLDPASS  DS    CL(PASSLEN)         OLD PASSIVE POINTER                          
NEWPASS  DS    CL(PASSLEN)         NEW PASSIVE POINTER                          
SAVOUTEL DS    CL(BUOUTLNQ)        OUTLINE ELEMENT SAVE AREA                    
*                                                                               
NOPER    DS    X                   N'OPERANDS IN OPLIST                         
LASTSTOP DS    C                   LAST STOP CHARACTER IN SCAN                  
OPERTN   DS    C                   OPERATION                                    
CONFLAG  DS    C                   Y=CONSTANT FOUND                             
AOPER    DS    A                   A(CURRENT OPERAND ELEMENT)                   
*                                                                               
RULESW   DS    C                   Y=OUTLINE HAS RULES                          
*                                                                               
FIELDSW  DS    C                   R=RULES, F=FORMULA                           
EMPTYSW  DS    C                                                                
INPUTSW  DS    C                                                                
*                                                                               
AOPTTAB  DS    A                   A(OPTION TABLE)                              
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
       ++INCLUDE BUFILOUTSV                                                     
* DSECT TO COVER PRINT CONTROL OPTION TABLE                                     
*                                                                               
OPTTABD  DSECT                                                                  
OPTNAME  DS    CL8                 NAME                                         
OPTMINL  DS    X                   MINIMUM LENGTH OF VALID KEYWORD              
OPTCTL   DS    X                   CONTROL VALUES                               
*                                  X'80'=KEYWORD ONLY OPTION                    
*                                  X'01'=1 DIGIT NUMERIC VALUE                  
OPTEQU   DS    X                   BIT SETTING FOR BUOUTIND                     
OPTAVAL  DS    AL2                 DISPL. TO VALUE IN SYSD                      
OPTAED   DS    AL2                 DISPL. TO PARM VALIDATION RTN.               
OPTPARM  DS    CL3                 PARM VALUE FOR DISPLAY                       
OPTTABL  EQU   *-OPTTABD           TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    C                                                                
PRTCODE  DS    CL13                                                             
         DS    C                                                                
PRTDESC  DS    CL25                                                             
         DS    C                                                                
PRTRUFO  DS    CL56                                                             
         DS    C                                                                
PRTCON   DS    CL20                                                             
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
PASSLEN  EQU   BUFRSTEL-BUKEY      L'PASSIVE POINTER                            
PLUS     EQU   C'+'                                                             
MINUS    EQU   C'-'                                                             
COMMA    EQU   C','                                                             
QUOTE    EQU   C''''                                                            
OPLISTL  EQU   BUROPER+L'BUROPER-BUROPD                                         
MAXOPS   EQU   32                  MAXIMUM OPERANDS IN ROW FORMULA              
KEYONLY  EQU   X'80'                                                            
NUM1     EQU   X'01'                                                            
PF9      EQU   9                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027BUFIL05   05/01/02'                                      
         END                                                                    
