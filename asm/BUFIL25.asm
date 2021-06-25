*          DATA SET BUFIL25    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T50225A                                                                  
         TITLE 'T50225 - BUDGET CONTROL LFM - OUTLINE COPY OVERLAY'             
T50225   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI25**,RA,RR=R2                                              
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
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BE    OCY                                                              
         CLI   MODE,VALREC                                                      
         BE    OCY4                COPY OUTLINE RECORDS                         
         B     XIT                                                              
*                                                                               
* VALKEY LOGIC                                                                  
*                                                                               
OCY      MVC   ANODBLK2,ATIA       BORROW TIA FOR SECOND NODIO BLOCK            
         MVC   HALF(1),ACTNUM      ACTION NUMBER                                
         MVC   HALF+1(1),RECNUM    RECORD NUMBER                                
         CLC   HALF,TWALACT        TEST FOR CHANGE IN RECORD/ACTION             
         BE    *+10                NO                                           
         XC    SVCTLVAL,SVCTLVAL   YES-FORCE FIRST TIME FOR COPY                
         BAS   RE,VKEY             VALIDATE KEY FIELDS ON SCREEN                
         MVI   INTMODE,NEXT                                                     
         CLC   CTLVALS(CTLVALN1),SVCTLVAL                                       
         BE    *+8                                                              
         MVI   INTMODE,FIRST       FORCE COPY TO START OVER                     
*                                                                               
         CLI   INTMODE,NEXT        TEST CONTINUING COPY                         
         BNE   OCY2                                                             
         CLC   ASSIGN,SVASSIGN     TEST IF CODE ASSIGN CHANGED                  
         BE    OCY2                NO                                           
*                                                                               
         LA    R2,OCYASSH                                                       
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
OCY2     MVC   SVCTLVAL,CTLVALS    REFRESH CONTROL VALUES                       
         B     XIT                                                              
*                                                                               
OCY4     CLI   SVASSIGN,YES        TEST FOR AUTOMATIC CODE ASSIGN               
         BNE   OCY6                NO                                           
*                                                                               
* AUTOMATIC CODE ASSIGN                                                         
*                                                                               
         GOTO1 VCLEARF,DMCB,(1,OCYNEW1H),OCYLAST                                
         GOTO1 (RF),(R1),(0,OCYNEW1H),OCYLAST                                   
         MVC   SVLEVADJ,LEVADJ                                                  
         BAS   RE,PRE              PREVIEW THE COPY                             
         BAS   RE,FSTNUM           FIND FIRST OUTLINE NUMBER                    
         ZIC   R4,SVNENTS          COPY ALL OUTLINES                            
         GOTO1 COPY,PARAS,1,(R4)                                                
         B     OCY10               WRAP UP W COPY COMPLETED LOGIC               
*                                                                               
OCY6     TM    INTMODE,NEXT        TEST FOR COPY                                
         BO    OCY8                YES                                          
*                                                                               
* FIRST TIME LOGIC                                                              
*                                                                               
         GOTO1 VCLEARF,DMCB,(1,OCYNEW1H),OCYLAST                                
         GOTO1 (RF),(R1),(0,OCYNEW1H),OCYLAST                                   
         MVC   SVLEVADJ,LEVADJ     SAVE LEVEL ADJ                               
         BAS   RE,PRE              PREVIEW THE COPY                             
         LA    R3,1                                                             
         GOTO1 DISOUT,PARAS,(R3)                                                
         BNE   *+6                 MUST BE AT LEAST ONE                         
         DC    H'0'                                                             
         STC   R3,SVFIRST          SET FIRST ENTRY NUMBER                       
         MVC   CONHEAD(L'FIRSTMSG),FIRSTMSG                                     
         LA    R2,OCYNEW1H                                                      
         OI    6(R2),X'80'         XMIT BACK                                    
         ST    R2,ACURFORC                                                      
         B     OCYX                                                             
*                                                                               
* CONTINUING WITH COPY                                                          
*                                                                               
OCY8     ZIC   R3,SVFIRST                                                       
         GOTO1 CHECK,PARAS,(R3)                                                 
         ZIC   R4,SVNOUTS                                                       
         GOTO1 COPY,PARAS,(R3),(R4)                                             
         AR    R3,R4               FIND NEXT OUTLINE                            
         STC   R3,SVFIRST          UPDATE FIRST ENTRY                           
         LA    R2,OCYNEW1H                                                      
         GOTO1 VCLEARF,PARAS,(1,(R2)),OCYLAST                                   
         GOTO1 VCLEARF,PARAS,(0,(R2)),OCYLAST                                   
         GOTO1 DISOUT,PARAS,(R3)                                                
         BE    OCY10               NO MORE OUTLINES                             
*                                                                               
         MVC   CONHEAD(L'NEXTMSG),NEXTMSG                                       
         LA    R2,OCYNEW1H                                                      
         OI    6(R2),X'80'                                                      
         ST    R2,ACURFORC                                                      
         B     OCYX                                                             
*                                                                               
* COMPLETED COPY LOGIC                                                          
*                                                                               
OCY10    XC    SVCTLVAL,SVCTLVAL   CLEAR CONTROLS                               
         LA    R2,OCYCLTH                                                       
         OI    6(R2),X'80'                                                      
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'COPYMSG),COPYMSG                                       
         LA    R3,CONHEAD+L'COPYMSG+1                                           
         MVI   0(R3),C'-'                                                       
         LA    R3,2(R3)                                                         
         ZIC   R0,SVNENTS                                                       
         EDIT  (R0),(2,(R3)),ALIGN=LEFT                                         
         AR    R3,R0                                                            
         LA    R3,1(R3)                                                         
         MVC   0(15,R3),=C'OUTLINES COPIED'                                     
*                                                                               
OCYX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO VALIDATE KEY FIELDS ON SCREEN                                  
*                                                                               
VKEY     NTR1                                                                   
         BAS   RE,CLRNAME                                                       
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
         GOTO1 VVALCLT,PARAS,OCYCLTH,0                                          
         MVC   CLT,CLTCODE         SAVE CLIENT CODE                             
         MVC   OCYCLN,CLTNAM                                                    
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
VKEY2    GOTO1 VVALPRD,PARAS,OCYPRDH,0                                          
         MVC   PRD,PRDCODE         SAVE PRODUCT CODE                            
         MVC   OCYPRN,PRDNAM                                                    
*                                                                               
* VALIDATE PLAN                                                                 
*                                                                               
VKEY4    GOTO1 VVALPLAN,PARAS,OCYPLAH,0                                         
         MVC   PLAN,PLANCODE                                                    
         MVC   SVPLNKEY,NODKEY     SAVE PLAN NODAL KEY                          
         MVC   OCYPLN(L'PLANNAM),PLANNAM                                        
         MVC   ATTKEY,SVPLNKEY     INITIALIZE ATTACH KEY                        
         MVI   NEXTLEV,1           DEFAULT NEXT LEVEL=1                         
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         LA    R3,NDLVTABL(R3)                                                  
         MVC   NEXTNODE,NDLVNOD    EXTRACT NODE ESTABLISHED BY PLAN             
*                                                                               
* EDIT AND VALIDATE ATTACH TO POINT                                             
*                                                                               
VKEY6    LA    R2,OCYATTH                                                       
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         CLI   FLDH+5,0                                                         
         BE    VKEY8                                                            
*                                                                               
         MVC   ATTACH,FLD          EXTRACT ATTACH TO CODE                       
         GOTO1 VFINDOUT,PARAS,ATTACH,NDIOA                                      
         BNE   TRAPERR             INVALID PARENT                               
         GOTO1 VTRACE              GET NODAL KEY                                
         MVC   ATTKEY,NODKEY       SAVE NODAL KEY                               
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         GOTO1 VGETVAL                                                          
         ZIC   R1,OUTLEV                                                        
         LA    R1,1(R1)            INCREMENT OUTLINE LEVEL                      
         STC   R1,NEXTLEV          SET NEXT OUTLINE LEVEL                       
         MVI   ERROR,LEVERR                                                     
         CLI   NEXTLEV,MAXOUTS     TEST OVERFLOWED LEVEL LIMIT                  
         BH    TRAPERR             YES                                          
         LA    R3,NDLVTABL(R3)     NEXT LEVEL TABLE ENTRY                       
         MVC   NEXTNODE,NDLVNOD    EXTRACT NODE ESTABLISHED BY PARENT           
         MVC   OCYATTN(L'OCYATTN),OUTNAME EXTRACT PARENT OUTLINE'S NAME         
*                                                                               
* EDIT ADD BEFORE POINT                                                         
*                                                                               
VKEY8    LA    R2,OCYBEFH                                                       
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         CLI   FLDH+5,0                                                         
         BE    VKEY10                                                           
         MVC   BEFORE,FLD          EXTRACT BEFORE OUTLINE CODE                  
*                                                                               
         GOTO1 VFINDOUT,PARAS,BEFORE,NDIOA                                      
         BNE   TRAPERR                                                          
         MVI   ERROR,NOTCHILD                                                   
         L     R4,AIO              MAKE THE RECORD ADDRESSABLE                  
         USING BURECD,R4                                                        
         CLC   BUKNODE,NEXTNODE    TEST THAT RECORD IS CHILD                    
         BNE   TRAPERR                                                          
*                                                                               
* EDIT COPY FROM OUTLINE                                                        
*                                                                               
VKEY10   LA    R2,OCYFROMH                                                      
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   FROM,FLD                                                         
         GOTO1 VFINDOUT,PARAS,FROM,NDIOA                                        
         BNE   TRAPERR                                                          
         MVI   ERROR,OVRLPERR                                                   
         CLC   ATTACH,FROM         TEST FOR OVERLAP                             
         BE    TRAPERR                                                          
*                                                                               
         GOTO1 VTRACE                                                           
         MVC   FRKEY,NODKEY                                                     
         GOTO1 VGETVAL                                                          
         MVC   FRLEV,OUTLEV                                                     
         ZIC   R0,OUTLEV                                                        
         ZIC   R1,NEXTLEV          COMPUTE LEVEL ADJUSTMENT                     
         SR    R1,R0               =ATTACH LEVEL-FROM LEVEL                     
         STH   R1,LEVADJ                                                        
         MVC   OCYFRNM(L'OCYFRNM),OUTNAME                                       
*                                                                               
* EDIT COPY UNTIL OUTLINE                                                       
*                                                                               
VKEY12   LA    R2,OCYUPTOH                                                      
         GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   UNTIL,FLD           EXTRACT OUTLINE CODE                         
*                                                                               
         GOTO1 VFINDOUT,DMCB,UNTIL,NDIOA                                        
         BNE   TRAPERR                                                          
         GOTO1 VGETVAL                                                          
*                                                                               
         MVI   ERROR,OVRLPERR                                                   
         CLC   ATTACH,UNTIL                                                     
         BE    TRAPERR                                                          
         MVC   OCYUPNM(L'OCYUPNM),OUTNAME                                       
*                                                                               
* EDIT ASSIGN CODES FIELD                                                       
*                                                                               
VKEY14   LA    R2,OCYASSH                                                       
         MVI   ASSIGN,NO                                                        
         GOTO1 VGETFLD,DMCB,(R2)                                                
         CLI   FLDH+5,0                                                         
         BE    VKEY16                                                           
         MVC   ASSIGN,FLD                                                       
         MVI   ERROR,INVALID                                                    
         CLI   ASSIGN,NO                                                        
         BE    VKEY16                                                           
         CLI   ASSIGN,YES                                                       
         BNE   TRAPERR                                                          
         MVI   ERROR,COASSERR                                                   
         TM    PLANIND,BUPLNCOD    TEST PLAN ENABLED FOR CODE ASSIGN            
         BZ    TRAPERR             NO                                           
*                                                                               
* EDIT COPY RULES FIELD                                                         
*                                                                               
VKEY16   LA    R2,OCYRULH                                                       
         GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   RULES,NO            DEFAULT IS TO DROP RULES                     
         CLI   FLDH+5,0                                                         
         BE    VKEYX                                                            
         MVI   ERROR,INVALID                                                    
         MVC   RULES,FLD                                                        
         CLI   RULES,YES                                                        
         BE    VKEYX                                                            
         CLI   RULES,NO                                                         
         BNE   TRAPERR                                                          
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PREVIEW THE COPY CHECKING FOR ERROR CONDITIONS                 
* AND BUILDING A SAVE TABLE OF THE OUTLINES TO BE COPIED                        
*                                                                               
PRE      NTR1                                                                   
         MVC   NOUTS,PLANCNT       INITIALIZE N'OUTLINES ON PLAN                
         MVI   SVNENTS,0           INITIALIZE TABLE ENTRY COUNT                 
         LA    R6,SVTAB                                                         
         ST    R6,ATHISENT         SET ENTRY POINTER                            
         LR    RE,R6                                                            
         LA    RF,SVTABX-SVTAB                                                  
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR THE TABLE                              
*                                                                               
PRE1     MVC   NODKEY,FRKEY        COPY FROM OUTLINE                            
         LA    R1,PREHK                                                         
         ST    R1,NDHOOK                                                        
         MVI   RECLIM,MAXAUTO      INITIALIZE RECORD LIMIT                      
         CLI   SVASSIGN,YES        TEST FOR AUTOMATIC CODE ASSIGN               
         BE    *+8                                                              
         MVI   RECLIM,MAXMAN       NO-MANUAL COPY                               
*                                                                               
PRE2     GOTO1 VNODIO,DMCB,(R5),=C'READ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BE    PRE4                                                             
         LA    R2,OCYFROMH                                                      
         GOTO1 VNODERR                                                          
*                                                                               
PRE4     MVI   NDSQBACK,3                                                       
         GOTO1 VNODIO,DMCB,(R5),=C'LSEQ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BE    PRE6                                                             
         LA    R2,OCYFROMH                                                      
         GOTO1 VNODERR                                                          
*                                                                               
PRE6     CLI   UNTILSW,YES         TEST COPY UNTIL FOUND                        
         BE    PREX                YES                                          
         LA    R2,OCYUPTOH                                                      
         MVI   ERROR,UNTILERR                                                   
         B     TRAPERR                                                          
*                                                                               
PREX     B     XIT                                                              
         SPACE 2                                                                
* PREVIEW HOOK ROUTINE                                                          
*                                                                               
PREHK    ST    RE,SAVERE                                                        
         CLI   NDMODE,NDPROC                                                    
         BNER  RE                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCOUT                                                  
         BZR   RE                                                               
*                                                                               
PREHK2   GOTO1 VGETVAL                                                          
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         CLC   SVATTACH,NDLVCOD    TEST FOR COPY OVERLAP                        
         BE    OVERLAPR                                                         
*                                                                               
         ZIC   R2,OUTLEV           GET EXISTING LEVEL                           
         AH    R2,SVLEVADJ         APPLY ADJUSTMENT                             
         CH    R2,=H'1'            TEST VALIDITY OF NEW LEVEL                   
         BL    LEVHIGHR                                                         
         CH    R2,=Y(MAXOUTS)                                                   
         BH    LEVLOWR                                                          
*                                                                               
PREHK4   LH    R1,NOUTS                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=Y(OUTLIMIT)                                                  
         BH    TOOMANYR            OUTLINE LIMIT BLOWN                          
         STH   R1,NOUTS                                                         
*                                                                               
PREHK6   ZIC   R1,SVNENTS          INCREMENT TABLE ENTRY COUNT                  
         LA    R1,1(R1)                                                         
         STC   R1,SVNENTS                                                       
         CLC   SVNENTS,RECLIM                                                   
         BH    TOOBIGR             COPY TOO BIG OR DIDN'T FIND UNTIL            
*                                                                               
PREHK8   L     R6,ATHISENT                                                      
         USING SVTABD,R6                                                        
         MVC   SVNODE,NDLVNOD      EXTRACT NODE/CODE/DA/LEVEL                   
         MVC   SVCODE,NDLVCOD                                                   
         MVC   SVDA,NDLVDA                                                      
         MVC   SVLEV,OUTLEV                                                     
         LA    R6,SVTABL(R6)       ADVANCE POINTER                              
         ST    R6,ATHISENT                                                      
         CLC   SVUNTIL,NDLVCOD     TEST COPY UNTIL REACHED                      
         BNE   PREHKX                                                           
         MVI   UNTILSW,YES         NOTE COPY UNTIL FOUND                        
         MVI   NDMODE,NDEND        FORCE READ TO END                            
*                                                                               
PREHKX   L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
OVERLAPR MVI   ERROR,OVRLPERR                                                   
         B     PREHKR                                                           
*                                                                               
LEVHIGHR MVI   ERROR,OUTLVERR                                                   
         MVC   XTRA(L'BUKCODE),NDLVCOD                                          
         OC    XTRA(L'BUKCODE),SPACES                                           
         B     PREHKR                                                           
*                                                                               
LEVLOWR  MVI   ERROR,LEVERR                                                     
         MVC   XTRA(L'BUKCODE),NDLVCOD                                          
         OC    XTRA(L'BUKCODE),SPACES                                           
         B     PREHKR                                                           
*                                                                               
TOOMANYR MVI   ERROR,LIMERR                                                     
         MVC   XTRA(L'BUKCODE),NDLVCOD                                          
         OC    XTRA(L'BUKCODE),SPACES                                           
         B     PREHKR                                                           
*                                                                               
TOOBIGR  MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'UNTILMSG),UNTILMSG                                        
         ZIC   R0,RECLIM           GET RECORD LIMIT                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB+6(2)                                               
         B     PREHKR                                                           
*                                                                               
PREHKR   LA    R2,OCYFROMH                                                      
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         XC    SVCTLVAL,SVCTLVAL   CLEAR CONTROLS                               
         MVI   SVNENTS,0                                                        
         B     SPERR                                                            
         EJECT                                                                  
* SUB-ROUTINE TO FIND FIRST OUTLINE CODE FOR AUTOMATIC ASSIGNMENT               
*                                                                               
* ON EXIT, OUTNUM CONTAINS NEXT NUMBER TO ATTACH TO 'X' PREFIX                  
*                                                                               
FSTNUM   NTR1                                                                   
         SR    R2,R2                                                            
         ICM   R2,3,PLANCNT        GET OUTLINE COUNT ON PLAN                    
         LA    R2,1(R2)            INCREMENT IT                                 
         ST    R2,OUTNUM           INITIALIZE OUTLINE NUMBER                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BUCRECD,R4                                                       
         MVI   BUCSYS,C'B'                                                      
         MVC   BUCAGY,AGENCY       BUILD PASSIVE POINTER KEY                    
         MVI   BUCRTYP,BUCRTYPQ                                                 
         MVC   BUCCLT,CLTCODE                                                   
         MVC   BUCPRD,PRDCODE                                                   
         MVC   BUCPLAN,PLANCODE                                                 
         MVI   BUCCODE,C'X'        CODE START WITH 'X'                          
*                                                                               
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUCCODE+1(5),DUB                                                 
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
*                                                                               
FSTNUM2  CLC   BUCKEY(BUCCODE+1-BUCKEY),KEYSAVE                                 
         BNE   FSTNUMX                                                          
*                                                                               
         PACK  DUB,BUCCODE+1(5)    CODE MATCHES THROUGH 'X'                     
         CVB   R2,DUB              GET NUMBER FROM POINTER                      
         LA    R2,1(R2)            INCREMENT IT                                 
         ST    R2,OUTNUM                                                        
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 SEQ                                                              
         B     FSTNUM2                                                          
*                                                                               
FSTNUMX  NI    DMINBTS,X'FF'-X'08' TURN OFF PASS DELETES                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ THE OUTLINES FOR NEXT SCREEN AND DISPLAY THEM             
*                                                                               
* AT ENTRY, P1=NEXT ENTRY NUMBER                                                
* ON EXIT, CC=EQ IF NO OUTLINES ON SCREEN, SVNOUTS=N'OUTLINES ON SCR            
*                                                                               
DISOUT   NTR1                                                                   
         MVI   SVNOUTS,0                                                        
         L     R6,0(R1)            GET NEXT ENTRY NUMBER                        
         ZIC   R3,SVNENTS                                                       
         LA    R3,1(R3)                                                         
         SR    R3,R6               ESTABLISH COUNTER                            
         BNP   DISOUTX             NOTHING TO DISPLAY                           
         LA    R1,MAXLIN                                                        
         CR    R3,R1                                                            
         BL    *+6                                                              
         LR    R3,R1               CANNOT DISP MORE THAN SCREEN LIMIT           
         STC   R3,SVNOUTS                                                       
*                                                                               
DISOUT2  BCTR  R6,0                                                             
         MH    R6,=Y(SVTABL)                                                    
         LA    R6,SVTAB(R6)        R6=A(TABLE ENTRY)                            
         LA    R2,OCYNEW1H         R2=SCREEN LINE POINTER                       
         L     R4,NDIOA                                                         
         ST    R4,AIO                                                           
*                                                                               
DISOUT4  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,NDKEY                                                      
         MVC   BUKNODE,SVNODE                                                   
         MVC   BUKCODE,SVCODE                                                   
         GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE                                                    
         BNE   DISOUT6                                                          
         CLC   BUKDA,SVDA                                                       
         BNE   DISOUT6                                                          
*                                                                               
         GOTO1 GETREC                                                           
         GOTO1 VTRACE                                                           
         GOTO1 VGETVAL                                                          
*                                                                               
         LA    RE,DISPOLD+8(R2)                                                 
         ZIC   R1,OUTLEV                                                        
         BCTR  R1,0                                                             
         LA    RE,0(R1,RE)                                                      
         MVC   0(L'OUTCODE,RE),OUTCODE                                          
         LA    RE,DISPNAME+8(R2)                                                
         MVC   0(L'OCYNAM1,RE),OUTNAME                                          
*                                                                               
DISOUT6  LA    R2,DISPLIN(R2)      NEXT SCREEN LINE                             
         LA    R6,SVTABL(R6)       NEXT TABLE ENTRY                             
         BCT   R3,DISOUT4                                                       
*                                                                               
DISOUTX  CLI   SVNOUTS,0           SET CC ON EXIT                               
         B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR DUPLICATE OUTLINE CODES                              
*                                                                               
* AT ENTRY, P1=FIRST TABLE ENTRY NUMBER                                         
*                                                                               
CHECK    NTR1                                                                   
         L     R6,0(R1)            GET FIRST ENTRY NUMBER                       
         LA    R2,OCYNEW1H                                                      
         ZIC   R3,SVNOUTS                                                       
         BCTR  R6,0                                                             
         MH    R6,=Y(SVTABL)                                                    
         LA    R6,SVTAB(R6)                                                     
         ST    R6,ATHISENT                                                      
         USING SVTABD,R6                                                        
*                                                                               
CHECK2   GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   OUTCODE,FLD                                                      
         BAS   RE,TSTAN                                                         
         BAS   RE,DUPCHK           CHECK FOR DUPLICATIONS ON SCREEN             
         GOTO1 VFINDOUT,PARAS,(X'08',OUTCODE),0                                 
         BNE   *+12                                                             
         MVI   ERROR,DUPCDERR                                                   
         B     TRAPERR                                                          
*                                                                               
CHECK4   LA    R2,DISPLIN(R2)                                                   
         LA    R6,SVTABL(R6)                                                    
         BCT   R3,CHECK2                                                        
*                                                                               
CHECK5   LA    R4,MAXLIN                                                        
         ZIC   R3,SVNOUTS                                                       
         SR    R4,R3                                                            
         BNP   CHECK8              NO MORE LINES ON SCREEN                      
*                                                                               
CHECK6   GOTO1 VGETFLD,DMCB,(R2)                                                
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0                                                         
         BNE   TRAPERR                                                          
         LA    R3,DISPNAME(R2)                                                  
         GOTO1 VGETFLD,DMCB,(R3)                                                
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,0                                                         
         BNE   TRAPERR                                                          
         LA    R2,DISPLIN(R2)                                                   
         BCT   R4,CHECK6                                                        
*                                                                               
CHECK8   L     R6,ATHISENT         RESTORE POINTER TO FIRST ENTRY               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY,NDKEY         GET FIRST OUTLINE ON SCREEN                  
         MVC   BUKNODE,SVNODE      READ OUTLINE IN ORIGINAL POSITION            
         MVC   BUKCODE,SVCODE                                                   
         GOTO1 HIGH                                                             
         CLC   BUKEY,KEYSAVE       TEST OUTLINE FOUND                           
         BNE   CHECKR                                                           
         CLC   BUKDA,SVDA          TEST DISK ADDRESS IS THE SAME                
         BNE   CHECKR                                                           
*                                                                               
         GOTO1 GETREC                                                           
         GOTO1 VTRACE                                                           
*                                                                               
         LA    R1,CHECKHK                                                       
         ST    R1,NDHOOK                                                        
         GOTO1 VNODIO,DMCB,(R5),=C'READ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BNE   CHECKR                                                           
*                                                                               
         MVI   NDSQBACK,3                                                       
         GOTO1 VNODIO,DMCB,(R5),=C'LSEQ',NODKEY,0                               
*                                                                               
         MVC   XTRA,SPACES                                                      
         CLI   NDERR,0                                                          
         BNE   CHECKR                                                           
         CLC   NRECS,SVNOUTS       TEST FOUND ALL RECORDS                       
         BNE   CHECKR              NO-MUST BE AN ERROR                          
*                                                                               
CHECKX   B     XIT                                                              
*                                                                               
CHECKR   MVI   ERROR,SUPPLIED                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'CHGMSG),CHGMSG                                            
         XC    SVCTLVAL,SVCTLVAL                                                
         MVI   SVNENTS,0                                                        
         LA    R2,OCYFROMH                                                      
         ST    R2,FADDR                                                         
         LA    R2,8(R2)                                                         
         STCM  R2,7,FLAST                                                       
         B     SPERR                                                            
         SPACE 2                                                                
* HOOK ROUTINE TO CHECK SCREEN AGAINST FILE                                     
*                                                                               
CHECKHK  ST    RE,SAVERE                                                        
         CLI   NDMODE,NDPROC                                                    
         BNER  RE                                                               
         L     R4,NDIOA                                                         
         USING BURECD,R4                                                        
         TM    BURCTYP,BUKCOUT                                                  
         BZR   RE                                                               
*                                                                               
CHECKHK2 GOTO1 VGETVAL                                                          
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         L     R6,ATHISENT                                                      
         MVC   XTRA(L'BUKCODE),NDLVCOD                                          
         OC    XTRA(L'BUKCODE),SPACES                                           
         CLC   SVNODE,NDLVNOD      CHECK SAME NODE                              
         BNE   CHECKR                                                           
         CLC   SVCODE,NDLVCOD      SAME CODE                                    
         BNE   CHECKR                                                           
         CLC   SVDA,NDLVDA         SAME DISK ADDRESS                            
         BNE   CHECKR                                                           
         CLC   SVLEV,OUTLEV        AND SAME LEVEL                               
         BNE   CHECKR                                                           
*                                                                               
CHECKHK4 LA    R6,SVTABL(R6)                                                    
         ST    R6,ATHISENT                                                      
         ZIC   R1,NRECS                                                         
         LA    R1,1(R1)                                                         
         STC   R1,NRECS                                                         
         CLC   NRECS,SVNOUTS                                                    
         BL    *+8                                                              
         MVI   NDMODE,NDEND                                                     
*                                                                               
CHECKHKX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ALPHANUMERIC OUTLINE CODE (CALLED BY CHECK)          
*                                                                               
TSTAN    ST    RE,SAVERE                                                        
         LA    R1,OUTCODE                                                       
         ZIC   RE,FLDH+5           GET FIELD LENGTH                             
         NI    FLDH+4,X'F0'                                                     
         OI    FLDH+4,X'0C'        SET NUMERIC AND ALPHA BITS                   
         MVI   ERROR,INVALID                                                    
         CLI   OUTCODE,C'X'        TEST FOR CODE STARTING WITH 'X'              
         BNE   TSTAN1                                                           
         TM    PLANIND,BUPLNCOD    TEST CODE RESERVED ON PLAN                   
         BO    TRAPERR                                                          
*                                                                               
TSTAN1   CLI   0(R1),C'0'          TEST FOR NUMBER                              
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    TRAPERR                                                          
         B     TSTAN2                                                           
         NI    FLDH+4,X'FF'-X'08'  NOT A NUMERIC CODE                           
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
         BH    TRAPERR                                                          
*                                                                               
TSTAN2   LA    R1,1(R1)            NEXT CHARACTER IN FIELD                      
         BCT   RE,TSTAN1                                                        
*&&UK                                                                           
         TM    FLDH+4,X'08'        TEST FOR NUMERIC CODE                        
         BZ    TRAPERR                                                          
*&&                                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK THAT CODE IN OUTCODE DOES NOT DUPLICATE                  
* ONE ALREADY INPUT ON SCREEN                                                   
* CALLED BY CHECK, AT ENTRY, R3=N'OUTLINE REMAINING TO BE CHECKED               
*                                                                               
DUPCHK   ST    RE,SAVERE                                                        
         ZIC   RE,SVNOUTS                                                       
         LA    RF,OUTTAB                                                        
         SR    RE,R3                                                            
         BZ    DUPCHK2             FIRST ONE ON THIS SCREEN                     
*                                                                               
         MVI   ERROR,DUPCDERR                                                   
         CLC   OUTCODE,0(RF)                                                    
         BE    TRAPERR                                                          
         LA    RF,L'OUTTAB(RF)                                                  
         BCT   RE,*-14                                                          
*                                                                               
DUPCHK2  MVC   0(L'OUTCODE,RF),OUTCODE                                          
*                                                                               
DUPCHKX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO COPY THE OUTLINES ON SCREEN                                    
*                                                                               
* AT ENTRY, P1=N'FIRST ENTRY, P2=N'OUTLINES TO COPY                             
*                                                                               
COPY     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     RE,ANODBLK2                                                      
         LA    RF,LENODBLK                                                      
         L     R0,ANODBLK                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0               MOVE NODBLK TO NODBLK2                       
*                                                                               
         L     R5,ANODBLK2         USE NODBLK2 TO POSITION 'TO' PLAN            
         MVC   NDIOA2,AIO3                                                      
         MVI   NDREREAD,YES        FORCE RE-READ OF HIGHER LEVELS               
         XC    NDHOOK,NDHOOK                                                    
         MVC   NODKEY2,SVPLNKEY                                                 
         GOTO1 VNODIO,DMCB,(R5),=C'READ',NODKEY2,0                              
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
COPY2    L     R5,ANODBLK          POINT BACK TO ORIGINAL NODBLK                
         LR    R6,R2               GET FIRST ENTRY                              
         BCTR  R6,0                                                             
         MH    R6,=Y(SVTABL)                                                    
         LA    R6,SVTAB(R6)        INDEX INTO TABLE                             
         USING SVTABD,R6                                                        
         LA    R2,OCYNEW1H         R2=A(SCREEN LINE)                            
         MVC   NOUTS,PLANCNT       SET N'OUTLINE IN PLAN                        
         MVC   LOWLEV,PLANLOW      SET LOWEST LEVEL IN PLAN                     
         L     R4,NDIOA                                                         
         ST    R4,AIO              SET IO AREA POINTER                          
*                                                                               
COPY4    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKDA,SVDA                                                       
         GOTO1 GETREC                                                           
         GOTO1 VTRACE                                                           
         GOTO1 VGETVAL                                                          
*                                                                               
COPY5    CLI   SVASSIGN,YES        TEST FOR CODE ASSIGN                         
         BNE   COPY6               NO                                           
*                                                                               
         L     RE,OUTNUM           GET OUTLINE NUMBER                           
         XC    NEWCODE,NEWCODE     SET NEW OUTLINE CODE                         
         MVI   NEWCODE,C'X'        STARTS WITH PREFIX OF 'X'                    
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NEWCODE+1(5),DUB                                                 
         LA    RE,1(RE)            INCREMENT OUTLINE NUMBER FOR NEXT            
         ST    RE,OUTNUM                                                        
         B     COPY7                                                            
*                                                                               
COPY6    GOTO1 VGETFLD,DMCB,(X'FF',(R2))                                        
         MVC   NEWCODE,FLD         SET NEW OUTLINE CODE                         
         LA    R0,DISPNAME(R2)     POINT TO NAME FIELD                          
         GOTO1 VGETFLD,DMCB,(R0)                                                
         CLI   FLDH+5,0            TEST FOR A NAME                              
         BNE   *+16                YES                                          
         MVC   FLD(L'NEWCODE),NEWCODE NO-USE THE NEW CODE AS DEFAULT            
         OC    FLD(L'NEWCODE),SPACES                                            
         MVC   NEWNAME,FLD         SET NEW OUTLINE NAME                         
*                                                                               
COPY7    BAS   RE,OUT              BUILD NEW OUTLINE RECORD AND ADD IT          
*                                                                               
COPY8    CLI   SVASSIGN,YES        TEST FOR CODE ASSIGN                         
         BE    *+8                 YES-CAN SKIP SCREEN POINTER BUMP             
         LA    R2,DISPLIN(R2)      NEXT SCREEN LINE                             
         LA    R6,SVTABL(R6)       NEXT TABLE ENTRY                             
         BCT   R3,COPY4                                                         
*                                                                               
COPY10   MVC   NODKEY,SVPLNKEY     GET THE PLAN AGAIN                           
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,(R5),=C'READ',NODKEY,0                               
         CLI   NDERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,NDIOA                                                         
         MVI   ELCODE,BUPLNELQ     GET THE PLAN ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPLND,R6                                                        
         MVC   BUPLNCNT,NOUTS      UPDATE N'OUTLINES ON PLAN                    
         MVC   BUPLNLOW,LOWLEV     UPDATE LOWEST LEVEL ON PLAN                  
         GOTO1 VNODIO,DMCB,(R5),=C'PUT',NODKEY,0                                
*                                                                               
COPYX    B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO ADD AN OUTLINE RECORD                                          
*                                                                               
* AT ENTRY, NDIOA=A(OUTLINE RECORD), NEWCODE=NEW CODE FOR OUTLINE               
*                                                                               
OUT      NTR1                                                                   
         L     R4,NDIOA                                                         
         ST    R4,AIO                                                           
         MVI   RULSW,NO            INITIALIZE RULE ELEMENT SWITCH               
         USING BURECD,R4                                                        
         MVC   BUKEY,NDKEY                                                      
         LA    R6,BUFRSTEL                                                      
         SR    R0,R0                                                            
*                                                                               
OUT1     CLI   0(R6),0                                                          
         BE    OUT4                                                             
*                                                                               
         CLI   0(R6),BUOUTELQ      KEEP OUTLINE ELEMENT                         
         BNE   OUT2                                                             
         USING BUOUTD,R6                                                        
         OC    NEWNAME,NEWNAME     TEST FOR A NEW NAME                          
         BZ    *+10                NO-ITS A CODE ASSIGN                         
         MVC   BUOUTNAM,NEWNAME    SET NEW OUTLINE NAME                         
         B     OUT3                                                             
*                                                                               
OUT2     CLI   0(R6),BURULELQ      AND RULES ELEMENT                            
         BNE   *+16                                                             
         MVI   RULSW,YES                                                        
         CLI   SVRULES,YES         TEST TO COPY RULES                           
         BE    OUT3                YES                                          
*                                                                               
         CLI   0(R6),BUINELQ       AND SCREEN INPUT ELEMENTS                    
         BE    OUT3                                                             
         CLI   0(R6),BUPTRELQ                                                   
         BE    OUT3                                                             
         MVI   0(R6),X'FF'                                                      
*                                                                               
OUT3     IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     OUT1                                                             
*                                                                               
OUT4     GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'FF',(R4)),0                          
*                                                                               
         CLI   RULSW,YES           TEST IF RULE ELEMENTS PRESENT                
         BNE   *+12                                                             
         CLI   SVRULES,YES         TEST IF KEEPING RULES                        
         BE    OUT5                YES-LEAVE THE SCREEN INPUT ELEMS             
*                                                                               
         MVI   ELCODE,BUINELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
OUT5     MVI   ELCODE,BUPTRELQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING BUPTRD,R6                                                        
         LA    R1,BUPOINT                                                       
         USING BUCRECD,R1                                                       
         MVC   BUCCODE,NEWCODE     SET NEW OUTLINE CODE                         
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 VADDACTV                                                         
*                                                                               
* ADD THE FIRST OUTLINE TO BE COPIED                                            
*                                                                               
OUT6     CLC   OUTCODE,SVFROM      TEST DOING FIRST OUTLINE                     
         BNE   OUT15                                                            
*                                                                               
         MVC   NODKEY2,ATTKEY                                                   
         LA    RE,NODKEY2                                                       
         LA    R0,L'NODKEY2                                                     
         CLI   0(RE),C' '          TEST FOR END OF KEY                          
         BNH   OUT7                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
*                                                                               
OUT7     MVC   0(1,RE),NDDELIM                                                  
         LA    RE,1(RE)                                                         
         MVC   0(L'NEWCODE,RE),NEWCODE  ADD THE NEW OUTLINE                     
         OC    0(L'NEWCODE,RE),SPACES                                           
         OC    SVBEFORE,SVBEFORE   TEST DOING ADD BEFORE                        
         BZ    OUT8                                                             
*                                                                               
         L     R5,ANODBLK2         POINT TO SECOND NODIO BLOCK                  
         MVC   NDIOA,AIO                                                        
         MVC   NDIOA2,AIO3                                                      
         XC    NDHOOK,NDHOOK                                                    
         LA    R0,L'SVBEFORE                                                    
         GOTO1 VNODIO,DMCB,(R5),(C'B',=C'ADD'),NODKEY2,((R0),SVBEFORE),X        
               0                                                                
         CLI   NDERR,0                                                          
         BE    OUT10                                                            
         DC    H'0'                                                             
*                                                                               
OUT8     L     R5,ANODBLK2         POINT TO SECOND NODIO BLOCK                  
         MVC   NDIOA,AIO                                                        
         MVC   NDIOA2,AIO3                                                      
         XC    NDHOOK,NDHOOK       ATTACH AFTER ALL OFFSPRING                   
         GOTO1 VNODIO,DMCB,(R5),(C'A',=C'ADD'),NODKEY2,0,0                      
         CLI   NDERR,0                                                          
         BE    OUT10                                                            
         DC    H'0'                                                             
*                                                                               
* INITIALIZE THE LEVEL TABLE FOR BUILDING SUBSEQUENT KEYS                       
*                                                                               
OUT10    LA    R0,MAXOUTS          CLEAR LEVEL TABLE                            
         LA    RE,SVLEVTAB                                                      
         XC    0(L'SVLEVTAB,RE),0(RE)                                           
         LA    RE,L'SVLEVTAB(RE)                                                
         BCT   R0,*-10                                                          
*                                                                               
OUT11    LA    R3,NDLVTAB          INITIALIZE LEVEL TABLE USING                 
         LA    R3,NDLVTABL*4(R3)   FIRST KEY ADDED                              
         ZIC   R1,NDLEV                                                         
         SH    R1,=H'3'            COMPUTE NEW OUTLINE LEVEL                    
         LA    RE,SVLEVTAB                                                      
*                                                                               
OUT12    MVC   0(L'BUKCODE,RE),NDLVCOD                                          
         LA    R3,NDLVTABL(R3)                                                  
         LA    RE,L'SVLEVTAB(RE)                                                
         BCT   R1,OUT12                                                         
         B     OUT20                                                            
*                                                                               
* ADD THE SECOND THROUGH NTH OUTLINES                                           
*                                                                               
OUT15    BAS   RE,BLDKEY           CONSTRUCT THE NEW KEY                        
         L     R5,ANODBLK2         POINT TO SECOND NODIO BLOCK                  
         MVC   NDIOA,AIO                                                        
         MVC   NDIOA2,AIO3                                                      
         XC    NDHOOK,NDHOOK                                                    
         SR    R0,R0                                                            
         OC    AFTER,AFTER                                                      
         BZ    *+8                                                              
         LA    R0,L'AFTER                                                       
         GOTO1 VNODIO,DMCB,(R5),(C'A',=C'ADD'),NODKEY2,((R0),AFTER),0           
         CLI   NDERR,0                                                          
         BE    OUT20                                                            
         DC    H'0'                                                             
*                                                                               
* BUILD A PASSIVE POINTER AND ADD IT                                            
*                                                                               
OUT20    L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         L     R4,NDIOA                                                         
         MVI   ELCODE,BUPTRELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING BUPTRD,R6                                                        
         LA    RE,PASSKEY          BUILD A PASSIVE POINTER                      
         USING BUCRECD,RE                                                       
         MVC   BUCKEY,BUPOINT      LOGICAL KEY                                  
         MVC   BUCCTL,BURCTL                                                    
         MVC   BUCDA,NDLVDA                                                     
         DROP  RE                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PASSKEY),PASSKEY                                           
         GOTO1 ADD                                                              
*                                                                               
* UPDATE PLAN OUTLINE COUNT AND LOWEST LEVEL SO FAR                             
*                                                                               
OUT25    LH    R1,NOUTS            INCREMENT OUTLINE COUNT                      
         LA    R1,1(R1)                                                         
         STH   R1,NOUTS                                                         
         ZIC   R1,NDLEV                                                         
         SH    R1,=H'3'            FIND NEW OUTLINE LEVEL                       
         CLM   R1,1,LOWLEV                                                      
         BL    *+8                                                              
         STC   R1,LOWLEV                                                        
*                                                                               
OUTX     B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO BUILD THE NEW OUTLINE KEY                                      
*                                                                               
* INPUT VARIABLES - TABLE OF LAST OUTLINE CODE AT EACH LEVEL (SVLEVTAB)         
*                   NEW OUTLINE CODE(NEWCODE), ASSUMES GETVAL HAS BEEN          
*                   DONE                                                        
* ON EXIT - SET NODKEY2 TO ADD KEY AND AFTER TO ADD AFTER CODE                  
*                                                                               
BLDKEY   NTR1                                                                   
         ZIC   R2,OUTLEV           GET EXISTING LEVEL                           
         AH    R2,SVLEVADJ         APPLY LEVEL ADJUSTMENT                       
*                                                                               
BLDKEY2  LR    R3,R2                                                            
         BCTR  R3,0                                                             
         MH    R3,=Y(L'SVLEVTAB)                                                
         LA    R3,SVLEVTAB(R3)     R3=A(LEVEL TABLE ENTRY)                      
         MVC   AFTER,0(R3)         EXTRACT ADD AFTER CODE                       
         MVC   0(L'NEWCODE,R3),NEWCODE INSERT NEW CODE                          
*                                                                               
         LA    R4,MAXOUTS                                                       
         SR    R4,R2               N'LEVELS AFTER NEW LEVEL                     
         BNP   BLDKEY4                                                          
         LA    R3,L'SVLEVTAB(R3)                                                
         XC    0(L'SVLEVTAB,R3),0(R3)  ZERO THE REST OF THE TABLE               
         BCT   R4,*-10                                                          
*                                                                               
BLDKEY4  MVC   NODKEY2,SPACES                                                   
         LA    R0,12               MAXIMUM L'PLAN KEY                           
         LA    RE,NODKEY2          RE=A(DESTINATION)                            
         LA    R1,SVPLNKEY         R1=A(SOURCE)                                 
*                                                                               
BLDKEY5  MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '          TEST FOR END OF KEY                          
         BNH   BLDKEY6             YES                                          
         BCT   R0,BLDKEY5                                                       
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
BLDKEY6  LA    RE,1(RE)            SPACE AFTER PLAN KEY                         
         LA    R4,SVLEVTAB         R4=A(LEVEL TABLE)                            
*                                                                               
BLDKEY7  MVC   0(L'SVLEVTAB,RE),0(R4) EXTRACT NEXT CODE                         
         OC    0(L'SVLEVTAB,RE),SPACES SPACE PAD THE CODE                       
         LA    RE,L'SVLEVTAB(RE)   BUMP AHEAD IN KEY                            
         LA    R4,L'SVLEVTAB(R4)   NEXT LEVEL TABLE ENTRY                       
         BCT   R2,BLDKEY7                                                       
*                                                                               
BLDKEY8  LA    R6,L'NODKEY2                                                     
         GOTO1 SQUASHER,DMCB,NODKEY2,(NDDELIM,(R6))                             
*                                                                               
BLDKEYX  B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO CLEAR PROTECTED NAME FIELDS AT START OF VALKEY                 
*                                                                               
CLRNAME  ST    RE,SAVERE                                                        
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
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
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
RELO     DS    A                                                                
         SPACE 2                                                                
* TABLE OF DISPLACEMENTS OF PROTECTED 'NAME' FIELDS                             
*                                                                               
NAMETAB  DS    0XL2                                                             
         DC    AL2(OCYCLNH-T502FFD)                                             
         DC    AL2(OCYPRNH-T502FFD)                                             
         DC    AL2(OCYPLNH-T502FFD)                                             
         DC    AL2(OCYATTNH-T502FFD)                                            
         DC    AL2(OCYBEFNH-T502FFD)                                            
         DC    AL2(OCYFRNMH-T502FFD)                                            
         DC    AL2(OCYUPNMH-T502FFD)                                            
NAMES    EQU   (*-NAMETAB)/L'NAMETAB                                            
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
COPYMSG  DC    C'COPY COMPLETED'                                                
NEXTMSG  DC    C'COPY CONTINUING-TYPE IN NEW CODES FOR OUTLINES BELOW'          
FIRSTMSG DC    C'OUTLINES DISPLAYED - TYPE IN NEW CODES'                        
UNTILMSG DC    C'** XX OUTLINES READ - DID NOT FIND COPY UNTIL OUTLINE X        
               **'                                                              
CHGMSG   DC    C'**FROM OUTLINES HAVE BEEN CHANGED - COPY STOPPED**'            
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER OUTLINE COPY SCREEN                                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILC5D                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
CTLVALS  DS    0C                                                               
CLT      DS    CL3                                                              
PRD      DS    CL3                                                              
PLAN     DS    CL3                                                              
ATTACH   DS    CL(L'BUKCODE)                                                    
BEFORE   DS    CL(L'BUKCODE)                                                    
FROM     DS    CL(L'BUKCODE)                                                    
UNTIL    DS    CL(L'BUKCODE)                                                    
CTLVALN1 EQU   *-CTLVALS           CONTROL LENGTH EXCLUDING ASSIGN              
ASSIGN   DS    C                                                                
RULES    DS    C                                                                
CTLVALLN EQU   *-CTLVALS           CONTROL FIELDS LENGTH                        
*                                                                               
LEVADJ   DS    H                   LEVEL ADJUSTMENT ATTACH/FROM LEVEL           
FRLEV    DS    X                   FROM OUTLINE'S LEVEL                         
*                                                                               
INTMODE  DS    X                   INTERNAL MODE                                
NODKEY2  DS    CL(L'NODKEY)        SECOND NODAL KEY                             
PASSKEY  DS    CL(BUKLNQ)          PASSIVE POINTER AREA                         
FRKEY    DS    CL(L'NODKEY)        FROM OUTLINE KEY                             
OUTNUM   DS    F                   START NUMBER FOR OUTLINE CODE                
NOUTS    DS    H                   N'OUTLINES ON PLAN                           
LOWLEV   DS    X                   LOWEST LEVEL                                 
NRECS    DS    X                                                                
RECLIM   DS    X                                                                
UNTILSW  DS    C                   Y=UNTIL RECORD FOUND                         
RULSW    DS    C                   Y/N-RULE ELEMENTS ON RECORD                  
*                                                                               
ANODBLK2 DS    A                   A(SECOND NODIO BLOCK)                        
ATHISENT DS    A                   A(THIS OUTLINE TABLE ENTRY)                  
*                                                                               
NEWCODE  DS    CL(L'BUKCODE)                                                    
NEWNAME  DS    CL(L'BUOUTNAM)                                                   
AFTER    DS    CL(L'BUKCODE)                                                    
ATTKEY   DS    CL(L'NODKEY)                                                     
NEXTNODE DS    CL(L'BUKNODE)                                                    
NEXTLEV  DS    X                                                                
*                                                                               
OUTTAB   DS    (MAXLIN)CL(L'BUKCODE)                                            
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 2                                                                
         ORG   TWA1USER                                                         
SVCTLVAL DS    0CL(CTLVALLN)                                                    
SVCLT    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVPLAN   DS    CL3                                                              
SVATTACH DS    CL(L'BUKCODE)                                                    
SVBEFORE DS    CL(L'BUKCODE)                                                    
SVFROM   DS    CL(L'BUKCODE)                                                    
SVUNTIL  DS    CL(L'BUKCODE)                                                    
SVASSIGN DS    C                                                                
SVRULES  DS    C                                                                
*                                                                               
SVPLNKEY DS    CL(L'NODKEY)        PLAN KEY                                     
SVLEVADJ DS    H                   ATTACH LEVEL-FROM OUTLINE LEVEL              
SVFRLEV  DS    X                   COPY FROM OUTLINE'S LEVEL                    
SVNENTS  DS    X                   N'OUTLINE SAVE TABLE ENTRIES                 
SVFIRST  DS    X                   FIRST TABLE ENTRY NUMBER ON SCREEN           
SVNOUTS  DS    X                   N'OUTLINES ON SCREEN                         
SVLEVTAB DS    (MAXOUTS)CL(L'BUKCODE+1)                                         
SVTAB    DS    (MAXMAN)CL(SVTABL)  OUTLINE SAVE TABLE                           
SVTABX   EQU   *                                                                
         SPACE 2                                                                
* DSECT TO COVER OUTLINE SAVE TABLE                                             
*                                                                               
SVTABD   DSECT                                                                  
SVNODE   DS    CL(L'BUKNODE)       OUTLINE NODE                                 
SVCODE   DS    CL(L'BUKCODE)       OUTLINE CODE                                 
SVDA     DS    CL(L'BUKDA)         OUTLINE DISK ADDRESS                         
SVLEV    DS    X                   OUTLINE LEVEL                                
SVTABL   EQU   *-SVTABD                                                         
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
DISPNEW  EQU   OCYNEW1H-OCYNEW1H                                                
DISPOLD  EQU   OCYOLD1H-OCYNEW1H                                                
DISPNAME EQU   OCYNAM1H-OCYNEW1H                                                
DISPLIN  EQU   OCYNEW2H-OCYNEW1H                                                
MAXLIN   EQU   (OCYLAST-OCYNEW1H)/DISPLIN                                       
MAXAUTO  EQU   25                  MAXIMUM SIZE FOR AUTOMATIC COPY              
MAXMAN   EQU   50                  MAXIMUM SIZE FOR MANUAL COPY                 
FIRST    EQU   X'01'                                                            
NEXT     EQU   X'02'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003BUFIL25   05/01/02'                                      
         END                                                                    
