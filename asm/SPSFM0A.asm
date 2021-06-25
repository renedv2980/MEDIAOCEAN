*          DATA SET SPSFM0A    AT LEVEL 058 AS OF 02/06/03                      
*PHASE T2170AA                                                                  
         TITLE 'T2170A  STATION EQUIVALENCE RECORDS'                            
T2170A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2170A                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING STEKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(2),=X'0D44'                                                
*                                                                               
         LA    R2,SEDMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   STEKAGMD,BAGYMD                                                  
*                                                                               
         LA    R2,SEDCLIH          CLIENT                                       
         CLC   CONREC(5),=C'ACNEQ' CLIENT ALL FOR MGREQ ONLY                    
         BE    VK005                                                            
         CLC   =C'ALL',8(R2)       CLIENT ALL?                                  
         BNE   VK005               NOPE                                         
         MVC   BCLT,=X'FFFF'       INDICATE CLIENT ALL                          
         B     VK006                                                            
*                                                                               
VK005    GOTO1 VALICLT                                                          
VK006    MVC   STEKCLT,BCLT                                                     
*                                                                               
         LA    R2,SEDSTAH          STATION                                      
         XC    QSTA,QSTA                                                        
         CLI   ACTNUM,ACTLIST      ACTION LIST DOESN'T NEED STATION             
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VK010                                                            
         GOTO1 ANY                                                              
*                                                                               
         LA    R1,SEDSTA+1         START CHECKING FOR C'/' AT 2ND CHAR          
         ZIC   R3,SEDSTAH+5        INPUT LENGTH                                 
         CHI   R3,3                INPUT LENGTH < 3?                            
         BL    VK009               YES, CANT BE ?/?                             
         SHI   R3,2                START AT 2ND POS UP TO 1 BEFORE END          
*                                                                               
VK008    CLI   0(R1),C'/'          NETWORK NOT ALLOWED...CABLE STA ONLY         
         BE    ERRSTA                                                           
         LA    R1,1(R1)                                                         
         BCT   R3,VK008                                                         
*                                                                               
VK009    GOTO1 VALISTA                                                          
         MVC   STEKSTA,QSTA                                                     
*                                                                               
VK010    MVC   KEY(13),SVKEY       SET KEY                                      
*                                                                               
         MVI   SVELCODE,2          ACN ELEMENTS                                 
         CLC   CONREC(5),=C'ACNEQ'                                              
         BE    VK900                                                            
         MVI   SVELCODE,3          MARKET GROUP ELEMENTS                        
         CLC   CONREC(5),=C'MGREQ'                                              
         BE    VK900                                                            
         DC    H'0'                                                             
*                                                                               
VK900    B     EXIT                                                             
*                                                                               
VKACTERR LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING STERECD,R6                                                       
         MVC   SEDSTA(4),STEKSTA   FORMAT STATION                               
         MVC   SEDSTA+4(3),=C'-TV'                                              
         CLI   STEKSTA,C'0'        CABLE                                        
         BNL   DK10                                                             
         CLI   STEKSTA+4,C'T'                                                   
         BE    DK10                                                             
         MVC   SEDSTA+5(1),STEKSTA+4                                            
         MVI   SEDSTA+6,C'M'                                                    
*                                                                               
DK10     OI    SEDSTAH+6,X'80'     TRANSMIT                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       MVC   SVKEY,KEY           SAVE THE STEQ RECORD KEY                     
         CLI   SVELCODE,3          MARKET GROUP EQUIVALENCE?                    
         BNE   VR001               NO                                           
*                                                                               
         BAS   RE,BLDTAB           BUILD A TABLE OF MGRP EQU ELEMS              
         LA    R7,MGTABLE          MARKET GROUP TABLE                           
         MVI   SEQNUM,0            SEQ# STARTS AT 1 (BUMPED BE4 ADDED)          
         B     VR002               DON'T REMOVE THE ELEMENTS                    
*                                                                               
VR001    MVC   ELCODE,SVELCODE     REMOVE ALL ELEMENTS                          
         GOTO1 REMELEM                                                          
*                                                                               
VR002    XC    ELEM,ELEM           BUILD NEW ELEMENTS                           
         LA    R6,ELEM                                                          
         MVC   ELEM(1),SVELCODE                                                 
         SR    R0,R0                                                            
         LA    R2,SEDIDH                                                        
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VR005               YES                                          
         CLI   SVELCODE,2          ACN NUMBER?                                  
         BE    VR040               YES, JUST BUMP                               
         OC    0(6,R7),0(R7)       TABLE HAS AN ENTRY?                          
         BZ    VR040               NOPE                                         
         B     VR900A              YES, ERROR                                   
*                                                                               
VR005    CLI   SVELCODE,2          ACN NUMBER?                                  
         BNE   VR010               NO                                           
         MVI   1(R6),9                                                          
         USING STEEL02,R6                                                       
         MVC   STEACN,8(R2)        ID IS ACN                                    
         OC    STEACN,SPACES                                                    
         LA    R5,STEACNMK                                                      
         LR    R3,R2               SAVEN ACN FIELD ADDRESS                      
         B     VR020                                                            
*                                                                               
VR010    ZIC   R1,SEQNUM           NEXT SEQ NUMBER                              
         AHI   R1,1                                                             
         STC   R1,SEQNUM                                                        
         LA    R1,SEDID8H          8TH ENTRY                                    
         CR    R1,R2               UP TO IT?                                    
         BNH   ERR7                YES, CANNOT ADD MORE THAN 7                  
         MVI   1(R6),8             NEW LENGTH WITH SEQ#                         
         USING STEEL03,R6                                                       
         MVC   STEMGSEQ,SEQNUM     SEQUENCE NUMBER                              
         MVC   AIO,AIO2                                                         
         GOTO1 VALIMGID                                                         
         MVC   STEMGID,MGRPID      ID IS MKTGRPID/MKTGRPNO                      
         MVI   MGRPLEN,4           FORCE 4 MKTGRP DIGITS                        
         GOTO1 VALIMGNO                                                         
         MVC   STEMGRP,MGRPNO                                                   
         CLI   ACTNUM,ACTADD       ACTION=ADD                                   
         BNE   VR019               NOPE                                         
         MVC   AIO,AIO1            THIS REC IS NOT ON FILE YET!                 
         B     VR019A                                                           
*                                                                               
VR019    MVC   KEY,SVKEY        RECOVER FROM MKTGRP VALIDATION ROUTINES         
         GOTO1 HIGH                                                             
         GOTO1 GETREC           (FOR GENCON'S EVENTUAL PUTREC)                  
VR019A   LA    R5,STEMGMKT                                                      
*                                                                               
VR020    IC    R0,0(R2)                                                         
         AR    R2,R0               MARKET FIELD                                 
         CLI   5(R2),0                                                          
         BE    VRMISERR                                                         
         TM    4(R2),X'08'         CHECK NUMERIC                                
         BZ    VRINVMKT                                                         
         ZIC   RE,5(R2)                                                         
         LA    RF,4                                                             
         SR    RF,RE                                                            
         LA    R1,QMKT(RF)                                                      
         BCTR  RE,0                                                             
         MVC   QMKT,=C'0000'                                                    
         EX    RE,VRMOVE                                                        
         PACK  DUB,QMKT                                                         
         CVB   R4,DUB                                                           
         STCM  R4,3,0(R5)          MOVE MARKET TO ELEMENT                       
         CLI   SVELCODE,3          TEST FOR MGREQ                               
         BNE   VR030                                                            
*                                                                               
         XC    KEY,KEY             CHECK MARKET IS IN MARKET GROUP              
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+8(1),MGRPID                                                  
         MVC   KEY+9(2),MGRPNO                                                  
         MVC   KEY+11(2),0(R5)                                                  
*                                                                               
VR025    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST RECORD EXISTS                           
         BE    VR026                                                            
         OC    KEYSAVE+3(2),KEYSAVE+3  NO - TEST FOR CLIENT SPECIFIC            
         BZ    VRMKTMGR                     NO - ERROR                          
         MVC   KEY,KEYSAVE                  YES- TRY WITHOUT CLIENT             
         XC    KEY+3(2),KEY+3                                                   
         B     VR025                                                            
*                                                                               
VR026    OC    0(6,R7),0(R7)       ADDING A NEW MGROUP EQU?                     
         BZ    VR027               YES                                          
*                                                                               
         CLC   2(6,R6),0(R7)       BUILT THE SAME ELEMENT?                      
         BNE   VR900A              NO, CAN'T CHANGE THIS                        
*                                                                               
         LA    R7,6(R7)            POINT TO NEXT ENTRY                          
         BAS   RE,GETMKT           GET THE MARKET NAME                          
         IC    R0,0(R2)                                                         
         AR    R2,R0               MARKET NAME                                  
         FOUT  (R2),MKTNM                                                       
         B     VR031B              DON'T RECUP!                                 
*                                                                               
VR027    BAS   RE,CHKDUPS          CHECK FOR DUPS AND ADD TO TABLE              
         B     VR031                                                            
*                                                                               
         DROP  R6                                                               
         USING STEEL02,R6          CHECK ELEMENTS FOR DUPLICATES                
VR030    LR    R5,R6                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
VR030A   BNE   VR030C                                                           
         CLC   STEACN,ELEM+2                                                    
         BNE   VR030B                                                           
         LR    R2,R3               ACN FIELD ADDRESS                            
         B     VRDUPERR            DUPLICATE ACN NUMBER                         
VR030B   CLC   STEACNMK,ELEM+7                                                  
         BE    VRDUPERR            DUPLICATE MARKET NUMBER                      
         BAS   RE,NEXTEL                                                        
         B     VR030A                                                           
VR030C   LR    R6,R5                                                            
         DROP  R6                                                               
*                                                                               
VR031    BAS   RE,GETMKT           GET THE MARKET NAME                          
         IC    R0,0(R2)                                                         
         AR    R2,R0               MARKET NAME                                  
         FOUT  (R2),MKTNM                                                       
*        CLI   ACTNUM,ACTADD       ACTION = ADD?                                
*        BE    *+12                YES, USE ADDELEM                             
         CLI   SVELCODE,3          TEST FOR MGREQ                               
         BE    VR031A                                                           
*                                                                               
         GOTO1 ADDELEM             ADD THE ELEMENT                              
         B     VR031B                                                           
*                                                                               
VR031A   BAS   RE,INSERTMG         A(LAST MARKET GROUP EQUIVALENCE)             
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,ALASTMGE                              
*                                                                               
VR031B   IC    R0,0(R2)                                                         
         AR    R2,R0               NEXT LINE                                    
*                                                                               
VR032    CLI   0(R2),0             TEST END OF SCREEN                           
         BE    VR900                                                            
         CLI   5(R2),0             TEST ANY INPUT                               
         BNE   VR005               YES                                          
*                                                                               
VR040    SR    RE,RE                                                            
         LA    R1,2                                                             
VR042    IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         BCT   R1,VR042                                                         
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR032                                                            
*                                                                               
VR900    CLI   ELEM+1,0            TEST ANY ID'S INPUT                          
         BE    VR905               NOPE, ERROR                                  
*                                                                               
         CLI   SVELCODE,3          TEST FOR MGREQ                               
         BNE   EXIT                                                             
*                                                                               
         CLI   SEQNUM,7            DID WE LIST ALL 7?                           
         BE    EXIT                YES, THERE IS NO ERROR                       
*                                                                               
         OC    0(6,R7),0(R7)       DID WE TRY TO DELETE ANY?                    
         BZ    EXIT                NO                                           
***                                                                             
* CANNOT DELETE ANY                                                             
***                                                                             
VR900A   LA    R6,MGTABLE          MARKET GROUP TABLE                           
         SR    R1,R1                                                            
*                                                                               
VR900A1  CLC   0(6,R7),0(R6)       MATCH?                                       
         BE    VR901               YES                                          
         AHI   R1,1                                                             
         LA    R6,6(R6)            BUMP                                         
         B     VR900A1                                                          
*                                                                               
VR901    LA    R7,MGTABLE          REBUILD SCREEN BASED ON TABLE                
         LA    R2,SEDIDH           FIRST DATA FIELD                             
         LA    R3,7                MAX ENTRIES FOR MGREQ                        
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
*                                                                               
VR901A   CLI   0(R7),0             END OF TABLE?                                
         BE    VR901B              YES                                          
*                                                                               
         MVC   WORK(1),0(R7)       ID                                           
         UNPK  WORK+1(5),1(3,R7)   ID NUMBER                                    
         FOUT  (R2),WORK,5                                                      
         SR    R4,R4                                                            
         ICM   R4,3,3(R7)          MARKET                                       
*                                                                               
         CVD   R4,DUB                                                           
         UNPK  QMKT(4),DUB                                                      
         OI    QMKT+3,X'F0'                                                     
         IC    R0,0(R2)            BUMP TO MARKET FIELD                         
         AR    R2,R0                                                            
         FOUT  (R2),QMKT           DISPLAY MARKET                               
         OI    4(R2),X'08'         XMIT                                         
*                                                                               
         BAS   RE,GETMKT           GET MARKET NAME                              
         IC    R0,0(R2)            BUMP TO MARKET NAME FIELD                    
         AR    R2,R0                                                            
         FOUT  (R2),MKTNM          DISPLAY MARKET NAME                          
         IC    R0,0(R2)            BUMP TO NEXT ID FIELD                        
         AR    R2,R0                                                            
         LA    R7,6(R7)            BUMP THE TABLE                               
         BCT   R3,VR901A           DONT LET IT GO PAST 7                        
*                                                                               
VR901B   LA    R2,SEDIDH           FIRST DATA FIELD                             
         CHI   R1,0                FIRST ONE WAS DELETED?                       
         BE    VR903               YES                                          
         SR    R0,R0               SO WE DONT HAVE TO ZIC IN LOOP               
*                                                                               
VR902    LA    R3,3                HAVE 3 FIELDS PER ENTRY                      
*                                                                               
VR902A   IC    R0,0(R2)                                                         
         AR    R2,R0               NEXT LINE                                    
         BCT   R3,VR902A                                                        
*                                                                               
         BCT   R1,VR902                                                         
*                                                                               
VR903    B     ERRCHG              CAN'T CHANGE THIS                            
*                                                                               
VR905    LA    R2,SEDIDH           NO-MISSING INPUT FIELD                       
         B     VRMISERR                                                         
*                                                                               
VRMISERR MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VRINVMKT MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
VRMKTMGR MVI   ERROR,MKTINMGR                                                   
         B     TRAPERR                                                          
*                                                                               
VRDUPERR MVI   ERROR,DUPENTRY                                                   
         B     TRAPERR                                                          
*                                                                               
VRMOVE   MVC   0(0,R1),8(R2)       * EXECUTED                                   
         EJECT                                                                  
***********************************************************************         
* BUILD A TABLE BASED ON THE MGROUP EQU ELEMS (X'03')IN STA EQU REC   *         
***********************************************************************         
BLDTAB   NTR1                                                                   
*                                                                               
         XC    MGTABLE,MGTABLE     CLEAR THE TABLE                              
         CLI   ACTNUM,ACTADD       ACTION=ADD?                                  
         BE    BTX                 YES                                          
*                                                                               
         LA    R1,MGTABLE                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BT10     BAS   RE,NEXTEL                                                        
         BNE   BTX                                                              
*                                                                               
         MVC   0(6,R1),2(R6)        1 ENTRY = MGID+MG(2)+MKT(2)+SEQ#            
         LA    R1,6(R1)                                                         
         B     BT10                                                             
*                                                                               
BTX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK FOR DUPLICATE MGROUP IDS                                      *         
***********************************************************************         
CHKDUPS  NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CK10     BAS   RE,NEXTEL                                                        
         BNE   CKX                                                              
*                                                                               
         CLC   ELEM+2(3),2(R6)      SAME MGROUP?                                
         BNE   CK10                 NO                                          
*                                                                               
         B     ERRDUP               YES, GIVE ERROR                             
*                                                                               
CKX      B     EXIT                                                             
***********************************************************************         
* GET A(WHERE TO INSERT NEXT X'03' ELEMENT)                           *         
***********************************************************************         
INSERTMG NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL             DO WE HAVE ONE?                             
         BE    IMG10                YES                                         
*                                                                               
IMG05    L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         ST    R6,ALASTMGE          A(WHERE TO PUT FITST X'03' ELEM)            
         B     IMGX                                                             
*                                                                               
IMG10    LR    R5,R6                COULD BE LAST X'03' ELEMENT                 
         BAS   RE,NEXTEL                                                        
         BE    IMG10                                                            
*                                                                               
         ZIC   R1,1(R5)             PUT NEXT X'03' AFTER LAST ONE               
         AR    R5,R1                                                            
         ST    R5,ALASTMGE          A(WHERE TO PUT NEXT X'03' ELEM)             
*                                                                               
IMGX     B     EXIT                                                             
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,SEDIDH           CLEAR SCREEN                                 
         BAS   RE,CLRSCRN                                                       
         SR    R0,R0                                                            
         L     R6,AIO              GET THE ELEMENTS                             
         MVC   ELCODE,SVELCODE                                                  
         BAS   RE,GETEL                                                         
         BE    DR030                                                            
         B     DRNOTFND                                                         
*                                                                               
DR020    MVC   ELCODE,SVELCODE                                                  
         BAS   RE,NEXTEL                                                        
         BNE   DR900                                                            
*                                                                               
DR030    SR    R4,R4                                                            
         CLI   ELCODE,2                                                         
         BNE   DR040                                                            
         USING STEEL02,R6                                                       
         FOUT  (R2),STEACN         ID IS ACN                                    
         ICM   R4,3,STEACNMK                                                    
         B     DR050                                                            
*                                                                               
         USING STEEL03,R6                                                       
DR040    MVC   WORK(1),STEMGID     ID IS MKTGRP                                 
         UNPK  WORK+1(5),STEMGRP(3)                                             
         FOUT  (R2),WORK,5         FORMAT MARKET GROUP TO SCREEN                
         ICM   R4,3,STEMGMKT                                                    
*                                                                               
DR050    CVD   R4,DUB              MARKET                                       
         UNPK  QMKT(4),DUB                                                      
         OI    QMKT+3,X'F0'                                                     
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         FOUT  (R2),QMKT                                                        
         OI    4(R2),X'08'                                                      
*                                                                               
         BAS   RE,GETMKT           GET MARKET NAME                              
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         FOUT  (R2),MKTNM                                                       
         IC    R0,0(R2)            NEXT ID                                      
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    DR900                                                            
         B     DR020                                                            
*                                                                               
DR900    B     EXIT                                                             
*                                                                               
DRNOTFND LA    R2,SEDMEDH                                                       
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R2,SELSELH                                                       
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
         LA    R6,KEY                                                           
         USING STERECD,R6                                                       
         MVC   WORK(2),=X'0D44'                                                 
         MVC   WORK+2(1),BAGYMD                                                 
         MVC   WORK+3(2),BCLT                                                   
         CLC   KEY(5),WORK         TEST FIRST TIME                              
         BE    LR010                                                            
         MVC   KEY(5),WORK                                                      
         MVC   STEKSTA,QSTA                                                     
         MVC   AIO,AIO1                                                         
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(5),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
         XC    LISTAR,LISTAR                                                    
         CLI   MODE,LISTRECS                                                    
         BNE   LR090                                                            
         GOTO1 GETREC              GET THE STEQ RECORD                          
         L     R6,AIO                                                           
         MVC   QSTA,STEKSTA        SAVE THE STATION                             
         MVC   LISTAR(4),STEKSTA   FORMAT THE STATION                           
         MVC   LISTAR+4(3),=C'-TV'                                              
         CLI   LISTAR,C'0'         CABLE                                        
         BNL   LR035                                                            
         CLI   STEKSTA+4,C'T'                                                   
         BE    LR035                                                            
         MVC   LISTAR+5(1),STEKSTA+4                                            
         MVI   LISTAR+6,C'M'                                                    
*                                                                               
LR035    MVC   ELCODE,SVELCODE                                                  
         BAS   RE,GETEL            FIND THE APROPRIATE ELEMENTS                 
         BNE   LR020                                                            
         LA    R3,LISTAR+8                                                      
         LA    R4,6                                                             
         B     LR050                                                            
*                                                                               
LR040    BAS   RE,NEXTEL                                                        
         BE    LR050                                                            
         BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         B     LR080                                                            
*                                                                               
LR050    BCT   R4,LR055            TEST FOR ANY MORE ROOM IN LINE               
         MVC   0(2,R3),=C'++'      INDICATE MORE THAN 5 ID'S                    
         B     LR080                                                            
*                                                                               
LR055    CLI   ELCODE,2                                                         
         BNE   LR060                                                            
         USING STEEL02,R6                                                       
         MVC   0(5,R3),STEACN      ID IS ACN                                    
         LA    R5,STEACNMK                                                      
         B     LR070                                                            
*                                                                               
         USING STEEL03,R6                                                       
LR060    MVC   0(1,R3),STEMGID     ID IS MKTGRPID/MKTGRPNO                      
         UNPK  1(5,R3),STEMGRP(3)                                               
         LA    R5,STEMGMKT                                                      
*                                                                               
LR070    MVI   5(R3),C'='                                                       
         SR    RE,RE                                                            
         ICM   RE,3,0(R5)          MARKET                                       
         CVD   RE,DUB                                                           
         UNPK  6(4,R3),DUB                                                      
         OI    9(R3),X'F0'                                                      
         MVI   10(R3),C','         NEXT ID                                      
         LA    R3,11(R3)                                                        
         B     LR040                                                            
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR090    CLI   MODE,PRINTREP                                                    
         BE    LR020                                                            
         DC    H'0'                                                             
*                                                                               
LR900    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
GETMKT   NTR1                                                                   
*                                                                               
*        ROUTINE TO GET MARKET RECORD                                           
*        OUTPUT: MKTNM                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         MVI   ERROR,INVMKT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         L     R6,AIO                                                           
         CLC   KEY(15),0(R6)                                                    
         BNE   VRINVMKT                                                         
         L     R5,AIO                                                           
         USING MKTRECD,R5                                                       
         MVC   MKTNM,MKTNAME                                                    
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRDUP   MVC   ERRNUM,=AL2(1173)    DUPLICATE MGROUP                            
         B     SPERREX                                                          
ERR7     MVC   ERRNUM,=AL2(1174)    > 7 MGREQ ENTRIES                           
         B     SPERREX                                                          
ERRCHG   MVC   ERRNUM,=AL2(1175)    CANT CHNGE PREVIOUSLY ADDED ENTRIES         
         B     SPERREX                                                          
ERRSTA   MVC   ERRNUM,=AL2(1178)   NETWORK NOT ALLOWED...CABLE STA ONLY         
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMFAD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMEAD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE FAGETTXTD          ERROR MSGS                                  
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
         ORG   SYSSPARE                                                         
*                                                                               
* ---------------------------- SAVED STORAGE -------------------------          
*                                                                               
ALASTMGE DS    A                                                                
ERRNUM   DS    XL2                                                              
MGTABLE  DS    CL42                                                             
SEQNUM   DS    X                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENSTEQ                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058SPSFM0A   02/06/03'                                      
         END                                                                    
