*          DATA SET REROM15    AT LEVEL 017 AS OF 03/07/03                      
*PHASE T83F15B                                                                  
         TITLE 'T83F15 - REROM15 - CONTRACT LIST'                               
***********************************************************************         
*                                                                     *         
*  REROM15 (T83F15) --- CONTRACT LIST                                 *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
* OUTPUT FROM VALISTA IS PATCH TO A FOR TESTING PURPOSE               *         
* ADDING NEW COLUMNS FOR PREBOOK ORDER                                          
*                                                                     *         
* 23MAY94 (SKU) INITIAL RELEASE                                       *         
*                                                                     *         
* 30JAN95 (SKU) DO NOT PASS LINKED CONTRACTS                          *         
*                                                                     *         
* 07MAR97 (SKU) SUPPORT VARIOUS/BRAND ORDERS                          *         
*                                                                     *         
* 02SEP02 (HQ ) DIPLAY CON W/BUYS FOR RADIO EDI ORDER                 *         
*                                                                     *         
***********************************************************************         
T83F15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T83F15*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
                                                                                
         NI    GLSTSTAT,X'FF'-APPLCDSP  SET GENCON DISPLAY LIST DATA            
         XC    LLIST,LLIST         USE DEFAULT LIST LENGTH                      
         MVI   MYSCRNUM,X'F1'                                                   
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
K        USING RDARKEY,SELECTKY                                                 
         CLI   K.RDARKSTA+4,C'A'   RADIO?                                       
         BE    *+12                                                             
         CLI   K.RDARKSTA+4,C'F'                                                
         BNE   MAIN01                                                           
         MVC   AGENCY,K.RDARKREP   USE SUBREP CODE INSTEAD OF MASTER            
         DROP  K                                                                
*                                                                               
MAIN01   DS    0H                                                               
         GOTO1 CKGLOB                                                           
         BNE   MAIN05                                                           
*                                                                               
         MVC   CNLLAST+1(2),=X'0101'                                            
*                                                                               
MAIN05   DS    0H                                                               
         TM    MYFLAG,MYFSWAPC                                                  
         BZ    MAIN10                                                           
         GOTO1 SWAPCON                                                          
         NI    MYFLAG,X'FF'-MYFSWAPC                                            
         B     EXIT                                                             
*                                                                               
MAIN10   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
                                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                SET CONDITION CODE AT EXIT                   
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         CLI   CALLSP,0            MUST BE CALLED TO GET HERE                   
         BNE   VK10                                                             
         LA    R2,CONRECH                                                       
         B     INVLRCAC            INVALID REC/ACTION                           
                                                                                
VK10     DS    0H                  VALIDATE STATION TO GET GRP/SUBGRP           
         LA    R2,CNLSTATH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALISTA                                                          
         MVC   STAFILT1,WORK                                                    
         MVC   STAGRP,WORK+31                                                   
                                                                                
VK20     DS    0H                  VALIDATE OFFICE CODE                         
         LA    R2,CNLOFFH                                                       
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 VALIOFF                                                          
         MVC   OFFFILT1,CNLOFF                                                  
                                                                                
VK30     DS    0H                  VALIDATE DARE AGENCY                         
         LA    R2,CNLDAGYH         FIND EQUIVALENCY CODE AND PUT                
         CLI   5(R2),0             IT IN THE AGENCY FILTER FIELD                
         BE    VK40                                                             
                                                                                
         BAS   RE,VALIDAGY         AGYFILT1 HAS LIST OF REP AGENCIES            
                                                                                
VK40     DS    0H                                                               
         LA    R2,CNLDATEH                                                      
         CLI   5(R2),0                                                          
         BE    VK50                                                             
                                                                                
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=-='                              
         CLI   DMCB+4,2            MUST BE 2 DATES                              
         BNE   INVLFLD             ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,BLOCK+12,WORK                                        
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVSTDT                                                          
         GOTO1 DATCON,DMCB,WORK,(3,STDTFILT)  START DATE                        
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         GOTO1 DATVAL,DMCB,BLOCK+44,WORK                                        
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVENDT                                                          
         GOTO1 DATCON,DMCB,WORK,(3,ENDTFILT)   END DATE                         
                                                                                
         CLC   STDTFILT,ENDTFILT   CHECK IF START DATE BEFORE END DATE          
         BH    ERSTENDT                                                         
                                                                                
VK50     DS    0H                  VALIDATE PREBOOK                             
                                                                                
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* VALIDATE AND CONVERT TO DARE AGENCY CODE                                      
* R2 IS FIELD HEADER                                                            
* AGYFILT1 WILL HAVE A LIST OF REP AGENCIES ASSIGNED TO THE SPECIFIC            
* DARE AGENCY. USUALLY, THERE IS A ONE-TO-ONE RELATIONSHIP BETWEEN              
* THE REP AGENCY WITH A DARE AGENCY. IN SOME CASES, HOWEVER, THERE IS           
* A MANY-TO-ONE RELATIONSHIP. THEREFORE, SPACE FOR A LIST OF 20 REP             
* AGENCIES ARE ALLOCATED HERE, WHICH SHOULD BE MORE THAN SUFFICIENT.            
*********************************************************************           
VALIDAGY NTR1                                                                   
         XC    AGYFILT1(120),AGYFILT1                                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RAGKDKEY,R6                                                      
         MVI   RAGKDTYP,X'9A'                                                   
         MVC   RAGKDREP,AGENCY     REP                                          
         MVC   RAGKDDAG(5),SPACES                                               
         XC    WORK,WORK                                                        
         MVC   WORK(7),8(R2)                                                    
         OC    WORK(10),SPACES     USE SPACE AS SENTINEL                        
         LA    RE,WORK                                                          
                                                                                
VAGY10   DS    0H                  CHECK FOR AGENCY OFFICE                      
         CLI   0(RE),C'-'                                                       
         BE    VAGY20                                                           
         CLI   0(RE),C' '                                                       
         BE    VAGY30                                                           
         LA    RE,1(RE)                                                         
         B     VAGY10                                                           
                                                                                
VAGY20   DS    0H                  AGENCY OFFICE                                
         MVC   RAGKDDAO,1(RE)                                                   
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,MOVEAGY                                                       
         B     VAGY40                                                           
*                                                                               
MOVEAGY  MVC   RAGKDDAG(0),WORK                                                 
*                                                                               
VAGY30   MVC   RAGKDDAG(3),WORK                                                 
*                                                                               
VAGY40   DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BNE   VAGYX                                                            
                                                                                
         LA    R3,AGYFILT1         BUILD THE LIST OF REP AGENCIES               
         LA    R4,20               ASSIGNED TO THIS ONE DARE AGENCY             
         B     VAGY60              UP TO 20 IS ALLOWED                          
                                                                                
VAGY50   DS    0H                                                               
         CLC   KEY(21),KEYSAVE                                                  
         BNE   VAGYX                                                            
         LA    R3,6(R3)                                                         
                                                                                
VAGY60   DS    0H                                                               
         MVC   0(6,R3),RAGKDAGY                                                 
         GOTO1 SEQ                                                              
         BCT   R4,VAGY50                                                        
         DROP  R6                                                               
                                                                                
VAGYX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
         OC    KEY(L'RCONKEY),KEY  FIRST TIME THRU?                             
         BNZ   LR10                                                             
                                                                                
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVI   RCONKTYP,X'0C'                                                   
         MVC   RCONKREP,AGENCY                                                  
         MVC   RCONKGRP,STAGRP                                                  
         MVC   RCONKSTA,STAFILT1                                                
                                                                                
         CLI   CNLOFFH+5,0                                                      
         BE    LR10                                                             
         MVC   RCONKOFF,OFFFILT1                                                
         DROP  R6                                                               
                                                                                
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
LR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCONKEY,R6                                                       
                                                                                
         CLI   RCONKTYP,X'0C'      CONTRACT RECORD?                             
         BNE   LRX                                                              
         CLC   RCONKREP,AGENCY     THIS REP CODE?                               
         BNE   LRX                                                              
         CLC   RCONKGRP,STAGRP     THIS GRP/SUBGRP?                             
         BNE   LRX                                                              
         CLC   RCONKSTA,STAFILT1   THIS STATION?                                
         BNE   LRX                                                              
                                                                                
LR22     DS    0H                                                               
         CLI   CNLOFFH+5,0         FILTER ON OFFICE?                            
         BE    LR24                                                             
         CLC   RCONKOFF,OFFFILT1                                                
         BNE   LRX                                                              
                                                                                
LR24     DS    0H                                                               
         CLI   CNLDAGYH+5,0        FILTER ON AGENCY?                            
         BE    LR26                                                             
                                                                                
         LA    R3,AGYFILT1         YES, CHECK AGAINST LIST                      
         LA    R4,20                                                            
                                                                                
LR25     DS    0H                                                               
         OC    0(6,R3),0(R3)       NO MORE MEANS NO MATCH, SO NEXT              
         BZ    LRSEQ                                                            
         CLC   RCONKAGY(L'AGYFILT1),0(R3)                                       
         BE    LR26                                                             
         LA    R3,6(R3)                                                         
         BCT   R4,LR25                                                          
         B     LRSEQ                                                            
         DROP  R6                                                               
                                                                                
LR26     DS    0H                                                               
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         CLI   CNLDATEH+5,0        FLIGHT DATE FILTER?                          
         BE    LR27                                                             
         CLC   STDTFILT,RCONDATE+3                                              
         BH    LRSEQ                                                            
         CLC   ENDTFILT,RCONDATE                                                
         BL    LRSEQ                                                            
                                                                                
LR27     DS    0H                                                               
         TM    RCONMODR,X'10'      IS THIS A PENDING CONTRACT?                  
         BZ    LR27A               YES                                          
*                                                                               
********                                                                        
* TESTING                                                                       
*        MVI   STAFILT+4,C'A'                                                   
********                                                                        
         CLI   STAFILT1+4,C'A'     NO, BUYLINES ADDED                           
         BE    LR29A               ALLOW FOR RADIO(AM,FM)                       
         CLI   STAFILT1+4,C'F'                                                  
         BE    LR29A                                                            
         B     LRSEQ                                                            
         DROP  R6                                                               
*                                                                               
LR27A    DS    0H                                                               
         L     R6,AIO              YES, BUT IS IT A FORECAST CONTRACT?          
         MVI   ELCODE,X'12'        EXPANPDED SAR ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   LR28                                                             
                                                                                
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH   ONLY NEW SAR ELEM HAS FORECAST FLAG          
         BL    LR28                                                             
         TM    RSARXFLG,X'10'      FLAGGED AS FORECAST?                         
         BO    LRSEQ                                                            
         DROP  R6                                                               
                                                                                
LR28     DS    0H                                                               
         L     R6,AIO              PENDING/FORECAST K HAS                       
         MVI   ELCODE,3            NO ESTIMATE BUCKET                           
         BAS   RE,GETEL                                                         
         BE    LRSEQ                                                            
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,4            NO INVOICE BUCKET                            
         BAS   RE,GETEL                                                         
         BE    LRSEQ                                                            
                                                                                
         L     R6,AIO              NO SPL/EPL DATA                              
         MVI   ELCODE,6                                                         
         BAS   RE,GETEL                                                         
         BE    LRSEQ                                                            
*                                                                               
* DO NOT PASS IF ALREADY LINKED TO AN AGENCY DARE ORDER                         
*                                                                               
LR29A    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'1D'        FIND DARE ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   LR29                                                             
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      LINKED?                                      
         BO    LRSEQ                                                            
         DROP  R6                                                               
                                                                                
LR29     DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
                                                                                
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
* PREBOOK?                                                                      
         TM    RCONMODR,X'10'                                                   
         BZ    LRPBX                                                            
         MVI   LSTPB,C'*'                                                       
LRPBX    DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
* UNWIRED?                                                                      
         CLI   RCONTYPE,C'N'                                                    
         BE    LR29UN                                                           
         CLI   RCONTYPE,C'X'                                                    
         BE    LR29UN                                                           
         GOTO1 SOFTYP              CONTYPE VS. OPTIONAL UNWIRE TYPE             
         BE    LR29UN                                                           
         B     *+10                                                             
LR29UN   DS    0H                                                               
         MVC   LSTUN(L'RCONTYPE),RCONTYPE                                       
*                                                                               
         GOTO1 HIGH                 RESTORE SEQUENCE                            
                                                                                
* CONTRACT NUMBER                                                               
         ZAP   MYWORK(5),=P'0'                                                  
         MVO   MYWORK(5),RCONKCON                                               
         EDIT  (P5,MYWORK),(8,LSTCON#),ALIGN=LEFT                               
                                                                                
* ESTIMATE NUMBER                                                               
         MVI   ELCODE,X'A2'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRESTX                                                           
         USING RCONIEL,R6                                                       
         OC    RCONIEST,RCONIEST                                                
         BZ    *+14                                                             
         MVC   LSTEST#,RCONIEST                                                 
         B     LRESTX                                                           
         OC    RCONXEST,RCONXEST                                                
         BZ    LRESTX                                                           
         MVC   LSTEST#,RCONXEST                                                 
LRESTX   DS    0H                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
* OFFICE                                                                        
LR30     DS    0H                                                               
         MVC   LSTOFF,RCONKOFF                                                  
                                                                                
* AGENCY                                                                        
         MVC   LSTAGY(L'RCONKAGY),RCONKAGY                                      
         CLC   RCONKAOF,SPACES                                                  
         BE    LR40                                                             
         LA    RE,LSTAGY                                                        
         MVI   LSTAGY+4,C' '                                                    
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RCONKAOF    AGENCY OFFICE                                
                                                                                
* FLIGHT DATES                                                                  
LR40     DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,LSTSTDT)                             
         MVI   LSTDASH,C'-'                                                     
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(5,LSTENDT)                           
                                                                                
         MVC   CONADV,RCONKADV                                                  
         MVC   CONPRD,RCONPRD                                                   
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'05'        GET EXPANSION ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   LR45                                                             
         USING RCONEXEL,R6                                                      
         MVC   CONPRDX,RCONEXPR                                                 
         DROP  R6                                                               
                                                                                
* GET ADVERTISER EXPANSION NAME                                                 
LR45     DS    0H                                                               
         MVC   MYSVKEY,KEY                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RADVKEY,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV(4),CONADV                                               
         MVC   RADVKREP,AGENCY                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   LR50                                                             
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         MVC   LSTADV,RADVNAME                                                  
         DROP  R6                                                               
                                                                                
* PRODUCT EXPANSION NAME                                                        
LR50     DS    0H                                                               
         CLC   CONPRD(3),SPACES                                                 
         BNE   LR60                                                             
         MVC   LSTPRD,CONPRDX                                                   
         B     LR70                                                             
                                                                                
LR60     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RPRDKEY,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CONADV                                                  
         MVC   RPRDKPRD,CONPRD                                                  
         MVC   RPRDKREP,AGENCY                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   LR70                                                             
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RPRDREC,R6                                                       
         MVC   LSTPRD,RPRDNAME                                                  
         DROP  R6                                                               
                                                                                
LR70     DS    0H                  RE-ESTABLISH SEQ ORDER                       
         MVC   KEY,MYSVKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         GOTO1 LISTMON                                                          
                                                                                
LRSEQ    GOTO1 SEQ                                                              
         B     LR20                                                             
                                                                                
LRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* SWAP TO CONTRACT AND APPLY MAKEGOOD                                           
***********************************************************************         
SWAPCON  NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'ROM'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
*        OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* SEND THE CONTRACT NUMBER AND DISPLAY CONTRACT                                 
*                                                                               
SWPK040  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLCONNUM,R6                                                      
         MVC   GLCONNUM,KSELECTD                                                
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DSM'                                               
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'      MASTER REP SIGNED ON?                        
         BZ    SWPK050             NO                                           
K        USING RDARKEY,SELECTKY    PASS THE SUBREP CODE FOR MASTER CF           
         OI    GLCONRPY+2,X'80'                                                 
         MVC   GLCONRPY+3(2),K.RDARKREP                                         
         DROP  K                                                                
         DROP  R2                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         DROP  R6                                                               
SWPK050  DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,GLCONLNQ,GLRKACT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
SWPK100  DS    0H                                                               
         L     R1,ATIOB                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LA    R2,CONSERVH                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
*                                                                               
SWAPCONX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CHECK IF WE RETURN FROM GLOBBER                                               
***********************************************************************         
CKGLOB   NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BNE   CKGLONO                                                          
         DROP  R4                                                               
*                                                                               
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRPR,=C'CON'    CON PROGRAM                                  
         BNE   CKGLONO                                                          
         DROP  R1                                                               
*                                                                               
CKGLOYES SR    RC,RC                                                            
CKGLONO  LTR   RC,RC                                                            
         B     EXIT                                                             
***********************************************************************         
* SOFTYP MATCH REP RECORD OPTIONAL UNWIRE CONTRACT TYPE WITH CON TYPE           
***********************************************************************         
SOFTYP   NTR1                                                                   
         L     R6,AIO1                                                          
         USING RCONREC,R6                                                       
*                                                                               
         MVC   MYWORK(L'KEY),KEY                                                
         XC    KEY,KEY                                                          
K        USING RREPKEY,KEY                                                      
         MVI   KEY,X'01'                                                        
         MVC   K.RREPKREP,AGENCY                                                
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO2                                                          
         USING RREPREC,R4                                                       
         CLC   RCONTYPE,RREPPROF+11    SOFT TYPE MATCH CON TYPE?                
         BNE   SOFTNO                  NO                                       
         DROP  R4,R6                                                            
*                                                                               
SOFTYES  CR    RC,RC                                                            
         B     SOFTX                                                            
SOFTNO   LTR   RC,RC                                                            
SOFTX    DS    0H                                                               
         MVC   KEY,MYWORK           RESTORE KEY                                 
         B     EXIT                                                             
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFKL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   STPFINIT                                                         
                                                                                
         CLI   PFKEY,0             ENTER KEY IS OKAY                            
         BE    STPFKL10                                                         
*                                                                               
         CLI   PFKEY,2             IF DSM                                       
         BNE   *+12                                                             
         OI    MYFLAG,MYFSWAPC                                                  
         B     STPFKL05                                                         
*                                                                               
         CLI   PFKEY,9             IF SELECTION                                 
         BNE   STPFKL10                                                         
*                                                                               
STPFKL05 DS    0H                                                               
         CLI   CALLSP,0                                                         
         BE    INVPFERR                                                         
*                                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         LA    R0,CNLSELH          CURSOR SHOULD BE WITHIN LIST                 
         CR    R2,R0                                                            
         BL    RECNTFND                                                         
         LA    R0,CNLPFLNH                                                      
         CR    R2,R0                                                            
         BNL   RECNTFND                                                         
*                                                                               
STPFKL10 LA    R2,LPFTABLE         YES, USE LIST PFKEY TABLE                    
         TM    CTLRFLG1,CF1BRDQ    BRAND ORDER IN PROCESS??                     
         BZ    STPFINIT                                                         
         LA    R2,BLPFTAB                                                       
*                                                                               
STPFINIT DS    0H                                                               
         OI    CTLRFLG1,CF1SVDAQ   DON'T SAVE D/A OF SELECTED                   
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
***************                                                                 
* FOR ACTION LIST                                                               
***************                                                                 
STPFLL00 CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BNE   STPFX                                                            
                                                                                
         CLI   PFKEY,0                                                          
         BE    STPFX                                                            
                                                                                
         CLI   CALLSP,0            WE MUST HAVE PFKEY'D HERE FROM MAIN          
         BE    INVPFERR             SCREEN                                      
                                                                                
         CLI   PFKEY,12            RETURN                                       
         BE    STPFLL10                                                         
                                                                                
         CLI   PFKEY,2             SWAP TO CONTRACT                             
         BE    STPFLL05                                                         
         CLI   PFKEY,9             SELECTION                                    
         BNE   STPFX                                                            
                                                                                
STPFLL05 DS    0H                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         AHI   R2,LSTCON#-LISTAR   GET DISPLACEMENT OF CON#                     
         MVC   KSELECTD,8(R2)                                                   
         OC    KSELECTD,KSELECTD                                                
         BZ    ERNOCON             IF THERE IS NO CONTRACT SELECTED             
                                                                                
STPFLL10 DS    0H                                                               
         TM    MYFLAG,MYFSWAPC                                                  
         BO    STPFLL15                                                         
         TWAXC CNLSTATH,CNLDATEH   CLEAR KEY FIELDS, SO THEY WON'T              
*                                  BE PASSED TO THE MYSELECT SCREEN             
STPFLL15 DS    0H                                                               
         ZIC   R0,PFKEY                                                         
         AH    R0,=H'12'                                                        
         STC   R0,PFKEY                                                         
         LA    R2,LPFTABLE         YES, USE SELECT PFKEY TABLE                  
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFLL20                                                         
         LA    R2,BLPFTAB                                                       
*                                  DON'T TEST THE SEL CODES IN TESTSEL          
*                                  DON'T SAVE D/A OF SELECTED                   
STPFLL20 DS    0H                                                               
         OI    CTLRFLG1,CF1TSELQ+CF1SVDAQ                                       
         GOTO1 INITIAL,DMCB,(R2)   2ND PASS                                     
                                                                                
STPFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                                  
***********************************************************************         
LPFTABLE DS    0C                                                               
*                                                                               
* DSM                                                                           
         DC    AL1(LPF02X-*,02,0,0,0,PFTRETRN)                                  
*        DC    CL3' ',CL8' ',CL8' '                                             
LPF02X   EQU   *                                                                
*                                                                               
* SELECT                                                                        
         DC    AL1(LPF09X-*,09,0,0,0,PFTRETRN)                                  
         DC    CL3'S',CL8' ',CL8' '                                             
LPF09X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(LPF12X-*,12,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
*                                                                               
* DSM                                                                           
         DC    AL1(LPF14X-*,14,0,0,0,PFTRETRN)                                  
*        DC    CL3' ',CL8' ',CL8' '                                             
LPF14X   EQU   *                                                                
*                                                                               
* ACTUAL SELECT                                                                 
         DC    AL1(LPF21X-*,21,0,0,(LPF21X-LPF21)/KEYLNQ,0)                     
         DC    CL3' ',CL8'ORDER',CL8'SELECT '                                   
LPF21    DC    AL1(KEYTYWS,L'KSELECTD-1),AL2(KSELECTD-MYAREAD)                  
LPF21X   EQU   *                                                                
*                                                                               
* ACTUAL RETURN TO CALLER                                                       
         DC    AL1(LPF24X-*,24,0,0,0,0)                                         
         DC    CL3' ',CL8'ORDER',CL8'SELECT'                                    
LPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
***********************************************************************         
* BRAND LIST PFKEY TABLE DEFINITIONS                                            
***********************************************************************         
BLPFTAB  DS    0C                                                               
*                                                                               
* DSM                                                                           
         DC    AL1(BLPF02X-*,02,0,0,0,PFTRETRN)                                 
         DC    CL3'S',CL8' ',CL8' '                                             
BLPF02X  EQU   *                                                                
*                                                                               
* SELECT                                                                        
         DC    AL1(BLPF09X-*,09,0,0,0,PFTRETRN)                                 
         DC    CL3'S',CL8' ',CL8' '                                             
BLPF09X  EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(BLPF12X-*,12,0,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8' ',CL8' '                                             
BLPF12X  EQU   *                                                                
*                                                                               
* ACTUAL SELECT                                                                 
         DC    AL1(BLPF21X-*,21,0,0,(BLPF21X-BLPF21)/KEYLNQ,0)                  
         DC    CL3' ',CL8'BRAND',CL8'SELECT '                                   
BLPF21   DC    AL1(KEYTYWS,L'KSELECTD-1),AL2(KSELECTD-MYAREAD)                  
BLPF21X  EQU   *                                                                
*                                                                               
* ACTUAL RETURN TO CALLER                                                       
         DC    AL1(BLPF24X-*,24,0,0,0,0)                                        
         DC    CL3' ',CL8'BRAND',CL8'SELECT'                                    
BLPF24X  EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
ERNOCON  MVC   RERROR,=AL2(953)                                                 
         LA    R2,CNLSELH                                                       
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
INVPFERR MVI   GERROR1,ERINVPFK    INVALID PFKEY                                
         LA    R2,CONRECH                                                       
         B     ERREND                                                           
*                                                                               
INVSTDT  MVC   RERROR,=AL2(79)     INVALID START DATE                           
         B     ERREND                                                           
*                                                                               
INVENDT  MVC   RERROR,=AL2(80)     INVALID END DATE                             
         B     ERREND                                                           
*                                                                               
ERSTENDT MVC   RERROR,=AL2(64)     END DATE BEFORE START DATE                   
         B     ERREND                                                           
*                                                                               
ERDAREAG MVC   RERROR,=AL2(439)    NO REP AGENCIES FOUND                        
         B     ERREND                                                           
*                                                                               
NEXTREQ  MVC   RERROR,=AL2(3)      ENTER NEXT REQUEST                           
         B     INFEND                                                           
*                                                                               
ERREND   DS    0H                                                               
         MVI   RMSGTYPE,C'E'                                                    
         GOTO1 MYERROR                                                          
*                                                                               
INFEND   DS    0H                                                               
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             DO A GETTXT CALL                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLCON                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE REROMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE REROMF1D                                                       
       ++INCLUDE REDARTW2                                                       
       ++INCLUDE REDARWORKD                                                     
       ++INCLUDE REDARDSECT                                                     
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
MYSVKEY  DS    CL32                                                             
KSELECTD DS    CL8                 SELECTED CONTRACT NUMBER                     
MYWORK   DS    CL64                                                             
STAGRP   DS    CL2                 STATION GROUP/SUBGROUP                       
*                                                                               
STAFILT1 DS    CL5                                                              
AGYFILT1 DS    20CL6               LIST OF REP AGENCIES                         
OFFFILT1 DS    CL2                                                              
STDTFILT DS    XL3                                                              
ENDTFILT DS    XL3                                                              
CONADV   DS    CL4                                                              
CONPRD   DS    CL3                                                              
CONPRDX  DS    CL20                                                             
TEMPSTA  DS    CL7                 SAVE STUFF BEFORE GOING TO GLOBBER           
TEMPOFF  DS    CL2                                                              
TEMPAGY  DS    CL6                                                              
TEMPDATE DS    CL17                                                             
MYFLAG   DS    X                                                                
MYFSWAPC EQU   X'80'               SWAP TO CONTRACT                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTPB    DS    CL2                                                              
         DS    CL1                                                              
LSTUN    DS    CL2                                                              
         DS    CL1                                                              
LSTCON#  DS    CL8                                                              
         DS    CL1                                                              
LSTEST#  DS    CL4                                                              
         DS    CL1                                                              
LSTOFF   DS    CL2                                                              
         DS    CL1                                                              
LSTAGY   DS    CL7                                                              
         DS    CL1                                                              
LSTSTDT  DS    CL8                                                              
LSTDASH  DS    CL1                                                              
LSTENDT  DS    CL8                                                              
         DS    CL1                                                              
LSTADV   DS    CL14                                                             
         DS    CL1                 64                                           
LSTPRD   DS    CL10                                                             
REPRECD  DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017REROM15   03/07/03'                                      
         END                                                                    
