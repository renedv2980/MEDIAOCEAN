*          DATA SET PPLFM03    AT LEVEL 016 AS OF 09/11/06                      
*PHASE T40403A                                                                  
*                                                                               
         TITLE 'T40403G  PRINT LOGICAL FILE MAINT.   EST SCREEN'                
*                                                                               
* BPLA  9/06    FIX RATE TYPE CHANGE BUY CHECKING                               
*                                                                               
* KWAN 11/17/04 BILLING REP (PESTBREP)                                          
*                                                                               
* SMYE  12/00   MASTER TERMINAL CHANGE FOR MICHAEL HARNEY                       
*                                                                               
* BPLA  5/9/99  ALLOW RATE TYPE CHANGES IF NO BUYS ARE FOUND                    
*                                                                               
* KWAN 02/10/99 REVISE CODE FOR COST2 FACTOR ELEMENT                            
*                                                                               
* KWAN 01/20/99 ADD CODE FOR COST2 FACTOR UNDER EST OPTS FIELD                  
*                                                                               
* BPLA  8/98    IF DDS TERMINAL AND BRUCEPLATT IS ENTERED                       
*               IN ESTIMATE NAME FIELD - TREAT LIKE A MASTER                    
*     **NOTE**  MASTER TERMINALS CAN NO LONGER CHANGE ESTIMATE                  
*               NAMES                                                           
*               LAST CHANGE TO MASTTAB MADE ALSO                                
*                                                                               
* BPLA  8/98    MASTER TERMINAL CHANGE FOR HDTO                                 
*                                                                               
* BPLA 5/6/98   ACCEPT SHF= FOR ALL AGENCIES                                    
*          ALSO ACCEPT SFH=NO AND SHF=YES                                       
*                                                                               
* SMYE 5/06/98  MORE MASTER TERMINAL CHANGES                                    
*                                                                               
* SMYE 4/13/98  IF NOT MASTER TERMINAL DISALLOW END DATES EARLIER               
*               THAN START OF CURRENT YEAR                                      
*                                                                               
* BPLA  3/98    ON VIRGIN SCREENS FOR SFH CLIENT, DISPLAY                       
*               SFH=Y IN OPTIONS FIELD.                                         
*               OPTIONS DISPLAY MOVE TO SEPERATE ROUTINE                        
*               OPTIONS REDISPLAYED ON ADD.                                     
*                                                                               
* SMYE 3/25/98  ANOTHER MASTER TERMINAL CHG                                     
*                                                                               
* BPLA  3/13/98  YET ANOTHER MASTER TERMINAL CHANGE                             
*                REQUESTED BY DEBBIE WOOD                                       
*                                                                               
* SMYE 3/10/98  MASTER TERMINAL CHG FOR MICHAEL TARTAGLIA                       
*                                                                               
* SMYE 1/16/98  MASTER TERMINAL CHG FOR MICHAEL TARTAGLIA                       
*                                                                               
* SMYE 01/98    DISALLOW "SFH" CHANGES UNLESS MASTER TERMINAL                   
*                                                                               
* SMYE 12/97    CODE FOR A SECOND STANDARD COMMENT                              
*                                                                               
* SMYE 10/14/97 CODE FOR ESTIMATE OPTIONS (2ND ONE IS SPECIAL                   
*               FINANCIAL HANDLING ("SFH"))                                     
*                                                                               
* SMYE 10/8/97  CODE FOR ESTIMATE OPTIONS (1ST ONE IS PURCHASE ORDER $)         
*                                                                               
* SMYE 7/29/97  MASTER TERMINAL CHG FOR MICHAEL TARTAGLIA                       
*                                                                               
* BPLA 4/97     MASTER TERNIMAL CHANGES FOR HDTO                                
*                                                                               
* SMYE 1/06/97  MASTER TERMINAL CHG FOR MARIA DASILVA                           
*                                                                               
* SMYE 9/06/96  MASTER TERMINAL CHG FOR CARA ONG                                
*                                                                               
* SMYE 8/28/96  MASTER TERMINAL CHG FOR MARIA DASILVA                           
*                                                                               
* SMYE 12/7/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                      
*                                                                               
* BPLA 9/22/95  MASTER TERMINAL CHG FOR MARIA DASILVA                           
*                                                                               
* BPLA 9/19/95  MASTER TERMINAL CHG FOR L.A.                                    
*                                                                               
* BPLA 7/13/95  YET ANOTHER MASTER TERNIMAL CHANGE                              
*                                                                               
* BPLA 6/15/95  MASTER TERMINAL CHANGE FOR HDTO                                 
*                                                                               
* BPLA 10/13/94 MASTER TERMINALS CHANGED -                                      
*                                                                               
* BPLA 7/22/93  CHECK F0PROF TO SEE IF FILTERS ARE REQUIRED                     
*                                                                               
* BPLA 6/30/93  DDLA MASTER TERMINAL CHG DDLA104T TO DDLAA04T                   
*                                                                               
* BPLA 2/25/93  ANOTHER MASTER TERMINAL CHANGE HDT1C4C2 TO HDTO11FT             
*                                                                               
* BPLA 2/10/93  MASTER TERMINAL CHANGED FROM DDLA110T TO DDLA104T               
*                                                                               
* LWEI 1/18/93  ADD UDEF FIELDS                                                 
*                                                                               
* BPLA 9/5/91   ADD MASTER TERMINAL FOR DDS-LA                                  
*                                                                               
* BPLA 10/15/90 MASTER TERMINAL CHANGED - CANADA                                
*                                                                               
* ROSA 6/2/88 WHEN ADDING ESTIMATE, TEST TO SEE IF PRODUCT IS OAN   L01         
*             PRODUCT.  IF SO MAKE THIS EST A TEST EST.             L01         
*         2-  PREVENT TEST OAN ESTIMATES BECOMING LIVE..            L01         
*                                                                               
* BPLA 3/17/89 MASTER TERMINAL CHANGED                              L02         
*                                                                   L02         
* BPLA 3/30/89 RATE TYPE ADDED                                      L03         
*                                                                               
* BPLA 1/4/90 MASTER TERMINAL FOR WEST COAST ADDED                  L04         
*                                                                               
* BPLA 4/17/90 MASTER TERNIMAL FOR CANADA CHANGED AGAIN             L05         
*                                                                               
T40403   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40403                                                         
         L     RC,0(1)                                                          
         USING GENOLD,RC                                                        
         USING   T404FFD,RA                                                     
         LA    R8,4095(RB)                                                      
         LA    R8,1(R8)                                                         
         USING T40403+4096,R8                                                   
*                                                                               
*              GET LINE ID AND ADDR                                             
*              NEEDED FOR DATE CUTBACK CHK                                      
*                                                                               
         L     RF,VTWA             REALLY VCOMFACS                              
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   LINID,FALINE                                                     
         MVC   LINADDR,FAADDR                                                   
         DROP  R1                                                               
         DROP  RF                                                               
*               AND SET MASTER SWITCH                                           
*                                                                               
         MVI   MASTER,0                                                         
CKMAST   LA    RF,MASTTAB   TABLE OF MASTER TERMINAL IDS                        
CKMAST5  CLI   0(RF),X'FF'                                                      
         BE    CKMAST10                                                         
         CLC   LINID(8),0(RF)                                                   
         BE    CKMAST8                                                          
         LA    RF,8(RF)                                                         
         B     CKMAST5                                                          
*                                                                               
CKMAST8  MVI   MASTER,C'Y'      SET ON MASTER TERMINAL SWITCH                   
         B     CKMASTX                                                          
*                                                                               
CKMAST10 DS    0H                                                               
         CLI   1(RA),C'*'       SEE IF DDS TERMINAL                             
         BNE   CKMASTX                                                          
         CLI   ESTESTNH+5,10    CHECK FOR 10 INPUT CHAR                         
         BNE   CKMASTX                                                          
         CLC   ESTESTN(10),=C'BRUCEPLATT'                                       
         BNE   CKMASTX                                                          
         MVI   MASTER,C'Y'                                                      
*                                                                               
*        NOTE THAT MASTER TERMINALS CAN NO LONGER CHANGE THE NAME               
*                                                                               
CKMASTX  DS    0H                                                               
*                                                                               
*                                                                   L01         
* READ PRODUCT HEADER AND SAVE THE OAN CODE // IF NON ZERO, THIS    L01         
*      IS AN OAN PRODUCT AND WILL PROCUDE A TEST ESTIMATE HDR.      L01         
*                                                                   L01         
         MVC  KEY+27(4),PRDADDR ADDR F PRODHDR SET IN BASE OVERLAY  L01         
         BAS   RE,GETREC    GET PRODUCT RECORD IN IOAREA            L01         
         CLI   DMCB+8,0     SHOULD BE NO ERRORS                     L01         
         BNE   *+2          CHANGE WHEN DEBUGGED                    L01         
         LA    R2,IOAREA                                            L01         
         USING PPRDREC,R2                                           L01         
         MVC   OANPROD,PPRDOAN SAVE OAN AGENCY CODE                 L01         
         DROP  R2                                                   L01         
*                                                                               
*                                                                               
         EJECT                                                                  
         LA    R4,IOAREA                                                        
         LA    R5,1000                                                          
         BAS   RE,CLEARWRK                                                      
         MVC   PESTKEY,KEY                                                      
         MVC   PESTELEM(2),=X'07D8'                                             
         MVC   PESTPROF,=32C'0'                                                 
         MVC   PESTLEN,=X'00F9'                                                 
         CLI   SCRNUM,X'F3'                                                     
         BE    EST2                                                             
         MVI   DSPSW,1                                                          
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F3'                                 
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVCLSTAT,X'01'              SEE IF SFH CLIENT                    
         BNE   EST0                                                             
         FOUT  ESTOPTSH,=C'SFH=Y',5        DEFAULT IS NOW SFH - YES             
*                                                                               
EST0     DS    0H                                                               
         FOUT  ESTSTATH,=C'LIVE',4         DEFAULT IS NOW LIVE                  
         CLI   OANPROD,0                   NON OAN PRODUCT                      
         BE    EST1                                                L01          
         FOUT  ESTSTATH,=C'TEST',4         DEFAULT IS NOW TEST     L01          
*                                                                               
EST1     MVI   SCRNUM,X'F3'                                                     
         BAS   RE,SETUSER          PUT OUT DESC FIELDS FROM CLT REC             
         CLI   BACT,1                                                           
         BE    NOTDONE                                                          
         B     *+8                                                              
*                                                                               
EST2     DS    0H                                                               
         BAS   RE,SETUSER                                                       
         XC    SVESTST,SVESTST                                                  
         CLI   BACT,1                                                           
         BE    ESTSCRN                                                          
         MVC   KEY+27(4),ESTADDR                                                
         BAS   RE,GETREC                                                        
         CLC   PESTELEM(2),=X'07D8'              SEE IF NEW LENGTH              
         BE    *+16                YES - OK                                     
         MVC   PESTELEM(2),=X'07D8'                                             
         MVC   PESTLEN,=X'00F9'        SET NEW REC LENGTH                       
ESTSCRN  CLI   DSPSW,0                                                          
         BNE   FORMATE                                                          
*                      ESTIMATE SCREEN IN TWA SO EDIT IT UNLESS                 
*                      ACTION=DISPLAY                                           
         CLI   BACT,X'03'                                                       
         BE    FORMATE                                                          
EDIT     LA    R2,ESTESTNH                                                      
         BAS   RE,ANY                                                           
         MVC   SVESTST(12),PESTST     SAVE OLD START AND END                    
*                                                                               
*                                                                               
*        NOTE THAT MASTER TERMINALS CAN NO LONGER CHANGE THE NAME               
*                                                                               
         CLI   MASTER,C'Y'                                                      
         BE    EDIT3                                                            
         XC    PESTNAME,PESTNAME                                                
         MVC   PESTNAME,ESTESTN                                                 
*                                                                               
EDIT3    DS    0H                                                               
         XC    PESTNAM2,PESTNAM2       EST NAME LINE 2                          
         LA    R2,ESTNAM2H                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PESTNAM2,ESTNAM2                                                 
         LA    R2,ESTSTH                                                        
         BAS   RE,ANY                                                           
         GOTO1 VDATVAL,DMCB,(0,ESTST),WORK                                      
         LA    R3,20               INVALID DATE FORMAT                          
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLI   BACT,1                                                           
         BE    MVSTD                                                            
*                                                                               
*                   IF MASTER TERMINAL - CAN ADVANCE START DATE                 
*                                                                               
         CLI   MASTER,C'Y'                                                      
         BE    MVSTD                                                            
*                                                                               
         LA    R3,STDERR           START DATE CAN'T BE ADVANCED                 
         CLC   PESTST(6),WORK                                                   
         BL    ERROR                                                            
MVSTD    MVC   PESTST(6),WORK                                                   
         LA    R2,ESTENDH                                                       
         BAS   RE,ANY                                                           
         GOTO1 VDATVAL,DMCB,(0,ESTEND),WORK                                     
         LA    R3,20               INVALID DATE FORMAT                          
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLI   BACT,1                                                           
         BE    MVEND                                                            
*                                                                               
*                   IF MASTER TERMINAL - CAN CUTBACK END DATE                   
*                                                                               
         CLI   MASTER,C'Y'                                                      
         BE    MVEND                                                            
*                                                                               
         LA    R3,ENDERR           END DATE CANNOT BE CUTBACK                   
         CLC   PESTEND(6),WORK                                                  
         BH    ERROR                                                            
MVEND    MVC   PESTEND(6),WORK                                                  
         LA    R3,DATEERR                                                       
         CLC   PESTST,PESTEND      SEE IF STATC DATE PRECEDES END DATE          
         BH    ERROR                                                            
*                                                                               
*    IF MASTER TERMINAL - END DATE CAN BE BEFORE START OF CURRENT YEAR          
*                                                                               
         CLI   MASTER,C'Y'                                                      
         BE    MVEND50                                                          
*                                                                               
         CLI   BACT,1              IS THIS AN ADD ?                             
         BNE   MVEND50             NO                                           
*                                                                               
         CLI   F0PROF+3,C'Y'                                                    
         BE    MVEND50                                                          
*                                                                               
         LA    R3,ENDINV                                                        
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)         GET TODAY'S DATE             
         CLC   PESTEND(2),WORK     YEAR EARLIER THAN CURRENT YEAR ?             
         BL    ERROR                                                            
*                                                                               
MVEND50  LA    R3,YRERR                                                         
         XC    FULL,FULL                                                        
*        GOTO1 VDTCNV,DMCB,(0,PESTST),(1,FULL+1)                                
         GOTO1 VDATCON,DMCB,(0,PESTST),(3,FULL+1)                               
         XC    HALF(4),HALF                                                     
*        GOTO1 VDTCNV,DMCB,(0,PESTEND),(1,HALF+1)                               
         GOTO1 VDATCON,DMCB,(0,PESTEND),(3,HALF+1)                              
         L     R5,FULL                                                          
         L     R6,HALF                                                          
         SR    R6,R5                                                            
         C     R6,=F'65535'                                                     
         BH    ERROR                                                            
*                                                                               
         CLC   PESTST(12),SVESTST     SEE IF DATES CHANGED                      
         BE    CKFLTR              NO                                           
         CLC   KPRD(3),=C'ZZZ'                                                  
         BE    CKZZZ                                                            
         MVC   KPRD(3),=C'ZZZ'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CKFLTR                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               KEY+27,ZZZIO,(TERMNAL,DMWORK)                                    
         TM    8(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R3,DATERR                                                        
         LA    R2,ESTSTH                                                        
         CLC   PESTST(6),STDATE                                                 
         BNE   ERROR                                                            
         LA    R2,ESTENDH                                                       
         CLC   PESTEND(6),ENDDATE                                               
         BNE   ERROR                                                            
         B     CKFLTR                                                           
*                                                                               
*        WHEN ADDING OR CHANGING A ZZZ EST REC THE ST + END DATES               
*        MUST AGREE WITH ALL OTHER NON ZZZ ESTIMATES WITH THE SAME              
*        NUMBER                                                                 
*                                                                               
CKZZZ    MVC   KEY(25),PESTKEY                                                  
         XC    KPRD,KPRD                                                        
         MVC   KEYSAVE(25),KEY                                                  
         GOTO1 HIGH                                                             
CKKEY    TM    8(R1),X'40'         DISK ERROR                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CKZZZX                                                           
         CLC   KEY+7(3),=C'ZZZ'                                                 
         BE    CKZZZX                                                           
         CLC   KEY+10(2),KEYSAVE+10                                             
         BNE   CKZZZA                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               KEY+27,ZZZIO,(TERMNAL,DMWORK)                                    
         TM    8(R1),X'40'         DISK ERROR                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   BACT,1              SEE IF ADD                                   
         BNE   CKZ2                NO                                           
         LA    R3,DATERR1                                                       
         LA    R2,ESTSTH                                                        
         CLC   PESTST(6),STDATE                                                 
         BNE   ERROR                                                            
         LA    R2,ESTENDH                                                       
         CLC   PESTEND(6),ENDDATE                                               
         BNE   ERROR                                                            
         B     CKZZZA                                                           
*                                                                               
CKZ2     DS    0H                                                               
         CLI   BACT,2                                                           
         BNE   CKZZZA                                                           
*                                  ON POL CHANGES UPDATE BRAND ESTS             
         MVC   STDATE,PESTST                                                    
         MVC   ENDDATE,PESTEND                                                  
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'PUTREC'),=C'PRTFILE',KEY+27,  X        
               ZZZIO,(TERMNAL,DMWORK)                                           
         B     CKZZZA                                                           
*                                                                               
*                                                                               
CKZZZA   GOTO1 SEQ                                                              
         B     CKKEY                                                            
*                                                                               
CKZZZX   CLI   BACT,1              SEE IF POL ADD                               
         BE    CKFLTR              YES - CONTINUE WITH EDITS                    
         B     OUTPUT              NO - SKIP OTHER EDITS                        
*                                                                               
         EJECT                                                                  
CKFLTR   DS    0H                                                               
         LA    R2,ESTFLTRH                                                      
         XC    PESTGRPS,PESTGRPS                                                
         CLI   5(R2),0                                                          
         BE    CKFLTR5                                                          
         LA    R3,FLDINV                                                        
         CLI   5(R2),3                                                          
         BH    ERROR                                                            
         MVC   PESTGRPS,8(R2)                                                   
*                                                                               
CKFLTR5  DS    0H                                                               
         OC    F0PROF,F0PROF     SEE IF I HAVE AN F0 PROFILE                    
         BZ    CKRSCHM                                                          
         LA    R3,FLDINV                                                        
         LA    R4,PESTGRPS                                                      
         LA    R5,3      FOR BCT                                                
         LA    R6,F0PROF                                                        
CKFLTR6  CLI   0(R6),C'Y'      SEE IF THE POSITION IS REQUIRED                  
         BNE   CKFLTR8                                                          
         CLI   0(R4),C' '                                                       
         BNH   ERROR                                                            
CKFLTR8  LA    R4,1(R4)                                                         
         LA    R6,1(R6)                                                         
         BCT   R5,CKFLTR6                                                       
*                                                                               
         B     CKRSCHM                                                          
*                                                                               
CKRSCHM  DS    0H                 RETAIL SCHEME                                 
         LA    R2,ESTRSCHH                                                      
         XC    PESTRSCH,PESTRSCH                                                
         CLI   5(R2),0                                                          
         BE    CKRTYP                                        L03                
         LA    R3,FLDINV                                                        
         CLI   5(R2),2                                                          
         BH    ERROR                                                            
         MVC   PESTRSCH,8(R2)                                                   
         B     CKRTYP                                         L03               
*                                                                               
CKRTYP   DS    0H                 RATE TYPE                   L03               
         LA    R2,ESTRTYPH                                    L03               
         MVC   WORK(1),PESTRTYP          SAVE OLD RTYPE       L03               
         MVI   PESTRTYP,0                                     L03               
         CLI   5(R2),0                                        L03               
         BE    CKRTYP5                                        L03               
         LA    R3,FLDINV                                      L03               
         CLI   5(R2),1                                        L03               
         BH    ERROR                                          L03               
         CLI   8(R2),C'C'          ONLY ACCEPT 'C' FOR NOW    L03               
         BNE   ERROR                                          L03               
         MVC   PESTRTYP,8(R2)                                 L03               
*                                                             L03               
CKRTYP5  CLI   BACT,1                  SEE IF ADD             L03               
         BE    CKPROF                                         L03               
         CLC   WORK(1),PESTRTYP        CAN'T CHANGE RATE TYPE L03               
         BE    CKPROF                                         L03               
*                                                                               
         CLI   PESTRTYP,C'C'      IS NEW TYPE C?                                
         BNE   CKRTYP5X                                                         
         LA    RF,PESTBILP       CHECK BILLING FORMULA                          
         USING BILPROF,RF                                                       
         CLI   BILCMSW,C'C'      C BILLING FORMULA                              
         BE    CKRTYP5X          OK TO PROCEED                                  
         B     CKRTERR           CAN'T CHANGE RATE TYPE                         
*                                                                               
         DROP  RF                                                               
*                                IF NONE C FORMULA                              
CKRTYP5X DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),PESTREC    AGY/MED/CODE/CLT/PRD                          
         MVI   KEY+3,X'20'                                                      
CKRTYP6  GOTO1 HIGH                                                             
CKRTYP7  CLC   KEY(10),KEYSAVE      MATCH THROUGH PRD                           
         BNE   CKPROF                                                           
         CLC   KEY+19(2),PESTKEST    SEE IF RIGHT ESTIMATE                      
         BE    CKRTERR                                                          
         BH    CKRTYP9                                                          
         MVC   KEY+19(2),PESTKEST    LOW - SET TO MY ESTIMATE                   
         XC    KEY+21(4),KEY+21      CLEAR ACTIVE PRD AND LINE                  
         B     CKRTYP6                                                          
*                                                                               
CKRTYP9  MVC   KEY+19(2),=X'FFFF'    SKIP TO NEXT INSERTION DATE                
         XC    KEY+21(4),KEY+21      CLEAR ACTIVE PRD AND LINE                  
         B     CKRTYP6                                                          
*                                                                               
CKRTERR  LA    R3,NOCHGERR                                    L03               
         B     ERROR                                          L03               
*                                                                               
CKPROF   LA    R2,ESTPROFH                                                      
         BAS   RE,ANY                                                           
         LA    R3,PROFERR                                                       
         CLI   5(R2),32                                                         
         BNE   ERROR                                                            
         XC    PESTPROF,PESTPROF                                                
         MVC   PESTPROF,ESTPROF                                                 
CKSTAT   LA    R2,ESTSTATH                                                      
         LA    R3,STATERR                                                       
         CLI   5(R2),0                                                          
         BE    CKSTAT4                                                          
         CLI   ESTSTAT,C'1'        SOFT LOCKOUT                                 
         BE    CKSTAT4                                                          
         CLI   ESTSTAT,C'2'            PERMANENT LOCKOUT                        
         BE    CKSTAT4                                                          
         CLI   OANPROD,0                                           L01          
         BE    ISNONOAN                                            L01          
         CLC   8(4,R2),=C'TEST'      MUST BE PRESENT               L01          
         BNE   ERROR                                               L01          
         B     ISNONAO                                             L01          
ISNONOAN DS    0H                                                  L01          
*                                                                  L01          
*                                                                  L01          
         CLC   8(4,R2),=C'LIVE'                                                 
         BE    CKSTAT4                                                          
         CLC   8(4,R2),=C'TEST'                                                 
         BE    CKSTAT4                                                          
         B     ERROR                                                            
*                                                                               
CKSTAT4  CLI   OANPROD,0                                           L01          
         BE    ISNONAO                                             L01          
         CLC   9(4,R2),=C'TEST'      MUST BE PRESENT               L01          
         BNE   ERROR                                               L01          
ISNONAO  DS    0H                                                  L01          
*                                                                  L01          
*                                                                  L01          
         CLI   PESTSTAT,C'2'           WAS IT PERMANENTLY LOCKED OUT            
         BNE   CKSTAT6                 NO - OK TO CHANGE STATUS                 
         CLI   ESTSTAT,C'2'                                                     
         BE    CKSTAT6                                                          
*                                                                               
         CLI   MASTER,C'Y'       SEE IF MASTER TERMINAL                         
         BE    CKSTAT6                                                          
*                                                                               
         LA    R3,PERMERR                                                       
         B     ERROR                                                            
*****                                                                           
CKSTAT6  DS    0H                                                               
*****                                                                           
CKSTAT6A MVC   PESTSTAT,ESTSTAT                                                 
         CLI   5(R2),1                                                          
         BE    CKSTAT9                                                          
         CLI   5(R2),0                                                          
         BNE   CKSTAT6B                                                         
         MVI   PESTSTAT,0                                                       
         B     CKSTAT9                                                          
*                                                                               
*        ALLOW TEST ESTIMATES FOR ALL CLIENTS                                   
*                                                                               
CKSTAT6B DS    0H                                                               
CKSTAT6C CLI   5(R2),4                                                          
         BL    ERROR                                                            
         LA    R5,8(R2)                                                         
         CLI   5(R2),4                                                          
         BNE   CKSTAT6D                                                         
         MVI   PESTSTAT,0          NEEDED SINCE CKSTAT6 PUT L OR T              
         B     CKSTAT7                                                          
CKSTAT6D LA    R5,9(R2)            BUMP PAST 1 OR 2                             
CKSTAT7  CLC   0(4,R5),=C'LIVE'                                                 
         BE    CKSTAT8                                                          
         CLC   0(4,R5),=C'TEST'                                                 
         BNE   ERROR                                                            
         TM    PESTTEST,X'80'      SEE IF IT WAS TEST                           
         BO    CKAD                YES                                          
         CLI   BACT,1              SEE IF ADD                                   
         BNE   ERROR               NO - MEANS IT WAS LIVE                       
*                                  THEN THEY CAN'T MAKE IT TEST                 
         OI    PESTTEST,X'80'      MAKE IT TEST                                 
         B     CKAD                                                             
*                                                                               
CKSTAT8  TM    PESTTEST,X'80'         WAS THIS A TEST ESTIMATE?                 
         BZ    CKSTAT8A               NO                                        
*                                     WHEN MAKING A TEST EST LIVE               
*                                     CLEAR BUCKET ELEMS                        
*                                                                               
         MVC   KEY(25),PESTKEY        USING ESTIMATE KEY READ BUCKET            
         MVI   KEY+3,X'09'            RECORD.                                   
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CKSTAT8A               NO BUCKET RECORD                          
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               KEY+27,ZZZIO,(TERMNAL,DMWORK)                                    
         LA    R7,ZZZIO+33                                                      
         USING BKELEM,R7                                                        
CKST8A   CLI   0(R7),X'00'                                                      
         BE    CKST8J                                                           
         CLI   0(R7),X'31'              TODAY WITH GST                          
         BE    CKST8C                                                           
         CLI   0(R7),X'21'              WHEN CHANGING FROM TEST STATUS          
         BL    CKST8E                   TO LIVE MUST CLEAR ALL 21,22,23         
         CLI   0(R7),X'23'              ELEMENTS IN BUCKET RECORD.              
         BH    CKST8E                                                           
CKST8C   ZAP   BKOGRS,=P'0'                                                     
         ZAP   BKONET,=P'0'                                                     
         ZAP   BKOCD,=P'0'                                                      
         ZAP   BKPGRS,=P'0'                                                     
         ZAP   BKPNET,=P'0'                                                     
         ZAP   BKPCD,=P'0'                                                      
         NI    BKIND,X'FE'              TURN OFF TEST EST INDICATOR             
CKST8E   SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     CKST8A                                                           
CKST8J   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'PUTREC'),=C'PRTFILE',KEY+27,  +        
               ZZZIO,(TERMNAL,DMWORK)                                           
*****CKSTAT8A MVI   PESTTEST,0          MEANS IT'S LIVE                         
CKSTAT8A NI    PESTTEST,X'FF'-X'80'     MEANS IT'S LIVE                         
         B     CKAD                                                             
*                                                                               
CKSTAT9  CLI   FINANSW,C'Y'        SEE IF FINANCIAL                             
         BNE   CKAD                                                             
         B     ERROR               LIVE OR TEST MUST BE SPECIFIED               
*                                                                               
         DROP  R7                                                               
*                                                                               
CKAD     LA    R2,ESTADH           JOB NUMBER                                   
         XC    PESTJOB,PESTJOB                                                  
         CLI   5(R2),0                                                          
         BE    CKADX                                                            
         MVC   KEY(25),PESTKEY                                                  
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+10(6),8(R2)                                                  
         OC    KEY+10(6),SPACES                                                 
         GOTO1 HIGH                                                             
         TM    8(R1),X'40'         DISK ERROR                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(16),KEYSAVE                                                  
         BE    CKAD10              FOUND                                        
         LA    R3,NOTFND                                                        
         B     ERROR                                                            
*                                                                               
CKAD10   MVC   PESTJOB,KEY+10                                                   
*                                                                               
CKADX    EQU   *                                                                
*                                                                               
CKCOM    LA    R2,ESTCOMH          STND COMMENT(S)                              
         XC    PESTCOM,PESTCOM     CLEAR BOTH STANDARD COMMENTS                 
         XC    PESTCOM2,PESTCOM2                                                
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    CKCOMX              NO                                           
         MVC   WORK(20),SPACES                                                  
         ZIC   R7,5(R2)            INPUT LENGTH                                 
         LA    R7,8(R7,R2)         INPUT END ADDRESS                            
         LA    R4,ESTCOM           POINT TO BEGINNING OF STD COMMENT            
         LR    R5,R4               SAVE ADDRESS OF COMMENT                      
         SR    R6,R6               INITIALIZE CHARACTER COUNTER                 
*                           CHECK FOR MORE THAN TWO COMMENT ENTRIES             
CKCOM02  CLI   0(R5),C','          STOP CHARACTER ?                             
         BNE   CKCOM03             NO                                           
         LA    R6,1(R6)            ADD TO CHARACTER COUNTER                     
CKCOM03  LA    R5,1(R5)            BUMP TO NEXT POSITION                        
         CR    R5,R7               LAST CHARACTER FOUND ?                       
         BL    CKCOM02             NO                                           
         CH    R6,=H'1'                                                         
         BH    ERROR               MORE THAN 1 STOP CHARACTERS (COMMA)          
*                                                                               
CKCOM05  LR    R5,R4               SAVE ADDRESS OF COMMENT                      
         SR    R6,R6               INITIALIZE CHARACTER COUNTER                 
*                                                                               
CKCOM10  CLI   0(R4),C','          STOP CHARACTER ?                             
         BE    CKCOM25             YES                                          
         LA    R6,1(R6)            ADD TO CHARACTER COUNTER                     
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         CR    R4,R7               LAST CHARACTER FOUND ?                       
         BL    CKCOM10             NO                                           
*                                                                               
CKCOM25  MVC   WORK(6),SPACES                                                   
         LTR   R6,R6               ANY DATA FOUND                               
         BZ    ERROR               NO                                           
         CH    R6,=H'6'            MAXIMUM STANDARD COMMENT LENGTH              
         BH    ERROR                                                            
         LA    RE,6                                                             
         SR    RE,R6                                                            
         LA    RE,WORK(RE)         TO RIGHT ALIGN                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R5)                                                    
*                                                                               
CKCOM35  XC    KEY,KEY                                                          
         MVC   KEY(3),PESTKEY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         GOTO1 HIGH                                                             
         TM    8(R1),X'40'         DISK ERROR                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    CKCOM45                                                          
         LA    R3,NOTFND                                                        
         B     ERROR                                                            
*                                                                               
CKCOM45  DS    0H                                                               
         CLI   PESTCOM,0           "FIRST" COMMENT NULL ?                       
         BNE   CKCOM55             NO - SHOULD BE "SECOND" COMMENT              
         MVC   PESTCOM,KEY+4                                                    
         CLI   0(R4),C','          STOP CHARACTER ?                             
         BNE   CKCOMX              NO - ONLY ONE COMMENT - DONE                 
         LA    R4,1(R4)            NEXT COMMENT                                 
         B     CKCOM05             CHECK NEXT                                   
CKCOM55  MVC   PESTCOM2,KEY+4                                                   
CKCOMX   EQU   *                                                                
*                                                                               
CKREV    LA    R2,ESTREVH          REVISION NUMBER                              
         XC    PESTREVN,PESTREVN                                                
         CLI   5(R2),0                                                          
         BE    CKSREP                                                           
         BAS   RE,ANY                                                           
         BAS   RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  PESTREVN,DUB                                                     
*                                                                               
CKSREP   LA    R2,ESTREPH          SPECIAL REP                                  
         XC    PESTREP,PESTREP                                                  
         FOUT  ESTREPNH,SPACES,30                                               
         CLI   5(R2),0                                                          
         BE    CKBREP                                                           
         BRAS  RE,CKREPCOD         VALIDATE REP CODE                            
         BE    *+12                                                             
         LH    R3,HALF2                                                         
         B     ERROR                                                            
         XC    ESTREPN,ESTREPN     CLEAR REP NAME                               
         LA    R7,ZZZIO                                                         
         USING PREPRECD,R7                                                      
         MVC   PESTREP,PREPKREP    VALIDATED REP CODE                           
         FOUT  ESTREPNH,PREPNAME,30                                             
         DROP  R7                                                               
*                                                                               
CKBREP   LA    R2,ESTBREPH         BILLING REP                                  
         XC    PESTBREP,PESTBREP                                                
         FOUT  ESTBRPNH,SPACES,30                                               
         CLI   5(R2),0                                                          
         BE    CKALLO                                                           
         BRAS  RE,CKREPCOD         VALIDATE REP CODE                            
         BE    *+12                                                             
         LH    R3,HALF2                                                         
         B     ERROR                                                            
         XC    ESTBRPN,ESTBRPN     CLEAR REP NAME                               
         LA    R7,ZZZIO                                                         
         USING PREPRECD,R7                                                      
         MVC   PESTBREP,PREPKREP   VALIDATED REP CODE                           
         FOUT  ESTBRPNH,PREPNAME,30                                             
         DROP  R7                                                               
*                                                                               
CKALLO   LA    R2,ESTALLOH         ALLOCATION LINE FOR ZZZ ESTS                 
         XC    PESTZZZ,PESTZZZ                                                  
         CLI   5(R2),0                                                          
         BE    CKALLOX                                                          
         CLC   PESTKPRD,=C'ZZZ'                                                 
         BE    CKALLO5                                                          
         LA    R3,FLDINV                                                        
         B     ERROR                                                            
*                                                                               
CKALLO5  MVC   PESTZZZ,ESTALLO                                                  
*                                                                               
CKALLOX  EQU   *                                                                
*                                                                               
CKOPT    LA    R2,ESTOPTSH         EDIT OPTION(S)                               
         XC    PESTPURO,PESTPURO   CLEAR PURCHASE ORDER $                       
         MVC   SESTTEST,PESTTEST   SAVE PESTTEST FOR EDIT AT CKOPTXT            
         NI    PESTTEST,X'FF'-X'01'    TURN "SFH" OFF                           
*                                                                               
         CLI   BACT,1              IS THIS AN ADD ?                             
         BNE   CKOPT2              NO                                           
         TM    SVCLSTAT,X'01'      "SFH" CLIENT ?                               
         BNO   CKOPT2              NO                                           
         OI    PESTTEST,X'01'      YES - TURN ESTIMATE "SFH" ON                 
*                                                                               
CKOPT2   CLI   5(R2),0             ANY INPUT ?                                  
         BE    CKOPTXT             NO - TEST FOR SFH "CHANGE"                   
         LA    R3,FLDINV                                                        
         GOTO1 VSCANNER,DMCB,(20,(R2)),(12,ZZZIO)                               
         CLI   DMCB+4,0                                                         
         BE    ERROR                                                            
         LA    R5,ZZZIO                                                         
         USING SCAND,R5                                                         
         ZIC   R9,DMCB+4           NUMBER OF ENTRIES                            
*                                                                               
*                      NOTE: PO$ ENTRIES MUST NOT CONTAIN COMMAS                
*                 COMMAS WILL RESULT IN INVALID MESSAGE AFTER SCANNER           
CKOPT05  ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTPO            IS IT PO$ ?                                  
         BNE   CKOPT10             NO - TEST "SFH"                              
         ZIC   R6,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(2,FLD2),(R6),0                                    
         CLI   DMCB,0              VALID "CASH" ?                               
         BNE   ERROR               NO                                           
         MVC   PESTPURO,DMCB+4     PURCHASE ORDER $ IN PENNIES                  
         B     CKOPT40                                                          
*                                                                               
CKOPT10  ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTSFH     IS IT SPECIAL FINANCIAL HANDLING (SFH) ?           
         BNE   CKOPT20             NO - CHECK FOR COST2 FACTOR OPT              
**NO-OP  TM    SVAGYSW,X'01'       WESTERN AGENCY (OR SJR) ?                    
**NO-OP  BNO   ERROR               NO - SFH NOT ALLOWED                         
         CLI   FLD2LEN,1                                                        
         BNE   CKOPT10A            ENTRY FIELD MUST BE 'Y' OR 'N'               
         CLI   FLD2,C'N'           SFH=N ?                                      
         BE    CKOPT10C            YES                                          
         CLI   FLD2,C'Y'           SFH=Y ?                                      
         BNE   ERROR               NO - ERROR - MUST BE 'Y' OR 'N'              
         OI    PESTTEST,X'01'      YES - TURN SFH "ON"                          
         B     CKOPT40             DONE                                         
*                                                                               
CKOPT10A CLI   FLD2LEN,2           ALSO ACCEPT NO                               
         BNE   CKOPT10B                                                         
         CLC   FLD2(2),=C'NO'                                                   
         BE    CKOPT10C                                                         
         B     ERROR                                                            
*                                                                               
CKOPT10B CLI   FLD2LEN,3           ALSO ACCEPT YES                              
         BNE   ERROR                                                            
         CLC   FLD2(3),=C'YES'                                                  
         BNE   ERROR                                                            
         OI    PESTTEST,X'01'      YES - TURN SFH "ON"                          
         B     CKOPT40             DONE                                         
*                                                                               
CKOPT10C NI    PESTTEST,X'FF'-X'01'      YES - TURN SFH OFF                     
         B     CKOPT40             DONE                                         
*                                                                               
*                                                                               
*                                                                               
CKOPT20  DS    0H                  COST2 FACTOR                                 
         ZIC   R1,FLD1LEN                                                       
         CHI   R1,0                                                             
         BE    CKOPT40                                                          
         BCTR  R1,0                                                             
         EX    R1,TSTCOS           COST2 FACTOR OPTION?                         
         BNE   CKOPT30             NO - UNKNOWN OPT OR NEW OPT                  
*                                                                               
         CLI   F0PROF+4,C'$'       SEE IF PROF ALLOW COS2 OPT                   
         BE    ERROR                                                            
         CLI   F0PROF+4,C'F'       SEE IF PROF ALLOW COS2 OPT                   
         BNE   ERROR                                                            
*                                                                               
         CLI   FLD2LEN,10                                                       
         BH    ERROR               MAX INPUT LENGTH IS 10                       
         CLI   FLD2LEN,0                                                        
         BNE   *+14                                                             
         XC    PESTCF,PESTCF                                                    
         B     CKOPT40                                                          
*                                                                               
         ZAP   PESTCF,=P'0'        CLEAR COST2 FACTOR (PACKED)                  
*                                                                               
         ZIC   R6,FLD2LEN                                                       
         GOTO1 VCASHVAL,DMCB,(6,FLD2),(R6)                                      
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         L     R6,4(R1)                                                         
*                                                                               
         C     R6,=F'9999999'                                                   
         BH    ERROR               MAX INPUT IS 9.999999                        
         LTR   R6,R6                                                            
         BL    ERROR               LESS THAN ZERO IS NO GOOD                    
*                                                                               
         CVD   R6,DUB                                                           
         MVC   PESTCF,DUB+3        PESTCF IS PL5                                
*                                                                               
         B     CKOPT40                                                          
*                                                                               
*                                                                               
*                                                                               
CKOPT30  DS    0H                  UNKNOWN OPTION                               
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
CKOPT40  LA    R5,42(R5)           NEXT SCANNER FIELD                           
         BCT   R9,CKOPT05                                                       
*                                                                               
CKOPTXT  DS    0H           TEST SFH (MAY NOT BE CHANGED ONCE SET)              
         CLI   BACT,1              IS THIS AN ADD ?                             
         BE    CKOPTX              YES - OK                                     
         CLI   MASTER,C'Y'         IS THIS A MASTER TERMINAL ?                  
         BE    CKOPTX              YES - OK                                     
         TM    PESTTEST,X'01'      "CURRENTLY" SFH ?                            
         BNO   CKOPTXT4            NO                                           
         TM    SESTTEST,X'01'      "FORMERLY" SFH ?                             
         BNO   ERROR               NO - HAS BEEN CHANGED                        
         B     CKOPTX              YES - OK                                     
CKOPTXT4 DS    0H                                                               
         TM    SESTTEST,X'01'      "FORMERLY" SFH ?                             
         BO    ERROR               YES - HAS BEEN CHANGED                       
*                                                                               
CKOPTX   EQU   *                                                                
*                                                                               
OUTPUT   DS    0H                                                               
         LA    R6,PESTELEM                                                      
*                                                                               
OUT10    CLI   0(R6),0                                                          
         BE    OUT30                                                            
         CLI   0(R6),X'08'                                                      
         BE    OUT20                                                            
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     OUT10                                                            
*                                                                               
OUT20    GOTO1 VRECUP,DMCB,(1,PESTREC),0(R6)  DELETE OLD ELEMENT                
*                                                                               
OUT30    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ESTDSC1H                                                      
         LA    R3,SVE1USER                                                      
         BAS   RE,EDTUSR                                                        
*                                                                               
         LA    R1,ELEM                                                          
         USING PESTUDEF,R1                                                      
         MVC   PEUSER1,WORK                                                     
         MVC   ESTDSC1,WORK        CLEAR OR RE-TRANSMIT FIELD                   
         OI    ESTDSC1H+6,X'80'                                                 
*                                                                               
         LA    R2,ESTDSC2H                                                      
         LA    R3,SVE2USER                                                      
         BAS   RE,EDTUSR                                                        
*                                                                               
         LA    R1,ELEM             RESET R1 TO ELEM                             
         USING PESTUDEF,R1                                                      
         MVC   PEUSER2,WORK                                                     
         MVC   ESTDSC2,WORK        CLEAR OR RE-TRANSMIT FIELD                   
         OI    ESTDSC2H+6,X'80'                                                 
         DROP  R1                                                               
*                                                                               
         OC    ELEM+2(L'PEUSER1+L'PEUSER2),ELEM+2                               
         BZ    OUT40                                                            
         MVC   ELEM(2),=X'0832'    ELEMENT CODE/LENGTH                          
         GOTO1 VRECUP,DMCB,(1,PESTREC),ELEM,0(R6) ADD NEW ELEMENT               
*                                                                               
OUT40    CLI   BACT,1                                                           
         BNE   ESTCHG                                                           
         MVC   KEY(25),PESTKEY                                                  
         BAS   RE,ADDREC                                                        
***                                                                             
         BAS   RE,PUTOPTS          REFORMAT OPTIONS                             
***                                                                             
         MVC   KEY(25),PESTKEY                                                  
         CLC   PESTKPRD,=C'ZZZ'    NO BUCKET REC FOR ZZZ                        
         BE    DONE                                                             
         MVI   KEY+3,X'09'         ADD EMPTY BUCKET REC                         
         MVI   PESTKRCD,X'09'                                                   
         XC    PESTELEM(2),PESTELEM                                             
         MVC   PESTLEN,=X'0021'                                                 
         BAS   RE,ADDREC                                                        
         MVI   PESTKRCD,X'07'      RESTORE EST REC CODE                         
         MVC   KEY(25),PESTKEY                                                  
         B     DONE                                                             
*                                                                               
ESTCHG   GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               ESTADDR,ZZZIO,(TERMNAL,DMWORK)                                   
         MVC   KEY+27(4),ESTADDR                                                
         BAS   RE,PUTREC                                                        
         B     DONE                                                             
*                                                                               
*                                                                               
TSTPO    CLC   FLD1(0),=C'PO$'     EXECUTED                                     
*                                                                               
TSTSFH   CLC   FLD1(0),=C'SFH'     EXECUTED                                     
*                                                                               
TSTCOS   CLC   FLD1(0),=C'COS2'    EXECUTED                                     
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
FORMATE  DS    0H                                                               
*                                                                               
PUTEFLD  FOUT  ESTESTNH,PESTNAME,20                                             
         FOUT  ESTNAM2H,PESTNAM2,20                                             
*        GOTO1 VDTCNV,DMCB,(0,PESTST),(3,ESTST)                                 
         GOTO1 VDATCON,DMCB,(0,PESTST),(5,ESTST)                                
         FOUT  ESTSTH                                                           
*        GOTO1 VDTCNV,DMCB,(0,PESTEND),(3,ESTEND)                               
         GOTO1 VDATCON,DMCB,(0,PESTEND),(5,ESTEND)                              
         FOUT  ESTENDH                                                          
         FOUT  ESTPROFH,PESTPROF,32                                             
         FOUT  ESTFLTRH,PESTGRPS,3                                              
         FOUT  ESTRSCHH,PESTRSCH,2                                              
         XC    ESTRTYP,ESTRTYP                                                  
         FOUT  ESTRTYPH,PESTRTYP,1                                              
         XC    ESTSTAT,ESTSTAT                                                  
         CLI   PESTSTAT,C'1'                                                    
         BE    PUTF1                                                            
         CLI   PESTSTAT,C'2'                                                    
         BE    PUTF1                                                            
         MVI   PESTSTAT,0                                                       
PUTF1    FOUT  ESTSTATH,PESTSTAT,1                                              
*                                                                               
*****PUTF1C   CLI   PESTTEST,X'80'                                              
PUTF1C   TM    PESTTEST,X'80'      TEST ESTIMATE ?                              
         BZ    PUTF2               NO                                           
         MVC   ESTSTAT(5),=C'TEST '                                             
         CLI   PESTSTAT,0                                                       
         BE    PUTF3                                                            
         MVC   ESTSTAT+1(4),=C'TEST'                                            
         MVC   ESTSTAT(1),PESTSTAT                                              
         B     PUTF3                                                            
*                                                                               
PUTF2    MVC   ESTSTAT(5),=C'LIVE '                                             
         CLI   PESTSTAT,0                                                       
         BE    PUTF3                                                            
         MVC   ESTSTAT+1(4),=C'LIVE'                                            
         MVC   ESTSTAT(1),PESTSTAT                                              
         B     PUTF3                                                            
*                                                                               
PUTF3    FOUT  ESTSTATH,ESTSTAT,5                                               
*                                                                               
PUTF4    FOUT  ESTADH,PESTJOB,6                                                 
         XC    ESTCOM,ESTCOM       CLEAR STANDARD COMMENT(S)                    
*****    FOUT  ESTCOMH,PESTCOM,6                                                
         OI    ESTCOMH+6,X'80'                                                  
         MVC   ESTCOM(6),PESTCOM                                                
         CLI   PESTCOM2,0          SECOND STANDARD COMMENT ?                    
         BNH   PUTF6               NO                                           
         MVI   ESTCOM+6,C','                                                    
         MVC   ESTCOM+7(6),PESTCOM2                                             
PUTF6    FOUT  ESTREVH,PESTREVN,3                                               
         BAS   RE,PUTOPTS                                                       
         BAS   RE,PUTUSER          DISPLAY USER DEFINITION FIELDS               
*                                                                               
         XC    ESTREPN,ESTREPN     CLEAR SPECIAL REP NAME                       
         FOUT  ESTREPNH                                                         
         XC    ESTREP,ESTREP                                                    
         FOUT  ESTREPH,PESTREP,4                                                
         LA    R2,ESTREPNH                                                      
         LA    R7,PESTREP                                                       
         BRAS  RE,DSPREPNM                                                      
*                                                                               
         XC    ESTBRPN,ESTBRPN     CLEAR BILLING REP NAME                       
         FOUT  ESTBRPNH                                                         
         XC    ESTBREP,ESTBREP                                                  
         FOUT  ESTBREPH,PESTBREP,4                                              
         LA    R2,ESTBRPNH                                                      
         LA    R7,PESTBREP                                                      
         BRAS  RE,DSPREPNM                                                      
*                                                                               
         FOUT  ESTALLOH,PESTZZZ,47                                              
         CLI   BACT,X'02'                                                       
         BNE   DONE                                                             
NOTDONE  DS    0H                                                               
         LA    R2,ESTESTNH                                                      
         B     EXIT                                                             
*                                                                               
DONE     MVI   DONESW,1                                                         
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*        SET USER DESCRIPTION FIELDS ON THE SCREEN                              
*                                                                               
SETUSER  DS    0H                                                               
         ST    RE,FULL             SAVE RE                                      
         MVC   MYAREA(L'SVE1USER),SVE1USER    DISPLAY 1ST DESC LINE             
         LA    R1,ESTDEF1H                                                      
         LA    R6,ESTDSC1H                                                      
         BAS   RE,FMTUSR                                                        
*                                                                               
         MVC   MYAREA(L'SVE2USER),SVE2USER    DISPLAY 2ND DESC LINE             
         LA    R1,ESTDEF2H                                                      
         LA    R6,ESTDSC2H                                                      
         BAS   RE,FMTUSR                                                        
         L     RE,FULL             RESTORE RE                                   
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        PUT OUT USER DESCRIPTION FIELDS                                        
*             R1   = A(DESC FIELD).                                             
*             R6   = A(INPUT FIELD).                                            
*             DESC = PRODUCT DESCRIPTION.                                       
*                                                                               
FMTUSR   DS    0H                                                               
         OI    1(R6),X'20'              PROTECT INPUT FIELD                     
         MVC   8(L'SVE1USER,R1),MYAREA  CLEAR ANY PREVIOUS DESC FIELDS          
         CLC   SPACES(L'SVE1USER),MYAREA NEED TO SHOW DESCRIPTION               
         BL    FMTUSR10                                                         
         LR    R0,R1                    SAVE C(R1) AROUND.                      
         ZIC   R1,0(R6)                 R1=L(HEADER)+L(INPUT FIELD)             
         SH    R1,=H'8'                 R1=L(INPUT FIELD)                       
         TM    1(R6),X'02'              CHECK FOR EXTENDED HEADER               
         BZ    *+8                                                              
         SH    R1,=H'8'                 SUBTRACT L(X-HEADER)                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R6),8(R6)            CLEAR ANY GARBAGE                       
         LR    R1,R0                                                            
         B     FMTX                                                             
*                                                                               
FMTUSR10 NI    1(R6),X'FF'-X'20'        UNPROTECT INPUT FIELD                   
*                                                                               
FMTX     OI    6(R1),X'80'              TRANSMIT FIELD                          
         OI    6(R6),X'80'                                                      
         BR    RE                                                               
         EJECT                                                                  
PUTOPTS  DS    0H                  DISPLAY OPTIONS                              
         XC    ESTOPTS,ESTOPTS     CLEAR ESTIMATE OPTIONS                       
         LA    R1,ESTOPTS                                                       
         OC    PESTPURO,PESTPURO   ANY PURCHASE ORDER$ ?                        
         BZ    PUTO10              NO - TEST NEXT OPTION                        
*                                  FORMAT OPTION(S)                             
         MVC   0(4,R1),=C'PO$='                                                 
         EDIT  PESTPURO,(13,4(R1)),2,ALIGN=LEFT,ZERO=BLANK                      
*                NOTE: NO COMMAS ALLOWED IN ABOVE MONEY FIELD                   
         AR    R1,R0               R0 HAS # OF CHARACTERS FROM EDIT             
         LA    R1,4(R1)            POINT R1 TO "NEXT" OPTION ADDRESS            
         MVI   0(R1),C','          USE "," AS SEPARATOR (FOR SCANNER)           
*                                                                               
PUTO10   DS    0H                                                               
**NO-OP  TM    SVAGYSW,X'01'       WESTERN AGENCY (OR SJ) ?                     
**NO-OP  BZ    PUTO25           NO - NO SPECIAL FIN'L HANDLING OPTION           
         TM    PESTTEST,X'01'      ESTIMATE SFH "ON" ?                          
         BO    PUTO15              YES                                          
*                                  ESTIMATE SFH IS "OFF'                        
         TM    SVCLSTAT,X'01'      CLIENT SFH "ON" ?                            
         BZ    PUTO25              NO - DO NOT DISPLAY SFH=..                   
PUTO15   DS    0H                  DISPLAY ESTIMATE SFH                         
         CLI   0(R1),C','          OTHER OPTION ENTERED ?                       
         BNE   *+8                 NO                                           
         LA    R1,1(R1)            YES - BUMP PAST ","                          
         MVC   0(5,R1),=C'SFH=Y'                                                
         TM    PESTTEST,X'01'      ESTIMATE SFH "ON" ?                          
         BO    PUTO20              YES - DONE WITH THIS OPTION                  
         MVI   4(R1),C'N'          ESTIMATE SFH IS "OFF"                        
PUTO20   LA    R1,5(R1)            POINT R1 TO "NEXT" OPTION ADDRESS            
         MVI   0(R1),C','                                                       
*                                                                               
PUTO25   DS    0H                  DISPLAY COST2 FACTOR                         
         OC    PESTCF,PESTCF                                                    
         BZ    PUTO60              NO NEED TO DISPLAY COST2 FACTOR              
*                                                                               
         CLI   0(R1),C','          OTHER OPTION ENTERED ?                       
         BNE   *+8                 NO                                           
         LA    R1,1(R1)            YES - BUMP PAST ","                          
         MVC   0(5,R1),=C'COS2='                                                
         LA    R1,5(R1)                                                         
*                                                                               
         CP    PESTCF,=P'0'                                                     
         BNE   *+18                                                             
         MVC   0(3,R1),=C'0.0'     COS2=0.0                                     
         LA    R1,3(R1)                                                         
         B     PUTO25V                                                          
*                                                                               
         EDIT  (P5,PESTCF),(8,0(R1)),6,ALIGN=LEFT,FILL=0                        
         LA    R1,8(R1)                                                         
         LA    RF,8                MAX LOOP IS 8 TIMES                          
PUTO25H  BCTR  R1,0                                                             
         CLI   0(R1),C'.'          DECIMAL POINT?                               
         BE    PUTO25M                                                          
         CLI   0(R1),C'0'          TRAILING ZERO?                               
         BNE   PUTO25T                                                          
         MVI   0(R1),X'00'         CLEAR TRAILING ZERO                          
         BCT   RF,PUTO25H                                                       
*                                                                               
         DC    H'0'                SOMETHING IS WRONG...                        
*                                                                               
PUTO25M  AHI   R1,1                                                             
         MVI   0(R1),C'0'          NON-SIGNIFICANT ZERO                         
         LA    R1,1(R1)                                                         
         B     PUTO25V                                                          
*                                                                               
PUTO25T  AHI   R1,1                                                             
*                                                                               
PUTO25V  MVI   0(R1),C','                                                       
*                                                                               
*                                                                               
*                                                                               
PUTO60   DS    0H                                                               
         CLI   0(R1),C','          R1 POINTING TO "," ?                         
         BNE   *+8                 NO                                           
         MVI   0(R1),C' '          YES - CLEAR IT                               
         FOUT  ESTOPTSH                                                         
         BR    RE                  RETURN                                       
*                                                                               
*                                                                               
*        FIND UDEF ELEMENT & DISPLAY INFO                                       
*                                                                               
PUTUSER  DS    0H                                                               
         MVC   ESTDSC1,SPACES      CLEAR PREVIOUS INFO                          
         MVC   ESTDSC2,SPACES                                                   
         OI    ESTDSC1H+6,X'80'                                                 
         OI    ESTDSC2H+6,X'80'                                                 
*                                                                               
         LA    R6,PESTELEM                                                      
         USING PESTUDEF,R6                                                      
*                                                                               
PU10     CLI   0(R6),0                                                          
         BE    PUX                                                              
         CLI   0(R6),X'08'                                                      
         BE    PU20                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     PU10                                                             
*                                                                               
PU20     OC    SVE1USER,SVE1USER                                                
         BZ    *+10                                                             
         MVC   ESTDSC1,PEUSER1                                                  
*                                                                               
         OC    SVE2USER,SVE2USER                                                
         BZ    *+10                                                             
         MVC   ESTDSC2,PEUSER2                                                  
*                                                                               
         OI    ESTDSC1H+6,X'80'                                                 
         OI    ESTDSC2H+6,X'80'                                                 
*                                                                               
PUX      BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE REP - R2 POINTS TO INPUT FLD                                         
* CC EQUAL     - ZZZIO RETURNS REP RECORD                                       
* CC NOT EQUAL - ERROR DETECTED, HALF2 RETURNS ERROR CODE                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKREPCOD NTR1                                                                   
         CLI   5(R2),4                                                          
         BH    CKREPER             MAX 4 DIGITS                                 
         XC    KEY,KEY                                                          
         MVC   KEY(3),PESTKEY                                                   
         MVI   KEY+3,X'11'                                                      
         BRAS  RE,PACK                                                          
         CP    DUB,=P'0'                                                        
         BNE   CKREP5                                                           
CKREPER  LA    R3,FLDINV                                                        
         B     CKREPERX                                                         
*                                                                               
CKREP5   OI    DUB+7,X'0F'                                                      
         UNPK  KEY+4(4),DUB                                                     
         GOTO1 HIGH                                                             
         TM    8(R1),X'40'         DISK ERROR                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    CKREP10             FOUND                                        
         LA    R3,NOTFND                                                        
         B     CKREPERX                                                         
*                                                                               
CKREP10  GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',KEY+27,  +        
               ZZZIO,(TERMNAL,DMWORK)                                           
*                                                                               
CKREPX   J     SETCCEQ                                                          
*                                                                               
CKREPERX STH   R3,HALF2                                                         
         J     SETCCNEQ                                                         
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXITXIT1 XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* R2 = REP NAME FLD TO BE DISPLAYED                                             
* R7 = REP CODE TO BE LOOKED UP                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPREPNM NTR1                      DISPLAY REP NAME                             
         OC    0(L'PESTREP,R7),0(R7)                                            
         BZ    DREPNM_X                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+4(4),0(R7)                                                   
         MVC   KEY(3),PESTKAGY                                                  
         MVI   KEY+3,X'11'                                                      
         BRAS  RE,HIGH                                                          
         CLC   KEYSAVE(25),KEY                                                  
         BNE   DREPNM40                                                         
         GOTO1 VDATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'PRTFILE',         X        
               KEY+27,ZZZIO,(TERMNAL,DMWORK)                                    
         LA    R7,ZZZIO                                                         
         USING PREPRECD,R7                                                      
         MVC   8(L'ESTREPN,R2),PREPNAME                                         
         OI    6(R2),X'80'                                                      
         B     DREPNM_X                                                         
*                                                                               
DREPNM40 MVC   8(21,R2),=C'** REP NOT ON FILE **'                               
         OI    6(R2),X'80'                                                      
*                                                                               
DREPNM_X J     EXITXIT1                                                         
         DROP  R7                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*         INPUT:                                                                
*             R2 = A(INPUT FIELD)                                               
*             R3 = INPUT BLOCK                                                  
*         OUTPUT:                                                               
*             WORK = DATA OR NULLS                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTUSR   DS    0H                                                               
         USING UDEFD,R3                                                         
         ST    RE,SVRE                                                          
         ST    R4,SVR4                                                          
         XC    WORK,WORK                                                        
         OC    0(L'SVE1USER,R3),0(R3)  ANY INFO                                 
         BZ    XEDTUSR                                                          
         CLI   5(R2),0             CHECK FOR ANY INPUT                          
         BNE   EDTUSR5                                                          
         TM    UFLG1,X'80'         IS INPUT REQUIRED                            
         BNO   XEDTUSR                                                          
         B     MISSERR                                                          
*                                                                               
EDTUSR5  CLC   5(1,R2),ULEN        CHECK IF L'INPUT IS VALID                    
         BH    LONGERR             NOT VALID                                    
         CLI   UTYPE,C' '          ANY INPUT ALLOWED                            
         BNH   EDTUSR30                                                         
         ZIC   R1,5(R2)                                                         
         LA    R4,8(R2)                                                         
*                                                                               
         CLI   UTYPE,C'N'          IF TYPE S/B NUMERIC                          
         BNE   EDTUSR10                                                         
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BO    EDTUSR30                                                         
*                                                                               
EDTUSR6  CLI   0(R4),C'0'                                                       
         BL    EDTUSR7                                                          
         CLI   0(R4),C'9'                                                       
         BNH   EDTUSR9                                                          
*                                                                               
EDTUSR7  CLI   0(R4),C' '                                                       
         BE    EDTUSR9                                                          
         CLI   0(R4),C'/'                                                       
         BE    EDTUSR9                                                          
         CLI   0(R4),C'-'                                                       
         BNE   INVERR                                                           
*                                                                               
EDTUSR9  LA    R4,1(R4)                                                         
         BCT   R1,EDTUSR6                                                       
         B     EDTUSR30                                                         
*                                                                               
EDTUSR10 CLI   UTYPE,C'C'          IF TYPE S/B ALPHABETIC                       
         BNE   EDTUSR20                                                         
*                                                                               
EDTUSR15 CLI   0(R4),C'0'          ALLOW ALL INPUT EXCEPT NUMBERS               
         BL    EDTUSR19                                                         
         CLI   0(R4),C'9'                                                       
         BNH   INVERR                                                           
*                                                                               
EDTUSR19 LA    R4,1(R4)                                                         
         BCT   R1,EDTUSR15                                                      
         B     EDTUSR30                                                         
*                                                                               
EDTUSR20 CLI   UTYPE,C'D'                                                       
         BE    *+6                 INPUT MUST BE A DATE                         
         DC    H'0'                                                             
         GOTO1 VDATVAL,DMCB,(0,0(R4)),TDATE                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         L     R1,0(R1)                                                         
         ZIC   R4,5(R2)                                                         
         SR    R1,R4                                                            
         BNZ   INVERR                                                           
*                                                                               
EDTUSR30 ZIC   R1,5(R2)            R1=L(INPUT)                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       MOVE INPUT INTO WORK                         
*                                                                               
XEDTUSR  L     R4,SVR4                                                          
         L     RE,SVRE                                                          
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
DTERR    LA    R3,20               INVALID DATE FORMAT                          
         B     ERROR                                                            
*                                                                               
INVERR   LA    R3,FLDINV           INVALID                                      
         B     ERROR                                                            
*                                                                               
MISSERR  LA    R3,1                MISSING                                      
         B     ERROR                                                            
*                                                                               
LONGERR  LA    R3,32               INPUT TOO LONG                               
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
MASTTAB  DS    0H                                                               
*NOP*    DC    C'DDNYF11T'          DDS                                         
*NOP*    DC    C'DDNY720T'                                                      
*NOP*    DC    C'DX03901T'                                                      
*NOP*    DC    C'DDNYD26T'          DDS                                         
         DC    C'DDNYD03T'                                                      
*NOP*    DC    C'DDNY700T'                                                      
         DC    C'DDNYE00T'                                                      
         DC    C'DX06200T'         (WAS DDNY700T)                               
         DC    C'HDTO80CT'          HDTO                                        
         DC    C'HDTO800T'          HDTO     (WAS HDTO830T)                     
         DC    C'DDL1136T'          DDS-LA                                      
         DC    C'DDL1137T'          DDS-LA                                      
         DC    C'DDL1138T'          DDS-LA                                      
         DC    X'FFFF'             END OF TABLE                                 
*                                                                               
LINID    DS    CL4                 LINE ID FROM FAFACTS                         
LINADDR  DS    CL4                 LINE ID FROM FAFACTS                         
*                                                                               
SVRE     DS    F                                                                
SVR4     DS    F                                                                
SESTTEST DS    XL1                 OLD PESTTEST (FOR SFH VALIDATION)            
MASTER   DS    CL1                                                              
OANPROD  DS    CL2              OAN AGENCY CODE - IF X0000' NON OAN L01         
TDATE    DS    CL6                                                              
MYAREA   DS    CL24                                                             
ZEROS    DC    40C'0'                                                           
SPACES   DC    CL40' '                                                          
VIRERR   DC    H'0'                                                             
SVESTST  DS    CL6                 OLD START DATE                               
SVESTEND DS    CL6                 OLD END DATE                                 
ELEM     DS    CL255                                                            
NOTFND   EQU   53                                                               
PROFERR  EQU   61                                                               
DATEERR  EQU   80                                                               
DATERR   EQU   64                                                               
DATERR1  EQU   65                                                               
STDERR   EQU   66                                                               
ENDERR   EQU   67                                                               
YRERR    EQU   68                                                               
NOCHGERR EQU   105               CAN'T CHANGE THIS DATA                         
STATERR  EQU   2                                                                
PERMERR  EQU   2                                                                
FLDINV   EQU   2                                                                
ENDINV   EQU   221    EST. END DATE CAN'T BE BEFORE START OF CURRENT YR         
         EJECT                                                                  
       ++INCLUDE PLFMWRK                                                        
         EJECT                                                                  
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF3D                                                       
         EJECT                                                                  
         ORG   HDRLAST+2000                                                     
         DS    CL1                                                              
SVP1USER DS    CL20                PRD USER DESCRIPTION FIELD 1                 
SVP1TYPE DS    CL1                          TYPE                                
SVP1LEN  DS    XL1                          LENGTH                              
SVP1FLG1 DS    XL1                          FLAG                                
SVP1FLG2 DS    XL1                          FLAG                                
SVP2USER DS    CL20                PRD USER DESCRIPTION FIELD 2                 
SVP2TYPE DS    CL1                          TYPE                                
SVP2LEN  DS    XL1                          LENGTH                              
SVP2FLG1 DS    XL1                          FLAG                                
SVP2FLG2 DS    XL1                          FLAG                                
SVE1USER DS    CL20                EST USER DESCRIPTION FIELD 1                 
SVE1TYPE DS    CL1                          TYPE                                
SVE1LEN  DS    XL1                          LENGTH                              
SVE1FLG1 DS    XL1                          FLAG                                
SVE1FLG2 DS    XL1                          FLAG                                
SVE2USER DS    CL20                EST USER DESCRIPTION FIELD 2                 
SVE2TYPE DS    CL1                          TYPE                                
SVE2LEN  DS    XL1                          LENGTH                              
SVE2FLG1 DS    XL1                          FLAG                                
SVE2FLG2 DS    XL1                          FLAG                                
SVULNQ   EQU   *-SVP1USER                                                       
*                                                                               
F0PROF   DS    CL16               FO PROFILE READ IN 00                         
*                                 WHEN CLIENT IS VALIDATED                      
SVACCAGY DS    CL24              ROOM FOR 12 ACC AGENCYS                        
SVCTAGY  DS    CL2               CTFILE ID                                      
*                                                                               
SVXFRSY  DS    CL3              TRANSFERRED FROM SYSTEM                         
SVXFRPR  DS    CL3              TRANSFERRED FROM PROGRAM                        
*                                                                               
SVCLSTAT DS    XL1              PCLTSTAT FROM PCLTREC SET IN 00                 
*                                 WHEN CLIENT IS VALIDATED                      
SVAGYSW  DS    XL1           AGENCY "SWITCH" SET IN 00 AT CKVALC..              
*                               X'01' = WESTERN AGENCY (OR SJR)                 
*                                                                               
         EJECT                                                                  
UDEFD    DSECT                                                                  
UDEF     DS    CL20                                                             
UTYPE    DS    CL1                                                              
ULEN     DS    XL1                                                              
UFLG1    DS    XL1                                                              
UFLG2    DS    XL1                                                              
         SPACE 3                                                                
*                                                                               
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL20                                                             
*                                                                               
         EJECT                                                                  
PREPRECD DSECT                                                                  
       ++INCLUDE PREPREC                                                        
         EJECT                                                                  
*                                                                               
BILPROFD DSECT                                                                  
       ++INCLUDE PBILPROF                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDBKELEM                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016PPLFM03   09/11/06'                                      
         END                                                                    
