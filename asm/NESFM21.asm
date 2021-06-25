*          DATA SET NESFM21    AT LEVEL 088 AS OF 10/31/05                      
*PHASE T31C21A,*                                                                
         TITLE 'T31C21  PG ESTIMATE RECORDS'                                    
T31C21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*PGEST*                                                        
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
         CLI   MODE,PRINTREP       REPORT                                       
         BE    LR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING PGKEY,R6                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVI   NOPTFLG,0                                                        
         MVI   ESTZERO,0                                                        
*                                                                               
         LA    R2,PNGMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   PGKAM,BAGYMD                                                     
*                                                                               
         CLI   ACTNUM,ACTLIST      IF LIST                                      
         BNE   *+8                                                              
         MVI   NOPTFLG,1           FIELDS ARE OPTIONAL                          
         CLI   ACTNUM,ACTREP       IF REPORT                                    
         BNE   *+8                                                              
         MVI   NOPTFLG,1           FIELDS ARE OPTIONAL                          
*                                                                               
         LA    R2,PNGCLIH          CLIENT                                       
         GOTO1 VALIFLD                                                          
         BZ    VK10                                                             
         GOTO1 VALICLT                                                          
         MVC   PGKCLT,BCLT                                                      
         MVC   PNGCLNM,CLTNM                                                    
         OI    PNGCLNMH+6,X'80'                                                 
*                                                                               
VK10     LA    R2,PNGPROH          PRODUCT                                      
         GOTO1 VALIFLD                                                          
         BZ    VK20                                                             
         GOTO1 VALIPRD                                                          
         MVC   PGKNETP,QPRD                                                     
         MVC   PNGPRNM,PRDNM                                                    
         OI    PNGPRNMH+6,X'80'                                                 
*                                                                               
VK20     DS    0H                                                               
         MVI   MYFLAG,0                                                         
*                                                                               
         OI    MYFLAG,PRDPOL                                                    
         CLC   =C'POL',QPRD                                                     
         BE    *+12                                                             
         NI    MYFLAG,X'FF'-PRDPOL                                              
         OI    MYFLAG,PRDBRND                                                   
*                                                                               
         LA    R2,PNGESTH          ESTIMATE                                     
         GOTO1 VALIFLD                                                          
         BZ    VK50                                                             
         LTR   R0,R0               ..IF ZERO ESTIMATE                           
         BNZ   VK25                                                             
         CLC   =C'POL',QPRD        ..PROD CAN NOT = POL                         
         BE    EINV                                                             
         MVI   PGKNETE,0                                                        
         MVI   ESTZERO,C'Y'                                                     
         B     VK50                                                             
VK25     CLI   ACTNUM,ACTREP       IF REPORT                                    
         BNE   VK30                                                             
         STCM  R0,1,PGKNETE                                                     
         B     VK50                                                             
VK30     GOTO1 VALIEST                                                          
         MVC   PGKNETE,BEST                                                     
*                                                                               
         CLI   BEST,0                                                           
         BNE   VK40                                                             
         CLC   =C'POL',QPRD                                                     
         BE    EINV                                                             
         B     VK50                                                             
*                                                                               
VK40     DS    0H                                                               
         CLC   =C'POL',QPRD                                                     
         BNE   EINV                                                             
*                                                                               
VK50     MVC   KEY(13),SVKEY       SET KEY                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PGKEY,R6                                                         
         MVC   PNGMED,QMED                                                      
         GOTO1 CLUNPK,DMCB,PGKCLT,PNGCLI                                        
         OI    PNGCLIH+6,X'80'                                                  
         MVC   QPRD,PGKNETP                                                     
         MVC   PNGPRO,PGKNETP                                                   
         OI    PNGPROH+6,X'80'                                                  
         ZIC   RE,PGKNETE                                                       
         EDIT  (RE),(3,PNGEST),ALIGN=LEFT                                       
         LTR   RE,RE                  ..IF ZERO ESTIMATE                        
         BNZ   DK10                                                             
         MVC   PNGEST(3),=C'000'      ..PUT OUT 3 ZEROS                         
         MVI   ESTZERO,C'Y'                                                     
DK10     OI    PNGESTH+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD TYPE ELEMENT                           
         LA    R6,ELEM                                                          
         USING PGNETELM,R6                                                      
         MVI   PGNETID,PGNETIDQ                                                 
         MVI   PGNETELN,PGNETLNQ                                                
         CLI   ESTZERO,C'Y'        ..IF ZERO EST                                
         BE    VR20                ..ONLY ALLOW PGBRAND                         
*                                                                               
         LA    R2,PNGCHPH          CHARGE PERIOD                                
         TM    MYFLAG,PRDPOL                                                    
         BO    VR5                                                              
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR15                                                             
*                                                                               
VR5      GOTO1 VALIFLD                                                          
         CLI   5(R2),0                                                          
         BE    EINV                                                             
         CLI   NFLD+2,C'1'         AND 3D DIGIT =1                              
         BE    VR10                                                             
         CLI   NFLD+2,C'2'               OR = 2                                 
         BNE   EINV                                                             
VR10     MVC   PGNETCP,NFLD                                                     
*                                                                               
VR15     LA    R2,PNGACCH          ACCOUNT                                      
         TM    MYFLAG,PRDPOL                                                    
         BO    VR17                                                             
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR20                                                             
*                                                                               
VR17     GOTO1 VALIFLD                                                          
         MVC   PGNETACC,NFLD                                                    
         MVC   PGNETAC2,NFLD+3                                                  
*                                                                               
VR20     LA    R2,PNGPGBH          PG BRAND                                     
         TM    MYFLAG,PRDBRND                                                   
         BO    VR22                                                             
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR25                                                             
*                                                                               
VR22     CLC   =C'POL',PNGPRO      ..IF POL                                     
         BE    VR25                ..SKIP PGBRAND                               
         GOTO1 VALIFLD                                                          
         MVC   PGNETPB,NFLD                                                     
         MVC   PGNETPB2,NFLD+3                                                  
         CLI   ESTZERO,C'Y'                                                     
         BE    VR70                                                             
*                                                                               
VR25     LA    R2,PNGPGEH          PG ESTIMATE                                  
         TM    MYFLAG,PRDPOL                                                    
         BO    VR26                                                             
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR27                                                             
*                                                                               
VR26     GOTO1 VALIFLD                                                          
         LTR   R0,R0               CHK NUMERIC                                  
         BZ    EINV                                                             
         EDIT  (R0),(4,PGNETPE),FILL=0                                          
*                                                                               
VR27     LA    R2,PNGEVCH          EVENT CODE                                   
         TM    MYFLAG,PRDPOL                                                    
         BO    VR28                                                             
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR29                                                             
*                                                                               
VR28     GOTO1 VALIFLD                                                          
         MVC   PGNETEC,NFLD                                                     
*                                                                               
         MVI   NOPTFLG,1                                                        
         MVI   PGNETMB,C'N'                                                     
*                                                                               
VR29     LA    R2,PNGMLTH          MULTIBRAND                                   
         TM    MYFLAG,PRDPOL                                                    
         BO    VR29A                                                            
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR31                                                             
*                                                                               
VR29A    GOTO1 VALIFLD                                                          
         BZ    *+10                                                             
         MVC   PGNETMB,NFLD                                                     
         CLI   PGNETMB,C'Y'                                                     
         BE    VR30                                                             
         CLI   PGNETMB,C'N'                                                     
         BNE   EINV                                                             
VR30     MVC   PNGMLT,PGNETMB                                                   
         OI    PNGMLTH+6,X'80'                                                  
*                                                                               
         MVI   NOPTFLG,1                                                        
         MVI   PGNETNOB,C'Y'                                                    
VR31     LA    R2,PNGNOBH          NO BRAND                                     
         TM    MYFLAG,PRDPOL                                                    
         BO    VR32                                                             
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR70                                                             
*                                                                               
VR32     GOTO1 VALIFLD                                                          
         BZ    VR35                                                             
         MVC   PGNETNOB,NFLD                                                    
         CLI   PGNETNOB,C'Y'                                                    
         BE    VR35                                                             
         CLI   PGNETNOB,C'N'                                                    
         BNE   EINV                                                             
VR35     MVC   PNGNOB,PGNETNOB                                                  
         OI    PNGNOBH+6,X'80'                                                  
*                                                                               
VR70     DS    0H                  BRAND SUFFIX                                 
         LA    R2,PNGBSFXH                                                      
         TM    MYFLAG,PRDBRND                                                   
         BO    VR75                                                             
         CLI   5(R2),0                                                          
         BNE   EINV                                                             
         B     VR100                                                            
*                                                                               
VR75     GOTO1 VALIFLD                                                          
         BZ    EMIS                                                             
         CLI   5(R2),2                                                          
         BNE   EINV                                                             
*                                                                               
         CLI   NFLD,C'A'                                                        
         BL    EINV                                                             
         CLI   NFLD,C'Z'                                                        
         BH    EINV                                                             
*                                                                               
         CLI   NFLD+1,C'A'                                                      
         BL    EINV                                                             
         CLI   NFLD+1,C'Z'                                                      
         BH    EINV                                                             
*                                                                               
         OC    NFLD,SPACES                                                      
         MVC   PGNETBFX,NFLD                                                    
*                                                                               
VR100    GOTO1 ADDELEM                                                          
*                                                                               
VRX      B     DR                                                               
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,PNGCHPH                                                       
         BAS   RE,CLRSCRN          CLEAR SCREEN                                 
*&&DO                                                                           
         LA    R2,PNGFYEH                                                       
         XC    PNGFYE,PNGFYE                                                    
         FOUT  (R2)                                                             
         LA    R2,PNGXFRH                                                       
         XC    PNGXFR,PNGXFR                                                    
         FOUT  (R2)                                                             
*&&                                                                             
         LA    R2,PNGBSFXH                                                      
         XC    PNGBSFX,PNGBSFX                                                  
         FOUT  (R2)                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         USING PGNETELM,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         LA    R2,PNGCHPH                                                       
         MVC   PNGCHP,PGNETCP      CHARGE PERIOD                                
         FOUT  (R2)                                                             
*                                                                               
         CLI   PGNETELN,PGNETLNQ   NEW X'30' ELEMENT                            
         BNE   DR5                                                              
*                                                                               
         LA    R2,PNGBSFXH                                                      
         MVC   PNGBSFX,PGNETBFX    BRAND SUFFIX                                 
         FOUT  (R2)                                                             
*                                                                               
DR5      DS    0H                                                               
         LA    R2,PNGACCH          ACCOUNT                                      
         MVC   PNGACC(3),PGNETACC                                               
         MVC   PNGACC+3(3),PGNETAC2                                             
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGPGBH          PG BRAND                                     
         MVC   PNGPGB(3),PGNETPB                                                
         MVC   PNGPGB+3(1),PGNETPB2                                             
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGPGEH          PG EST                                       
         MVC   PNGPGE,PGNETPE                                                   
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGEVCH          EVENT CODE                                   
         MVC   PNGEVC,PGNETEC                                                   
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGMLTH          MULTI-BRAND                                  
         MVC   PNGMLT,PGNETMB                                                   
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGNOBH          NOBRAND                                      
         MVC   PNGNOB,PGNETNOB                                                  
         FOUT  (R2)                                                             
*&&DO                                                                           
         LA    R2,PNGPCTH          99.3%                                        
         MVC   PNGPCT,PGNET993                                                  
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGACTH          USE ASSIGNED                                 
         MVC   PNGACT,PGNETASS                                                  
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGCOMH                                                       
         MVI   PNGCOM,0            DEFAULT = N OR BLANK                         
         CLI   PNGACT,X'40'        IF ASSIGNED IS SET                           
         BNH   *+8                                                              
         MVI   PNGCOM,C'N'           DEFAULT=NO                                 
         FOUT  (R2)                                                             
*                                                                               
DR20     CLI   1(R6),X'19'         IF OLD ELEM                                  
         BE    DRX                 SKIP IT                                      
*                                                                               
         LA    R2,PNGCOMH          COMMISSION-ONLY ESTIMATE                     
         MVC   PNGCOM,PGNETCOM                                                  
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGFYEH          FISCAL YEAR END                              
         OC    PGNETFYE,PGNETFYE                                                
         BZ    DRX                                                              
         GOTO1 DATCON,DMCB,(2,PGNETFYE),(8,PNGFYE)                              
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PNGXFRH          ACCOUNTING TRANSFER ESTIMATE                 
         EDIT  (B1,PGNETXFR),(3,PNGXFR),ALIGN=LEFT                              
         FOUT  (R2)                                                             
*&&                                                                             
DRX      B     EXIT                                                             
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       MVI   NLISTS,13           SET NUM OF LIST LINES                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR1                                                              
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
LR1      OC    KEY(13),KEY                                                      
         BNZ   LR2                                                              
         MVC   KEY(13),SVKEY                                                    
*                                                                               
LR2      GOTO1 HIGH                                                             
         B     LR6                                                              
*                                                                               
LR4      GOTO1 SEQ                                                              
*                                                                               
LR6      DS    0H                                                               
         CLC   KEY(3),KEYSAVE                                                   
         BNE   LRX                                                              
         OC    SVKEY+3(2),SVKEY+3  CLIENT                                       
         BZ    *+14                                                             
         CLC   SVKEY+3(2),KEY+3                                                 
         BNE   LR4                                                              
         CLI   ESTZERO,C'Y'        IF EST =000                                  
         BE    *+14                THEN FILTER ON 0                             
         OC    SVKEY+5(1),SVKEY+5  ESTIMATE                                     
         BZ    *+14                                                             
         CLC   SVKEY+5(1),KEY+5                                                 
         BNE   LR4                                                              
         OC    SVKEY+6(3),SVKEY+6  PRODUCT                                      
         BZ    *+14                                                             
         CLC   SVKEY+6(3),KEY+6                                                 
         BNE   LR4                                                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PGKEY,R6                                                         
         MVI   ELCODE,X'30'        SKIP OLD RECORDS                             
         BAS   RE,GETEL                                                         
         BNE   LR4                                                              
         L     R6,AIO                                                           
         CLI   MODE,PRINTREP                                                    
         BE    LR20                                                             
         LA    R4,LISTAR                                                        
         USING LINED,R4                                                         
         MVC   LISTAR,SPACES                                                    
         GOTO1 CLUNPK,DMCB,PGKCLT,LLCLT                                         
         EDIT  PGKNETE,(3,LLEST),FILL=0                                         
         MVC   LLPRD,PGKNETP                                                    
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR4                                                              
         USING PGNETELM,R6                                                      
         MVC   LLCHP,PGNETCP                                                    
LR10     GOTO1 LISTMON                                                          
         B     LR4                                                              
*                                                                               
LR20     DS    0H                                                               
         USING PGKEY,R6                                                         
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         CLC   PGKCLT,CLTSV                                                     
         BE    LR22                                                             
         MVC   CLTSV,PGKCLT                                                     
         GOTO1 CLUNPK,DMCB,PGKCLT,PCLT                                          
         B     *+14                FORCE EST IF CLIENT CHANGES                  
LR22     CLC   PGKNETE,ESTSV                                                    
         BE    LR24                                                             
         MVC   ESTSV,PGKNETE                                                    
         EDIT  PGKNETE,(3,PEST),FILL=0                                          
LR24     MVC   PPROD,PGKNETP                                                    
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR4                                                              
         USING PGNETELM,R6                                                      
         MVC   PPER,PGNETCP                                                     
         MVC   PACC(3),PGNETACC                                                 
         MVC   PACC+3(3),PGNETAC2                                               
         MVC   PBRAND(3),PGNETPB                                                
         MVC   PBRAND+3(1),PGNETPB2                                             
         MVC   PGEST,PGNETPE                                                    
         MVC   PEVENT,PGNETEC                                                   
         MVC   PMULTI,PGNETMB                                                   
*!!!     MVC   PNET99,PGNET993                                                  
*!!!     MVC   PASSIGN,PGNETASS                                                 
         MVC   PNOBRAN,PGNETNOB                                                 
*!!!     MVI   PCOM,C'N'           SET DEFAULT                                  
         CLI   PGNETELN,X'19'       OLD ELEM                                    
         BNH   LR30                DOES NOT HAVE FYE AND XFR DATA               
*!!!     MVC   PCOM,PGNETCOM                                                    
         MVC   PBRSFX+2(2),PGNETBFX                                             
*!!!     EDIT  (B1,PGNETXFR),(3,PXFR)                                           
         OC    PGNETFYE,PGNETFYE                                                
         BZ    LR30                                                             
*!!!     GOTO1 DATCON,DMCB,(2,PGNETFYE),(8,PYREND)                              
LR30     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR4                                                              
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK PG ESTIMATES'                                    
         SSPEC H2,46,C'--------------------'                                    
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8                                                            
         USING PLINED,R2                                                        
*!!!     MVC   PYREND(7),=C' SHORT '                                            
         MVC   PPER,=C'CHR'                                                     
         MVC   PGEST,=C'PG  '                                                   
*!!!     MVC   PXFR,=C'ORG'                                                     
         MVC   PBRSFX(5),=C'BRAND '                                             
         LA    R2,H9                                                            
         MVC   PCLT,=C'CLT'                                                     
         MVC   PCLT+132(3),DSH                                                  
         MVC   PEST,=C'EST'                                                     
         MVC   PEST+132(3),DSH                                                  
         MVC   PPROD,=C'PRD'                                                    
         MVC   PPROD+132(3),DSH                                                 
         MVC   PPER,=C'PER'                                                     
         MVC   PPER+132(3),DSH                                                  
         MVC   PACC+1(3),=C'ACC'                                                
         MVC   PACC+132(3),DSH                                                  
         MVC   PBRAND,=C'BRND'                                                  
         MVC   PBRAND+132(3),DSH                                                
         MVC   PGEST,=C'EST '                                                   
         MVC   PGEST+132(4),DSH                                                 
         MVC   PEVENT(5),=C'EVENT'                                              
         MVC   PEVENT+132(6),DSH                                                
         MVC   PMULTI(2),=C'MU'                                                 
         MVC   PMULTI+132(2),DSH                                                
*!!!     MVC   PNET99(2),=C'99'                                                 
*!!!     MVC   PNET99+132(2),DSH                                                
*!!!     MVC   PASSIGN(2),=C'AS'                                                
*!!!     MVC   PASSIGN+132(2),DSH                                               
         MVC   PNOBRAN(2),=C'NO'                                                
         MVC   PNOBRAN+132(2),DSH                                               
*!!!     MVC   PCOM(2),=C'CM'                                                   
*!!!     MVC   PCOM+132(2),DSH                                                  
*!!!     MVC   PYREND,=C' START  '                                              
*!!!     MVC   PYREND+132(8),DSH                                                
*!!!     MVC   PXFR,=C'EST'                                                     
*!!!     MVC   PXFR+132(3),DSH                                                  
         MVC   PBRSFX,=C'SUFFIX'                                                
         MVC   PBRSFX+132(6),DSH                                                
         B     EXIT                                                             
DSH      DC    C'----------'                                                    
         EJECT                                                                  
*                                                                               
* ROUTINE TO CLEAR THE SCREEN                                                   
* FROM FIELD AT R2                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'20'         SKIP PROTECTED FIELDS                        
         BO    CS4                                                              
         EX    RE,CSCLC                                                         
         BE    CS4                                                              
         EX    RE,CSOC                                                          
         BZ    CS4                                                              
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS4      LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS2                                                              
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
* ERROR EXITS                                                                   
*                                                                               
EMIS     MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
EPRD     MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMCDD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMCED                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         ORG   SYSSPARE                                                         
MYWORK   DS    0CL1          ***** MY WORK AREA *****                           
MYKEY    DS    CL20                                                             
ESTZERO  DS    CL1                                                              
CLTSV    DS    CL2                                                              
ESTSV    DS    CL1                                                              
*                                                                               
MYFLAG   DS    XL1                                                              
PRDBRND  EQU   X'01'              BRAND PRODUCT                                 
PRDPOL   EQU   X'02'              POL PRODUCT                                   
*                                                                               
         SPACE 2                                                                
PLINED   DSECT                                                                  
PSTART   DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PPROD    DS    CL3                                                              
         DS    CL2                                                              
PPER     DS    CL3                                                              
         DS    CL2                                                              
PACC     DS    CL6                                                              
         DS    CL2                                                              
PBRAND   DS    CL4                                                              
         DS    CL2                                                              
PGEST    DS    CL4                                                              
         DS    CL2                                                              
PEVENT   DS    CL6                                                              
         DS    CL2                                                              
PMULTI   DS    CL1                                                              
         DS    CL2                                                              
*!!!PNET99   DS    CL1                                                          
*!!!         DS    CL2                                                          
*!!!PASSIGN  DS    CL1                                                          
*!!!         DS    CL2                                                          
PNOBRAN  DS    CL1                                                              
         DS    CL2                                                              
*!!!PCOM     DS    CL1                                                          
*!!!         DS    CL2                                                          
*!!!PYREND   DS    CL8                                                          
*!!!         DS    CL2                                                          
*!!!PXFR     DS    CL3                                                          
*!!!         DS    CL2                                                          
PBRSFX   DS    CL6                                                              
PLINLEN  EQU   *-PSTART                                                         
         DS    CL2                                                              
         EJECT                                                                  
LINED    DSECT                     LIST LINE DSECT                              
LLCLT    DS    CL3                                                              
         DS    CL8                                                              
LLEST    DS    CL3                                                              
         DS    CL7                                                              
LLPRD    DS    CL3                                                              
         DS    CL6                                                              
LLCHP    DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088NESFM21   10/31/05'                                      
         END                                                                    
