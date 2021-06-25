*          DATA SET SPSFM54    AT LEVEL 034 AS OF 04/30/13                      
*PHASE T21754A                                                                  
         TITLE 'SPSFM54 - ESTIMATE COPY'                                        
         PRINT NOGEN                                                            
T21754   CSECT                                                                  
         NMOD1 0,**1754**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,VALREC                                                      
         BE    VCOP                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
         USING ESTHDR,R4                                                        
VK       LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,X'00'                                                   
*                                                                               
         MVC   ESTMEDN,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    ESTMEDNH+6,X'80'      AND PRODUCT NAME                           
         MVC   ESTCLIN,SPACES                                                   
         OI    ESTCLINH+6,X'80'                                                 
         MVC   ESTPRDN,SPACES                                                   
         OI    ESTPRDNH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   ESTMEDN,MEDNM         MEDIA NAME                                 
         OI    ESTMEDNH+6,X'80'                                                 
         MVC   EKEYAM,BAGYMD         COPY MEDIA INTO KEY                        
*                                                                               
         CLI   QMED,C'C'           COPY INVALID FOR MEDIUM C AND N              
         BE    ERRINV                                                           
         CLI   QMED,C'N'                                                        
         BE    ERRINV                                                           
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTCLIKH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
         MVC   ESTCLIN,CLTNM         CLIENT NAME                                
         OI    ESTCLINH+6,X'80'                                                 
         MVC   EKEYCLT,BCLT          COPY CLIENT INTO KEY                       
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTPRDKH           PRODUCT                                    
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD               VALIDATE PRODUCT CODE AND TRANSMIT         
         MVI   AAAOK,C'N'            PRODUCT NAME                               
         MVC   ESTPRDN,PRDNM                                                    
         OI    ESTPRDNH+6,X'80'                                                 
*                                                                               
         MVC   ERRNUM,=AL2(NOTRDCPY) NO TRADE COPY                              
         CLI   QPRD+2,C'#'                                                      
         BE    SPERREX                                                          
*                                                                               
         MVC   EKEYPRD,QPRD          COPY PRODUCT INTO KEY                      
         OC    EKEYPRD,SPACES                                                   
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTESTKH           ESTIMATE                                   
         CLI   ESTESTKH+5,0                                                     
         BE    ERRMIS                                                           
         BAS   RE,SPTOZER                                                       
*                                                                               
         MVI   ESTESTKH+5,3          LENGTH                                     
         CLI   ESTESTK+2,C' '                                                   
         BH    VK15                                                             
         MVI   ESTESTKH+5,2                                                     
         CLI   ESTESTK+1,C' '                                                   
         BH    VK15                                                             
         MVI   ESTESTKH+5,1                                                     
*                                                                               
VK15     GOTO1 VALIEST               VALIDATE ESTIMATE CODE AND                 
         MVC   ESTESTN,ESTNM         TRANSMIT ESTIMATE DESCRIPTION              
         OI    ESTESTNH+6,X'80'                                                 
         MVC   EKEYEST,BEST          SAVE ESTIMATE CODE INTO KEY                
         MVC   ESTKEY,KEY            SAVE ESTIMATE KEY                          
*                                                                               
**********************************************************************          
*                                                                               
         GOTO1 HIGH                  POL ESTIMATE RECORD MUST EXIST             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SPERREX                                                          
*                                                                               
*                                    READ POL ESTIMATE RECORD INTO              
         MVC   AIO,AIO1              AIO1 AND MOVE START AND END DATE           
         GOTO1 GETREC                TO SCREEN                                  
         L     R4,AIO1                                                          
         GOTO1 DATCON,DMCB,ESTART,(5,WORK)                                      
         GOTO1 (RF),(R1),EEND,(5,WORK+9)                                        
         MVI   WORK+8,C'-'                                                      
         MVC   ESTEDAT,WORK                                                     
         OI    ESTEDATH+6,X'80'                                                 
*                                                                               
         MVC   ERRNUM,=AL2(MSTREST)   POL RECORD CANNOT BE A MASTER             
         CLI   EMSTRIND,0             OR SUB-ESTIMATE                           
         BNE   SPERREX                                                          
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        DCOP --- DISPLAY COPY                                        *         
***********************************************************************         
*                                                                               
DCOP     LA    R2,ESTDSPH            CLEAR 'BRANDS NOT OPEN' AND                
DCOP20   ZIC   RE,0(R2)              'BRANDS ALREADY OPEN' FIELDS               
         AHI   RE,-9                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)                                                    
         BE    DCOP30                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
DCOP30   LA    R2,9(RE,R2)                                                      
         LA    R0,ESTLSTH                                                       
         CR    R2,R0                                                            
         BNH   DCOP20                                                           
*                                                                               
**********************************************************************          
*                                                                               
DCOP40   XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY         READ CLIENT RECORD INTO AIO2               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE       MARK OFF WHETHER OR NOT THE                
         BE    *+6                   PRODUCTS ARE OPEN IN CLIST                 
         DC    H'0'                                                             
         USING CLTHDR,R3                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO2                                                          
         LA    R3,CLIST                                                         
         MVC   AIO,AIO1                                                         
*                                                                               
DCOP50   XC    KEY,KEY                                                          
         MVC   KEY,ESTKEY            IF BRAND FOR POL ESTIMATE NOT OPEN         
         MVC   KEY+4(3),0(R3)        AND PRD EXISTS SET CODE TO 0               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+12                                                             
         MVI   3(R3),1               IF BRAND FOR POL ESTIMATE OPEN             
         B     DCOP55                SET CODE TO 1                              
         MVI   3(R3),0                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
         MVC   KEY+4(3),0(R3)        IF BRAND FOR POL ESTIMATE NOT OPEN         
         GOTO1 HIGH                  AND PRD DOES NOT EXIST SET CODE            
         CLC   KEY(13),KEYSAVE       TO 3                                       
         BE    DCOP55                                                           
         MVI   3(R3),3                                                          
*                                                                               
DCOP55   LA    R3,4(R3)              BUMP TO NEXT PRODUCT IN CLIST              
         CLI   0(R3),0                                                          
         BNE   DCOP50                                                           
*                                                                               
**********************************************************************          
*                                                                               
         LA    R2,ESTDSPH                                                       
         MVC   8(21,R2),=C'** BRANDS NOT OPEN **'                               
         OI    ESTDSPH+6,X'80'                                                  
*                                                                               
         ZAP   HALF,=P'0'            DISPLAY LIST OF PRODUCTS THAT ARE          
         L     R3,AIO2               NOT OPEN                                   
         LA    R3,CLIST                                                         
DCOP60   CLI   3(R3),0                                                          
         BNE   DCOP80                IF PRD FROM CLIST SET TO 0 ...             
DCOP70   BAS   RE,FMTPRD             FORMAT IT                                  
*                                                                               
DCOP80   LA    R3,4(R3)              BUMP TO NEXT PRODUCT IN CLIST              
         CLI   0(R3),0                                                          
         BNE   DCOP60                                                           
*                                                                               
**********************************************************************          
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(25,R2),=C'** BRANDS ALREADY OPEN **'                           
         OI    6(R2),X'80'                                                      
*                                                                               
         ZAP   HALF,=P'0'            DISPLAY LIST OF PRODUCTS THAT ARE          
         L     R3,AIO2               OPEN                                       
         LA    R3,CLIST                                                         
         DROP  R3                                                               
DCOP90   CLI   3(R3),1                                                          
         BNE   *+8                   IF PRD FROM CLIST SET TO 1 ...             
         BAS   RE,FMTPRD             FORMAT IT                                  
*                                                                               
         LA    R3,4(R3)              BUMP TO NEXT PRODUCT IN CLIST              
         CLI   0(R3),0                                                          
         BNE   DCOP90                                                           
*                                                                               
**********************************************************************          
*                                                                               
         LA    R2,ESTPRD1H                                                      
DCOP100  CLI   5(R2),0                                                          
         BNE   XIT                                                              
*                                                                               
         ZIC   R0,0(R2)              IF NO PRODUCTS WERE ENTERED TO             
         AR    R2,R0                 COPY ... RETURN ERROR                      
         CLI   0(R2),9                                                          
         BNE   DCOP100                                                          
         LA    R2,ESTPRD1H                                                      
         MVC   ERRNUM,=AL2(ENTPRD)                                              
         B     SPERREX2                                                         
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
FMTPRD   CP    HALF,=P'0'                                                       
         BNE   FMTPRD2                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,ESTLSTH                                                       
         CR    R2,R0                 TEST PAST EOS                              
         BH    FMTPRDX                                                          
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'20'                                                      
*                                                                               
FMTPRD2  MVC   0(3,R4),0(R3)                                                    
         LA    R4,4(R4)                                                         
         SP    HALF,=P'1'                                                       
         OI    6(R2),X'80'                                                      
FMTPRDX  BR    RE                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        VCOP --- VALIDATE COPY                                      *          
**********************************************************************          
*                                                                               
         USING ESTHDR,R4                                                        
VCOP     L     R4,AIO                                                           
*                                                                               
         XC    CLIPRO,CLIPRO         GET CLIENT'S SVFOPROF                      
         MVC   CLIPRO(4),=C'S0F0'                                               
         MVC   CLIPRO+4(2),AGENCY                                               
         MVC   CLIPRO+6(1),QMED                                                 
         MVC   CLIPRO+7(3),QCLT                                                 
         GOTO1 GETPROF,DMCB,CLIPRO,WORK,DATAMGR                                 
         MVC   SVF0PROF,WORK                                                    
*                                                                               
         MVC   ERRNUM,=AL2(NOMATCH)  MAKE SURE POL ESTIMATE HAS CORRECT         
         CLI   SVF0PROF+2,C'Y'       FILTERS VS F0 PROFILE                      
         BNE   VCOP10                                                           
         CLI   EPROF,C' '                                                       
         BNH   SPERREX                                                          
*                                                                               
VCOP10   CLI   SVF0PROF+3,C'Y'                                                  
         BNE   VCOP20                                                           
         CLI   EPROF+1,C' '                                                     
         BNH   SPERREX                                                          
*                                                                               
VCOP20   CLI   SVF0PROF+4,C'Y'                                                  
         BNE   VCOP30                                                           
         CLI   EPROF+2,C' '                                                     
         BNH   SPERREX                                                          
*                                                                               
**********************************************************************          
*                                                                               
VCOP30   XC    KEY,KEY               READ CLIENT RECORD INTO AIO2               
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         USING CLTHDR,R3                                                        
         L     R3,AIO2                                                          
         LA    R3,CLIST                                                         
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,ESTPRD1H                                                      
VCOP40   CLI   5(R2),0                                                          
         BE    VCOP50                                                           
         BAS   RE,SPMOVE                                                        
*                                                                               
         CLI   SVCXTRA+8,C'P'      TEST P&G                                     
         BNE   VCOP42                                                           
         CLC   =C'ALL',8(R2)                                                    
         BE    VCOPALL                                                          
*                                                                               
VCOP42   BAS   RE,GETPRD                                                        
*                                                                               
         MVC   ERRNUM,=AL2(NOPOLCPY)                                            
         CLC   =C'POL',WORK          CANNOT MAKE ANOTHER POL                    
         BE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(NOTRDCPY)                                            
         CLI   WORK+2,C'#'                                                      
         BE    SPERREX                                                          
*                                                                               
         BAS   RE,CHECKDUP         CHECK FOR DUPLICATE PRODUCTS                 
*                                                                               
         MVC   KEY,ESTKEY            CANNOT ADD A BRAND THAT ALREADY            
         MVC   KEY+4(3),0(R3)        EXISTS OR A BRAND THAT HAS BEEN            
         OI    DMINBTS,X'08'         DELETED                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCOP50                                                           
         MVC   ERRNUM,=AL2(DUPERR)                                              
         TM    KEY+13,X'80'                                                     
         BZ    SPERREX                                                          
         MVC   ERRNUM,=AL2(DELERR)                                              
         B     SPERREX                                                          
*                                                                               
VCOP50   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BNE   VCOP40                                                           
*                                                                               
**********************************************************************          
*                                                                               
         LA    R2,ESTPRD1H                                                      
VCOP60   CLI   5(R2),0               ADD NEW ESTIMATE                           
         BE    VCOP70                                                           
         BAS   RE,SPMOVE                                                        
         BAS   RE,GETPRD                                                        
         MVC   EKEY+4(3),0(R3)                                                  
         MVC   EPRDCD+1(1),3(R3)                                                
         BAS   RE,CLRACCS                                                       
*                                                                               
         MVI   ECPPTYPE,0                                                       
         MVC   KEY(13),EKEY                                                     
         GOTO1 HIGH                MAKE SURE NO DUP ADDREC ON RECOVERY          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCOP65                                                           
         MVC   ERRNUM,=AL2(DUPERR)                                              
         B     SPERREX                                                          
*                                                                               
VCOP65   BAS   RE,ADREC                                                         
         BAS   RE,CANTV                                                         
         BAS   RE,XADD             ADD PASSIVE KEYS                             
*                                                                               
VCOP70   BAS   RE,ADDREQ                                                        
*                                                                               
VCOP80   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST EOL                                     
         BNE   VCOP60                                                           
         B     DCOP                GO DISPLAY PRD LISTS                         
         EJECT                                                                  
*================================================================               
* CODE TO COPY POL EST TO ALL PRODUCTS FOR P&G ONLY                             
*================================================================               
                                                                                
VCOPALL  DS    0H                                                               
         BAS   RE,CLRACCS                                                       
         MVI   ECPPTYPE,0                                                       
*                                                                               
VCOPALL2 MVC   EKEY+4(3),0(R3)     R3 POINTS TO CLIST ON ENTRY                  
         MVC   EPRDCD+1(1),3(R3)                                                
*                                                                               
         MVC   KEY(13),EKEY                                                     
         GOTO1 HIGH                SKIP PRD IF ALREADY OPEN                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCOPALL6                                                         
*                                                                               
         BAS   RE,ADREC                                                         
         BAS   RE,CANTV                                                         
         BAS   RE,XADD             ADD PASSIVE KEYS                             
         BAS   RE,ADDREQ                                                        
*                                                                               
VCOPALL6 LA    R3,4(R3)            NEXT PRODUCT                                 
         CLI   3(R3),X'FF'         TEST POL                                     
         BE    VCOPALL6                                                         
         CLI   3(R3),0             TEST EOL                                     
         BNE   VCOPALL2                                                         
*                                                                               
VCOPALLX B     DCOP                                                             
         EJECT                                                                  
*=====================================================                          
* CLEAR PACKED ACCUMS                                                           
*=====================================================                          
                                                                                
CLRACCS  NTR1                                                                   
         ZAP   ECURPDN,=PL6'0'                                                  
         LA    R1,26                                                            
         LA    RE,EORD                                                          
         ZAP   0(6,RE),=PL6'0'                                                  
         LA    RE,6(RE)                                                         
         BCT   R1,*-10                                                          
         LA    R1,13                                                            
         LA    RE,EAUTH                                                         
         ZAP   0(6,RE),=PL6'0'                                                  
         LA    RE,6(RE)                                                         
         BCT   R1,*-10                                                          
         LA    R1,26                                                            
         LA    RE,EPAID                                                         
         ZAP   0(6,RE),=PL6'0'                                                  
         LA    RE,6(RE)                                                         
         BCT   R1,*-10                                                          
         J     XIT                                                              
         EJECT                                                                  
*==================================================                             
* ADD L2 REQUEST RECORD                                                         
*==================================================                             
                                                                                
ADDREQ   NTR1                                                                   
         MVC   AIO,AIO3              ADD L2 REQUEST RECORD                      
         L     R1,AIO                                                           
         XC    0(150,R1),0(R1)                                                  
         MVI   10(R1),132                                                       
         MVI   14(R1),106                                                       
         LA    R1,26(R1)                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'L2'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),QMED                                                     
         MVC   5(3,R1),QCLT                                                     
         MVC   11(3,R1),EKEY+4                                                  
         ZIC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVI   61(R1),C'N'                                                      
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   65(R1),C'A'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVI   WORK,0                                                           
         MVC   AIO,AIO1                                                         
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        CHECKDUP    -    CHECK FOR DUPLICATE PRODUCTS               *          
**********************************************************************          
*        WITHIN COMPARISON:     R2 POINTS TO ACTIVE PRODUCT                     
*                               R3 POINTS TO POSSIBLE DUP PRODUCT               
CHECKDUP DS    0H                                                               
         OC    8(3,R2),SPACES      PAD SPACES IN CASE OF 2 CHAR PRD             
*                                                                               
         ZIC   R6,0(R2)            BUMP R2 TO BE ONE PROD AHEAD                 
         LA    R3,0(R6,R2)                                                      
*                                                                               
CDUP10   CLI   5(R3),0             ANYMORE PRODUCTS?                            
         BE    CDUPX                NAH, HEAD OUT                               
         CLC   0(8,R3),ESTPRD0H    HAS R3 SURPASSED LAST PRODUCT?               
         BH    CDUPX                                                            
*                                                                               
         OC    8(3,R3),SPACES      PAD SPACES IN CASE OF 2 CHAR PRD             
*                                                                               
         CLC   8(3,R2),8(R3)                                                    
         BE    CDUPERR                                                          
         ZIC   R6,0(R3)            BUMP R3                                      
         AR    R3,R6                                                            
         B     CDUP10                                                           
*                                                                               
CDUPX    BR    RE                                                               
*                                                                               
CDUPERR  MVC   ERRNUM,=AL2(DUPPRD)   DUPLICATE PRODUCTS, ERROR                  
         LR    R2,R3               POINT R2 TO THE DUP PRD                      
         B     SPERREX                                                          
**********************************************************************          
*        GETPRD                                                      *          
**********************************************************************          
*                                                                               
GETPRD   L     R3,AIO2                                                          
         LA    R3,CLIST                                                         
         DROP  R3                                                               
GETP1    CLC   0(3,R3),WORK                                                     
         BE    GETP5                                                            
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   GETP1                                                            
GETPERR  MVC   ERRNUM,=AL2(NOPRDERR)                                            
         B     SPERREX                                                          
*                                                                               
GETP5    ST    RE,FULL             SAVE RE FOR RETURN                           
         XC    KEY,KEY             TRY TO READ PRODUCT                          
         MVC   KEY(4),ESTKEY                                                    
         MVC   KEY+4(3),0(R3)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPERR                 PRD NOT FOUND                            
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*        SPMOVE                                                      *          
**********************************************************************          
SPMOVE   MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZR   RE                                                               
         BCTR  R1,0                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
***********************************************************************         
*        ADD CANADIAN ESTIMATE                                        *         
***********************************************************************         
*        IF CANADIAN AGENCY ADDS AN ESTIMATE FOR TV - ADD ESTIMATE    *         
*        RECORD FOR MEDIA N(03) & MEDIA C(08)                         *         
***********************************************************************         
                                                                                
CANTV    NTR1                                                                   
         L     R4,AIO1                                                          
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTX                                                              
         CLI   QMED,C'T'             TV?                                        
         BNE   CTX                                                              
*                                                                               
         LA    R2,MEDTAB             MEDIA TABLE                                
         LA    R2,1(R2)              BUMP PAST MEDIA T - JUST ADDED IT          
*                                                                               
CT00     CLI   0(R2),X'FF'           END OF TABLE?                              
         BE    CT10                  YES, DONE                                  
         CLI   SVCXTRA+8,C'P'        P&G CLIENT?                                
         BNE   *+12                  NO                                         
         CLI   0(R2),X'03'           MEDIA N ENTRY?                             
         BE    CT05                  YES - SKIP MEDIA N                         
*                                                                               
         XC    KEY,KEY               MEDIA N(03) AND C(08)                      
         MVC   KEY,ESTKEY                                                       
         MVC   KEY+4(3),0(R3)                                                   
         NI    KEY+1,X'F0'                                                      
         OC    KEY+1(1),0(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEYSAVE      RECORD DOESN'T EXIST                       
         MVC   KEY(13),KEYSAVE       MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         BAS   RE,ADREC                                                         
CT05     LA    R2,1(R2)              BUMP MEDIA TABLE                           
         B     CT00                  AND PROCESS NEXT MEDIA                     
*                                                                               
CT10     MVC   KEY,ESTKEY            RESTORE KEY                                
         NI    1(R5),X'F0'                                                      
         OI    1(R5),X'01'                                                      
CTX      XIT1                                                                   
*                                                                               
***********************************************************************         
* ADD THE PASSIVE POINTER                                             *         
***********************************************************************         
                                                                                
XADD     NTR1                                                                   
*                                                                               
         LA    R2,MEDTAB                                                        
*                                                                               
XADD00   L     R4,AIO               LAST RECORD WE JUST ADDED                   
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R4)                                                    
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   XADD01               NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XADD01               NO, ONLY CHANGE 1 RECORD                    
         CLI   SVCXTRA+8,C'P'       P&G CLIENT?                                 
         BNE   *+12                 NO                                          
         CLI   0(R2),X'03'          MEDIA N ENTRY?                              
         BE    XADD20               YES - SKIP!                                 
         NI    KEY+1,X'F0'          TURN OFF MEDIA BIT                          
         OC    KEY+1(1),0(R2)       USE THIS MEDIA                              
*                                                                               
XADD01   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      WE BETTER HAVE IT!!!                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(4),KEY+14       SAVE THE D/A                                
         USING ESTHDR,R4                                                        
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING EPKEY,R3                                                         
         MVI   EPKEYTYP,EPKEYTYQ    X'0D'                                       
         MVI   EPKEYSUB,EPKEYSBQ    X'F2'                                       
         MVC   EPKEYAM,KEYSAVE+1    A/M                                         
         MVC   EPKEYCLT,EKEYCLT     CLIENT                                      
         GOTO1 DATCON,DMCB,(0,ESTART),(2,EPKEYSDT)                              
         GOTO1 DATCON,DMCB,(0,EEND),(2,EPKEYEDT)                                
         MVC   EPKEYEST,EKEYEST     EST                                         
         MVC   EPKEYPRD,EKEYPRD     PRD                                         
         DROP  R3,R4                                                            
*                                                                               
         MVC   KEY+14(4),WORK       SET DISK ADDRESS                            
         OI    DMINBTS,X'08'        PASS DELETES                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND?                                      
         BNE   XADD10               NO                                          
         NI    KEY+13,X'7F'         YES, UNDELETE                               
         MVC   AIO,AIO2             DO NOT CLOBBER AIO1                         
         GOTO1 WRITE                AND WRITE BACK                              
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
         B     XADD20                                                           
*                                                                               
XADD10   MVC   KEY,KEYSAVE          RESTORE KEY                                 
         MVC   AIO,AIO2             DO NOT CLOBBER AIO1                         
         GOTO1 ADD                                                              
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XADD20   CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   XADDX                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XADDX                NO, WE ARE DONE HERE                        
         LA    R2,1(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         BNE   XADD00               NO                                          
*                                                                               
XADDX    J     XIT                                                              
**********************************************************************          
*        SPACES TO ZEROS                                                        
**********************************************************************          
SPTOZER  NTR1                                                                   
SPTOZ10  CLI   ESTESTK,C' '          CHANGE LEADING SPACES TO ZEROS             
         BH    SPTOZX                                                           
         MVI   ESTESTK,X'F0'                                                    
         CLI   ESTESTK+1,C' '                                                   
         BH    SPTOZ20                                                          
         MVI   ESTESTK+1,X'F0'                                                  
*                                                                               
SPTOZ20  MVC   ERRNUM,=AL2(ESTERR1)  EST CODE MUST BE NUMERIC                   
         LA    R3,3                                                             
         LA    R4,ESTESTK                                                       
SPTOZ25  CLI   0(R4),X'F9'                                                      
         BH    SPERREX                                                          
         CLI   0(R4),X'F0'                                                      
         BL    SPERREX                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,SPTOZ25                                                       
         OI    ESTESTKH+4,X'08'      SET FOR VALID NUMERIC                      
SPTOZX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         MVI   USEIO,C'N'                                                       
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         OI    ESTREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         CLI   CALLSP,0                                                         
         BE    *+8                                                              
         NI    ESTREH+1,X'FF'-X'04'  LIGHT UP PF12 FIELD                        
         OI    ESTREH+6,X'80'                                                   
*                                                                               
SETUP10  GOTO1 INITPFKY,DMCB,PFTABLE PF TABLE                                   
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       ADD RECORD                                    *         
***********************************************************************         
*                                                                               
ADREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    ADREC10                                                          
         CLI   DMCB+8,X'20'        RECORD ALREADY EXIST?                        
         BE    *+6                                                              
         DC    H'0'                DIE IF ANOTHER ERROR                         
         MVC   ERRNUM,=AL2(DUPERR)                                              
         B     SPERREX                                                          
ADREC10  MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        ERROR MESSAGES                                              *          
**********************************************************************          
                                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SPERREX2 OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMINF                                                    
         MVI   GTMSYS,2                                                         
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
NOPOLCPY EQU   768                 MAY NOT ADD POL EST WITH ACTN COPY           
NOTRDCPY EQU   1128                NO TRADE COPY                                
DUPERR   EQU   769                 RECORD ALREADY ON FILE                       
DELERR   EQU   770                 RECORD IS DELETED                            
NOPRDERR EQU   771                 PRODUCT NOT FOUND                            
MSTREST  EQU   767                 POL MSTR OR SUB EST IS OPEN-NO ADD           
ESTERR1  EQU   563                 ESTIMATE CODE MUST BE NUMERIC                
ENTPRD   EQU   8                   ENTER PRODUCT CODES                          
NOMATCH  EQU   773                 FILTERS DON'T MATCH F0 PROFILES              
DUPPRD   EQU   983                 DUP PRODUCT ON ADD PRODUCT LINE              
**********************************************************************          
*        MEDIA TABLE                                                 *          
**********************************************************************          
MEDTAB   DC   XL1'01'                                                           
         DC   XL1'03'                                                           
         DC   XL1'08'                                                           
         DC   X'FF'                                                             
         EJECT                                                                  
**********************************************************************          
*        PFKEY TABLES                                                *          
**********************************************************************          
*                                                                               
PFTABLE  DS   0H                                                                
*        CLIENT MAINT DISPLAY                                                   
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'CM '                 MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'CM2'                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
MPF05X   EQU  *                                                                 
*                                                                               
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                 
         DC   CL3'PM '                 DISPLAY                                  
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF02    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
MPF02X   EQU  *                                                                 
*                                                                               
*        ESTIMATE MAINT DISPLAY                                                 
         DC   AL1(MPF07X-*,07,PFTCPROG,(MPF07X-MPF07)/KEYLNQ,0)                 
         DC   CL3'EM '                 LIST                                     
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF07    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTESTK-1),AL2(ESTESTK-T217FFD)                    
MPF07X   EQU  *                                                                 
*                                                                               
*        ESTIMATE DOLLAR                                                        
         DC   AL1(MPF09X-*,09,PFTCPROG,(MPF09X-MPF09)/KEYLNQ,0)                 
         DC   CL3'ED '                 LIST                                     
         DC   CL8'ESTD'                RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF09    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTESTK-1),AL2(ESTESTK-T217FFD)                    
MPF09X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        DSECTS                                                      *          
**********************************************************************          
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENESTD                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE SCSFM74D                                                       
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD           ERROR MESSAGES                             
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
         ORG   SYSSPARE                                                         
ESTKEY   DS    CL13                                                             
ERRNUM   DS    XL2                                                              
SVF0PROF DS    CL16                                                             
CLIPRO   DS    CL10                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPSFM54   04/30/13'                                      
         END                                                                    
