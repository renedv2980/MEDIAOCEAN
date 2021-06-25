*          DATA SET NESFM07    AT LEVEL 032 AS OF 10/16/09                      
*PHASE T31C07A                                                                  
***********************************************************************         
         TITLE 'NESFM07 - ESTIMATE COPY'                                        
         PRINT NOGEN                                                            
T31C07   CSECT                                                                  
         NMOD1 0,**1C07**                                                       
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
MAXPRD   EQU   252                 MAX NUMBER OF PRODUCTS                       
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
         USING ESTHDR,R4                                                        
VK       LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,X'00'                                                   
         MVI   RESETSW,C'N'          LIST FROM THE TOP                          
         CLI   ENDSW,C'Y'                                                       
         BNE   *+8                                                              
         MVI   RESETSW,C'Y'          LIST FROM THE TOP                          
         MVI   ENDSW,C'N'            RESET END SWITCH                           
*                                                                               
         MVC   ESTMEDN,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    ESTMEDNH+6,X'80'      AND PRODUCT NAME                           
         MVC   ESTPRDN,SPACES                                                   
         OI    ESTPRDNH+6,X'80'      AND PRODUCT NAME                           
*                                                                               
***********************************************************************         
*                                                                               
         CLC   LASTMED,ESTMEDK       CHECK MEDIA CHANGE                         
         BE    *+8                                                              
         MVI   RESETSW,C'Y'          LIST FROM THE TOP                          
         MVC   LASTMED,ESTMEDK       UPDATE LAST MEDIA FIELD                    
         LA    R2,ESTMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   ESTMEDN,MEDNM         MEDIA NAME                                 
         OI    ESTMEDNH+6,X'80'                                                 
         MVC   EKEYAM,BAGYMD         COPY MEDIA INTO KEY                        
*                                                                               
***********************************************************************         
*                                                                               
         CLC   LASTCLI,ESTCLIK       CHECK TO SEE IF CLIENT CHANGED             
         BE    VK05                                                             
         MVI   RESETSW,C'Y'          LIST FROM THE TOP                          
         LA    R2,ESTCLIKH           CLIENT                                     
         PRINT GEN                                                              
         GOTO1 VALICLT,DMCB,500      VALIDATE CLIENT CODE AND TRANSMIT          
         PRINT NOGEN                                                            
         MVC   ESTCLIN,SPACES                                                   
         MVC   ESTCLIN,CLTNM         CLIENT NAME                                
         OI    ESTCLINH+6,X'80'                                                 
VK05     MVC   EKEYCLT,BCLT          COPY CLIENT INTO KEY                       
         MVC   LASTCLI,ESTCLIK       SAVE CLIENT                                
*                                                                               
***********************************************************************         
*                                                                               
         CLC   LASTPRD,ESTPRDK       CHECK TO SEE IF PRODUCT CHANGED            
         BE    *+8                                                              
         MVI   RESETSW,C'Y'          LIST FROM THE TOP                          
         MVC   LASTPRD,ESTPRDK       UPDATE LAST PRODUCT FIELD                  
         LA    R2,ESTPRDKH           PRODUCT                                    
         GOTO1 VALIPRD               VALIDATE PRODUCT CODE AND TRANSMIT         
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
         CLC   LASTEST,ESTESTK       CHECK TO SEE IF ESTIMATE CHANGED           
         BE    *+8                                                              
         MVI   RESETSW,C'Y'          LIST FROM THE TOP                          
         MVC   LASTEST,ESTESTK       UPDATE LAST ESTIMATE FIELD                 
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
         MVC   ESTESTN,ESTNAME       TRANSMIT ESTIMATE DESCRIPTION              
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
         BZ    DCOP30                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
DCOP30   LA    R2,9(RE,R2)           TEST ON SPOT/SFM IF NEED UPDATE!           
         LA    R0,ESTLSTH            TEST ON SPOT/SFM/TST                       
         CR    R2,R0                 TEST ON TST!!                              
         BNH   DCOP20                                                           
*                                                                               
**********************************************************************          
*                                                                               
DCOP40   CLI   RESETSW,C'N'         DONT RESET TABLE                            
         BE    DCOP59                                                           
         LA    R3,SVCLIST                                                       
         LA    R5,500                                                           
*                                                                               
DCOP50   CLI   0(R3),0                                                          
         BE    DCOP59                                                           
         XC    KEY,KEY                                                          
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
DCOP55   LA    R3,4(R3)              BUMP TO NEXT PRODUCT IN SVCLIST            
         BCT   R5,DCOP50                                                        
*                                                                               
**********************************************************************          
*                                                                               
DCOP59   LA    R2,ESTDSPH                                                       
         MVC   8(21,R2),=C'** Brands Not Open **'                               
         OI    ESTDSPH+6,X'80'                                                  
*                                                                               
         ZAP   HALF,=P'0'            DISPLAY LIST OF PRODUCTS THAT ARE          
         LA    R3,SVCLIST                                                       
******   LHI   R5,MAXPRD                                                        
         LA    R5,260                                                           
DCOP60   CLI   0(R3),0               END OF SVCLIST?                            
         BE    DCOP85                                                           
         CLI   3(R3),0                                                          
         BNE   DCOP80                IF PRD FROM SVCLIST SET TO 0 ...           
DCOP70   BAS   RE,FMTPRD             FORMAT IT                                  
         OI    3(R3),X'F0'           SET UP AS STATUS DISPLAYED                 
         BCT   R5,DCOP60                                                        
         B     DCOP99                                                           
*                                                                               
DCOP80   LA    R3,4(R3)              BUMP TO NEXT PRODUCT IN SVCLIST            
         B     DCOP60                GET NEXT WITHOUT BCT                       
*                                                                               
**********************************************************************          
*                                                                               
DCOP85   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(25,R2),=C'** Brands Already Open **'                           
         OI    6(R2),X'80'                                                      
*                                                                               
         ZAP   HALF,=P'0'            DISPLAY LIST OF PRODUCTS THAT ARE          
         LA    R3,SVCLIST                                                       
******   LHI   R5,MAXPRD                                                        
DCOP90   CLI   0(R3),0                                                          
         BE    DCOP98                                                           
         CLI   3(R3),1                                                          
         BNE   DCOP95                IF PRD FROM SVCLIST SET TO 1 ...           
         BAS   RE,FMTPRD             FORMAT IT                                  
         OI    3(R3),X'F0'           SET UP AS STATUS DISPLAYED                 
         LA    R3,4(R3)              BUMP TO NEXT PRODUCT IN SVCLIST            
         BCT   R5,DCOP90                                                        
         B     DCOP99                                                           
*                                                                               
DCOP95   LA    R3,4(R3)              BUMP TO NEXT PRODUCT IN SVCLIST            
         B     DCOP90                WITHOUT BCT                                
*                                                                               
DCOP98   MVI   ENDSW,C'Y'            END OF PRODUCT LIST                        
*                                                                               
**********************************************************************          
*                                                                               
DCOP99   LA    R2,ESTPRD1H                                                      
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
VCOP30   LA    R2,ESTPRD1H         START WITH FIRST PRODUCT                     
VCOP40   CLI   5(R2),0                                                          
         BE    VCOP50                                                           
         CLC   8(3,R2),=C'AAA'                                                  
         BE    ERRINV                                                           
         BAS   RE,SPMOVE           MOVE THE PRODUCT INTO WORK                   
         BAS   RE,GETPRD                                                        
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
         MVI   3(R3),1                                                          
         MVC   EKEY+4(3),0(R3)                                                  
         MVC   EPRDCD+1(1),3(R3)                                                
**NOP         XC    ECURPDN,ECURPDN                                             
**NOP         XC    EAUTHN,EAUTHN                                               
**NOP         XC    EORDN,EORDN                                                 
**NOP         XC    EPAIDN,EPAIDN                                               
*                                  CLEAR NEW PACKED ACCUMS                      
         ZAP   ECURPDN,=PL6'0'                                                  
         LA    R1,26                                                            
         LA    RE,EORD                                                          
         ZAP   0(6,RE),=PL6'0'                                                  
         AHI   RE,6                                                             
         BCT   R1,*-10                                                          
         LA    R1,13                                                            
         LA    RE,EAUTH                                                         
         ZAP   0(6,RE),=PL6'0'                                                  
         AHI   RE,6                                                             
         BCT   R1,*-10                                                          
         LA    R1,26                                                            
         LA    RE,EPAID                                                         
         ZAP   0(6,RE),=PL6'0'                                                  
         AHI   RE,6                                                             
         BCT   R1,*-10                                                          
*                                                                               
         XC    ETYPE,ETYPE                                                      
         MVC   KEY(13),EKEY                                                     
*                                                                               
         XC    ECPPTYPE,ECPPTYPE                                                
         MVC   KEY(13),EKEY                                                     
         GOTO1 HIGH                MAKE SURE NO DUP ADDREC ON RECOVERY          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCOP65                                                           
         MVC   ERRNUM,=AL2(DUPERR)                                              
         B     SPERREX                                                          
*                                                                               
VCOP65   DS    0H                                                               
         BAS   RE,ADREC                                                         
*                                                                               
**********************************************************************          
*                                                                               
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
*                                                                               
         CLC   =X'F0F0F0',5(R1)        BUG CATCHER                              
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVI   WORK,0                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
VCOP70   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST EOL                                     
         BNE   VCOP60                                                           
         B     DCOP                GO DISPLAY PRD LISTS                         
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
*   RETURN R3 = POINTS TO PRODUCT IN SVCLIST                                    
**********************************************************************          
*                                                                               
GETPRD   LA    R3,SVCLIST                                                       
         LHI   R5,MAXPRD                                                        
GETP1    CLI   0(R3),0             END OF RECORD?                               
         BE    GETP5               CHECK OVERFLOW PRODUCTS                      
         CLC   0(3,R3),WORK        MATCH ON PRODUCT?                            
         BE    GETP10                                                           
         LA    R3,4(R3)                                                         
         BCT   R5,GETP1                                                         
*  SET FOR OVERFLOW PRODUCT                                                     
GETP5    LA    R3,WORK                                                          
         MVI   WORK+3,0                                                         
*                                                                               
GETP10   ST    RE,FULL             SAVE RE FOR RETURN                           
         XC    KEY,KEY             TRY TO READ PRODUCT                          
         MVC   KEY(4),ESTKEY                                                    
         MVC   KEY+4(3),0(R3)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPERR                 PRD NOT FOUND                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
GETPERR  MVC   ERRNUM,=AL2(NOPRDERR)                                            
         B     SPERREX                                                          
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
         DC    H'0'                DIE ON ANOTHER ERROR                         
         MVC   ERRNUM,=AL2(DUPERR)                                              
         B     SPERREX                                                          
ADREC10  MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
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
NONONPOL EQU   772                 CAN ONLY COPY POL EST                        
VKERR4   EQU   536                 CLIENT MUST BE 'CC'                          
ESTERR1  EQU   563                 ESTIMATE CODE MUST BE NUMERIC                
ENTPRD   EQU   8                   ENTER PRODUCT CODES                          
ACTCOMP  EQU   9                   ACTION COMPLETED                             
NOMATCH  EQU   773                 FILTERS DON'T MATCH F0 PROFILES              
DUPPRD   EQU   983                 DUP PRODUCT ON ADD PRODUCT LINE              
*                                                                               
**********************************************************************          
*        DSECTS                                                      *          
**********************************************************************          
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE NESFMB0D                                                       
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
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
**********************************************************************          
*        SAVED STORAGE                                               *          
**********************************************************************          
         ORG   SYSSPARE                                                         
ESTKEY   DS    CL13                                                             
ERRNUM   DS    XL2                                                              
SVF0PROF DS    CL16                                                             
CLIPRO   DS    CL10                                                             
*                                                                               
LASTMED  DS    CL1                                                              
LASTCLI  DS    CL3                                                              
LASTPRD  DS    CL3                                                              
LASTEST  DS    CL3                                                              
*                                                                               
ENDSW    DS    CL1                 C'Y' LAST SCREEN DISPLAYED RESET             
RESETSW  DS    CL1                 C'O', C'N' HEADLN TO STRT SCREEN             
*                                                                               
DISPRD   DS    CL3                 PRODUCT TO START DISPLAY AT                  
FRZPRD   DS    CL3                 FIRST PRODUCT ON SCREEN                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032NESFM07   10/16/09'                                      
         END                                                                    
