*          DATA SET NESFM1C    AT LEVEL 096 AS OF 08/06/19                      
*PHASE T31C1CA                                                                  
         TITLE 'T31C1C  -SPB- SPLIT BILLING RECORDS'                            
T31C1C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C1C                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   AIO,AIO1            BECAUSE DISPKEY LOGIC SCREWS IT UP           
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
         CLI   MODE,RECDEL                                                      
         BNE   EXIT                                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    EXIT                                                             
         BAS   RE,CKBIL                                                         
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* CHECK FOR BILL RECORDS                                                        
*                                                                               
CKBIL    NTR1                                                                   
         BAS   RE,GETPROD          GET ALL PRODUCTS                             
*                                                                               
         MVC   WORK(L'KEY),KEY   FIRST SAVE KEY                                 
         LA    R4,PRODLST                                                       
*                                                                               
CKBIL10  DS    0H                                                               
         CLI   0(R4),X'FF'         END OF PRODUCTS                              
         BE    CKBIL90                                                          
*                                                                               
         CLC   0(3,R4),=C'POL'                                                  
         BE    CKBIL85                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING BILLREC,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   BKEYAM,WORK+2                                                    
         MVC   BKEYCLT,WORK+3      CLIENT FROM SPB REC                          
         MVC   BKEYPRD,0(R4)       PRODUCT FROM PRODLST                         
         OC    BKEYPRD(3),SPACES                                                
         MVC   BKEYEST,WORK+8      SET EST IN KEY                               
*                                                                               
CKBIL40  GOTO1 HIGH                                                             
         B     CKBIL60                                                          
*                                                                               
CKBIL50  GOTO1 SEQ                                                              
*                                                                               
CKBIL60  CLC   KEYSAVE(8),KEY     FIRST CHK TRROUGH ESTIMATE                    
         BNE   CKBIL85                                                          
*                                                                               
CKBIL70  DS    0H                                                               
         OC    KEY+8(4),KEY+8       SEE IF BILLREC                              
         BZ    CKBIL50              NO - KEEP LOOKING                           
         CLI   MODE,RECADD                                                      
         BE    CKBIL50                                                          
*                                                                               
* CANNOT DELETE                                                                 
*                                                                               
CKBIL80  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=C'CANNOT CHANGE // BILL RECORDS PRESENT'            
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   *+10                                                             
         MVC   CONHEAD(37),=C'CANNOT DELETE // BILL RECORDS PRESENT'            
*                                                                               
         GOTO1 ERREX2                                                           
*                                                                               
CKBIL85  DS    0H                                                               
         LA    R4,4(R4)            CHECK NEXT PRODUCT                           
         B     CKBIL10                                                          
*                                                                               
CKBIL90  MVC   KEY,WORK      RESTORE KEY                                        
*                                                                               
         CLI   MODE,RECADD                                                      
         BE    CKBILX                                                           
         GOTO1 HIGH                                                             
*                                                                               
CKBILX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* GET A LIST OF ALL PRODUCTS                                                    
*                                                                               
GETPROD  NTR1                                                                   
         XC    PRODLST,PRODLST                                                  
*                                                                               
         LA    R6,PRODLST                                                       
         MVC   0(3,R6),SFMPRD                                                   
         LA    R6,4(R6)                                                         
         LA    R2,SFMPRD1H                                                      
*                                                                               
GP10     DS    0H                                                               
         CLI   5(R2),0             ANY MORE PRODUCTS?                           
         BE    GP100                                                            
         MVC   0(3,R6),8(R2)                                                    
*                                                                               
GP50     DS    0H                                                               
         LA    R6,4(R6)                                                         
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT PRODUCT FIELD                   
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         B     GP10                                                             
*                                                                               
GP100    DS    0H                                                               
         MVI   0(R6),X'FF'                                                      
*                                                                               
GPX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING SPBKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVI   SPBKTYP,SPBKTYPQ                                                 
         MVI   SPBKSUB,SPBKSUBQ                                                 
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION=LIST                             
         BNE   VK10                                                             
         LA    R2,SFMMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   SPBKAGMD,BAGYMD                                                  
         LA    R2,SFMCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VK2                                                              
         OI    LISTIND,LCLT                                                     
         GOTO1 VALICLT                                                          
         MVC   SPBKCLT,BCLT                                                     
*                                                                               
VK2      LA    R2,SFMPRDH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VK4                                                              
         OI    LISTIND,LPRD                                                     
         GOTO1 VALIPRD                                                          
         MVC   SPBKPRD,QPRD                                                     
*                                                                               
VK4      LA    R2,SFMESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BE    VK6                                                              
         OI    LISTIND,LEST                                                     
         GOTO1 VALIEST                                                          
         MVC   SPBKEST,BEST                                                     
*                                                                               
VK6      LA    R3,SPBKCLT-SPBKEY-1                                              
         CLI   LISTIND,0           CHECK FOR MISSING FIELDS                     
         BE    VK8                                                              
         LA    R2,SFMCLTH                                                       
         TM    LISTIND,LCLT                                                     
         BZ    EMIS                                                             
         LA    R3,L'SPBKCLT(R3)                                                 
         TM    LISTIND,LPRD+LEST                                                
         BZ    VK8                                                              
         LA    R2,SFMPRDH                                                       
         TM    LISTIND,LPRD                                                     
         BZ    EMIS                                                             
         LA    R3,L'SPBKPRD(R3)                                                 
         TM    LISTIND,LEST                                                     
         BZ    VK8                                                              
         LA    R3,L'SPBKEST(R3)                                                 
*                                                                               
VK8      STC   R3,LKEYCOMP         SAVE LENGTH FOR KEY COMPARE                  
         B     VK20                                                             
*                                                                               
*                                  ACTION NOT LIST                              
VK10     LA    R2,SFMMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   SPBKAGMD,BAGYMD                                                  
*                                                                               
         LA    R2,SFMCLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
         MVC   SPBKCLT,BCLT                                                     
*                                                                               
         LA    R2,SFMPRDH          PRODUCT                                      
         GOTO1 VALIPRD                                                          
         MVC   SPBKPRD,QPRD                                                     
*                                                                               
         BRAS  RE,CKCHILD                                                       
         BNE   ECHILD                                                           
*                                                                               
         LA    R2,SFMESTH          ESTIMATE                                     
         GOTO1 VALIEST                                                          
         MVC   SPBKEST,BEST                                                     
*                                                                               
VK20     MVC   KEY(13),SVKEY       SET KEY                                      
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING SPBRECD,R6                                                       
*                                                                               
         MVI   BYTE,C'A'           SO THE NEXT INSTRUCTION IS HONORED           
         MVC   AIO,AIO3             (SEE NESFM00)                               
*                                                                               
         MVC   SFMMED(1),QMED      MEDIA                                        
         LA    R2,SFMMEDH                                                       
****     GOTO1 VALIMED                                                          
*                                                                               
         GOTO1 CLUNPK,DMCB,SPBKCLT,SFMCLT   CLIENT                              
         OI    SFMCLTH+6,X'80'                                                  
         MVI   SFMCLTH+5,3         FUDGE INPUT LENGTH                           
         LA    R2,SFMCLTH                                                       
****     GOTO1 VALICLT                                                          
*                                                                               
         MVC   QPRD,SPBKPRD        PRODUCT                                      
         MVC   SFMPRD,SPBKPRD                                                   
         OI    SFMPRDH+6,X'80'                                                  
         MVI   SFMPRDH+5,3         FUDGE INPUT LENGTH                           
         LA    R2,SFMPRDH                                                       
*****    GOTO1 VALIPRD                                                          
*                                                                               
         ZIC   RE,SPBKEST          ESTIMATE                                     
         EDIT  (RE),(3,SFMEST),ALIGN=LEFT                                       
         OI    SFMESTH+6,X'80'                                                  
         STC   R0,SFMESTH+5        FUDGE INPUT LENGTH                           
         OI    SFMESTH+4,X'08'     FUDGE NUMERIC                                
         LA    R2,SFMESTH                                                       
***      GOTO1 VALIEST                                                          
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         CLI   MODE,RECADD                                                      
         BE    VR1                                                              
         CLI   ACTNUM,ACTADD                                                    
         BE    VR1                                                              
         BAS   RE,CKBIL                                                         
*                                                                               
VR1      DS    0H                                                               
         XC    PRODLST,PRODLST                                                  
         MVI   ELCODE,NETPCDQ      REMOVE ALL ELEMENTS                          
         GOTO1 REMELEM                                                          
         MVI   ELCODE,NECELCDQ                                                  
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           BUILD TYPE ELEMENT                           
         LA    R6,ELEM                                                          
         USING NETYPE,R6                                                        
         MVI   NETPCD,NETPCDQ                                                   
         MVI   NETPLN,NETPLNQ                                                   
         LA    R2,SFMTYPH                                                       
         CLI   5(R2),0                                                          
         BE    VRB                                                              
         CLI   SFMTYP,C'P'                                                      
         BNE   VRBB                                                             
VRB      OI    NETPID,X'02'        SET PERCENT                                  
         B     VRC                                                              
VRBB     CLI   SFMTYP,C'D'                                                      
         BNE   EMIS                                                             
         OI    NETPID,X'04'        SET DOLLARS                                  
VRC      CLI   SFMTYP+1,X'40'                                                   
         BNH   VRD                                                              
** WHAT IS THIS STORY ON NET BILLING ***                                        
VRD      GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           BUILD PRODUCT ELEMENTS                       
         USING NECEL,R6                                                         
         MVI   NECELCD,NECELCDQ                                                 
         MVI   NECELLN,NECELLNQ                                                 
         LA    R0,NPRODS           R0=MAX N'PRODUCTS                            
**NO-OP  CLC   AGENCY,=CL2'DU'     CHECK FOR MEDIAVEST                          
**       BNE   *+8                                                              
**08/19  LA    R0,NPRODSDU         R0=MAX N'PRODUCTS FOR MEDIAVEST              
         LA    R3,1                R3=SEQUENCE NUMBER                           
         SR    R5,R5               R5=PERCENT ACCUMULATOR                       
         LA    R2,SFMPRD1H                                                      
         LR    R4,R2                                                            
         USING SFMPRD1H,R4                                                      
         CLI   5(R2),0             TEST FIRST PRODUCT MISSING                   
         BE    EMIS                                                             
         B     VR3                                                              
*                                                                               
VR2      CLI   5(R4),0             TEST END OF PRODUCTS                         
         BE    VR12                                                             
*                                                                               
VR3      STC   R3,NECSEQ           SET SEQUENCE NUMBER                          
         LA    R2,SFMPRD1H                                                      
         CLI   5(R2),3             VALIDATE PRODUCT                             
         BH    EINV                                                             
         CLC   =C'POL',8(R2)       DO NOT ALLOW POL AS A SPLIT PRODUCT          
         BE    EINV                                                             
*                                                                               
         MVC   MYKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+4(0),8(R2)                                                   
         OC    KEY+4(3),=X'404040'                                              
         MVC   QPRD,KEY+4                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         L     RF,AIO1             SPLIT BILLING RECORD                         
         CLC   QPRD,SPBKPRD-SPBKEY(RF)  IS THIS THE PRODUCT IN THE KEY?         
         BE    EPRDKEY             YES -- THAT'S A NO-NO                        
*                                                                               
         BRAS  RE,CKPARNT                                                       
         BNE   EPARNT                                                           
*                                  MAKE SURE EST IS OPEN FOR PROD               
         MVC   KEY+7(1),BEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EESTPRD                                                          
*                                                                               
         MVC   KEY,MYKEY                                                        
         MVC   NECPRD,QPRD         SET PRODUCT                                  
         LA    R1,PRODLST          CHK DUPLICATE PROD                           
VR2A     CLI   0(R1),0                                                          
         BE    VR2B                                                             
         CLC   0(3,R1),8(R2)                                                    
         BE    EDUPPRD                                                          
         LA    R1,3(R1)                                                         
         B     VR2A                                                             
VR2B     MVC   0(3,R1),8(R2)                                                    
*                                                                               
         CLI   SFMTYP,C'D'                                                      
         BE    VR8                                                              
         LA    R2,SFMRATEH         VALIDATE PERCENT                             
         CLC   8(4,R2),=C'ZERO'                                                 
         BNE   VR4                                                              
         XC    FULL,FULL                                                        
         B     VR5                                                              
VR4      SR    R7,R7                                                            
         ICM   R7,1,5(R2)                                                       
         BZ    EMIS                                                             
         GOTO1 CASHVAL,DMCB,(4,SFMRATE),(R7)     TO 4 DECIMALS                  
         CLI   0(R1),0                                                          
         BNE   EINV                                                             
         L     RE,4(R1)                                                         
         LTR   RE,RE                                                            
         BNP   EINV                                                             
         AR    R5,RE                                                            
         C     R5,=F'1000000'      TEST TOTAL PCT NOT OVER 100                  
         BH    EPCT                                                             
         ST    RE,FULL                                                          
VR5      MVC   NECPCT,FULL                                                      
         B     VR10                                                             
*                                                                               
*                                                                               
VR8      LA    R2,SFMRATEH         VALIDATE DOLLARS                             
         CLC   8(4,R2),=C'ZERO'                                                 
         BNE   VR9                                                              
         XC    FULL,FULL                                                        
         B     VR9C                                                             
VR9      SR    R7,R7                                                            
         ICM   R7,1,5(R2)                                                       
         BZ    EMIS                                                             
         GOTO1 CASHVAL,DMCB,(0,SFMRATE),(R7)  NO DECIMALS                       
         CLI   0(R1),0                                                          
         BNE   EINV                                                             
         L     RF,4(R1)                                                         
         LTR   RF,RF                                                            
         BNP   EINV                                                             
         SR    RE,RE               CASHVAL RETURNS IN PENNIES                   
         D     RE,=F'100'                                                       
         ST    RF,FULL                                                          
VR9C     MVC   NECPCT,FULL                                                      
VR10     GOTO1 ADDELEM             ADD THE ELEMENT                              
*                                                                               
VR12     LA    R3,1(R3)                  SEQUENCE NUMBER                        
         LA    R4,SFMPRD2H-SFMPRD1H(R4)  NEXT PRODUCT                           
         BCT   R0,VR2                                                           
*                                                                               
         CLI   5(R4),0             MORE THAN MAX # OF PRODUCTS                  
         BNE   MAX#PRD                                                          
*                                                                               
         CLI   SFMTYP,C'D'         IF ITS PERCENT                               
         BE    VRX                                                              
         C     R5,=F'1000000'      TEST TOTAL PERCENT = 100                     
         BNE   EPCT                                                             
*                                                                               
VRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,SFMPRD1H         CLEAR SCREEN                                 
         BAS   RE,CLRSCRN                                                       
         CLI   SFMTYP,C'X'         IS IT CONVERT DOLLARS TO PERCENT             
         BNE   DR1B                                                             
         TM    SFMTYPH+4,X'80'     YES/ INPUT THIS TIME                         
         BNO   DR1B                     YES/GET TOTAL DOLLARS                   
         ZAP   PAKAMT,=P'0'                                                     
         ZAP   PAKWRK,=P'0'                                                     
         ZAP   PCTTOT,=P'0'                                                     
         ZAP   HUNDS,=P'0'                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'04'                                                     
         USING NECEL,R6                                                         
         BAS   RE,GETEL                                                         
DR1A     MVC   FULL,NECPCT                                                      
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    PAKAMT,DUB          PAKAMT GETS TOTAL DOLLARS                    
         BAS   RE,NEXTEL                                                        
         BE    DR1A                                                             
         MVC   PCT100,=P'1000000' NEED PERCENT TO 4 DECIMALS                    
         B     DR1C                                                             
*                                                                               
DR1B     XC    SFMTYP,SFMTYP                                                    
         L     R6,AIO                                                           
         USING NETYPE,R6                                                        
         MVI   ELCODE,X'03'        FIND TYPE ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SFMTYP,C'P'         DEFAULT IS PERCENT                           
         TM    NETPID,X'04'        IS IT DOLLORS                                
         BNO   *+8                                                              
         MVI   SFMTYP,C'D'                                                      
         OI    SFMTYPH+6,X'80'                                                  
*                                                                               
DR1C     LA    R2,SFMPRD1H                                                      
         USING SFMPRD1H,R2                                                      
         MVI   ELCODE,X'04'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NECEL,R6                                                         
         LA    R7,NPRODS                                                        
**NO-OP  CLC   AGENCY,=CL2'DU'     CHECK FOR MEDIAVEST                          
**       BNE   *+8                                                              
**08/19  LA    R7,NPRODSDU         R0=MAX N'PRODUCTS FOR MEDIAVEST              
DR2      MVC   SFMPRD1,NECPRD      PRODUCT                                      
         OI    SFMPRD1H+6,X'80'                                                 
         OC    NECPCT,NECPCT                                                    
         BNZ   DR2A                                                             
         MVC   SFMRATE(4),=C'ZERO'                                              
         B     DR5                                                              
DR2A     CLI   SFMTYP,C'D'       IS IT DOLLARS                                  
         BNE   DR2B                                                             
         EDIT  (4,NECPCT),(7,SFMRATE),ALIGN=LEFT                                
         B     DR5                                                              
DR2B     MVC   FULL,NECPCT       NO-PREPARE TO EDIT TO 4 DECIMALS               
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   PAKWRK,DUB                                                       
         CLI   SFMTYP,C'P'       IS IT PERCENT                                  
         BE    DR4                                                              
         EJECT                                                                  
*                           CONVERT DOLLARS TO PERCENT                          
*                                                                               
         ZAP   PAKWRK,PAKWRK(8)                                                 
         MP    PAKWRK,PCT100       MULT TO GET PCT TO 4 DECIMALS                
         DP    PAKWRK,PAKAMT       DIVIDE BY TOTAL DOLLARS                      
         AP    PCTTOT,PAKWRK(8)                                                 
         AP    HUNDS,PAKWRK+8(8)   TOTAL REMAINDER                              
         CP    HUNDS,=P'999999'    CHECK IF OVER                                
         BNH   DR4                                                              
         SP    HUNDS,=P'1000000'                                                
         AP    PCTTOT,=P'1'                                                     
         AP    PAKWRK(8),=P'1'                                                  
*                                                                               
DR4      DS    0H                                                               
         BAS   RE,EDPCT            EDITS TO 4 DECIMALS                          
DR5      OI    SFMRATEH+6,X'80'                                                 
         LA    R2,SFMPRD2H-SFMPRD1H(R2)                                         
         BAS   RE,NEXTEL                                                        
         BNE   DR6                                                              
         BCT   R7,DR2                                                           
*                                                                               
DR6      CLI   SFMTYP,C'X'         IF SHOWING DOLLARS AS PCT                    
         BNE   DRX                                                              
         CLC   PCTTOT,PCT100       CHECK IF PCT=100                             
         BE    DRX                                                              
         LA    R1,SFMPRD2H-SFMPRD1H   POINT TO LAST EDITED FIELD                
         SR    R2,R1                                                            
         XC    SFMRATE,SFMRATE     CLEAR IT                                     
         SP    PCT100,PCTTOT       GET DIFF                                     
         AP    PAKWRK(8),PCT100    ADD TO LAST FIGURE                           
         BAS   RE,EDPCT                                                         
         OI    SFMRATEH+6,X'80'                                                 
*                                                                               
DRX      B     EXIT                                                             
         SPACE                                                                  
EDPCT    NTR1              EDITS PERCENT IN PAKWRK TO 4 DECIMALS                
         MVC   WORK(9),=X'402021204B20202020'                                   
         ED    WORK(9),PAKWRK+4                                                 
         MVC   SFMRATE(7),WORK+2                                                
         B     EXIT                                                             
         DROP  R6,R2                                                            
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       MVI   NLISTS,13           SET NUM OF LIST LINES                        
         ZIC   R3,LKEYCOMP         R3=LENGTH FOR KEY COMPARE                    
         LA    R6,WORK                                                          
         XC    WORK,WORK                                                        
         USING SPBRECD,R6                                                       
         MVI   SPBKTYP,SPBKTYPQ                                                 
         MVI   SPBKSUB,SPBKSUBQ                                                 
         MVC   SPBKAGMD,BAGYMD                                                  
         CLC   SPBKEY(SPBKCLT-SPBKEY),KEY   TEST FIRST TIME                     
         BE    LR2                                                              
         CLC   KEY(3),KEYSAVE                                                   
         BNE   *+14                                                             
         MVC   KEY(13),KEYSAVE                                                  
         B     LR2                                                              
*                                                                               
         MVC   KEY(13),SVKEY                                                    
*                                                                               
LR2      GOTO1 HIGH                                                             
         B     LR6                                                              
*                                                                               
LR4      GOTO1 SEQ                                                              
*                                                                               
LR6      EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   LRX                                                              
         CLI   MODE,LISTRECS                                                    
         BNE   LR20                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
*  BYPASS PGSPB RECORDS                                                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    LR4                                                              
*                                                                               
         L     R6,AIO                                                           
         MVC   LISTAR,SPACES                                                    
         LA    R4,LISTAR                                                        
         USING LINED,R4                                                         
         GOTO1 CLUNPK,DMCB,SPBKCLT,LLCLT                                        
         MVC   LLPRD,SPBKPRD                                                    
         EDIT  SPBKEST,(3,LLEST),FILL=0                                         
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NETYPE,R6                                                        
         MVI   CURTYP,C'P'                                                      
         TM    NETPID,X'02'                                                     
         BO    *+8                                                              
         MVI   CURTYP,C'D'                                                      
*                                                                               
         LA    R5,LLPCTS                                                        
         L     R6,AIO                                                           
         SR    R7,R7               MAX PROD COUNTER                             
         MVI   ELCODE,X'04'                                                     
         USING NECEL,R6                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
LR8      MVC   0(3,R5),NECPRD                                                   
         LA    R5,2(R5)                                                         
         CLI   0(R5),X'40'                                                      
         BNH   *+8                                                              
         LA    R5,1(R5)                                                         
         MVI   0(R5),C'='                                                       
         LA    R5,1(R5)                                                         
         CLI   CURTYP,C'P'                                                      
         BE    LR10                                                             
         EDIT  (B4,NECPCT),(7,0(R5)),ALIGN=LEFT     DOLLARS                     
         AR    R5,R0                                                            
         B     LR10D                                                            
*                                                                               
LR10     MVC   WORK(9),=X'402021204B20202020'         PERCENT                   
         MVC   FULL,NECPCT                                                      
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         ED    WORK(9),DUB+4                                                    
         MVC   0(7,R5),WORK+2                                                   
         LA    R5,7(R5)                                                         
LR10D    C     R7,=F'4'            4=MAX PRODS FOR DISPLAY                      
         BE    LR10X                                                            
         BAS   RE,NEXTEL                                                        
         BNE   LR10X                                                            
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)            BUMP LISTAR                                  
         LA    R7,1(R7)            BUMP COUNTER                                 
         B     LR8                                                              
LR10X    GOTO1 LISTMON                                                          
         B     LR4                                                              
*                                                                               
LR20     DS    0H                                                               
*                                                                               
LRX      B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* ROUTINE TO CLEAR THE SCREEN                                                   
* FROM FIELD AT R2                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS2      IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
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
EPARNT   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'PRD ALREADY DEFINED AS PARENT'                 
         B     MSGERR                                                           
*                                                                               
ECHILD   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'PRD ALREADY DEFINED AS A CHILD'                
         B     MSGERR                                                           
*                                                                               
EPCT     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'PERCENTS MUST ADD UP TO 100PCT'                   
         B     MSGERR                                                           
*                                                                               
EESTPRD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'ESTIMATE NOT OPEN FOR PRODUCT'                    
         B     MSGERR                                                           
*                                                                               
EPRDKEY  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'PRODUCT CANNOT BE SAME AS KEY'                    
         B     MSGERR                                                           
*                                                                               
EDUPPRD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(17),=C'DUPLICATE PRODUCT'                                
         B     MSGERR                                                           
*                                                                               
MAX#PRD  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(17),=C'TOO MANY PRODUCTS'                                
         B     MSGERR                                                           
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
**RODS   EQU   5                   MAX PRODUCTS PER SCREEN                      
NPRODS   EQU   20                  MAX PRODUCTS PER SCREEN                      
NPRODSDU EQU   10                  MAX PRODUCTS PER SCREEN FOR MEDIAVST         
*                                                                               
LISTIND  DS    XL1                                                              
LCLT     EQU   X'80'                                                            
LPRD     EQU   X'40'                                                            
LEST     EQU   X'20'                                                            
*                                                                               
*                                                                               
CKCHILD  NTR1  BASE=*,LABEL=*                                                   
         MVC   FULL,AIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(SPBKPRD-SPBKEY),SVKEY                                        
*                                                                               
         GOTO1 HIGH                                                             
         B     CKCH20                                                           
*                                                                               
CKCH10   GOTO1 SEQ                                                              
CKCH20   CLC   KEY(SPBKPRD-SPBKEY),KEYSAVE                                      
         BNE   CKCHQX                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,NECELCDQ                                                  
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CKCH50   BRAS  RE,NEXTEL                                                        
         BNE   CKCH10                                                           
*                                                                               
         CLI   0(R6),NECELCDQ                                                   
         BNE   CKCH10                                                           
         CLC   SPBKPRD-SPBKEY+SVKEY(L'SPBKPRD),NECPRD-NECEL(R6)                 
         BE    CKCHNQX                                                          
         B     CKCH50                                                           
*                                                                               
CKCHQX   MVC   AIO,FULL                                                         
         J     EQXIT                                                            
CKCHNQX  MVC   AIO,FULL                                                         
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
* PRD IS IN QPROD                                                               
CKPARNT  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(SPBKPRD-SPBKEY),SVKEY                                        
*                                                                               
         GOTO1 HIGH                                                             
         B     CKPA20                                                           
*                                                                               
CKPA10   GOTO1 SEQ                                                              
CKPA20   CLC   KEY(SPBKPRD-SPBKEY),KEYSAVE                                      
         BNE   CKPAQX                                                           
*                                                                               
         CLC   SPBKPRD-SPBKEY+KEY(L'SPBKPRD),QPRD                               
         BE    CKPANQX                                                          
         B     CKPA10                                                           
*                                                                               
CKPAQX   MVC   KEY,WORK                                                         
         J     EQXIT                                                            
CKPANQX  MVC   KEY,WORK                                                         
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMFCD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMFDD                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         PRINT ON                                                               
*                                                                               
         ORG   SYSSPARE                                                         
LKEYCOMP DS    XL1                                                              
MYKEY    DS    CL20                                                             
CURTYP   DS    CL1                                                              
PRODLST  DS    CL200                                                            
PAKAMT   DS    CL8                                                              
PAKWRK   DS    CL16                                                             
PCTTOT   DS    CL8                                                              
PCT100   DS    CL4                                                              
HUNDS    DS    CL4                                                              
*                                                                               
LINED    DSECT                     LIST LINE DSECT                              
LLCLT    DS    CL3                                                              
         DS    CL1                                                              
LLPRD    DS    CL3                                                              
         DS    CL1                                                              
LLEST    DS    CL3                                                              
         DS    CL3                                                              
LLPCTS   DS    CL60                                                             
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE SPGENSPBL                                                      
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENBILL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096NESFM1C   08/06/19'                                      
         END                                                                    
