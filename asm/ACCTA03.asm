*          DATA SET ACCTA03    AT LEVEL 010 AS OF 05/01/02                      
*PHASE T61E03A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCTA03 -- CTA FINANCIAL ADJUST                      *         
*                                                                     *         
*  COMMENTS:     ADJUSTS CTA CONTRACTS - BAL BROUGHT FORWARD/TRANSFERS*         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61E00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCTAF9 (ADJUST)                             *         
*                                                                     *         
*  OUTPUTS:      NONE                                                 *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- SYSSPARE                                       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T61E03 - CTA FINANCIAL ADJUST'                                  
T61E03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1E03**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61EFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP            ANY INITIALIZING                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         ALWAYS CALLED WITH VALREC (BIT ON)           
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALKEY - VALIDATE SYSTEM/MEDIA AND CONTRACTOR                *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         NI    STATUS,X'FF'-STKEYCH                                             
*                                                                               
         LA    R2,FADMEDH          VALIDATE MEDIA                               
         CLI   5(R2),0                                                          
         BE    ERRPLS                                                           
         CLI   5(R2),2             MUST BE TWO CHARACTERS                       
         BNE   ERRMEDIA                                                         
         CLI   8(R2),C'S'          JUST SPOT FOR NOW                            
         BNE   ERRMEDIA                                                         
         MVC   QSYS,8(R2)          1ST CHAR IS SYSTEM                           
         MVC   QMED,9(R2)          2ND CHAR IS MEDIA                            
         GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VALMED              VALIDATE SPOT MEDIA                          
         BNE   ERRMEDIA                                                         
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    STATUS,STKEYCH                                                   
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         LA    R2,FADCNTRH         VALIDATE SPOT CONTRACTOR                     
         CLI   5(R2),0                                                          
         BE    ERRPLS              REQ'D                                        
         MVC   CONTRCTR,FADCNTR    FOR VALIDATION                               
         OC    CONTRCTR,SPACES                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 VALCNTR                                                          
         BNE   ERINVCTR            INVALID CONTRACTOR                           
         MVC   AIO,AIO1                                                         
         MVC   FADCNME,CNTRNAME    DISPLAY NAME                                 
         OI    FADCNMEH+6,X'80'                                                 
         MVC   SVCTRCTR,CONTRCTR   SAVE CONTRACTOR                              
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    STATUS,STKEYCH                                                   
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALKEY - VALIDATE CONTRACT AND CATEGORY FILTERS              *         
***********************************************************************         
*                                                                               
         XC    PCON#,PCON#         PACKED CONTRACT NUMBER                       
         MVC   CON#,SPACES                                                      
         LA    R2,FADCONTH         VALIDATE SPOT CONTRACT NUMBER                
         CLI   5(R2),0                                                          
         BE    ERRPLS              REQ'D                                        
         MVC   AIO,AIO2            READ SPOT CONTRACT RECORD INTO AIO2          
         GOTO1 VALCON#                                                          
         MVC   AIO,AIO1                                                         
         BNE   ERINVCON            INVALID CONTRACT                             
         CLC   SVCTRCTR,CONTRCTR   MAKE SURE CONTRACT/CONTRACTOR MATCH          
         BNE   ERINVCC             CONTRACT NOT VALID FOR CONTRACTOR            
         TM    4(R2),X'20'                                                      
         BO    *+8                                                              
         OI    STATUS,STKEYCH                                                   
         OI    4(R2),X'20'         VALIDATED                                    
         EJECT                                                                  
***********************************************************************         
*        VALKEY - BUILD KEY                                                     
***********************************************************************         
*                                                                               
VK100    GOTO1 ACCSYS              SWITCH BACK TO ACC                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,BLDCONT          BUILD LIST OF CONTRACTS                      
*                                                                               
         USING ACTRECD,R6          READ FOR FIRST CONTRACT                      
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'                                                     
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED                                       
         MVC   ACTKACT+L'QSYSMED(L'CON#),CONTLIST                               
         MVC   SAVEKEY,BIGKEY                                                   
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ERECNF              RECORD NOTFOUND                              
*                                                                               
VKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALREC - VALIDATE BALANCE BROUGHT FORWARD                    *         
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
         TM    STATUS,STKEYCH      CHANGE IN KEY                                
         BO    DR                                                               
*                                                                               
         TM    STATUS,STNOCHG      ALREADY ACTIVITY CANNOT ADD BBF              
         BO    ERRINV                                                           
*                                                                               
         L     R6,AIO                                                           
         XC    BIGKEY,BIGKEY            MAKE SURE HAVE RIGHT REC                
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALREC - VALIDATE MEDIA DELIVERED BALANCE B/F                *         
***********************************************************************         
*                                                                               
         LA    R2,FADMDLVH         BAL B/F  - MEDIA DLVD                        
         ZICM  R5,FADMDLVH+5                                                    
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',FADMDLV),(R5)                                
         CLI   DMCB,X'FF'                                                       
         BE    ERRAMNT             INVALID AMOUNT                               
         ZAP   ACCBMDLV,DMCB+4(8)                                               
*        CP    ACCBMDLV,TOTMGCI    CAN'T BE MORE THAN GCI                       
*        BH    ERRINV                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALREC - VALIDATE CATEGORY LINES                             *         
***********************************************************************         
*                                                                               
         USING DSPLINED,R3                                                      
         USING CATTABD,R4                                                       
         LA    R3,FADLIN1H                                                      
         LA    R4,CATTABLE                                                      
         ZAP   ACCBINV,=P'0'       BAL B/F INVOICED AMT                         
         ZAP   ACCBCOST,=P'0'      BAL B/F COST                                 
*                                                                               
VR20     BAS   RE,VALLINE          VALIDATE LINE - UPDATES X'93' ELEMS          
         LA    R3,DSPLEN(R3)                                                    
         LA    R4,CATLENQ(R4)                                                   
         CLI   0(R4),X'40'         END BEFORE TOTAL LINE                        
         BNE   VR20                                                             
         BAS   RE,ELEM92           UPDATE CONTRACT ELEM = X'92'                 
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         BAS   RE,SPOTCON          UPDATE SPOT CONTRACT RECORD                  
         MVC   AIO,AIO1                                                         
*                                                                               
VRX      B     DR                  DISPLAY REC CHANGES                          
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CATEGORY SCREEN LINE AT R3                                    
***********************************************************************         
*                                                                               
         USING DSPLINED,R3                                                      
         USING CATTABD,R4                                                       
         USING CTGELD,R6                                                        
VALLINE  NTR1                                                                   
*                                                                               
         LA    R2,DSPDLVDH         BAL B/F VALUE SERV. DLVD                     
         ZICM  R5,DSPDLVDH+5                                                    
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',DSPDLVD),(R5)                                
         CLI   DMCB,X'FF'                                                       
         BE    ERRAMNT             INVALID AMOUNT                               
         ZAP   CATBINV,DMCB+4(8)                                                
*                                                                               
         LA    R2,DSPCOSTH         BAL B/F COST OF SERV DLVD                    
         ZICM  R5,DSPCOSTH+5                                                    
         BZ    ERRMISS                                                          
         GOTO1 CASHVAL,DMCB,(X'82',DSPCOST),(R5)                                
         CLI   DMCB,X'FF'          VALID                                        
         BE    ERRAMNT             INVALID AMOUNT                               
         ZAP   CATBCOST,DMCB+4(8)                                               
*                                                                               
         LA    R2,DSPDLVDH         FOR ERROR EXIT                               
         BAS   RE,ELEM93           POINTS R6 TO CATEGORY ELEM                   
         BE    VL10                FOUND ELEM                                   
         CP    CATBINV,=P'0'                                                    
         BNE   ERRINV              CAN'T ADD TO NOTHING                         
         CP    CATBCOST,=P'0'                                                   
         BNE   ERRINV              CAN'T ADD TO NOTHING                         
         B     VLX                                                              
*                                                                               
*L10     CP    CATBINV,CTGNPAY     SERVICES DLVD > NET PAY                      
*        BH    ERRINV                                                           
VL10     CP    CATBCOST,=P'0'      IF ENTERED A COST                            
         BE    VL20                                                             
         CP    CATBINV,=P'0'       THEN MUST HAVE ENTERED A DLVD                
         BE    ERRINV                                                           
*                                                                               
VL20     XC    SVELEM,SVELEM                                                    
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   SVELEM(0),0(R6)     SAVE SHORT ELEM                              
         MVI   0(R6),X'FF'         THEN DELETE FROM REC                         
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             DELETE ELEMENT                               
*                                                                               
         LA    R6,SVELEM                                                        
         MVI   CTGLN,CTGLNQ        DEFAULT TO SHORT LEN                         
         CP    CATBINV,=P'0'                                                    
         BNE   VL50                                                             
         CP    CATBCOST,=P'0'                                                   
         BE    VL60                NO VALUES - ADD SHORT ELEM                   
VL50     MVI   CTGLN,CTGLN2Q       EXTENDED FOR BAL B/F                         
         ZAP   CTGBINV,CATBINV                                                  
         AP    ACCBINV,CTGBINV                                                  
         ZAP   CTGBCOST,CATBCOST                                                
         AP    ACCBCOST,CTGBCOST                                                
VL60     MVC   ELEM,SVELEM                                                      
         GOTO1 ADDELEM                                                          
*                                                                               
VLX      B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        FIND X'93' CATEGORY ELEMENT IN AIO OR MAKE NEW ONE IN ELEM             
*        RETURNS R6                                                             
***********************************************************************         
*                                                                               
         USING CATTABD,R4                                                       
         USING CTGELD,R6                                                        
ELEM93   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
EL93NX   BAS   RE,NEXTEL                                                        
         BNE   EL93XNO                                                          
         CLC   CATWC,CTGCTGY       MATCH ON CATEGORY=WORK CODE                  
         BNE   EL93NX                                                           
*                                                                               
EL93XYES SR    RC,RC                                                            
EL93XNO  LTR   RC,RC                                                            
EL93X    XIT1  REGS=(R6)           RETURN 93 ELEM IN R6                         
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        UPDATE X'92' CONTRACT TOTALS ELEMENT                                   
***********************************************************************         
*                                                                               
         USING CNTELD,R6                                                        
ELEM92   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92'                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVELEM,SVELEM                                                    
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   SVELEM(0),0(R6)     SAVE SHORT ELEM                              
         MVI   0(R6),X'FF'         THEN DELETE FROM REC                         
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             DELETE ELEMENT                               
         MVI   ELCODE,CNTELQ                                                    
*                                                                               
         LA    R6,SVELEM           BUILD IN SVELEM                              
         MVI   CNTLN,CNTLNQ        DEFAULT TO SHORT LEN                         
         CP    ACCBINV,=P'0'                                                    
         BNE   EL92A                                                            
         CP    ACCBCOST,=P'0'                                                   
         BE    EL92B               NO VALUES - ADD SHORT ELEM                   
EL92A    MVI   CNTLN,CNTLN2Q       EXTENDED FOR BAL B/F                         
         ZAP   CNTBINV,ACCBINV                                                  
         ZAP   CNTBCOST,ACCBCOST                                                
EL92B    MVC   ELEM,SVELEM                                                      
         GOTO1 ADDELEM                                                          
EL92X    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        UPDATE SPOT CONTRACT RECORD                                            
***********************************************************************         
*                                                                               
SPOTCON  NTR1                                                                   
         GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SC10     MVC   AIO,AIO2            MARK SPOT CONTRACT RECORD                    
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SPOTKEY),SPOTKEY                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'SPOTKEY),KEYSAVE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING CTAUSELD,R6                                                      
         MVI   ELCODE,CTAUSELQ     X'06' USAGE ELEM                             
         L     R6,AIO2                                                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SC30     BAS   RE,NEXTEL                                                        
         BNE   SC50                                                             
         TM    CTAUSTAT,CTAUSTBB   ELEM IS BAL B/F                              
         BNO   SC30                                                             
*                                                                               
         MVI   0(R6),X'FF'         THEN DELETE FROM REC                         
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             DELETE ELEMENT                               
*                                                                               
SC50     CP    ACCBMDLV,=P'0'                                                   
         BE    SC100               NO VALUES - NO ADD                           
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   CTAUSEL,CTAUSELQ    X'06' USAGE ELEM                             
         MVI   CTAUSELN,CTAUSLNQ                                                
         OI    CTAUSTAT,CTAUSTBB   BAL B/F                                      
         ZAP   DUB,ACCBMDLV        MEDIA DLVD                                   
         CVB   R1,DUB                                                           
         STCM  R1,15,CTAUSOGR      ORDERED                                      
         ZAP   DUB,ACCBMDLV        MEDIA DLVD                                   
         CVB   R1,DUB                                                           
         STCM  R1,15,CTAUSPGR      PAID                                         
         GOTO1 ADDELEM                                                          
*                                                                               
SC100    GOTO1 PUTREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 ACCSYS              SWITCH BACK TO ACC SYSTEM                    
         BE    *+6                                                              
         DC    H'0'                                                             
SCX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPREC - TOTAL BY CATEGORY                                  *         
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
         BAS   RE,CLRSCRN          CLEAR BOTTOM OF SCREEN                       
         MVI   STATUS,0                                                         
*                                                                               
         LA    R1,TOTS             CLEAR TOTALS                                 
         LA    R3,TOTSQ                                                         
         ZAP   0(L'TOTS,R1),=P'0'                                               
         LA    R1,L'TOTS(R1)                                                    
         BCT   R3,*-10                                                          
*                                                                               
         OC    CONTLIST,CONTLIST   ANY CONTRACTS                                
         BZ    DR100                                                            
         LA    R3,CONTLIST                                                      
         B     DR10HI                                                           
DR10SEQ  LA    R3,L'CON#(R3)       NEXT CONTRACT                                
         CLI   0(R3),X'FF'         ANY MORE                                     
         BE    DR100                                                            
*                                                                               
         USING ACTRECD,R6                                                       
DR10HI   LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'                                                     
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED                                       
         MVC   ACTKACT+L'QSYSMED(L'CON#),0(R3)                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         USING CATTABD,R4                                                       
         LA    R4,CATTABLE         TOTAL BY CATEGORY                            
*                                                                               
         USING CTGELD,R6                                                        
DR15     L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93' CONTRACT CATEGORY ELEM                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR20NX   BAS   RE,NEXTEL                                                        
         BNE   DR30NX                                                           
         CLC   CTGCTGY,CATWC       MATCH ON CATEGORY WORK CODE                  
         BNE   DR20NX                                                           
*                                                                               
         CP    CTGOPNPO,=P'0'      ACTIVITY = NO ADD BBF                        
         BE    *+8                                                              
         OI    STATUS,STNOCHG                                                   
         CP    CTGINVPO,=P'0'      ACTIVITY = NO ADD BBF                        
         BE    *+8                                                              
         OI    STATUS,STNOCHG                                                   
*                                                                               
DR22     ZICM  R1,CATNPAY,2        NET PAYABLE                                  
         AR    R1,RA                                                            
         AP    0(L'TOTS,R1),CTGNPAY                                             
         AP    TOTNPAY,CTGNPAY     TOTAL NET PAYABLE                            
*                                                                               
         ZICM  R1,CATDLVD,2        DLVD= OPEN PO'S + INVOICED PO'S              
         AR    R1,RA                                                            
         AP    0(L'TOTS,R1),CTGOPNPO                                            
         AP    0(L'TOTS,R1),CTGINVPO                                            
         AP    TOTGDLVD,CTGOPNPO                                                
         AP    TOTGDLVD,CTGINVPO   TOTAL GOODS DELIVERED                        
         CLI   CTGLN,CTGLNQ        SHORT LEN = NO BAL B/F                       
         BNH   DR24                                                             
         AP    TOTGDLVD,CTGBINV    BAL B/F                                      
         AP    0(L'TOTS,R1),CTGBINV                                             
*                                                                               
DR24     ZICM  R1,CATCOST,2        COST= ACTUAL COST INVOICED PO'S              
         AR    R1,RA                                                            
         AP    0(L'TOTS,R1),CTGCOST                                             
         AP    TOTINVPO,CTGCOST                                                 
         CLI   CTGLN,CTGLNQ        SHORT LEN = NO BAL B/F                       
         BNH   DR30NX                                                           
         AP    TOTINVPO,CTGBCOST   COST BAL B/F                                 
         AP    0(L'TOTS,R1),CTGBCOST                                            
*                                                                               
DR30NX   LA    R4,CATLENQ(R4)      NEXT CATEGORY                                
         CLI   0(R4),X'FF'                                                      
         BNE   DR15                                                             
         B     DR10SEQ                                                          
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPREC - TOTAL MEDIA RECS                                   *         
***********************************************************************         
*                                                                               
DR100    OC    CONTLIST,CONTLIST   ANY CONTRACTS                                
         BZ    DR200                                                            
         GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,CONTLIST                                                      
         B     *+8                                                              
DR110NX  LA    R3,L'CON#(R3)       NEXT CONTRACT                                
         CLI   0(R3),X'FF'         ANY MORE                                     
         BE    DR200                                                            
*                                                                               
         PACK  WORK+4(4),0(L'CON#,R3)    PWOS 9'S COMP FOR SPOT KEY             
         ZAP   WORK(4),=P'999999'                                               
         SP    WORK(4),WORK+4(4)                                                
         SRP   WORK(4),1,0                                                      
*                                                                               
         USING CTARECD,R6                                                       
         LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       GET SPOT CONTRACT RECORD                     
         MVI   CTAKTYP,CTAKTYPQ    X'0D'                                        
         MVI   CTAKSUB,CTAKSUBQ    X'7E'                                        
         MVC   CTAKAGMD,SBAGYMD    AGENCY MEDIA                                 
         MVC   CTAKCNUM,WORK                                                    
         MVC   SPOTKEY,BIGKEY      SAVE CONTRACT KEY                            
         GOTO1 HIGH                SPOT READ                                    
         CLC   BIGKEY(L'CTAKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         USING CTAEL,R6                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                 REQ'D                                        
         DC    H'0'                                                             
         ZICM  R1,CTDSCGCI,4                                                    
         CVD   R1,DUB                                                           
         AP    TOTMGCI,DUB(8)      MEDIA GCI TOTAL                              
*                                                                               
         USING CTAUSELD,R6                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,CTAUSELQ     X'06' USAGE ELEMENT                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR120    BAS   RE,NEXTEL                                                        
         BNE   DR110NX                                                          
         TM    CTAUSTAT,CTAUSTBB   BAL B/F                                      
         BO    *+8                                                              
         OI    STATUS,STNOCHG      ACTIVITY = NO CHANGE BAL B/F                 
         ZICM  R1,CTAUSOGR,4                                                    
         CVD   R1,DUB                                                           
         AP    TOTMRSVD,DUB(8)     MEDIA RESERVED                               
         ZICM  R1,CTAUSPGR,4                                                    
         CVD   R1,DUB                                                           
         AP    TOTMDLVD,DUB(8)     MEDIA DELIVERED                              
         B     DR120                                                            
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPREC - DISPLAY TOTALS                                     *         
***********************************************************************         
*                                                                               
         USING CATTABD,R3                                                       
DR200    LA    R3,CATTABLE         TOTALS BY CATEGORY                           
         USING DSPLINED,R2                                                      
         LA    R2,FADLIN1H                                                      
*                                                                               
DR210NX  CLC   CATWC,SPACES        TOTALS LINE                                  
         BNE   DR220               SKIP AN EXTRA LINE                           
         LA    R2,DSPLEN(R2)                                                    
         LA    R1,FADENDLH         LAST LINE                                    
         CR    R2,R1                                                            
         BH    DRX                                                              
*                                                                               
DR220    MVC   DSPCATG,CATWORD     CATEGORY NAME                                
*                                                                               
         ZICM  R4,CATNPAY,2        NET PAYABLE                                  
         AR    R4,RA                                                            
         EDIT  (P6,0(R4)),(11,DSPNPAY),2,MINUS=YES                              
         ZAP   WORK2(6),0(6,R4)                                                 
*                                                                               
         ZICM  R4,CATDLVD,2        GOODS DELIVERED                              
         AR    R4,RA                                                            
         EDIT  (P6,0(R4)),(11,DSPDLVD),2,MINUS=YES                              
         ZAP   WORK3(6),0(6,R4)                                                 
*                                                                               
         SP    WORK2(6),WORK3(6)   BALANCE = NET PAY - DLVD                     
         EDIT  (P6,WORK2),(11,DSPBAL),2,MINUS=YES                               
*                                                                               
         ZICM  R4,CATCOST,2        COST - INVOICED PO'S                         
         AR    R4,RA                                                            
         EDIT  (P6,0(R4)),(11,DSPCOST),2,MINUS=YES                              
*                                                                               
         ZAP   WORK2(6),0(6,R4)    COST%                                        
         CP    WORK2(6),=P'0'                                                   
         BE    DR230                                                            
         ZAP   WORK2(12),0(6,R4)   COST% = COST/DLVD                            
         MP    WORK2(12),=P'10000'                                              
         DP    WORK2(12),WORK3(6)                                               
DR230    EDIT  (P6,WORK2),(6,DSPCPCT),2                                         
*                                                                               
         LA    R3,CATLENQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BE    DR250                                                            
         LA    R2,DSPLEN(R2)                                                    
         LA    R1,FADENDLH         LAST LINE                                    
         CR    R2,R1                                                            
         BNH   DR210NX                                                          
*                                                                               
DR250    EDIT  (P6,TOTMGCI),(10,FADMGCI),2,MINUS=YES MEDIA GCI                  
         OI    FADMGCIH+6,X'80'                                                 
         EDIT  (P6,TOTMDLVD),(10,FADMDLV),2,MINUS=YES MEDIA DLVD                
         OI    FADMDLVH+6,X'80'                                                 
         ZAP   WORK2(6),TOTMGCI                     MEDIA BALANCE               
         SP    WORK2(6),TOTMDLVD                                                
         EDIT  (P6,WORK2),(10,FADMDBL),2,MINUS=YES                              
         OI    FADMDBLH+6,X'80'                                                 
*                                                                               
DRX      NI    STATUS,X'FF'-STKEYCH                                             
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD CONTRACT LIST GIVEN CONTRACTOR                                   
***********************************************************************         
*                                                                               
BLDCONT  NTR1                                                                   
         XC    CONTDISP,CONTDISP                                                
         XC    CONTLIST,CONTLIST                                                
         LA    R3,CONTLIST                                                      
         CLI   ACTEQU,ACTLIST                                                   
         BE    BC10                                                             
         CLC   CON#,SPACES         LOOKING FOR SPECIFIC CONTRACT                
         BNH   BC10                                                             
         MVC   0(L'CON#,R3),CON#                                                
         LA    R3,L'CON#(R3)                                                    
         B     BCX                                                              
*                                                                               
         USING CHDRECD,R6          LOOK FOR CONTRA ACCOUNT HEADER RECS          
BC10     LA    R6,BIGKEY           TO GET ALL CONTRACTS FOR CONTRACTOR          
         MVC   BIGKEY,SPACES                                                    
         MVC   CHDKCPY,CMPY                                                     
         MVI   CHDKUNT,C'S'                                                     
         MVI   CHDKLDG,C'1'                                                     
         MVC   CHDKACT(L'QSYSMED),QSYSMED               SYSTEM/MEDIA            
         MVC   CHDKACT+L'QSYSMED(L'CONTRCTR),CONTRCTR   CONRACTOR               
         MVC   CHDKCCPY,CMPY                                                    
         MVI   CHDKCUNT,C'S'                                                    
         MVI   CHDKCLDG,C'2'                                                    
         MVC   CHDKCACT(L'QSYSMED),QSYSMED                                      
         MVC   CHDKCACT+L'QSYSMED(L'CON#),CON#                                  
         XC    CHDKNULL,CHDKNULL                                                
         MVC   SAVEKEY,BIGKEY                                                   
BCHIGH   GOTO1 HIGH                                                             
         B     BC20                                                             
BCSEQ    GOTO1 SEQ                                                              
BC20     CLC   BIGKEY(L'CHDKCULA),SAVEKEY         SAME ACCOUNT                  
         BNE   BCX                                                              
         CLC   CHDKWRK,SPACES                     OFFICE IS SPACES              
         BNE   BCSEQ                                                            
         CLC   =C'S2',CHDKCUNT                    CONTRA S2                     
         BNE   BCSEQ                                                            
         CLC   CHDKCACT(L'QSYSMED),QSYSMED        CONTRA SYSTEM/MEDIA           
         BNE   BCSEQ                                                            
         OC    CHDKNULL,CHDKNULL                  CONTRA ACT HEADER?            
         BNZ   BCSEQ                                                            
         MVC   0(L'CON#,R3),CHDKCACT+L'QSYSMED                                  
         LA    R3,L'CON#(R3)                                                    
         LA    R1,CONTLEND                                                      
         MVI   CHDKNULL,X'FF'      SKIP TRANSACTIONS                            
         CR    R3,R1                                                            
         BL    BCHIGH                                                           
         DC    H'0'                CONTRACT TABLE FULL                          
*                                                                               
BCX      MVI   0(R3),X'FF'         END OF TABLE                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         LA    R3,FADPFKYH                                                      
         LA    R2,PFTABLE                                                       
         GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
SUX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CLEAR SCREEN                                                 *         
***********************************************************************         
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,FADLIN1H         FIRST FIELD                                  
         LA    R3,FADENDLH         LAST FIELD                                   
*                                                                               
CS10     ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         BM    CSX                                                              
         EX    R1,*+4                                                           
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         LA    R2,9(R1,R2)         BUMP TO NEXT FIELD                           
         CR    R2,R3                                                            
         BNH   CS10                                                             
CSX      B XIT                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        ERRORS AND RANDOM STUFF                                      *         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
ERECNF   MVC   GERROR,=AL2(ACERECNF)   RECORD NOT FOUND                         
         B     ACCERRX                                                          
ERRMEDIA MVC   GERROR,=AL2(ACEINVMD)   INVALID MEDIA                            
         B     ACCERRX                                                          
ERRNOS1  MVC   GERROR,=AL2(ACENOS1)    S1 ACCOUNT DOESN'T EXIST                 
         B     ACCERRX                                                          
ERRNOS2  MVC   GERROR,=AL2(ACENOS2)    S2 ACCOUNT DOESN'T EXIST                 
         B     ACCERRX                                                          
ERINVCON MVC   GERROR,=AL2(ACEINVCN)   INVALID CONTRACT                         
         B     ACCERRX                                                          
ERINVCC  MVC   GERROR,=AL2(ACECTCON)   CONTRACT INVALID FOR CONTRACTOR          
         B     ACCERRX                                                          
ERINVCTR MVC   GERROR,=AL2(ACEINVCT)   INVALID CONTRACTOR                       
         B     ACCERRX                                                          
ERINVCTG MVC   GERROR,=AL2(ACEINVCG)   INVALID CATEGORY                         
         B     ACCERRX                                                          
ERRAMNT  MVC   GERROR,=AL2(ACEAMNT)    INVALID AMOUNT                           
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        MAINTENANCE SCREEN PFKEY TABLE DEFINITIONS                             
***********************************************************************         
PFTABLE  DS    0C                                                               
         DC    AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                   
         DC    CL3' ',CL8'CTAGS   ',CL8'DISPLAY '                               
PF02     DC    AL1(KEYTYTWA,L'FADMED-1),AL2(FADMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FADCONT-1),AL2(FADCONT-T61EFFD)                   
PF02X    EQU   *                                                                
*                                                                               
         DC    AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'LIST    '                               
PF03     DC    AL1(KEYTYTWA,L'FADMED-1),AL2(FADMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FADCNTR-1),AL2(FADCNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'FADCONT-1),AL2(FADCONT-T61EFFD)                   
PF03X    EQU   *                                                                
*                                                                               
         DC    AL1(PF04X-*,04,PFTCPROG,(PF04X-PF04)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'DIS     '                               
PF04     DC    AL1(KEYTYTWA,L'FADMED-1),AL2(FADMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FADCNTR-1),AL2(FADCNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'FADCONT-1),AL2(FADCONT-T61EFFD)                   
PF04X    EQU   *                                                                
*                                                                               
         DC    AL1(PF05X-*,05,PFTCPROG,(PF05X-PF05)/KEYLNQ,0)                   
         DC    CL3' ',CL8'ORDER   ',CL8'DISPLAY '                               
PF05     DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'FADMED-1),AL2(FADMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FADCONT-1),AL2(FADCONT-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'FADCNTR-1),AL2(FADCNTR-T61EFFD)                   
PF05X    EQU   *                                                                
*                                                                               
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        CATEGORY TOTALS AND DISPLAY TABLE                            *         
***********************************************************************         
*                                                                               
CATTABLE DS    0C                                                               
         DC    CL2'MD',CL8'MDSE    '                                            
         DC    AL2(TMDNPAY-T61EFFD,TMDGDLVD-T61EFFD,TMDINVPO-T61EFFD)           
         DC    CL2'AX',CL8'AMEX    '                                            
         DC    AL2(TAXNPAY-T61EFFD,TAXGDLVD-T61EFFD,TAXINVPO-T61EFFD)           
         DC    CL2'CA',CL8'CASH/BP '                                            
         DC    AL2(TCANPAY-T61EFFD,TCAGDLVD-T61EFFD,TCAINVPO-T61EFFD)           
         DC    CL2'HL',CL8'HOTEL   '                                            
         DC    AL2(THLNPAY-T61EFFD,THLGDLVD-T61EFFD,THLINVPO-T61EFFD)           
         DC    CL2'SC',CL8'SCRIP   '                                            
         DC    AL2(TSCNPAY-T61EFFD,TSCGDLVD-T61EFFD,TSCINVPO-T61EFFD)           
         DC    CL2'OR',CL8'OTHER   '                                            
         DC    AL2(TORNPAY-T61EFFD,TORGDLVD-T61EFFD,TORINVPO-T61EFFD)           
         DC    CL2'  ',CL8'TOTALS  '                                            
         DC    AL2(TOTNPAY-T61EFFD,TOTGDLVD-T61EFFD,TOTINVPO-T61EFFD)           
         DC    X'FF'                                                            
*                                                                               
DOTLINE  DC    80C'-'                                                           
DOT2LINE DC    80C'='                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                     *         
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        ACCTAWORKD                                                             
*        ACCTADSECT                                                             
*        ACGENFILE                                                              
*        SPGENCTA                                                               
*        DDBIGBOX                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACCTAWORKD                                                     
       ++INCLUDE ACCTADSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE SPGENCTA                                                       
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREEENS                                                     *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCTAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCTAF9D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                          *         
***********************************************************************         
*                                                                               
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
FULL2    DS    F                                                                
WORK2    DS    4F                                                               
WORK3    DS    4F                                                               
*                                                                               
SVCTRCTR DS    CL(L'CONTRCTR)      SAVED CONTRACTOR                             
ACCNT    DS    CL12                TEMP ACCOUNT FIELD                           
SMNAME   DS    CL26                SYSTEM/MEDIA NAME                            
*                                                                               
STATUS   DS    XL1                 STATUS BITS                                  
STKEYCH  EQU   X'80'               CHANGE IN KEY                                
STNOCHG  EQU   X'40'               DO NOT ALLOW CHANGE                          
*                                                                               
CATBINV  DS    PL6                 BAL B/F INVOICED AMOUNT (CATEGORY)           
ACCBINV  DS    PL6                 BAL B/F INVOICED AMOUNT (CONTRACT)           
CATBCOST DS    PL6                 BAL B/F COST (CATEGORY)                      
ACCBCOST DS    PL6                 BAL B/F COST (CONTRACT)                      
ACCBMDLV DS    PL6                 BAL B/F MEDIA DLVD                           
*                                                                               
TOTS     DS    0PL6                                                             
TOTNPAY  DS    PL6                 NET PAYABLE                                  
TOTGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
         ORG   TOTGDLVD                                                         
TOTSDLVD DS    PL6                                                              
TOTOPNPO DS    PL6                 OPEN PO'S                                    
TOTINVPO DS    PL6                 INVOICED PO'S                                
TOTMGCI  DS    PL6                 GCI TOTAL                                    
TOTMRSVD DS    PL6                 MEDIA RSVD = USED = BOUGHT                   
TOTMDLVD DS    PL6                 MEDIA DLVD = DLVD = PAID                     
TOTMBAL  DS    PL6                 MEDIA BALANCE                                
TOTSBAL  DS    PL6                 SERVICE BALANCE                              
TOTSCOST DS    PL6                 SERVICE COST                                 
TOTTCOST DS    PL6                 TIME COST                                    
TOTINV   DS    PL6                 INVENTORY VALUE                              
TOTPAY   DS    PL6                 PAYABLE VALUE                                
*                                                                               
TOTSMD   DS    0PL6                MDSE TOTALS                                  
TMDNPAY  DS    PL6                 NET PAYABLE                                  
TMDGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
TMDINVPO DS    PL6                 INVOICED PO'S                                
*                                                                               
TOTSAX   DS    0PL6                AMEX TOTALS                                  
TAXNPAY  DS    PL6                 NET PAYABLE                                  
TAXGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
TAXINVPO DS    PL6                 INVOICED PO'S                                
*                                                                               
TOTSCA   DS    0PL6                CASH/BP TOTALS                               
TCANPAY  DS    PL6                 NET PAYABLE                                  
TCAGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
TCAINVPO DS    PL6                 INVOICED PO'S                                
*                                                                               
TOTSHL   DS    0PL6                HOTEL TOTALS                                 
THLNPAY  DS    PL6                 NET PAYABLE                                  
THLGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
THLINVPO DS    PL6                 INVOICED PO'S                                
*                                                                               
TOTSSC   DS    0PL6                SCRIP TOTALS                                 
TSCNPAY  DS    PL6                 NET PAYABLE                                  
TSCGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
TSCINVPO DS    PL6                 INVOICED PO'S                                
*                                                                               
TORSOT   DS    0PL6                OTHER TOTALS                                 
TORNPAY  DS    PL6                 NET PAYABLE                                  
TORGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
TORINVPO DS    PL6                 INVOICED PO'S                                
TOTSQ    EQU   (*-TOTS)/L'TOTS                                                  
*                                                                               
SAVEKEY  DS    CL(L'ACCKEY)                                                     
SPOTKEY  DS    CL(L'CTAKEY)                                                     
SVELEM   DS    CL100                                                            
*                                                                               
CONTDISP DS    XL2                 DISP TO LAST CONT                            
CONTLIST DS    CL(CONTLSTQ)        CONTRACT LIST FOR CONTRACTOR                 
CONTLEND DS    0C                                                               
CONTLSTQ EQU   40*(L'CON#)                                                      
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
*        CATEGORY TABLE DSECT                                                   
***********************************************************************         
*                                                                               
CATTABD  DSECT                                                                  
CATWC    DS    CL2                 CATEGORY WORK CODE                           
CATWORD  DS    CL8                 CATEGORY WORD                                
CATNPAY  DS    XL2                 DISP TO NET PAY TOTAL                        
CATDLVD  DS    XL2                 DISP TO DLVD TOTAL                           
CATCOST  DS    XL2                 DISP TO COST TOTAL                           
CATLENQ  EQU   *-CATTABD                                                        
         EJECT                                                                  
***********************************************************************         
*        DISPLAY LINE DSECT                                                     
***********************************************************************         
*                                                                               
DSPLINED DSECT                     DISPLAY LINE DSECT                           
DSPCATGH DS    CL8                                                              
DSPCATG  DS    CL8                 CATEGORY NAME                                
DSPNPAYH DS    CL8                                                              
DSPNPAY  DS    CL11                NET PAYABLE                                  
DSPDLVDH DS    CL8                                                              
DSPDLVD  DS    CL11                GOODS DELIVERED                              
DSPBALH  DS    CL8                                                              
DSPBAL   DS    CL11                BALANCE = NET PAY - DLVD                     
DSPCOSTH DS    CL8                                                              
DSPCOST  DS    CL11                INVOICED PO'S                                
DSPCPCTH DS    CL8                                                              
DSPCPCT  DS    CL6                 COST% = COST/DLVD                            
DSPLEN   EQU   *-DSPLINED                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        END                                                                    
***********************************************************************         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACCTA03   05/01/02'                                      
         END                                                                    
