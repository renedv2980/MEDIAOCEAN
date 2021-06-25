*          DATA SET ACCTA04X   AT LEVEL 070 AS OF 05/01/02                      
*PHASE T61E04A                                                                  
*INCLUDE SORTER                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCTA04 -- CTA FINANCIAL LIST/DIS/REPORT             *         
*                                                                     *         
*  COMMENTS:     LISTS AND DISPLAYS CTA CONTRACT FINANCIAL INFO       *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61E00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCTAF3 (LIST)                               *         
*                        ACCTAF4 (DIS)                                *         
*                        ACCTAF7 (REP)                                *         
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
         TITLE 'T61E04 - CTA FINANCIAL LIST/MAINT/REPORT'                       
T61E04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1E04**,R7,RR=R3                                              
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
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,VALREC         ALWAYS CALLED WITH VALREC (BIT ON)           
         BE    DR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALKEY - VALIDATE SYSTEM/MEDIA AND CONTRACTOR                *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         MVI   PRSTAT,0                                                         
         MVI   PRSTAT2,0                                                        
         LA    R2,FINMEDH          VALIDATE MEDIA                               
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
*                                                                               
         LA    R2,FINCNTRH         VALIDATE SPOT CONTRACTOR                     
         CLI   ACTEQU,ACTREP       REPORT                                       
         BNE   VK10                                                             
         CLI   5(R2),3                                                          
         BNE   VK10                                                             
         CLC   =C'ALL',8(R2)       ALL CONTRACTORS                              
         BNE   VK10                                                             
         TM    WHEN,X'40'          NOW REQUEST                                  
         BO    ERRINV              THEN CAN'T USE ALL                           
         OI    PRSTAT,PRSALLCN                                                  
         MVC   CNTRNAME,SPACES                                                  
         B     VK20                                                             
VK10     CLI   5(R2),0                                                          
         BE    ERRPLS              REQ'D                                        
         MVC   CONTRCTR,FINCNTR    FOR VALIDATION                               
         OC    CONTRCTR,SPACES                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 VALCNTR                                                          
         BNE   ERINVCTR            INVALID CONTRACTOR                           
         MVC   AIO,AIO1                                                         
         MVC   FINCNME,CNTRNAME    DISPLAY NAME                                 
         OI    FINCNMEH+6,X'80'                                                 
         MVC   SVCTRCTR,CONTRCTR   SAVE CONTRACTOR                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALKEY - VALIDATE CONTRACT AND CATEGORY FILTERS              *         
***********************************************************************         
*                                                                               
VK20     XC    PCON#,PCON#         PACKED CONTRACT NUMBER                       
         MVC   CON#,SPACES                                                      
         LA    R2,FINCONTH         VALIDATE SPOT CONTRACT NUMBER                
         CLI   5(R2),0             NOT REQ'D                                    
         BE    VK40                                                             
         CLC   =C'ALL',8(R2)       ALL CONTRACTS                                
         BE    VK40                                                             
         MVC   AIO,AIO2            READ SPOT CONTRACT RECORD INTO AIO2          
         GOTO1 VALCON#                                                          
         MVC   AIO,AIO1                                                         
         BNE   ERINVCON            INVALID CONTRACT                             
         CLC   SVCTRCTR,CONTRCTR   MAKE SURE CONTRACT/CONTRACTOR MATCH          
         BNE   ERINVCC             CONTRACT NOT VALID FOR CONTRACTOR            
*                                                                               
VK40     DS    0H                                                               
         XC    CATFILT,CATFILT     CATEGORY FILTER ON LIST                      
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK50                                                             
         LA    R2,FILCATGH         CATEGORY                                     
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         GOTO1 VALCATG             VALIDATE CATEGORY                            
         BNE   ERINVCTG            INVALID CATEGORY                             
         MVC   CATFILT,CATEGORY                                                 
         EJECT                                                                  
***********************************************************************         
*        VALKEY - VALIDATE REPORT OPTIONS                                       
***********************************************************************         
*                                                                               
VK50     DS    0H                                                               
         CLI   ACTEQU,ACTREP                                                    
         BNE   VK100                                                            
         LA    R2,FIRCLSDH                                                      
         CLI   FIRCLSD,C'Y'        INCLUDE CLOSED CONTRACTS                     
         BNE   *+12                                                             
         OI    PRSTAT,PRSCLSD                                                   
         B     VK52                                                             
         CLI   FIRCLSD,C'O'        ONLY CLOSED CONTRACTS                        
         BNE   *+12                                                             
         OI    PRSTAT,PRSOCLS                                                   
         B     VK52                                                             
         CLI   FIRCLSD,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VK52     LA    R2,FIRCOSTH                                                      
         CLI   FIRCOST,C'N'        PRINT COST INFO ON REPORT                    
         BNE   *+12                                                             
         OI    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         B     VK54                                                             
         CLI   FIRCOST,C'Y'                                                     
         BNE   ERRINV                                                           
*                                                                               
VK54     LA    R2,FIRSORTH                                                      
         CLI   FIRSORT,C'Y'        SORT ALL DETAILS BY TRANS DATE               
         BNE   *+12                                                             
         OI    PRSTAT,PRSSDATE     YES, SORT ON DATE NOT MEDIA/SERV             
         B     VK56                                                             
         CLI   FIRSORT,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VK56     LA    R2,FIRINVH                                                       
         CLI   FIRINV,C'Y'         INVENTORY REPORT ONLY                        
         BNE   *+12                                                             
         OI    PRSTAT,PRSINV       YES                                          
         B     VK58                                                             
         CLI   FIRINV,C'N'                                                      
         BNE   ERRINV                                                           
*                                                                               
VK58     LA    R2,FIRSUMMH                                                      
         CLI   FIRSUMM,C'Y'        SUMMARY REPORT ONLY                          
         BNE   *+12                                                             
         OI    PRSTAT,PRSSUMM      YES                                          
         B     VK60                                                             
         CLI   FIRSUMM,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VK60     LA    R2,FIRNDTEH         END DATE?                                    
         CLI   FIRNDTEH+5,0        NO DATE ENTERED?                             
         BE    VK67                USE TODAY'S DATE                             
         GOTO1 PERVAL,DMCB,(FIRNDTEH+5,FIRNDTE),BLOCK                           
         CLI   DMCB+4,1                                                         
         BE    ERINVDTE                                                         
         USING PERVALD,R3                                                       
         LA    R3,BLOCK                                                         
         MVC   WORK2(6),PVALESTA                                                
         B     VK69                                                             
*                                                                               
VK67     GOTO1 DATCON,DMCB,(5,0),(0,WORK2)                                      
VK69     GOTO1 DATCON,DMCB,(0,WORK2),(2,ENDDATE)                                
         GOTO1 ADDAY,DMCB,(C'Y',WORK2),WORK3,-1                                 
         GOTO1 DATCON,DMCB,(0,WORK3),(1,ENDPRIOR)                               
         GOTO1 ADDAY,DMCB,(C'Y',WORK2),WORK3,-2                                 
         GOTO1 DATCON,DMCB,(0,WORK3),(1,TWOPRIOR)                               
*                                                                               
VK70     LA    R2,FIRIATVH                                                      
         CLI   FIRIATV,C'Y'        INACTIVE REPORT ONLY                         
         BNE   *+12                                                             
         OI    PRSTAT2,PRSIATV     YES                                          
         B     VK80                                                             
         CLI   FIRIATV,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VK80     DS    0H                                                               
         B     VK100                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALKEY - BUILD KEY                                                     
***********************************************************************         
*                                                                               
VK100    GOTO1 ACCSYS              SWITCH BACK TO ACC                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ACTEQU,ACTREP       REPORT                                       
         BNE   VK105                                                            
         TM    FIRMEDH+4,X'20'     REVIOUSLY VALIDATED                          
         BO    *+8                                                              
         OI    PRSTAT,PRSFIRST     FIRST TIME THROUGH                           
         TM    FIRCNTRH+4,X'20'    REVIOUSLY VALIDATED                          
         BO    *+8                                                              
         OI    PRSTAT,PRSFIRST     FIRST TIME THROUGH                           
         OI    FIRMEDH+4,X'20'     VALIDATED                                    
         OI    FIRCNTRH+4,X'20'                                                 
         B     VK110                                                            
VK105    NI    FINMEDH+4,X'FF'-X'20'     UNVALIDATE IF NOT REPORT               
         NI    FINCNTRH+4,X'FF'-X'20'                                           
*                                                                               
VK110    BAS   RE,BLDCONT          BUILD LIST OF CONTRACTS                      
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
         CLI   ACTEQU,ACTDIS                                                    
         BNE   VKX                                                              
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ERECNF              RECORD NOTFOUND                              
*                                                                               
VKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD CONTRACT LIST GIVEN CONTRACTOR                                   
***********************************************************************         
*                                                                               
BLDCONT  NTR1                                                                   
         XC    CONTDISP,CONTDISP                                                
         XC    CONTLIST,CONTLIST                                                
*                                                                               
         TM    PRSTAT,PRSALLCN     ALL CONTRACTORS                              
         BO    XIT                 THEN NO TABLE                                
*                                                                               
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
*        DISPLAY KEY                                                            
***********************************************************************         
*                                                                               
         USING ACTRECD,R6                                                       
DK       LA    R6,BIGKEY           DISPLAY CONTRACT NUMBER                      
         MVC   FINCONT,ACTKACT+L'QSYSMED                                        
         OI    FINCONTH+6,X'80'                                                 
         MVC   SAVEKEY,BIGKEY                                                   
         XC    CONTLIST,CONTLIST                                                
         MVC   CONTLIST(L'CON#),ACTKACT+L'QSYSMED                               
         MVI   CONTLIST+L'CON#,X'FF'                                            
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPREC - TOTAL BY CATEGORY                                  *         
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
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
         USING CXFELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CXFELQ       X'95' TRANSFER ELEM                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR12     BAS   RE,NEXTEL                                                        
         BNE   DR14                                                             
         ZAP   WORK(6),CXFAMNT     AMOUNT OF XFR                                
         TM    CXFSTAT,CXFSPAID    ARE XFR $ PAID                               
         BO    *+10                ADD TO DLVD                                  
         MP    WORK(6),=P'-1'       IF AVAILABLE THEN MINUS FROM DLVD           
         AP    TXFGDLVD,WORK(6)                                                 
         AP    TOTGDLVD,WORK(6)                                                 
         B     DR12                                                             
*                                                                               
         USING CATTABD,R4                                                       
DR14     LA    R4,CATTABLE         TOTAL BY CATEGORY                            
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
         ZICM  R1,CATNPAY,2        NET PAYABLE                                  
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
         CLI   CTGLN,CTGLNQ        ANY BAL B/F                                  
         BNH   DR22                                                             
         AP    0(L'TOTS,R1),CTGBINV                                             
         AP    TOTGDLVD,CTGBINV                                                 
*                                                                               
DR22     ZICM  R1,CATCOST,2        COST= ACTUAL COST INVOICED PO'S              
         AR    R1,RA                                                            
         AP    0(L'TOTS,R1),CTGCOST                                             
         AP    TOTINVPO,CTGCOST                                                 
         CLI   CTGLN,CTGLNQ        ANY BAL B/F                                  
         BNH   DR30NX                                                           
         AP    0(L'TOTS,R1),CTGBCOST                                            
         AP    TOTINVPO,CTGBCOST                                                
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
         BNE   DR130                                                            
         ZICM  R1,CTAUSOGR,4                                                    
         CVD   R1,DUB                                                           
         AP    TOTMRSVD,DUB(8)     MEDIA RESERVED                               
         ZICM  R1,CTAUSPGR,4                                                    
         CVD   R1,DUB                                                           
         AP    TOTMDLVD,DUB(8)     MEDIA DELIVERED                              
         B     DR120                                                            
*                                                                               
         USING CTAXFELD,R6                                                      
DR130    L     R6,AIO                                                           
         MVI   ELCODE,CTAXFELQ     X'07' TRANSFER ELEM                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DR135    BAS   RE,NEXTEL                                                        
         BNE   DR110NX                                                          
         ZICM  R1,CTAXFOGR,4                                                    
         CVD   R1,DUB                                                           
         TM    CTAXSTAT,CTAXSTPD   $ ARE PAID                                   
         BO    *+10                YES THEN ADD                                 
         MP    DUB(8),=P'-1'       NO THEN MINUS                                
         AP    TOTMRSVD,DUB(8)     MEDIA RESERVED                               
         AP    TOTMDLVD,DUB(8)     MEDIA DELIVERED                              
         B     DR135                                                            
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
         LA    R2,FINLIN1H                                                      
*                                                                               
DR210NX  CLC   CATWC,SPACES        TOTALS LINE                                  
         BNE   DR220               SKIP AN EXTRA LINE                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R1,FINENDLH         LAST LINE                                    
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
DR230    EDIT  (P6,WORK2),(10,DSPCPCT),2,MINUS=YES                              
*                                                                               
         OI    DSPHEAD+6,X'80'                                                  
         LA    R3,CATLENQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BE    DR250                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R1,FINENDLH         LAST LINE                                    
         CR    R2,R1                                                            
         BNH   DR210NX                                                          
*                                                                               
DR250    EDIT  (P6,TOTMGCI),(11,FINMGCI),2,MINUS=YES MEDIA GCI                  
         OI    FINMGCIH+6,X'80'                                                 
         EDIT  (P6,TOTMRSVD),(11,FINMRSV),2,MINUS=YES MEDIA RESERVED            
         OI    FINMRSVH+6,X'80'                                                 
         ZAP   WORK2(6),TOTMGCI                     RSVD BALANCE                
         SP    WORK2(6),TOTMRSVD                                                
         EDIT  (P6,WORK2),(11,FINMRBL),2,MINUS=YES                              
         OI    FINMRBLH+6,X'80'                                                 
         EDIT  (P6,TOTMDLVD),(11,FINMDLV),2,MINUS=YES MEDIA DLVD                
         OI    FINMDLVH+6,X'80'                                                 
         ZAP   WORK2(6),TOTMGCI                     MEDIA BALANCE               
         SP    WORK2(6),TOTMDLVD                                                
         EDIT  (P6,WORK2),(11,FINMDBL),2,MINUS=YES                              
         OI    FINMDBLH+6,X'80'                                                 
*                                                                               
DRX      B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        LISTRECS                                                     *         
***********************************************************************         
*                                                                               
LR       GOTO1 =A(LRECS),DMCB,(RC),RR=RELO                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT - ADD TSAR RECS FROM ACC RECS                             
***********************************************************************         
*                                                                               
PR       DS    0H                                                               
         LA    R2,FINMEDH                                                       
         CLI   OFFLINE,C'Y'                                                     
         BNE   PR01                                                             
         BAS   RE,SORTINIT                                                      
         B     PR02                                                             
PR01     TM    PRSTAT,PRSFIRST     FIRST TIME THROUGH                           
         BO    ERRENTER            PRESS ENTER TO GEN REPORT                    
         BAS   RE,TSARINIT                                                      
PR02     ZAP   RTOTPAY,=P'0'                                                    
         ZAP   RTOTINV,=P'0'                                                    
*                                                                               
         MVC   ACCNT,SPACES        GET SYSTEM/MEDIA NAME                        
         MVC   ACCNT(L'QSYSMED),QSYSMED                                         
         MVC   AIO,AIO2                                                         
         GOTO1 GTLEVNM,DMCB,C'S2',ACCNT,0                                       
         MVC   AIO,AIO1                                                         
         MVC   SMNAME,WORK                                                      
*                                                                               
         TM    PRSTAT,PRSALLCN     ALL CONTRACTORS                              
         BO    PR12                                                             
*                                                                               
         OC    CONTLIST,CONTLIST   ANY CONTRACTS                                
         BZ    PRX                                                              
         LA    R3,CONTLIST                                                      
         B     *+8                                                              
PR10     LA    R3,L'CON#(R3)       NEXT CONTRACT                                
         CLI   0(R3),X'FF'         ANY MORE                                     
         BE    PR50                                                             
*                                                                               
         USING TRNRECD,R6                                                       
PR12     MVC   AIO,AIO1                                                         
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   TRNKCPY,CMPY                                                     
         MVI   TRNKUNT,C'S'                                                     
         MVI   TRNKLDG,C'2'                                                     
         MVC   TRNKACT(L'QSYSMED),QSYSMED                                       
         TM    PRSTAT,PRSALLCN     ALL CONTRACTORS                              
         BO    *+10                                                             
         MVC   TRNKACT+L'QSYSMED(L'CON#),0(R3)                                  
         MVC   SAVEKEY,BIGKEY                                                   
PRHIGH   GOTO1 HIGH                                                             
         B     PR20                                                             
PRSEQ    GOTO1 SEQ                                                              
PR20     LA    R6,BIGKEY                                                        
         TM    TRNKSTAT,ACTSLOCK   IS ACCOUNT LOCKED                            
         BZ    PR20A                                                            
         CLC   TRNKOFF(L'TRNKOFF+L'TRNKCULC),SPACES                             
         BNE   PR20A               HAS TO BE ACCT RECORD                        
         MVC   LCKCNTR,TRNKACT                                                  
         B     PRSEQ                                                            
*                                                                               
PR20A    TM    PRSTAT,PRSALLCN     ALL CONTRACTORS                              
         BNO   PR21                                                             
         TM    PRSTAT,PRSINV       INVENTORY REPORT?                            
         BO    PR20C               DON'T CHECK LOCKED CONTRACT                  
         CLC   TRNKACT,LCKCNTR     WAS THIS A LOCKED CONTRACT?                  
         BE    PRSEQ                                                            
*                                                                               
PR20C    CLC   BIGKEY(3+L'QSYSMED),SAVEKEY         SAME U/L                     
         BNE   PR50                                                             
         CLC   BIGKEY+3+L'QSYSMED(L'CON#),SPACES                                
         BE    PRSEQ                                                            
         CLC   BIGKEY+3(L'QSYSMED),QSYSMED                                      
         BNE   PR50                                                             
         B     PR21A                                                            
PR21     CLC   BIGKEY(L'TRNKCULA),SAVEKEY         SAME ACCOUNT                  
         BNE   PR10                                                             
PR21A    CLC   TRNKOFF(L'TRNKOFF+L'TRNKCULC),SPACES                             
         BNE   PR22                                                             
         GOTO1 =A(PROCREC),DMCB,(RC),ACTREC,RR=RELO                             
         B     PRSEQ                                                            
PR22     DS    0H                                                               
         CLC   TRNKREF,SPACES      ANY REFERENCE NUMBER                         
         BNH   PRSEQ                                                            
         TM    TRNKSTAT,TRNSDRFT   DRAFT TRANSACTION                            
         BO    PRSEQ                                                            
         TM    PRSTAT,PRSINV       SKIP TRANSACTIONS FOR INVENTORY              
*        BO    PRSEQ                                                            
PR25     GOTO1 =A(PROCREC),DMCB,(RC),TRNREC,RR=RELO                             
         B     PRSEQ                                                            
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT - ADD TSAR RECS FROM MEDIA RECS                           
***********************************************************************         
*                                                                               
PR50     GOTO1 SPTSYS              SWITCH TO SPOT SYSTEM                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PRSTAT,PRSALLCN     ALL CONTRACTORS                              
         BO    PR50A                                                            
         LA    R3,CONTLIST                                                      
         B     *+16                                                             
PR50NX   TM    PRSTAT,PRSALLCN     ALL CONTRACTORS                              
         BO    PR50SEQ                                                          
         LA    R3,L'CON#(R3)       NEXT CONTRACT                                
         CLI   0(R3),X'FF'         ANY MORE                                     
         BE    PR100                                                            
*                                                                               
         PACK  WORK+4(4),0(L'CON#,R3)    PWOS 9'S COMP FOR SPOT KEY             
         ZAP   WORK(4),=P'999999'                                               
         SP    WORK(4),WORK+4(4)                                                
         SRP   WORK(4),1,0                                                      
*                                                                               
         USING CTARECD,R6                                                       
PR50A    LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY       GET SPOT CONTRACT RECORD                     
         MVI   CTAKTYP,CTAKTYPQ    X'0D'                                        
         MVI   CTAKSUB,CTAKSUBQ    X'7E'                                        
         MVC   CTAKAGMD,SBAGYMD    AGENCY MEDIA                                 
         TM    PRSTAT,PRSALLCN     ALL CONTRACTORS                              
         BO    *+10                                                             
         MVC   CTAKCNUM,WORK                                                    
         GOTO1 HIGH                SPOT READ                                    
         B     PR52                                                             
PR50SEQ  GOTO1 SEQ                 SPOT READ                                    
         LA    R6,BIGKEY                                                        
PR52     TM    PRSTAT,PRSALLCN                                                  
         BO    PR53                                                             
         TM    CTAKCNTL,CTAKCLOC   CONTRACT LOCKED                              
         BO    PR50NX                                                           
         CLC   BIGKEY(L'CTAKEY),KEYSAVE                                         
         BE    PR54                                                             
         DC    H'0'                                                             
PR53     CLC   BIGKEY(3),KEYSAVE                                                
         BNE   PR100                                                            
         TM    CTAKCNTL,CTAKCACC   ACC REC EXISTS                               
         BNO   PR50SEQ                                                          
         TM    CTAKCNTL,CTAKCLOC   CONTRACT LOCKED                              
         BO    PR50SEQ                                                          
*                                                                               
         USING TSRECD,R4                                                        
PR54     L     R4,AIO3             BUILD TSAR REC HERE                          
         L     R6,AIO1             READ REC HERE                                
         GOTO1 GETREC                                                           
*                                                                               
         XC    0(TSRECLEN,R4),0(R4)                                             
         MVC   TSSYSMED,QSYSMED               SYSTEM/MEDIA                      
*        MVC   TSCNTR,CONTRCTR                CONTRACTOR                        
         OI    TSTYPE,TSTSUM                  INFO FOR SUMMARY                  
         OI    TSREC,TSRMED                   FROM MEDIA REC                    
*                                                                               
         ZAP   FULL,=P'0'                                                       
         MVO   FULL(4),CTAKCNUM      CONVERT TO PACKED                          
         ZAP   WORK(5),=P'999999'                                               
         SP    WORK(5),FULL(4)                                                  
         ZAP   FULL,WORK(5)                                                     
         EDIT  (P4,FULL),(5,TSCON#)                                             
*                                                                               
         USING CTAEL,R6                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                 REQ'D                                        
         DC    H'0'                                                             
         MVC   TSSTDATE,CTDSCST    START DATE                                   
         MVC   TSCNTR,CTDSCNTR                CONTRACTOR                        
         MVC   CONTRCTR,CTDSCNTR              CONTRACTOR                        
*                                                                               
         ZAP   TSMDLVD,=P'0'       MEDIA DLVD                                   
         ZAP   WORK2(8),=P'0'      SAVE BAL B/F                                 
         USING CTAUSEL,R6                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,CTAUSELQ     X'06' USAGE ELEM                             
         BAS   RE,GETEL                                                         
         B     *+8                 REQ'D                                        
PR55NX   BAS   RE,NEXTEL                                                        
         BNE   PR60                                                             
         ZICM  R1,CTAUSPGR,4       MEDIA DLVD                                   
         CVD   R1,DUB                                                           
         AP    TSMDLVD,DUB(8)                                                   
*                                                                               
         TM    CTAUSTAT,CTAUSTBB   BAL B/F                                      
         BNO   PR55NX                                                           
         ZAP   WORK2(8),DUB(8)                                                  
         B     PR55NX                                                           
*                                                                               
         USING CTAXFELD,R6                                                      
PR60     L     R6,AIO                                                           
         MVI   ELCODE,CTAXFELQ     X'07' TRANSFER ELEM                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PR65     BAS   RE,NEXTEL                                                        
         BNE   PR70                                                             
         ZICM  R1,CTAXFOGR,4                                                    
         CVD   R1,DUB                                                           
         TM    CTAXSTAT,CTAXSTPD   $ ARE PAID                                   
         BO    *+10                YES THEN ADD                                 
         MP    DUB(8),=P'-1'       NO THEN MINUS                                
         AP    TSMDLVD,DUB(8)                                                   
         B     PR65                                                             
*                                                                               
PR70     CLI   OFFLINE,C'Y'                                                     
         BNE   PR72                                                             
         TM    PRSTAT,PRSINV                                                    
         BZ    PR71                                                             
         ZAP   TSMDLVD,WORK2(8)                                                 
PR71     BAS   RE,SORTADD          SUMMARY REPORT                               
         B     *+8                                                              
PR72     BAS   RE,TSARADD          SUMMARY REPORT                               
         CLI   TSARERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PRSTAT,PRSINV       INVENTORY REPORT ONLY                        
         BO    PR50NX              THEN DON'T ADD DETAIL ITEM                   
         TM    PRSTAT,PRSSUMM      SUMMARY REPORT ONLY                          
         BO    PR50NX              THEN DON'T ADD DETAIL ITEM                   
         ZAP   TSMDLVD,WORK2(8)                                                 
         MVI   TSTYPE,TSTDET       FOR DETAIL USE BAL B/F MED DLVD              
         CLI   OFFLINE,C'Y'                                                     
         BNE   PR75                                                             
         BAS   RE,SORTADD                                                       
         B     *+8                                                              
PR75     BAS   RE,TSARADD                                                       
         CLI   TSARERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTAXFELD,R6                                                      
PR80     L     R6,AIO                                                           
         MVI   ELCODE,CTAXFELQ     X'07' TRANSFER ELEM                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PR90     BAS   RE,NEXTEL                                                        
         BNE   PR50NX                                                           
         ZICM  R1,CTAXFOGR,4                                                    
         CVD   R1,DUB                                                           
         TM    CTAXSTAT,CTAXSTPD   $ ARE PAID                                   
         BO    *+10                YES THEN ADD                                 
         MP    DUB(8),=P'-1'       NO THEN MINUS                                
         ZAP   TSMDLVD,DUB(8)                                                   
         ZAP   FULL,=P'0'                                                       
         MVO   FULL(4),CTAXFCON    TRANSFER CONTRACT                            
         ZAP   WORK(5),=P'999999'                                               
         SP    WORK(5),FULL(4)                                                  
         ZAP   FULL,WORK(5)                                                     
         EDIT  (P4,FULL),(5,TSXFRCON)                                           
         OI    TSREC,TSRXFR        XFR INFO                                     
         CLI   OFFLINE,C'Y'                                                     
         BNE   PR94                                                             
         TM    PRSTAT,PRSINV                                                    
         BO    PR90                                                             
         BAS   RE,SORTADD                                                       
         B     *+8                                                              
PR94     BAS   RE,TSARADD                                                       
         CLI   TSARERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PR90                                                             
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT - CONTRACT SUMMARY                                        
***********************************************************************         
         USING TSRECD,R4                                                        
LAST     USING TSRECD,R3                                                        
PR100    DS    0H                                                               
         GOTO1 ACCSYS              SWITCH BACK TO ACC                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,TOTS             CLEAR TOTALS                                 
         LA    R3,TOTSQ                                                         
         ZAP   0(L'TOTS,R1),=P'0'                                               
         LA    R1,L'TOTS(R1)                                                    
         BCT   R3,*-10                                                          
*                                                                               
         LA    R1,HDSPECS          HEAD SPECS AND HEAD HOOK                     
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   RCSUBPRG,0                                                       
         TM    PRSTAT,PRSINV       INVENTORY REPORT ONLY                        
         BNO   PR101                                                            
         MVI   RCSUBPRG,3                                                       
         GOTO1 =A(PRINV),DMCB,(RC),RR=RELO                                      
         B     XIT                                                              
*                                                                               
PR101    XC    LASTSAR,LASTSAR                                                  
         L     R4,AIO3                                                          
         XC    0(TSRECLEN,R4),0(R4)                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTGET                                                       
         B     PR110                                                            
         BAS   RE,TSARHIGH                                                      
         B     PR110                                                            
*                                                                               
PR110NX  L     R4,AIO3                                                          
         MVC   LASTSAR,0(R4)                                                    
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTGET                                                       
         B     PR110                                                            
         BAS   RE,TSARNEXT                                                      
PR110    LA    R3,LASTSAR                                                       
         TM    TSARERR,TSEEOF                                                   
         BO    PR290                                                            
         MVC   CONTRCTR,TSCNTR                                                  
         OC    LASTSAR,LASTSAR                                                  
         BZ    PR111                                                            
         CLC   LAST.TSCNTR,TSCNTR  SAME CONTRACTOR                              
         BE    PR111                                                            
         TM    LAST.TSTYPE,TSTSUM  IF LAST WAS SUMMARY TYPE                     
         BNO   PR110A                                                           
         GOTO1 =A(SUMTOTS),DMCB,(RC),RR=RELO  SUMMARY TOTALS                    
         B     PR110C                                                           
PR110A   GOTO1 =A(DETTOTS),DMCB,(RC),RR=RELO  DETAIL TOTS                       
PR110C   MVI   FORCEHED,C'Y'                                                    
PR111    TM    TSTYPE,TSTDET       DETAIL  REPORT                               
         BO    PR190                                                            
         TM    TSTYPE,TSTSUM       SUMMARY REPORT                               
         BNO   PR110NX                                                          
         MVI   RCSUBPRG,0                                                       
*                                                                               
PR112    TM    TSREC,TSRMED        MEDIA REC INFO                               
         BNO   PR120                                                            
         GOTO1 DATCON,DMCB,(3,TSSTDATE),(11,PR1STDTE)                           
         EDIT  (P6,TSMDLVD),(11,PR1MDLVD),2,ZERO=NOBLANK,MINUS=YES              
         AP    TOTMDLVD,TSMDLVD                                                 
         B     PR110NX                                                          
*                                                                               
PR120    TM    TSREC,TSRACC        ACCOUNT REC INFO                             
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   PR1CON#,TSCON#                                                   
         EDIT  (P6,TSGCI),(11,PR1GCI),2,ZERO=NOBLANK                            
         AP    TOTMGCI,TSGCI                                                    
*                                                                               
         EDIT  (P6,TSNET),(11,PR1NET),2,ZERO=NOBLANK                            
         AP    TOTNPAY,TSNET                                                    
*                                                                               
         EDIT  (P6,TSSDLVD),(11,PR1SDLVD),2,ZERO=NOBLANK,MINUS=YES              
         AP    TOTSDLVD,TSSDLVD                                                 
*                                                                               
         ZAP   WORK2(6),TSGCI                                                   
         OC    LASTSAR,LASTSAR                                                  
         BZ    *+10                                                             
         SP    WORK2(6),LAST.TSMDLVD                                            
         EDIT  (P6,WORK2),(11,PR1MBAL),2,ZERO=NOBLANK,MINUS=YES                 
         AP    TOTMBAL,WORK2(6)                                                 
*                                                                               
         ZAP   WORK2(6),TSNET                                                   
         SP    WORK2(6),TSSDLVD                                                 
         EDIT  (P6,WORK2),(11,PR1SBAL),2,ZERO=NOBLANK,MINUS=YES                 
         AP    TOTSBAL,WORK2(6)                                                 
*                                                                               
         TM    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         BO    PR150                                                            
         EDIT  (P6,TSSCOST),(11,PR1SCOST),2,ZERO=NOBLANK                        
         AP    TOTSCOST,TSSCOST                                                 
*                                                                               
         ZAP   WORK2(6),TSSCOST                                                 
         CP    WORK2(6),=P'0'                                                   
         BE    PR132                                                            
         CP    TSSDLVD,=P'0'                                                    
         BE    PR150                                                            
         ZAP   WORK2(12),TSSCOST        COST% = COST/DLVD                       
         MP    WORK2(12),=P'10000'                                              
         DP    WORK2(12),TSSDLVD                                                
PR132    EDIT  (P6,WORK2),(6,PR1CPCT),2                                         
*                                                                               
PR150    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR110NX                                                          
*                                                                               
PR190    TM    LAST.TSTYPE,TSTSUM  IF LAST WAS SUMMARY TYPE                     
         BNO   PR200                                                            
         GOTO1 =A(SUMTOTS),DMCB,(RC),RR=RELO     PRINT SUMMARY TOTALS           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         DROP  LAST,R4                                                          
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT - CONTRACT DETAIL                                         
***********************************************************************         
*                                                                               
LAST     USING TSRECD,R3                                                        
         USING TSRECD,R4                                                        
PR200    MVI   RCSUBPRG,1                                                       
         LA    R3,LASTSAR          LAST TSAR REC                                
         L     R4,AIO3                                                          
*                                                                               
         TM    TSREC,TSRMED        MEDIA REC INFO                               
         BNO   PR210                                                            
         TM    TSREC,TSRXFR        AND TRANSFER INFO                            
         BO    PR220                                                            
         TM    LAST.TSTYPE,TSTSUM  IF LAST WAS SUMMARY TYPE                     
         BO    PR205               SKIP TOTALS                                  
         GOTO1 =A(DETTOTS),DMCB,(RC),RR=RELO         PRINT TOTS                 
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PR205    GOTO1 DATCON,DMCB,(3,TSSTDATE),(11,PR2DATE)                            
         EDIT  (P6,TSMDLVD),(11,PR2MDLVD),2,ZERO=NOBLANK,MINUS=YES              
         AP    TOTMDLVD,TSMDLVD                                                 
         B     PR110NX                                                          
*                                                                               
PR210    TM    TSREC,TSRACC        ACCOUNT REC INFO                             
         BNO   PR230                                                            
         TM    TSREC,TSRXFR        ANY TRANSFER INFO                            
         BO    PR220                                                            
         MVC   CON#,TSCON#                                                      
         MVC   CONTNAME,TSNAME                                                  
         EDIT  (P6,TSGCI),(11,PR2GCI),2,ZERO=NOBLANK                            
         ZAP   TOTMGCI,TSGCI                                                    
         EDIT  (P6,TSNET),(11,PR2NET),2,ZERO=NOBLANK                            
         ZAP   TOTNPAY,TSNET                                                    
         B     PR222                                                            
*                                                                               
PR220    MVC   PR2CLI(10),=C'TRANSFER -'                                        
         MVC   PR2CLI+10(5),TSXFRCON                                            
         B     PR231                                                            
*                                                                               
PR222    CP    TSSDLVD,=P'0'       ANY BAL B/F                                  
         BZ    PR232                                                            
         MVC   PR2CLI(23),=C'BALANCE BROUGHT FORWARD'                           
         B     PR232               PRINT BAL LIKE TRN INFO                      
*                                                                               
PR230    TM    TSREC,TSRTRN        TRANSACTION INFO                             
         BNO   PR110NX                                                          
         GOTO1 DATCON,DMCB,(1,TSDATE),(11,PR2DATE)                              
         GOTO1 DATCON,DMCB,(2,TSADATE),(11,PR2ADATE)                            
PR231    OC    TSMDLVD,TSMDLVD                                                  
         BZ    PR232                                                            
         EDIT  (P6,TSMDLVD),(11,PR2MDLVD),2,ZERO=NOBLANK,MINUS=YES              
         AP    TOTMDLVD,TSMDLVD                                                 
PR232    OC    TSSDLVD,TSSDLVD                                                  
         BZ    PR234                                                            
         EDIT  (P6,TSSDLVD),(11,PR2SDLVD),2,ZERO=NOBLANK,MINUS=YES              
         AP    TOTSDLVD,TSSDLVD                                                 
PR234    OC    TSSCOST,TSSCOST                                                  
         BZ    PR240                                                            
         TM    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         BO    PR240                                                            
         EDIT  (P6,TSSCOST),(11,PR2SCOST),2,ZERO=NOBLANK                        
         AP    TOTSCOST,TSSCOST                                                 
         ZAP   WORK2(6),TSSCOST                                                 
         CP    WORK2(6),=P'0'                                                   
         BE    PR236                                                            
         CP    TSSDLVD,=P'0'                                                    
         BE    PR240                                                            
         ZAP   WORK2(12),TSSCOST            COST% = COST/DLVD                   
         MP    WORK2(12),=P'10000'                                              
         DP    WORK2(12),TSSDLVD                                                
PR236    EDIT  (P6,WORK2),(6,PR2CPCT),2                                         
*                                                                               
PR240    TM    TSREC,TSRACC        ACCOUNT REC INFO                             
         BO    PR250               THEN DONE                                    
         TM    TSREC,TSRXFR        TRANSFER INFO                                
         BO    PR250               THEN DONE                                    
*                                                                               
         MVC   PR2STINV,TSSTNINV   STATION INVOICE OR MEMO BILL #               
         MVC   PR2CLI,TSCLIENT     CLIENT NAME OR INPUT INV#'S                  
         MVC   PR2PRD,TSPRD        PRODUCT                                      
         OC    TSMOS,TSMOS                                                      
         BZ    PR250                                                            
         MVC   WORK2(L'TSMOS),TSMOS                                             
         MVI   WORK2+L'TSMOS,X'01'                                              
         GOTO1 DATCON,DMCB,(1,WORK2),(9,PR2MOS)                                 
PR250    GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR110NX                                                          
*                                                                               
PR290    OC    LASTSAR,LASTSAR                                                  
         BZ    XIT                                                              
         TM    PRSTAT,PRSSUMM      SUMMARY ONLY                                 
         BNO   PR295                                                            
         GOTO1 =A(SUMTOTS),DMCB,(RC),RR=RELO                                    
         B     PRX                                                              
PR295    GOTO1 =A(DETTOTS),DMCB,(RC),RR=RELO         PRINT TOTS                 
PRX      B     XIT                                                              
         DROP  LAST,R4                                                          
         EJECT                                                                  
***********************************************************************         
*        TSAR INTERFACE                                                         
***********************************************************************         
*                                                                               
         USING TSARD,R3                                                         
TSARINIT NTR1                                                                   
         LA    R3,TSARBLK                                                       
         XC    TSARBLK,TSARBLK                                                  
         MVI   TSACTN,TSAINI                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSPAGL,1            LOW TEMPSTR PAGE NUMBER                      
         MVI   TSPAGN,5            NUMBER OF PAGES                              
         MVI   TSKEYL,TSKEYLEN     KEY LENGTH                                   
         LA    R1,TSRECLEN                                                      
         TM    PRSTAT,PRSSUMM      SUMMARY ONLY                                 
         BNO   *+8                                                              
         LA    R1,TSREC2LN         THEN USE SHORTERREC                          
         STH   R1,TSRECL           RECORD LENGTH                                
         OI    TSINDS,TSIXTTWA     SPECIFIES EXTENDED TWA PAGES                 
         OI    TSINDS,TSIREUSE                                                  
         L     R1,AIO3                                                          
         ST    R1,TSAREC           ADDRESS OF RECORD                            
         XC    TSRNUM,TSRNUM       RECORD NUMBER                                
         B     TSARGO                                                           
*                                                                               
TSARHIGH NTR1                                                                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSARDH       READ HIGH  - BY KEY                          
         XC    TSRNUM,TSRNUM       RECORD NUMBER                                
         B     TSARGO                                                           
TSARGET  NTR1                                                                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAGET       GET - BY NUMBER                              
         B     TSARGO                                                           
TSARNEXT NTR1                                                                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSANXT       GET NEXT - BY NUMBER                         
         B     TSARGO                                                           
TSARADD  NTR1                                                                   
         LA    R3,TSARBLK                                                       
         MVI   TSACTN,TSAADD       ADD                                          
         B     TSARGO                                                           
*                                                                               
TSARGO   GOTO1 VTSAR,TSARD                                                      
         MVC   TSARERR,TSERRS      PASS BACK ERRORS                             
         CLI   TSARERR,0                                                        
         BE    XYES                                                             
         B     XNO                 EXIT WITH ERROR                              
         EJECT                                                                  
***********************************************************************         
*        SORTER INTERFACE                                                       
***********************************************************************         
*                                                                               
SORTINIT NTR1                                                                   
         LA    R0,TSKEYLEN         KEY LENGTH                                   
         EDIT  (R0),(3,SORTCARD+15),FILL=0                                      
         LA    R0,TSRECLEN                                                      
         TM    PRSTAT,PRSSUMM      SUMMARY ONLY                                 
         BNO   *+8                                                              
         LA    R0,TSREC2LN         THEN USE SHORTER REC                         
         LA    RF,RECCARD                                                       
         LA    RF,21(RF)                                                        
         EDIT  (R0),(4,(RF)),FILL=0                                             
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         B     SORTXY                                                           
*                                                                               
SORTGET  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)                                                      
         BZ    SG10                                                             
         L     R0,AIO3                                                          
         LA    R1,TSRECLEN                                                      
         TM    PRSTAT,PRSSUMM      SUMMARY ONLY                                 
         BNO   *+8                                                              
         LA    R1,TSREC2LN         USE SHORT REC                                
         LR    RE,R3                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE REC                                     
         B     SORTXY                                                           
SG10     GOTO1 =V(SORTER),DMCB,=C'END'                                          
         USING TSARD,R3                                                         
         LA    R3,TSARBLK                                                       
         MVI   TSARERR,TSEEOF                                                   
         B     XNO                                                              
*                                                                               
         USING TSARD,R3                                                         
SORTADD  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',AIO3                                     
SORTXY   LA    R3,TSARBLK                                                       
         MVI   TSARERR,0                                                        
         B     XYES                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        HEADSPECS                                                              
***********************************************************************         
*                                                                               
HDSPECS  SPROG 0,1,3,4                                                          
         SSPEC H1,50,C'BARTER INVENTORY/CONTRACT REPORTS'                       
         SSPEC H2,50,C'---------------------------------'                       
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,113,PAGE                                                      
         SSPEC H2,113,REQUESTOR                                                 
         SSPEC H3,113,RUN                                                       
         SSPEC H4,1,C'SYS/MEDIA :'                                              
         SPROG 0,1,3                                                            
         SSPEC H5,1,C'CONTRACTOR:'                                              
*                                                                               
         SPROG 0                                                                
         SSPEC H3,52,C'       CONTRACTOR SUMMARY'                               
         SSPEC H8,22,C'------------- MEDIA --------------'                      
         SSPEC H8,62,C'------------ SERVICES ------------'                      
         SSPEC H9,2,C'CON#'                                                     
         SSPEC H9,9,C'ST DATE'                                                  
         SSPEC H9,22,C'CONTRACT'                                                
         SSPEC H9,34,C'DELIVERED'                                               
         SSPEC H9,48,C'BALANCE'                                                 
         SSPEC H9,62,C'CONTRACT'                                                
         SSPEC H9,74,C'DELIVERED'                                               
         SSPEC H9,88,C'BALANCE'                                                 
*                                                                               
         SPROG 1                                                                
         SSPEC H3,50,C'        CONTRACT HISTORY'                                
         SSPEC H6,1,C'CONTRACT  :'                                              
         SSPEC H8,67,C'-------- MEDIA --------'                                 
         SSPEC H8,91,C'------ SERVICES -------'                                 
         SSPEC H9,4,C'DATE'                                                     
         SSPEC H9,11,C'ACT DATE'                                                
         SSPEC H9,20,C'CLIENT/CATEGORY'                                         
         SSPEC H9,44,C'PRD'                                                     
         SSPEC H9,48,C'INV#/MEMO#'                                              
         SSPEC H9,60,C'MONTH'                                                   
         SSPEC H9,68,C'CONTRACT'                                                
         SSPEC H9,80,C'TIME DLVD'                                               
         SSPEC H9,92,C'CONTRACT'                                                
         SSPEC H9,104,C'SERV DLVD'                                              
*                                                                               
         SPROG 3                                                                
         SSPEC H3,50,C'        INVENTORY REPORT'                                
         SSPEC H8,18,C'------- MEDIA --------'                                  
         SSPEC H8,44,C'------ SERVICES ------'                                  
         SSPEC H9,2,C'CON#'                                                     
         SSPEC H9,8,C'ST DATE'                                                  
         SSPEC H9,19,C'CONTRACT'                                                
         SSPEC H9,30,C'DELIVERED'                                               
         SSPEC H9,45,C'CONTRACT'                                                
         SSPEC H9,56,C'DELIVERED'                                               
*                                                                               
         SPROG 4                                                                
         SSPEC H3,50,C'        INVENTORY REPORT'                                
         SSPEC H4,50,C'          ** TOTALS **'                                  
         SSPEC H8,50,C'  INVENTORY           PAYABLE'                           
         SSPEC H9,50,C'---------------------------------'                       
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADHOOKS                                                              
***********************************************************************         
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+13(L'QSYSMED),QSYSMED                                         
         MVC   H4+20(L'SMNAME),SMNAME                                           
         CLI   RCSUBPRG,0          SUMMARY                                      
         BNE   HDHK10                                                           
         MVC   H5+13(L'CONTRCTR),CONTRCTR                                       
         MVC   H5+20(L'CNTRNAME),CNTRNAME                                       
         TM    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         BO    HDHKX                                                            
         MVC   H8+101(18),=C'------ COST ------'                                
         MVC   H9+102(9),=C'COST DLVD'                                          
         MVC   H9+114(5),=C'COST%'                                              
         B     HDHKX                                                            
*                                                                               
HDHK10   CLI   RCSUBPRG,1          CONTRACT DETAIL                              
         BNE   HDHK20                                                           
         MVC   H5+13(L'CONTRCTR),CONTRCTR                                       
         MVC   H5+20(L'CNTRNAME),CNTRNAME                                       
         MVC   H6+13(L'CON#),CON#                                               
         MVC   H6+20(L'CONTNAME),CONTNAME                                       
         TM    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         BO    HDHKX                                                            
         MVC   H8+114(18),=C'------ COST ------'                                
         MVC   H9+115(9),=C'COST DLVD'                                          
         MVC   H9+127(5),=C'COST%'                                              
         B     HDHKX                                                            
*                                                                               
HDHK20   CLI   RCSUBPRG,3          CONTRACT INVENTORY REPORT                    
         BNE   HDHKX                                                            
         MVC   H5+13(L'CONTRCTR),CONTRCTR                                       
         MVC   H5+20(L'CNTRNAME),CNTRNAME                                       
         MVC   H8+68(26),DOTLINE                                                
         MVC   H8+91(4),=C'COST'                                                
         MVC   H8+95(26),DOTLINE                                                
         MVC   H9+68(5),=C'COST%'                                               
         MVC   H9+80(4),=C'TIME'                                                
         MVC   H9+92(4),=C'DLVD'                                                
         MVC   H9+99(9),=C'INVENTORY'                                           
         MVC   H9+113(7),=C'PAYABLE'                                            
*                                                                               
HDHKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT2,DISTHSPG   REDISPLAY LIST PAGE AFTER SELECT             
*                                                                               
         L     R2,=A(PFTABLE)                                                   
         A     R2,RELO                                                          
         LA    R3,FINPFKYH                                                      
         CLI   ACTEQU,ACTREP       REPORT                                       
         BNE   *+12                                                             
         LA    R3,FIRPFKYH                                                      
         B     SET50                                                            
         CLI   ACTEQU,ACTLIST      LIST                                         
         BNE   SET50                                                            
         LA    R3,FILPFKYH                                                      
         L     R2,=A(LPFTABLE)                                                  
         A     R2,RELO                                                          
*                                                                               
SET50    GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
SUX      B     XIT                                                              
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
ERRENTER MVC   GERROR,=AL2(ACEENTER)   PRESS ENTER TO GEN REPORT                
         B     ACCERRX                                                          
ERINVDTE MVC   GERROR,=AL2(ACEIVDTE)   INVALID DATE                             
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
*                                                                               
ACTREC   EQU   1                   ACCOUNT RECORD                               
TRNREC   EQU   2                   TRANSACTION RECORD                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
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
         DC    CL2'XF',CL8'TRANSFER'                                            
         DC    AL2(TXFNPAY-T61EFFD,TXFGDLVD-T61EFFD,TXFINVPO-T61EFFD)           
         DC    CL2'  ',CL8'TOTALS  '                                            
         DC    AL2(TOTNPAY-T61EFFD,TOTGDLVD-T61EFFD,TOTINVPO-T61EFFD)           
         DC    X'FF'                                                            
*                                                                               
DOTLINE  DC    80C'-'                                                           
DOT2LINE DC    80C'='                                                           
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,XXX,A),FORMAT=CH,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XXXX'                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        MAINTENANCE SCREEN PFKEY TABLE DEFINITIONS                             
***********************************************************************         
PFTABLE  DS    0C                                                               
         DC    AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                   
         DC    CL3' ',CL8'CTAGS   ',CL8'DISPLAY '                               
PF02     DC    AL1(KEYTYTWA,L'FINMED-1),AL2(FINMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FINCONT-1),AL2(FINCONT-T61EFFD)                   
PF02X    EQU   *                                                                
*                                                                               
         DC    AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'LIST    '                               
PF03     DC    AL1(KEYTYTWA,L'FINMED-1),AL2(FINMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FINCNTR-1),AL2(FINCNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'FINCONT-1),AL2(FINCONT-T61EFFD)                   
PF03X    EQU   *                                                                
*                                                                               
         DC    AL1(PF05X-*,05,PFTCPROG,(PF05X-PF05)/KEYLNQ,0)                   
         DC    CL3' ',CL8'ORDER   ',CL8'ADD     '                               
PF05     DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'FINMED-1),AL2(FINMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FINCONT-1),AL2(FINCONT-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'FINCNTR-1),AL2(FINCNTR-T61EFFD)                   
PF05X    EQU   *                                                                
*                                                                               
         DC    AL1(PF09X-*,09,PFTCPROG,(PF09X-PF09)/KEYLNQ,0)                   
         DC    CL3' ',CL8'FIN     ',CL8'BALANCE '                               
PF09     DC    AL1(KEYTYTWA,L'FINMED-1),AL2(FINMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FINCNTR-1),AL2(FINCNTR-T61EFFD)                   
         DC    AL1(KEYTYTWA,L'FINCONT-1),AL2(FINCONT-T61EFFD)                   
PF09X    EQU   *                                                                
*                                                                               
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        LIST SCREEN PFKEY TABLE DEFINITIONS                                    
***********************************************************************         
LPFTABLE DS    0C                                                               
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DC    CL3'CTA',CL8'CTAGS   ',CL8'DISPLAY '                             
LPF02    DC    AL1(KEYTYTWA,L'FILMED-1),AL2(FILMED-T61EFFD)                     
         DC    AL1(KEYTYCUR,L'LSTCON#-1),AL2(LSTCON#-LSTLINED)                  
LPF02X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF04X-*,04,PFTCPROG,(LPF04X-LPF04)/KEYLNQ,0)                
         DC    CL3'DIS',CL8'FIN     ',CL8'DISPLAY '                             
LPF04    DC    AL1(KEYTYTWA,L'FILMED-1),AL2(FILMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FILCNTR-1),AL2(FILCNTR-T61EFFD)                   
         DC    AL1(KEYTYCUR,L'LSTCON#-1),AL2(LSTCON#-LSTLINED)                  
LPF04X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF05X-*,05,PFTCPROG,(LPF05X-LPF05)/KEYLNQ,0)                
         DC    CL3'ORD',CL8'ORDER   ',CL8'ADD     '                             
LPF05    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'FILMED-1),AL2(FILMED-T61EFFD)                     
         DC    AL1(KEYTYCUR,L'LSTCON#-1),AL2(LSTCON#-LSTLINED)                  
         DC    AL1(KEYTYTWA,L'FILCNTR-1),AL2(FILCNTR-T61EFFD)                   
LPF05X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF09X-*,09,PFTCPROG,(LPF09X-LPF09)/KEYLNQ,0)                
         DC    CL3'DIS',CL8'FIN     ',CL8'BALANCE '                             
LPF09    DC    AL1(KEYTYTWA,L'FILMED-1),AL2(FILMED-T61EFFD)                     
         DC    AL1(KEYTYTWA,L'FILCNTR-1),AL2(FILCNTR-T61EFFD)                   
         DC    AL1(KEYTYCUR,L'LSTCON#-1),AL2(LSTCON#-LSTLINED)                  
LPF09X   EQU   *                                                                
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8'        ',CL8'        '                               
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        PRINT SUMMARY TOTALS                                                   
***********************************************************************         
*                                                                               
SUMTOTS  NMOD1 0,**SUMT**                                                       
         L     RC,0(R1)                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   PR1CON#(12),=C'** TOTALS **'                                     
         EDIT  (P6,TOTMGCI),(11,PR1GCI),2,ZERO=NOBLANK                          
         EDIT  (P6,TOTMDLVD),(11,PR1MDLVD),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  (P6,TOTMBAL),(11,PR1MBAL),2,ZERO=NOBLANK,MINUS=YES               
         EDIT  (P6,TOTNPAY),(11,PR1NET),2,ZERO=NOBLANK                          
         EDIT  (P6,TOTSDLVD),(11,PR1SDLVD),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  (P6,TOTSBAL),(11,PR1SBAL),2,ZERO=NOBLANK,MINUS=YES               
         TM    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         BO    ST20                                                             
         EDIT  (P6,TOTSCOST),(11,PR1SCOST),2,ZERO=NOBLANK                       
         ZAP   WORK2(6),TOTSCOST                                                
         CP    WORK2(6),=P'0'                                                   
         BE    ST10                                                             
         CP    TOTSDLVD,=P'0'                                                   
         BE    ST20                                                             
         ZAP   WORK2(12),TOTSCOST       COST% = COST/DLVD                       
         MP    WORK2(12),=P'10000'                                              
         DP    WORK2(12),TOTSDLVD                                               
ST10     EDIT  (P6,WORK2),(6,PR1CPCT),2                                         
ST20     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R1,TOTS             CLEAR TOTALS                                 
         LA    R3,TOTSQ                                                         
         ZAP   0(L'TOTS,R1),=P'0'                                               
         LA    R1,L'TOTS(R1)                                                    
         BCT   R3,*-10                                                          
         B     XIT                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT CONTRACT TOTALS                                                  
***********************************************************************         
*                                                                               
DETTOTS  NMOD1 0,**DETS**                                                       
         L     RC,0(R1)                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PR2MDOTS,DOTLINE     PRINT DOTTED LINE                           
         MVC   PR2SDOTS,DOTLINE                                                 
         TM    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         BO    DT05                                                             
         MVC   PR2CDOTS,DOTLINE                                                 
DT05     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PR2DATE(12),=C'** TOTALS **'                                     
         EDIT  (P6,TOTMGCI),(11,PR2GCI),2,ZERO=NOBLANK                          
         EDIT  (P6,TOTMDLVD),(11,PR2MDLVD),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  (P6,TOTNPAY),(11,PR2NET),2,ZERO=NOBLANK                          
         EDIT  (P6,TOTSDLVD),(11,PR2SDLVD),2,ZERO=NOBLANK,MINUS=YES             
         TM    PRSTAT,PRSXCOST     EXCLUDE COST INFO                            
         BO    DT20                                                             
         EDIT  (P6,TOTSCOST),(11,PR2SCOST),2,ZERO=NOBLANK                       
         ZAP   WORK2(6),TOTSCOST                                                
         CP    WORK2(6),=P'0'                                                   
         BE    DT10                                                             
         CP    TOTSDLVD,=P'0'                                                   
         BE    DT20                                                             
         ZAP   WORK2(12),TOTSCOST       COST% = COST/DLVD                       
         MP    WORK2(12),=P'10000'                                              
         DP    WORK2(12),TOTSDLVD                                               
DT10     EDIT  (P6,WORK2),(6,PR2CPCT),2                                         
DT20     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PR2MDOTS,DOT2LINE    PRINT BALANCES                              
         MVC   PR2SDOTS,DOT2LINE                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PR2DATE(13),=C'** BALANCE **'                                    
         SP    TOTMGCI,TOTMDLVD                                                 
         EDIT  (P6,TOTMGCI),PR2MBAL,2,ZERO=NOBLANK,MINUS=YES                    
         SP    TOTNPAY,TOTSDLVD                                                 
         EDIT  (P6,TOTNPAY),PR2SBAL,2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R1,TOTS             CLEAR TOTALS                                 
         LA    R3,TOTSQ                                                         
         ZAP   0(L'TOTS,R1),=P'0'                                               
         LA    R1,L'TOTS(R1)                                                    
         BCT   R3,*-10                                                          
         B     XIT                                                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS RECORD                                                         
***********************************************************************         
*                                                                               
PROCREC  NMOD1 0,*PROCREC                                                       
         L     RC,0(R1)                                                         
         CLI   7(R1),ACTREC        ACCOUNT RECORD                               
         BE    PROCACC                                                          
         CLI   7(R1),TRNREC        TRANSACTION REC                              
         BE    PROCTRN                                                          
         DC    H'0'                                                             
***********************************************************************         
*        PROCESS ACCOUNT REC TO TSAR REC                                        
***********************************************************************         
*                                                                               
         USING TSRECD,R4                                                        
         USING ACTRECD,R6                                                       
PROCACC  DS    0H                                                               
         L     R4,AIO3             BUILD TSAR REC HERE                          
         L     R6,AIO1             READ REC HERE                                
         GOTO1 GETREC                                                           
*                                                                               
         XC    0(TSRECLEN,R4),0(R4)                                             
         MVC   TSSYSMED,QSYSMED               SYSTEM/MEDIA                      
*        MVC   TSCNTR,CONTRCTR                CONTRACTOR                        
         OI    TSTYPE,TSTSUM                  INFO FOR SUMMARY REPORT           
         OI    TSREC,TSRACC                   FROM ACCOUNT REC                  
         MVC   TSCON#,ACTKACT+L'QSYSMED       CONTRACT #                        
*                                                                               
         ZAP   TSMDLVD,=P'0'       MEDIA DELIVERED                              
         ZAP   TSSDLVD,=P'0'       SERVICES DELIVERED                           
         ZAP   TSSCOST,=P'0'       COST DELIVERED                               
*                                                                               
         USING NAMELD,R6                                                        
         MVI   ELCODE,NAMELQ       X'20' NAME                                   
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   PA10                                                             
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    PA10                                                             
         EX    R1,*+4                                                           
         MVC   TSNAME(0),NAMEREC                                                
*                                                                               
         USING CXFELD,R6                                                        
PA10     MVI   ELCODE,CXFELQ       X'95' TRANSFER ELEM                          
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PA15     BAS   RE,NEXTEL                                                        
         BNE   PA20                                                             
         ZAP   WORK(6),CXFAMNT     AMOUNT OF XFR                                
         TM    CXFSTAT,CXFSPAID    ARE XFR $ PAID                               
         BO    *+10                ADD TO DLVD                                  
         MP    WORK(6),=P'-1'      IF AVAILABLE THEN MINUS FROM DLVD            
         AP    TSSDLVD,WORK(6)                                                  
         B     PA15                                                             
*                                                                               
         USING CNTELD,R6                                                        
PA20     MVI   ELCODE,CNTELQ       X'92' CONTRACT ELEM                          
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TSCNTR,CNTCONTR                CONTRACTOR                        
         MVC   CONTRCTR,CNTCONTR              CONTRACTOR                        
         ZAP   TSGCI,CNTGCI        GCI                                          
         ZAP   TSNET,CNTNPAY       NET PAYABLE                                  
*        ZAP   TSSDLVD,CNTOPNPO    OPEN ORDERS                                  
         TM    PRSTAT,PRSINV                                                    
         BO    *+16                                                             
         AP    TSSDLVD,CNTINVPO    INVOICED ORDERS                              
         ZAP   TSSCOST,CNTCOST     ACTUAL COST OF SERVICES                      
         CLI   CNTLN,CNTLNQ        ANY BAL B/F                                  
         BNH   PA30                                                             
         AP    TSSDLVD,CNTBINV                                                  
         AP    TSSCOST,CNTBCOST                                                 
*                                                                               
PA30     CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTADD                                                       
         B     *+8                                                              
         BAS   RE,TSARADD          ADD FOR SUMMARY REPORT                       
         CLI   TSARERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    PRSTAT,PRSINV       INVENTORY REPORT ONLY                        
         BO    PAX                                                              
         TM    PRSTAT,PRSSUMM      SUMMARY REPORT ONLY                          
         BO    PAX                 THEN DON'T ADD DETAIL ITEM                   
         MVI   TSTYPE,TSTDET       INFO FOR DETAIL REPORT                       
         ZAP   TSSDLVD,=P'0'                                                    
         ZAP   TSSCOST,=P'0'                                                    
         CLI   CNTLN,CNTLNQ        ANY BAL B/F                                  
         BNH   *+16                                                             
         ZAP   TSSDLVD,CNTBINV     USE BAL B/F                                  
         ZAP   TSSCOST,CNTBCOST                                                 
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTADD                                                       
         B     *+8                                                              
         BAS   RE,TSARADD          ADD REC FOR DETAIL REPORT                    
         CLI   TSARERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    TSGCI,TSGCI         GCI                                          
         XC    TSNET,TSNET         NET PAYABLE                                  
         XC    TSSCOST,TSSCOST     ACTUAL COST OF SERVICES                      
         XC    TSMDLVD,TSMDLVD     MEDIA DLVD                                   
         USING CXFELD,R6                                                        
         MVI   ELCODE,CXFELQ       X'95' TRANSFER ELEM                          
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PA40     BAS   RE,NEXTEL                                                        
         BNE   PAX                                                              
         ZAP   WORK(6),CXFAMNT     AMOUNT OF XFR                                
         TM    CXFSTAT,CXFSPAID    ARE XFR $ PAID                               
         BO    *+10                ADD TO DLVD                                  
         MP    WORK(6),=P'-1'      IF AVAILABLE THEN MINUS FROM DLVD            
         ZAP   TSSDLVD,WORK(6)                                                  
         MVC   TSXFRCON,CXFCON     CONTRACT NUMBER                              
         OI    TSREC,TSRXFR        XFR INFO                                     
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTADD                                                       
         B     *+8                                                              
         BAS   RE,TSARADD          ADD REC FOR DETAIL REPORT                    
         CLI   TSARERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PA40                                                             
*                                                                               
PAX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        PROCESS TRANSACTION REC TO TSAR REC                                    
***********************************************************************         
*                                                                               
         USING TSRECD,R4                                                        
         USING TRNRECD,R6                                                       
PROCTRN  DS    0H                                                               
         TM    PRSTAT,PRSSUMM      SUMMARY REPORT ONLY                          
         BO    PTX                 THEN DON'T ADD DETAIL ITEM                   
         L     R4,AIO3             BUILD TSAR REC HERE                          
         L     R6,AIO1             READ REC HERE                                
         GOTO1 GETREC                                                           
*                                                                               
         XC    0(TSRECLEN,R4),0(R4)                                             
         MVC   TSSYSMED,QSYSMED               SYSTEM/MEDIA                      
         MVC   TSCNTR,CONTRCTR                CONTRACTOR                        
         MVC   TSCON#,TRNKACT+L'QSYSMED       CONTRACT #                        
*                                                                               
         TM    PRSTAT,PRSINV                  INVENTORY REPORT?                 
         BZ    PT05                                                             
         OI    TSTYPE,TSTSUM                                                    
         OI    TSREC,TSRMED                                                     
         ZAP   TSMDLVD,=P'0'                                                    
         ZAP   TSSDLVD,=P'0'                                                    
         ZAP   TSSCOST,=P'0'                                                    
         B     PT10                                                             
*                                                                               
PT05     DS    0H                                                               
         OI    TSTYPE,TSTDET                  INFO FOR DETAIL                   
         OI    TSREC,TSRTRN                   FROM TRANSACTION                  
*                                                                               
         USING TRSELD,R5                                                        
PT10     MVI   ELCODE,TRSELQ       X'60' TRANSACTION STATUS EL                  
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R6                                                            
*                                                                               
         TM    PRSTAT,PRSINV                                                    
         BZ    *+14                DONT TEST ENDDATE                            
         CLC   TRSDATE,ENDDATE     TRANS ACTIVE AFTER ENDDATE?                  
         BH    PTX                 YES, SKIP IT                                 
         MVC   TSADATE,TRSDATE                                                  
         ZAP   PACKNO,=P'0'                                                     
         ZAP   PACKNO2,=P'0'                                                    
*                                                                               
         USING TRNELD,R6                                                        
PT11     MVI   ELCODE,TRNELQ       X'44' TRANSACTION EL                         
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TSIREF,TRNREF                                                    
         MVC   TSISUB,TRNSUB                                                    
         MVC   TSIDATE,TRNDATE                                                  
*                                                                               
         TM    PRSTAT,PRSINV                                                    
         BZ    PT13                                                             
         TM    TRNSTAT,TRNSDR                                                   
         BZ    PT12                                                             
         ZAP   TSMDLVD,TRNAMNT     MEDIA DELIVERED                              
         B     PT30                                                             
PT12     ZAP   TSSDLVD,TRNAMNT     SERVICES DELIVERED                           
         B     PT30                                                             
*                                                                               
PT13     MVC   TSDATE,TRNDATE      DATE                                         
         MVC   TSMOS,TRNDATE       MONTH OF SERVICE                             
         MVC   TSREF,TRNREF        REFERENCE                                    
         MVC   TSSUB,TRNSUB        SUBREFERENCE                                 
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBIT= MEDIA DLVD                            
         BNO   PT20                                                             
PT15     ZAP   TSMDLVD,TRNAMNT     MEDIA DLVD                                   
         MVC   TSCLIENT(3),TRNREF  CLIENT CODE                                  
         MVI   TSCLIENT+3,C'-'                                                  
         MVC   TSPRD,TRNREF+3      PRODUCT CODE                                 
*                                                                               
         USING XPYELD,R6                                                        
         MVI   ELCODE,XPYELQ       X'46' EXTRA PAYMENT ELEM                     
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   PT25                                                             
         MVC   TSCLIENT+4(L'XPYCLI),XPYCLI    CLIENT NAME                       
         MVC   TSSTNINV,XPYINV                STATION INVOICE NUM               
         B     PT25                                                             
*                                                                               
         USING TRNELD,R6                                                        
PT20     ZAP   TSSDLVD,TRNAMNT     CREDIT = SERVICES DLVD                       
         OI    TSSORT,TSSSLAST     SORT = SERVICES LAST                         
         MVC   TSMEMO,TRNREF       REFERENCE = MEMO BILL NUMBER                 
         XC    TSMOS,TSMOS         NO MONTH OF SERVICE                          
*                                                                               
PT25     DS    0H                                                               
PT29     MVC   TSADATE,TRSDATE                                                  
*                                                                               
         USING SCIELD,R6                                                        
PT30     MVI   ELCODE,SCIELQ       X'50' SUBSIDIARY CASH INFO ELEM              
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PT30NX   BAS   RE,NEXTEL                                                        
         BNE   PT100                                                            
         CLI   SCITYPE,SCITSJXP    AMOUNT POSTED TO EXPENSES (COST)             
         BE    PT35                                                             
         CLI   SCITYPE,SCITTTAX    AMOUNT POSTED TO TAX (COST)                  
         BNE   PT30NX                                                           
         AP    TSSCOST,SCIAMNT                                                  
         B     PT30NX                                                           
*                                                                               
PT35     ZAP   TSSCOST,SCIAMNT                                                  
         TM    PRSTAT,PRSINV                                                    
         BO    PT100                                                            
         CLI   SCILN,SCILN2Q                                                    
         BNH   PT30NX                                                           
         USING CATTABD,R5                                                       
         LA    R5,CATTABLE                                                      
PT36     CLC   SCISUBWC,CATWC      MATCH ON CATEGORY WORK CODE                  
         BNE   PT40                                                             
         MVC   TSCTGRY(L'CATWORD),CATWORD     CATEGORY                          
         CLC   TSCTGRY(4),=C'MDSE'                                              
         BNE   *+10                                                             
         MVC   TSCTGRY(11),=C'MERCHANDISE'                                      
         B     PT30NX                                                           
PT40     LA    R5,CATLENQ(R5)      NEXT CATEGORY                                
         CLI   0(R5),X'FF'                                                      
         BNE   PT36                                                             
         B     PT30NX                                                           
         DROP  R5                                                               
*                                                                               
PT100    CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTADD                                                       
         B     *+8                                                              
         BAS   RE,TSARADD                                                       
         CLI   TSARERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PTX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT - INVENTORY REPORT                                        
***********************************************************************         
         USING TSRECD,R4                                                        
LAST     USING TSRECD,R3                                                        
*                                                                               
PRINV    NMOD1 0,**PRINV*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         ZAP   PACKNO,=P'0'                                                     
         ZAP   PACKNO2,=P'0'                                                    
         ZAP   PACKNO3,=P'0'                                                    
         L     R4,AIO3                                                          
         XC    0(TSRECLEN,R4),0(R4)                                             
         XC    LASTSAR,LASTSAR                                                  
         MVI   PRTFRST,C'Y'                                                     
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTGET                                                       
         B     *+8                                                              
         BAS   RE,TSARHIGH                                                      
         B     PI10                                                             
*                                                                               
PI10NX   L     R4,AIO3                                                          
         MVC   LASTSAR,0(R4)                                                    
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         BAS   RE,SORTGET                                                       
         B     *+8                                                              
         BAS   RE,TSARNEXT                                                      
PI10     LA    R3,LASTSAR                                                       
         TM    TSARERR,TSEEOF                                                   
         BO    PI200                                                            
*                                                                               
         MVC   CONTRCTR,TSCNTR                                                  
         GOTO1 =A(ACTVCNTR),CONTRCTR,RR=RELO    ACTIVE CONTRACTOR?              
         BNE   PI10NX                    NO, GET NEXT                           
         OC    LASTSAR,LASTSAR                                                  
         BZ    PI14                                                             
         CLI   PRTFRST,C'Y'                                                     
         BE    PI14                                                             
         CLC   LAST.TSCNTR,TSCNTR                                               
         BE    PI14                                                             
         BAS   RE,INVTOTS          INVENTORY TOTALS                             
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PI14     MVI   PRTFRST,C'N'                                                     
         TM    TSREC,TSRMED        MEDIA REC INFO                               
         BZ    PI20                                                             
         GOTO1 DATCON,DMCB,(3,TSSTDATE),(11,PR3STDTE)                           
         EDIT  (P6,TSMDLVD),(11,PR3MDLVD),2,ZERO=NOBLANK,MINUS=YES              
         AP    PACKNO,TSMDLVD                                                   
         AP    TOTMDLVD,TSMDLVD                                                 
         OC    TSSDLVD,TSSDLVD                                                  
         BNZ   *+10                                                             
         ZAP   TSSDLVD,=P'0'                                                    
         AP    PACKNO2,TSSDLVD                                                  
         AP    TOTSDLVD,TSSDLVD                                                 
         OC    TSSCOST,TSSCOST                                                  
         BNZ   *+10                                                             
         ZAP   TSSCOST,=P'0'                                                    
         AP    PACKNO3,TSSCOST                                                  
         AP    TOTSCOST,TSSCOST                                                 
         B     PI10NX                                                           
*                                                                               
PI20     TM    TSREC,TSRACC        ACCOUNT REC INFO                             
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   PR3CON#,TSCON#                                                   
         EDIT  (P6,TSGCI),(11,PR3GCI),2,ZERO=NOBLANK                            
         AP    TOTMGCI,TSGCI                                                    
         EDIT  PACKNO,(11,PR3MDLVD),2,ZERO=NOBLANK,MINUS=YES                    
         ZAP   LAST.TSMDLVD,PACKNO                                              
         ZAP   PACKNO,=P'0'                                                     
*                                                                               
         EDIT  (P6,TSNET),(11,PR3NET),2,ZERO=NOBLANK                            
         AP    TOTNPAY,TSNET                                                    
*                                                                               
         AP    PACKNO2,TSSDLVD     BAL/F FOR SERVICES DELIVERED                 
         AP    TOTSDLVD,TSSDLVD                                                 
         EDIT  PACKNO2,(11,PR3SDLVD),2,ZERO=NOBLANK,MINUS=YES                   
         ZAP   LAST.TSSDLVD,PACKNO2                                             
         ZAP   PACKNO2,=P'0'                                                    
*                                                                               
         ZAP   WORK2(6),TSGCI                                                   
         SP    WORK2(6),LAST.TSMDLVD                                            
         AP    TOTMBAL,WORK2(6)                                                 
*                                                                               
         ZAP   WORK2(6),TSNET                                                   
         SP    WORK2(6),LAST.TSSDLVD                                            
         AP    TOTSBAL,WORK2(6)                                                 
*                                                                               
         AP    PACKNO3,TSSCOST                                                  
         AP    TOTSCOST,TSSCOST                                                 
         EDIT  PACKNO3,(11,PR3SCOST),2,ZERO=NOBLANK                             
         ZAP   LAST.TSSCOST,PACKNO3                                             
         ZAP   PACKNO3,=P'0'                                                    
*                                                                               
         ZAP   WORK2(6),LAST.TSSCOST                                            
         CP    WORK2(6),=P'0'                                                   
         BE    PI30                                                             
         CP    LAST.TSSDLVD,=P'0'                                               
         BE    PI50                                                             
         ZAP   WORK2(12),LAST.TSSCOST    COST% = COST/DLVD                      
         MP    WORK2(12),=P'10000'                                              
         DP    WORK2(12),LAST.TSSDLVD                                           
PI30     EDIT  (P6,WORK2),(6,PR3CPCT),2                                         
*                                                                               
         ZAP   WORK3(15),TSNET           COST OF TIME DLVD = NET                
         MP    WORK3(15),WORK2(6)        *COST%                                 
         MP    WORK3(15),LAST.TSMDLVD    *MEDIA DLVD                            
         DP    WORK3(15),TSGCI           /GCI                                   
         DP    WORK3(9),=P'10000'                                               
         EDIT  (P6,WORK3),(11,PR3SCTIM),2                                       
         AP    TOTTCOST,WORK3(6)       ADD TO TOTAL COST OF TIME                
         ZAP   WORK2(6),LAST.TSSCOST   COST OF SERV - COST OF TIME              
         SP    WORK2(6),WORK3(6)                                                
         BM    PI40                                                             
         EDIT  (P6,WORK2),(11,PR3SINV),2      POSITIVE=INVENTORY                
         AP    TOTINV,WORK2(6)                ADD TO TOTAL INVENTORY            
         B     PI45                                                             
PI40     MP    WORK2(6),=P'-1'                                                  
         EDIT  (P6,WORK2),(11,PR3SPAY),2      NEGATIVE=PAYABLE                  
         AP    TOTPAY,WORK2(6)                ADD TO TOTAL PAYABLE              
*                                                                               
PI45     CP    LAST.TSSDLVD,=P'0'       NO SERVICES DELIVERED                   
         BNE   PI50                                                             
         CP    LAST.TSMDLVD,=P'0'       BUT MEDIA DELIVERED                     
         BE    PI50                                                             
         ZAP   WORK2(6),LAST.TSMDLVD                                            
         CLI   QMED,C'R'                                                        
         BNE   *+14                                                             
         MP    WORK2(6),=P'40'     RADIO MEDIA (40%)                            
         B     *+10                                                             
         MP    WORK2(6),=P'50'     TELEVISION MEDIA (50%)                       
         SRP   WORK2(6),62,5                                                    
         EDIT  (P6,WORK2),(11,PR3SPAY),2                                        
         AP    TOTPAY,WORK2(6)                                                  
*                                                                               
PI50     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PI10NX                                                           
*                                                                               
PI200    DS    0H                                                               
         BAS   RE,INVTOTS          INVENTORY TOTALS                             
         BAS   RE,REPTOTS          REPORT TOTALS                                
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
*                                                                               
         DROP  LAST,R4                                                          
         EJECT                                                                  
***********************************************************************         
*        ACTIVE CONTRACTOR, ACTIVITY FOR 1 YEAR                                 
***********************************************************************         
*                                                                               
ACTVCNTR NTR1                                                                   
         MVC   TMPCNTR,0(R1)       COPY CONTRACTOR                              
         USING CHDRECD,R6          LOOK FOR CONTRA ACCOUNT HEADER RECS          
         LA    R6,BIGKEY           TO GET ALL CONTRACTS FOR CONTRACTOR          
         MVC   BIGKEY,SPACES                                                    
         MVC   CHDKCPY,CMPY                                                     
         MVI   CHDKUNT,C'S'                                                     
         MVI   CHDKLDG,C'1'                                                     
         MVC   CHDKACT(L'QSYSMED),QSYSMED               SYSTEM/MEDIA            
         MVC   CHDKACT+L'QSYSMED(L'CONTRCTR),TMPCNTR    CONTRACTOR              
         MVC   SAVEKEY,BIGKEY                                                   
         GOTO1 HIGH                                                             
         CLC   BIGKEY(CHDKEND),SAVEKEY                                          
         BNE   ACNTRXNE            NOT FOUND, CONSIDER INACTIVE                 
         L     R6,AIO1                                                          
         GOTO1 GETREC                                                           
         LA    R6,CHDRFST          POINT TO FIRST ELEMENT                       
         SR    R1,R1                                                            
         SR    R4,R4                                                            
         SR    R5,R5                                                            
*                                                                               
ACNTR10  CLI   0(R6),0             EOT                                          
         BE    ACNTR30                                                          
         CLI   0(R6),RSTELQ        STATUS ELEMENT                               
         BNE   *+10                                                             
         LR    R4,R6                                                            
         B     ACNTR19                                                          
         CLI   0(R6),ABLELQ        ACCOUNT BALANCE ELEMENT                      
         BNE   ACNTR19                                                          
         LR    R5,R6                                                            
ACNTR19  IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     ACNTR10                                                          
*                                                                               
         USING RSTELD,R4                                                        
         USING ABLELD,R5                                                        
ACNTR30  LTR   R4,R4               MUST HAVE STATUS ELEMENT                     
         BZ    ACNTRXNE                                                         
         LTR   R5,R5               AND ACCOUNT BALANCE ELEMENT                  
         BZ    ACNTRXNE                                                         
         CLC   RSTTDATE,ENDPRIOR   LAST ACTIVITY AFTER 1 YEAR AGO?              
         BNL   ACNTR40                                                          
         TM    PRSTAT2,PRSIATV     INACTIVE REPORTING?                          
         BZ    ACNTRXNE                                                         
         CLC   RSTTDATE,TWOPRIOR   MUST BE ACTIVE 2 YEARS AGO                   
         BL    ACNTRXNE                                                         
         B     ACNTR50                                                          
*                                                                               
ACNTR40  TM    PRSTAT2,PRSIATV     FOR INACTIVE REPORTING, NO                   
         BO    ACNTRXNE                                                         
ACNTR50  CP    ABLFRWD,=P'0'       IF ALL 3 BUCKET=0                            
         BNE   ACNTRXEQ            CONSIDER IT INACTIVE                         
         CP    ABLDR,=P'0'                                                      
         BNE   ACNTRXEQ                                                         
         CP    ABLCR,=P'0'                                                      
         BNE   ACNTRXEQ                                                         
*                                                                               
ACNTRXNE LTR   RB,RB                                                            
         B     XIT                                                              
ACNTRXEQ CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        PRINT INVENTORY TOTALS                                                 
***********************************************************************         
*                                                                               
INVTOTS  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   PR3CON#(12),=C'** TOTALS **'                                     
         EDIT  (P6,TOTMGCI),(11,PR3GCI),2,ZERO=NOBLANK                          
         EDIT  (P6,TOTMDLVD),(11,PR3MDLVD),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  (P6,TOTNPAY),(11,PR3NET),2,ZERO=NOBLANK                          
         EDIT  (P6,TOTSDLVD),(11,PR3SDLVD),2,ZERO=NOBLANK,MINUS=YES             
         EDIT  (P6,TOTSCOST),(11,PR3SCOST),2,ZERO=NOBLANK                       
         ZAP   WORK2(6),TOTSCOST                                                
         CP    WORK2(6),=P'0'                                                   
         BE    IT10                                                             
         CP    TOTSDLVD,=P'0'                                                   
         BE    IT20                                                             
         ZAP   WORK2(12),TOTSCOST       COST% = COST/DLVD                       
         MP    WORK2(12),=P'10000'                                              
         DP    WORK2(12),TOTSDLVD                                               
IT10     EDIT  (P6,WORK2),(6,PR3CPCT),2                                         
         EDIT  (P6,TOTTCOST),(11,PR3SCTIM),2,ZERO=NOBLANK                       
         EDIT  (P6,TOTINV),(11,PR3SINV),2,ZERO=NOBLANK                          
         EDIT  (P6,TOTPAY),(11,PR3SPAY),2,ZERO=NOBLANK                          
IT20     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         AP    RTOTINV,TOTINV      REPORT TOTALS                                
         AP    RTOTPAY,TOTPAY                                                   
*                                                                               
         LA    R1,TOTS             CLEAR TOTALS                                 
         LA    R3,TOTSQ                                                         
         ZAP   0(L'TOTS,R1),=P'0'                                               
         LA    R1,L'TOTS(R1)                                                    
         BCT   R3,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PRINT INVENTORY REPORT TOTALS                                          
***********************************************************************         
*                                                                               
REPTOTS  NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,4                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   PR4TOTS(19),=C'** REPORT TOTALS **'                              
         EDIT  (P6,RTOTINV),(12,PR4SINV),2,ZERO=NOBLANK                         
         EDIT  (P6,RTOTPAY),(12,PR4SPAY),2,ZERO=NOBLANK                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PR4SINV-1(33),=C'---------------------------------'              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    RTOTINV,RTOTPAY                                                  
         BM    RT10                                                             
         EDIT  (P6,RTOTINV),(12,PR4SINV),2,ZERO=NOBLANK                         
         B     RTX                                                              
RT10     MP    RTOTINV,=P'-1'                                                   
         EDIT  (P6,RTOTINV),(12,PR4SPAY),2,ZERO=NOBLANK                         
RTX      GOTO1 SPOOL,DMCB,(R8)                                                  
         ZAP   RTOTPAY,=P'0'                                                    
         ZAP   RTOTINV,=P'0'                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        LISTRECS                                                     *         
***********************************************************************         
*                                                                               
LRECS    NMOD1 0,**LRECS*                                                       
         L     RC,0(R1)                                                         
         OI    GLSTSTAT,RETEXTRA   RETURN AN EXTRA TIME FOR TOTALS              
         OI    GENSTAT2,DISTHSPG   REDISPLAY SAME PAGE AFTER SEL                
         MVI   NLISTS,13                                                        
*                                                                               
         USING LSTLINED,R2                                                      
         LA    R2,LISTAR                                                        
         OC    CONTLIST,CONTLIST   ANY CONTRACTS                                
         BZ    LR100                                                            
*                                                                               
         LA    R3,CONTLIST                                                      
         ZICM  R1,CONTDISP,2       DISP TO LAST CONTRACT NUMBER                 
         AR    R3,R1                                                            
         B     *+8                                                              
LRSEQ    LA    R3,L'CON#(R3)       NEXT CONTRACT                                
         CLI   0(R3),X'FF'         ANY MORE                                     
         BNE   LR10                                                             
         XC    CONTDISP,CONTDISP   START FROM TOP NEXT                          
         B     LR100                                                            
*                                                                               
LR10     LA    R1,CONTLIST                                                      
         LR    R0,R3               R3 = NUMBER BEING DISPLAYED                  
         SR    R0,R1                                                            
         STCM  R0,3,CONTDISP       SAVE DISP TO NUMBER                          
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,BIGKEY                                                        
         MVC   BIGKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY                                                     
         MVI   ACTKUNT,C'S'                                                     
         MVI   ACTKLDG,C'2'                                                     
         MVC   ACTKACT(L'QSYSMED),QSYSMED                                       
         MVC   ACTKACT+L'QSYSMED(L'CON#),0(R3)                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    LRGET                                                            
         DC    H'0'                                                             
*                                                                               
LRGET    GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   LSTCON#,ACTKACT+L'QSYSMED     CONTRACT                           
         MVC   LSTHYPH(L'LSTHYPH+L'LSTSTAT),SPACES                              
         TM    ACTRSTAT,ACTSLOCK                                                
         BNO   *+14                                                             
         MVI   LSTHYPH,C'-'                                                     
         MVC   LSTSTAT,=C'LK'                                                   
         TM    ACTRSTAT,ACTSCLOS                                                
         BNO   *+14                                                             
         MVI   LSTHYPH,C'-'                                                     
         MVC   LSTSTAT,=C'CL'                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LISTRECS - LIST FROM CATEGORY ELEM                           *         
***********************************************************************         
*                                                                               
         OC    CATFILT,CATFILT     IS THERE A CATEGORY FILTER                   
         BZ    LR50                NO, THEN USE SUMMARY ELEM                    
*                                                                               
         USING CTGELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93' CONTRACT CATEGORY ELEM                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR30NX   BAS   RE,NEXTEL                                                        
         BNE   LRSEQ                                                            
         CLC   CATFILT,CTGCTGY     MATCH ON CATEGORY                            
         BNE   LR30NX                                                           
         EDIT  (P6,CTGNPAY),(11,LSTNPAY),2,MINUS=YES NET PAYABLE                
*                                                                               
         ZAP   WORK2(10),CTGOPNPO                OPEN PO'S                      
         AP    WORK2(10),CTGINVPO                INVOICED PO'S                  
         CLI   CTGLN,CTGLNQ                      ANY BAL B/F                    
         BNH   *+10                                                             
         AP    WORK2(10),CTGBINV                                                
         EDIT  (P10,WORK2),(11,LSTGDLVD),2,MINUS=YES VALUE PO'D                 
*                                                                               
         MP    WORK2(10),=P'10000'                                              
         DP    WORK2(10),CTGNPAY                 PO'D/NET PAY =                 
         EDIT  (P4,WORK2),(7,LSTPDLVD),2,MINUS=YES % PO'D                       
*                                                                               
         ZAP   WORK2(10),CTGNPAY                 BALANCE = NET PAY              
         SP    WORK2(10),CTGOPNPO                -OPEN PO'S                     
         SP    WORK2(10),CTGINVPO                -INVOICED PO'S                 
         CLI   CTGLN,CTGLNQ                      ANY BAL B/F                    
         BNH   *+10                                                             
         SP    WORK2(10),CTGBINV                                                
         EDIT  (P10,WORK2),(11,LSTBAL),2,MINUS=YES                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LISTRECS - LIST FROM SUMMARY ELEM                            *         
***********************************************************************         
*                                                                               
         USING CXFELD,R6                                                        
LR50     ZAP   WORK2(10),=P'0'                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,CXFELQ       X'95' TRANSFER ELEM                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR50NX   BAS   RE,NEXTEL                                                        
         BNE   LR60                                                             
         TM    CXFSTAT,CXFSPAID    TRANSFER OF PAID $                           
         BNO   *+10                                                             
         AP    WORK2(10),CXFAMNT    ADD TO DLVD VALUE                           
         TM    CXFSTAT,CXFSAVL     TRANSFER OF AVAILABLE $                      
         BNO   *+10                                                             
         SP    WORK2(10),CXFAMNT    MINUS TO DLVD VALUE                         
         B     LR50NX                                                           
*                                                                               
         USING CNTELD,R6                                                        
LR60     L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92' CONTRACT SUMMARY ELEM                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    CATFILT,CATFILT     IS THERE A CATEGORY FILTER                   
         BNZ   LR80                YES, THEN JUST NEED DATE                     
*                                                                               
         EDIT  (P6,CNTNPAY),(11,LSTNPAY),2,MINUS=YES NET PAYABLE                
         AP    WORK2(10),CNTOPNPO                OPEN PO'S                      
         AP    WORK2(10),CNTINVPO                INVOICED PO'S                  
         CLI   CNTLN,CNTLNQ                      ANY BAL B/F                    
         BNH   *+10                                                             
         AP    WORK2(10),CNTBINV                                                
         EDIT  (P10,WORK2),(11,LSTGDLVD),2,MINUS=YES VALUE PO'D                 
         ZAP   WORK3(10),WORK2(10) SAVE VALUE DLVD                              
*                                                                               
         MP    WORK2(10),=P'10000'                                              
         CP    CNTNPAY,=P'0'                                                    
         BNE   *+14                                                             
         ZAP   WORK2(4),=P'0'                                                   
         B     *+10                                                             
         DP    WORK2(10),CNTNPAY                                                
         EDIT  (P4,WORK2),(7,LSTPDLVD),2,MINUS=YES % PO'D                       
*                                                                               
         ZAP   WORK2(10),CNTNPAY                 BALANCE = NET PAY              
         SP    WORK2(10),WORK3(10)               -VALUE DLVD                    
         EDIT  (P10,WORK2),(11,LSTBAL),2,MINUS=YES                              
*                                                                               
LR80     MVC   LSTLMEMO,SPACES                                                  
         OC    CNTDELDT,CNTDELDT                 DATE OF LAST MEMO              
         BZ    LR90                                                             
         GOTO1 DATCON,DMCB,(1,CNTDELDT),(10,LSTLMEMO)                           
LR90     GOTO1 LISTMON                                                          
         BE    LRSEQ                                                            
         EJECT                                                                  
***********************************************************************         
*        LISTRECS - CALCULATE TOTALS                                  *         
***********************************************************************         
*                                                                               
LR100    DS    0H                                                               
         LA    R2,FILTOTL                                                       
         MVC   FILTOTL,SPACES                                                   
         OI    FILTOTLH+6,X'80'                                                 
*                                                                               
         LA    R1,TOTS             CLEAR TOTALS                                 
         LA    R3,TOTSQ                                                         
         ZAP   0(L'TOTS,R1),=P'0'                                               
         LA    R1,L'TOTS(R1)                                                    
         BCT   R3,*-10                                                          
*                                                                               
         OC    CONTLIST,CONTLIST   ANY CONTRACTS                                
         BZ    LR190                                                            
         LA    R3,CONTLIST                                                      
         B     *+8                 CHECK FOR EMPTY LIST                         
LR100SEQ LA    R3,L'CON#(R3)       NEXT CONTRACT                                
         CLI   0(R3),X'FF'         ANY MORE                                     
         BE    LR190                                                            
*                                                                               
         USING ACTRECD,R6                                                       
LR100HI  LA    R6,BIGKEY                                                        
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
         OC    CATFILT,CATFILT     IS THERE A CATEGORY FILTER                   
         BZ    LR150               NO, THEN USE SUMMARY ELEM                    
*                                                                               
         USING CTGELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CTGELQ       X'93' CONTRACT CATEGORY ELEM                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR130NX  BAS   RE,NEXTEL                                                        
         BNE   LR100SEQ                                                         
         CLC   CATFILT,CTGCTGY     MATCH ON CATEGORY                            
         BNE   LR130NX                                                          
*                                                                               
         AP    TOTNPAY,CTGNPAY     NET PAYABLE                                  
         AP    TOTOPNPO,CTGOPNPO   OPEN PO'S                                    
         AP    TOTINVPO,CTGINVPO   INVOICED PO'S                                
         AP    TOTGDLVD,CTGOPNPO                                                
         AP    TOTGDLVD,CTGINVPO   GOODS DELIVERED                              
         CLI   CTGLN,CTGLNQ        ANY BAL B/F                                  
         BNH   *+16                                                             
         AP    TOTINVPO,CTGBINV    INVOICED PO'S                                
         AP    TOTGDLVD,CTGBINV    GOODS DELIVERED                              
         B     LR100SEQ                                                         
         DROP  R6                                                               
*                                                                               
         USING CNTELD,R6                                                        
LR150    L     R6,AIO                                                           
         MVI   ELCODE,CNTELQ       X'92' CONTRACT SUMMARY ELEM                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AP    TOTNPAY,CNTNPAY     NET PAYABLE                                  
         AP    TOTOPNPO,CNTOPNPO   OPEN PO'S                                    
         AP    TOTINVPO,CNTINVPO   INVOICED PO'S                                
         AP    TOTGDLVD,CNTOPNPO                                                
         AP    TOTGDLVD,CNTINVPO   GOODS DELIVERED                              
         CLI   CNTLN,CNTLNQ        ANY BAL B/F                                  
         BNH   *+16                                                             
         AP    TOTINVPO,CNTBINV    INVOICED PO'S                                
         AP    TOTGDLVD,CNTBINV    GOODS DELIVERED                              
         B     LR100SEQ                                                         
         DROP  R6                                                               
*                                                                               
LR190    MVC   LSTCON#,=C'TOTALS'                                               
         EDIT  (P6,TOTNPAY),(11,LSTNPAY),2,MINUS=YES                            
         EDIT  (P6,TOTGDLVD),(11,LSTGDLVD),2,MINUS=YES                          
         ZAP   WORK2(10),TOTGDLVD                                               
         CP    WORK2(10),=P'0'                                                  
         BNE   LR192                                                            
         ZAP   WORK2(4),TOTGDLVD                                                
         B     LR194                                                            
LR192    MP    WORK2(10),=P'10000'                                              
         DP    WORK2(10),TOTNPAY                 PO'D/NET PAY =                 
LR194    EDIT  (P4,WORK2),(7,LSTPDLVD),2,MINUS=YES % PO'D                       
         ZAP   WORK2(10),TOTNPAY                 BALANCE = NET PAY              
         SP    WORK2(10),TOTGDLVD                -GOODS DLVD                    
         EDIT  (P10,WORK2),(11,LSTBAL),2,MINUS=YES                              
LRX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
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
       ++INCLUDE DDPERVALD                                                      
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
       ++INCLUDE ACCTAF7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCTAF4D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCTAF3D                                                       
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
PACKNO   DS    PL8                                                              
PACKNO2  DS    PL8                                                              
PACKNO3  DS    PL8                                                              
*                                                                               
SVCTRCTR DS    CL(L'CONTRCTR)      SAVED CONTRACTOR                             
TMPCNTR  DS    CL(L'CONTRCTR)      TEMP CONTRACTOR                              
ACCNT    DS    CL12                TEMP ACCOUNT FIELD                           
SMNAME   DS    CL26                SYSTEM/MEDIA NAME                            
*                                                                               
PRSTAT   DS    XL1                 PRINTING STATS                               
PRSCLSD  EQU   X'80'               INCLUDE CLOSED (FULFILLED) CONTRACTS         
PRSOCLS  EQU   X'40'               INCLUDE ONLY CLOSED CONTRACTS                
PRSXCOST EQU   X'20'               EXCLUDE COST INFO                            
PRSSDATE EQU   X'10'               SORT DETAILS BY DATE NOT MEDIA/SERV          
PRSINV   EQU   X'08'               INVENTORY REPORT ONLY                        
PRSALLCN EQU   X'04'               ALL CONTRACTORS                              
PRSSUMM  EQU   X'02'               SUMMARY REPORTS ONLY                         
PRSFIRST EQU   X'01'               FIRST TIME IN - DON'T GENERATE REP           
*                                                                               
PRSTAT2  DS    XL1                 PRINTING STATS                               
PRSIATV  EQU   X'80'               INACTIVE REPORT ONLY                         
*                                                                               
CATFILT  DS    CL2                 CATEGORY FILTER                              
TSARERR  DS    XL1                 TSAR ERROR RETURN                            
*                                                                               
RTOTINV  DS    PL6                 REPORT INVENTORY TOTAL                       
RTOTPAY  DS    PL6                 REPORT PAYABLE TOTAL                         
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
*                                                                               
TXFSOT   DS    0PL6                TRANSFER TOTALS                              
TXFNPAY  DS    PL6                 NET PAYABLE                                  
TXFGDLVD DS    PL6                 GOODS DLVD = TOTAL PO'S                      
TXFINVPO DS    PL6                 INVOICED PO'S                                
TOTSQ    EQU   (*-TOTS)/L'TOTS                                                  
*                                                                               
SAVEKEY  DS    CL(L'ACCKEY)                                                     
LASTSAR  DS    CL(TSRECLEN)                                                     
*                                                                               
ENDDATE  DS    PL3                 ENDDATE                                      
ENDPRIOR DS    PL3                 ENDDATE - (1 YEAR)                           
TWOPRIOR DS    PL3                 ENDDATE - (2 YEARS)                          
*                                                                               
CONTDISP DS    XL2                 DISP TO LAST CONT                            
CONTLIST DS    CL(CONTLSTQ)        CONTRACT LIST FOR CONTRACTOR                 
CONTLEND DS    0C                                                               
CONTLSTQ EQU   40*(L'CON#)                                                      
*                                                                               
LCKCNTR  DS    CL(L'TRNKACT)       LOCKED CONTRACT                              
PRTFRST  DS    CL1                 PRINT FIRST TIME                             
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
*        LIST LINE DSECT                                                        
***********************************************************************         
*                                                                               
LSTLINED DSECT                     LIST LINE DSECT                              
LSTCON#  DS    CL5                 CONTRACT NUMBER                              
LSTHYPH  DS    CL1                                                              
LSTSTAT  DS    CL2                 CONTRACT STATUS                              
         DS    CL2                                                              
LSTNPAY  DS    CL11                NET PAYABLE                                  
         DS    CL5                                                              
LSTGDLVD DS    CL11                VALUE OF GOODS DELIVERED                     
         DS    CL5                                                              
LSTPDLVD DS    CL7                 % DELIVERED = DLVD/NET PAY                   
         DS    CL2                                                              
LSTLMEMO DS    CL8                 DATE OF LAST MEMO BILL                       
         DS    CL2                                                              
LSTBAL   DS    CL11                BALANCE = NET  PAY - GOODS DLVD              
LSTLEN   EQU   *-LSTLINED                                                       
*                                                                               
         SPACE 5                                                                
***********************************************************************         
*        DISPLAY LINE DSECT                                                     
***********************************************************************         
*                                                                               
DSPLINED DSECT                     DISPLAY LINE DSECT                           
DSPHEAD  DS    CL8                 HEADER                                       
DSPCATG  DS    CL8                 CATEGORY NAME                                
         DS    CL2                                                              
DSPNPAY  DS    CL11                NET PAYABLE                                  
         DS    CL2                                                              
DSPDLVD  DS    CL11                GOODS DELIVERED                              
         DS    CL2                                                              
DSPBAL   DS    CL11                BALANCE = NET PAY - DLVD                     
         DS    CL2                                                              
DSPCOST  DS    CL11                INVOICED PO'S                                
         DS    CL2                                                              
DSPCPCT  DS    CL6                 COST% = COST/DLVD                            
DSPLEN   EQU   *-DSPLINED                                                       
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        TSAR RECORD DSECT                                                      
***********************************************************************         
*                                                                               
TSRECD   DSECT                                                                  
TSKEY    DS    0C                                                               
TSSYSMED DS    CL2                 SYSTEM/MEDIA                                 
TSCNTR   DS    CL6                 CONTRACTOR                                   
TSTYPE   DS    CL1                 TYPE OF REPORT                               
TSTSUM   EQU   X'40'                 CONTRACT SUMMARY                           
TSTDET   EQU   X'80'                 CONTRACT DETAIL                            
TSCON#   DS    CL5                 CONTRACT NUMBER                              
TSSORT   DS    XL1                 SORT ORDER IN DETAIL REPORT                  
TSSSLAST EQU   X'80'                 SERVICES LAST - MEDIA FIRST                
TSDATE   DS    PL3                 DATE OF ACTIVITY                             
TSREF    DS    CL6                 REFENCE NUMBER                               
TSSUB    DS    CL1                 SUBREFENCE NUMBER- FOR DUP KEYS              
TSREC    DS    CL1                 TYPE OF RECORD                               
TSRMED   EQU   X'01'                 INFO FROM MEDIA RECORD                     
TSRACC   EQU   X'02'                 INFO FROM ACCOUNT REC                      
TSRXFR   EQU   X'04'                 INFO FROM ACCOUNT REC - TRANSFER           
TSRTRN   EQU   X'08'                 INFO FROM TRANSACTION REC                  
TSIDATE  DS    PL3                 SAME AS ABOVE, BUT FOR INVENTORY SEQ         
TSIREF   DS    CL6                                                              
TSISUB   DS    CL1                                                              
TSKEYLEN EQU   *-TSKEY                                                          
*                                                                               
TSDATA   DS    0C                                                               
TSSTDATE DS    XL3                 START DATE                                   
TSADATE  DS    XL2                 ACTIVITY DATE                                
TSGCI    DS    PL6                 CONTRACT GCI                                 
TSMDLVD  DS    PL6                 MEDIA DELIVERED/PAID                         
TSNET    DS    PL6                 CONTRACT NET PAYABLE                         
TSSDLVD  DS    PL6                 SERVICES DELIVERED                           
TSSCOST  DS    PL6                 ACTUAL COST OF SERVICES                      
TSREC2LN EQU   *-TSRECD                                                         
TSCLIENT DS    0CL36               CLIENT CODE AND NAME                         
TSNAME   DS    0CL36               CONTRACT NAME                                
TSCTGRY  DS    0CL36               ORDER CATEGORY                               
         ORG   TSCLIENT                                                         
TSXFRCON DS    CL5                 TRANSFER CONTRACT NUMBER                     
TSXFRST  DS    XL1                 TRANSFER STATUS                              
         ORG   TSCLIENT                                                         
TSBREF   DS    CL4                 BATCH REF                                    
TSHYPH   DS    CL1                                                              
TSINVREF DS    CL21                INVOICE REFERENCE                            
         ORG   TSCLIENT+L'TSCLIENT                                              
TSSTNINV DS    0CL11               STATION INVOICE NUMBER                       
TSMEMO   DS    CL6                 MEMO BILL NUMBER                             
         ORG   TSSTNINV+L'TSSTNINV                                              
TSPRD    DS    CL3                 PRODUCT CODE                                 
TSMOS    DS    PL2                 MONTH OF ACTIVITY                            
TSDATALN EQU   *-TSDATA                                                         
TSRECLEN EQU   *-TSRECD                                                         
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE DSECTS - CONTRACT SUMMARY                                   
***********************************************************************         
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL1                                                              
PR1CON#  DS    CL5                 CONTRACT NUMBER                              
         DS    CL2                                                              
PR1STDTE DS    CL8                 CONTRACT START DATE                          
         DS    CL4                                                              
PR1GCI   DS    CL11                CONTRACT GCI                                 
         DS    CL1                                                              
PR1MDLVD DS    CL11                MEDIA DELIVERED/PAID                         
         DS    CL1                                                              
PR1MBAL  DS    CL11                MEDIA BALANCE                                
         DS    CL5                                                              
PR1NET   DS    CL11                CONTRACT NET PAYABLE                         
         DS    CL1                                                              
PR1SDLVD DS    CL11                SERVICES DELIVERED                           
         DS    CL1                                                              
PR1SBAL  DS    CL11                SERVICE BALANCE                              
         DS    CL5                                                              
PR1SCOST DS    CL11                COST OF SERVICES                             
         DS    CL2                                                              
PR1CPCT  DS    CL6                 COST%= COST/DLVD                             
         DS    CL1                                                              
PR1LENQ  EQU   *-P                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE DSECTS - CONTRACT DETAIL                                    
***********************************************************************         
*                                                                               
         ORG   P                                                                
         DS    CL1                                                              
PR2DATE  DS    CL8                 DATE OF ACTIVITY                             
         DS    CL1                                                              
PR2ADATE DS    CL8                 DATE OF ACTIVITY                             
         DS    CL1                                                              
PR2CLI   DS    0CL23               CLIENT                                       
PR2INV#S DS    CL23                INVOICE AND DETAIL REFERENCE                 
         DS    CL1                                                              
PR2PRD   DS    CL3                 PRODUCT CODE                                 
         DS    CL1                                                              
PR2STINV DS    0CL11               STATION INV#                                 
PR2ORD#  DS    CL6                 PURCHASE ORD#                                
         ORG   PR2STINV+L'PR2STINV                                              
         DS    CL1                                                              
PR2MOS   DS    CL6                 MONTH OF ACTIVITY                            
         DS    CL1                                                              
PR2GCI   DS    CL11                CONTRACT GCI                                 
         DS    CL1                                                              
PR2MDLVD DS    CL11                MEDIA DELIVERED/PAID                         
         DS    CL1                                                              
PR2NET   DS    CL11                CONTRACT NET PAYABLE                         
         DS    CL1                                                              
PR2SDLVD DS    CL11                SERVICES DELIVERED                           
         DS    CL1                                                              
PR2SCOST DS    CL11                ACTUAL COST OF SERVICES                      
         DS    CL1                                                              
PR2CPCT  DS    CL6                 COST%= COST/DLVD                             
PR2END   DS    CL1                                                              
PR2LENQ  EQU   *-P                                                              
*                                                                               
         ORG   P                                                                
         DS    CL(PR2MDLVD-P)                                                   
PR2MBAL  DS    CL11                                                             
         DS    CL(PR2SDLVD-PR2NET+1)                                            
PR2SBAL  DS    CL11                                                             
*                                                                               
         ORG   P                                                                
         DS    CL(PR2GCI-P)                                                     
PR2MDOTS DS    CL(PR2NET-PR2GCI-1)                                              
         DS    CL1                                                              
PR2SDOTS DS    CL(PR2SCOST-PR2NET-1)                                            
         DS    CL1                                                              
PR2CDOTS DS    CL(PR2END-PR2SCOST)                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE DSECTS - INVENTORY REPORT                                   
***********************************************************************         
*                                                                               
         ORG   P                                                                
         DS    CL1                                                              
PR3CON#  DS    CL5                 CONTRACT NUMBER                              
         DS    CL1                                                              
PR3STDTE DS    CL8                 CONTRACT START DATE                          
         DS    CL1                                                              
PR3GCI   DS    CL11                CONTRACT GCI                                 
         DS    CL1                                                              
PR3MDLVD DS    CL11                MEDIA DELIVERED/PAID                         
         DS    CL3                                                              
PR3NET   DS    CL11                CONTRACT NET PAYABLE                         
         DS    CL1                                                              
PR3SDLVD DS    CL11                SERVICES DELIVERED                           
         DS    CL2                                                              
PR3CPCT  DS    CL6                 COST%= COST/DLVD                             
         DS    CL1                                                              
PR3SCTIM DS    CL11                COST OF TIME USED                            
         DS    CL1                                                              
PR3SCOST DS    CL11                COST OF SERVICES                             
         DS    CL1                                                              
PR3SINV  DS    CL11                INVENTORY VALUE                              
         DS    CL1                                                              
PR3SPAY  DS    CL11                PAYABLE VALUE                                
         DS    CL1                                                              
PR3LENQ  EQU   *-P                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT LINE DSECTS - INVENTORY REPORT TOTALS                            
***********************************************************************         
*                                                                               
         ORG   P                                                                
         DS    CL1                                                              
PR4TOTS  DS    CL20                                                             
         DS    CL29                                                             
PR4SINV  DS    CL12                INVENTORY VALUE                              
         DS    CL6                                                              
PR4SPAY  DS    CL12                PAYABLE VALUE                                
         DS    CL1                                                              
PR4LENQ  EQU   *-P                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        END                                                                    
***********************************************************************         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070ACCTA04X  05/01/02'                                      
         END                                                                    
