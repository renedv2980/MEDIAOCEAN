*          DATA SET ACINV00    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T61300A,*                                                                
*INCLUDE SCUNKEY                                                                
         TITLE 'ACINV00 - INVOICE MARKING - ROOT'                               
ACINV00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1100,**INV**,RA,RR=RE                                            
         LR    R9,RC                                                            
         USING INVWORKD,R9         R9=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         L     R8,4(R1)                                                         
         USING TWAD,R8             R8=A(TWA)                                    
*                                  INITIALIZE STANDARD W/S VALUES               
         ST    RB,ABASE1                                                        
         ST    RA,ABASE2                                                        
         ST    RD,AWORK                                                         
         ST    R8,ATWA                                                          
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         MVC   COMPANY,0(R1)                                                    
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VCALLOV,CCALLOV     BUILD EXTERNAL DIRECTORY                     
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VGETFACT,CGETFACT                                                
         L     RE,8(R1)                                                         
         USING ACCFACSD,RE                                                      
         MVC   VADDAY,AADDAY                                                    
         MVC   VCASHVAL,ACASHVAL                                                
         MVC   VGETDAY,AGETDAY                                                  
         DROP  RE                                                               
         LA    RE,IOAREAS          SET A(IOAREAS 1-2)                           
         ST    RE,AIOAREA1                                                      
         LA    RE,1024(RE)                                                      
         ST    RE,AIOAREA2                                                      
         LA    RE,1024(RE)                                                      
         ST    RE,AIOAREA3                                                      
         LA    RE,1024(RE)                                                      
         ST    RE,ASAVE                                                         
         XCEF  (RE),3000                                                        
         LA    RE,ACRECORD-ACKEYD                                               
         STH   RE,DATADISP         SET DISP TO FIRST ELEMENT                    
         EJECT                                                                  
*                                  BUILD INTERNAL/EXTERNAL DIRECTORY            
         LA    R1,AROUTINE                                                      
         LA    RF,ROUTTAB                                                       
INIT2    ICM   RE,7,0(RF)                                                       
         LA    RE,0(RE,RB)         RELOCATE A/V TYPE                            
         LA    RF,3(RF)                                                         
INIT4    ICM   RE,8,0(RF)                                                       
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF SUB-LIST                              
         BNE   INIT4                                                            
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF LIST                                  
         BNE   INIT2                                                            
         L     R1,=V(SCUNKEY)                                                   
         A     R1,RELO                                                          
         ST    R1,VSCUNKEY                                                      
*                                  SET OTHER FIELDS                             
         MVI   NOCVB,C'N'                                                       
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   MSG,SPACES                                                       
         MVC   XTRAMESS,SPACES                                                  
         ZAP   SCRLINE,=P'13'                                                   
         XC    FLTBAT(FLTLEN),FLTBAT                                            
         CLI   MODE,INIT                                                        
         BE    INIT6                                                            
         B     INITX                                                            
         EJECT                                                                  
* FIRST TIME INITIALIZATION CODE                                                
*                                                                               
INIT6    LA    R1,INVACTH          SET FADR TO FIRST INPUT FIELD                
         ST    R1,FADR                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 AREAD,AIOAREA1      GET COMPANY RECORD                           
         BNE   ERROR                                                            
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
INIT7    CLI   0(RE),0             LOCATE COMPANY ELEMENT                       
         BE    INIT7A                                                           
         CLI   0(RE),X'10'                                                      
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     INIT7                                                            
         USING ACCOMPD,RE                                                       
         MVC   PRODUL,ACMPJOB                                                   
         MVC   SUPPUL,ACMPSUPP                                                  
         MVC   BANKUL,ACMPBANK                                                  
*                                                                               
INIT7A   MVC   KEY+1(2),PRODUL                                                  
         GOTO1 AREAD,AIOAREA1      GET PRODUCTION LEDGER RECORD                 
         BZ    INIT7B                                                           
         MVC   XTRAMESS(6),=C'LEDGER'                                           
         MVC   XTRAMESS+7(2),PRODUL                                             
         B     ERROR                                                            
INIT7B   LA    R1,PRODHEIR                                                      
         BAS   RE,INITHEIR         GET HEIRARCHY LENGTHS                        
*                                  GET TODAY'S DATE                             
         GOTO1 VDATCON,DMCB,(5,0),(1,TODAYP)                                    
*                                                                               
INIT8    XC    WORK,WORK           SAVE AGENCY-LEVEL PROGRAM PROFILE            
         MVC   WORK(4),=C'AINV'                                                 
         NI    WORK,X'FF'-X'40'    MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY                                                
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
*                                                                               
         MVI   WORK,C'A'                                                        
         XC    WORK+1(L'WORK-1),WORK+1                                          
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY   AGENCY ALPHA                                 
*                                  DIG OUT 2ND BYTE OF MASTER PROFILE           
         GOTO1 (RF),(R1),,TEMP                                                  
         MVC   INVREG,TEMP+1                                                    
         MVI   MODE,FIRST          SET SAVE W/S INITIALIZED                     
INITX    B     VALACT                                                           
         EJECT                                                                  
* EXTRACT HEIRARCHY LENGTHS FROM A LEDGER RECORD.                               
*                                                                               
* LENGTHS ARE EXTRACTED FROM RECORD ADDRESSED BY AIOAREA INTO 3-BYTE            
* FIELD ADDRESSED BY R1.                                                        
*                                                                               
INITHEIR NTR1                                                                   
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
INITH2   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'16'                                                      
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     INITH2                                                           
         USING ACHEIRD,RE                                                       
         MVC   0(1,R1),ACHRLEVA                                                 
         MVC   1(1,R1),ACHRLEVB                                                 
         MVC   2(1,R1),ACHRLEVC                                                 
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
* VALIDATE ACTION                                                               
*                                                                               
VALACT   GOTO1 AFVAL,INVACTH                                                    
         BZ    ERROR                                                            
         TM    INVACTH+4,X'20'                                                  
         BO    *+8                                                              
         MVI   OVMODE,INIT                                                      
         XC    ACTSCAN(256),ACTSCAN                                             
         XC    ACTSCAN+256(56),ACTSCAN+256                                      
         LA    R0,10+XTRASCAN                                                   
         GOTO1 VSCANNER,DMCB,((R0),FLDH),(5,ACTSCAN)                            
         MVC   FLAG1,4(R1)         SAVE NO OF INPUT FIELDS                      
         ZIC   RE,ACTSCAN                                                       
         BCTR  RE,0                FIRST FIELD IS ACTION                        
         MVI   FERN,TOOSMALL       MIN 2 CHARS                                  
         CLI   ACTSCAN,2                                                        
         BL    ERROR                                                            
         MVI   FERN,INVACTN                                                     
         L     R1,AACTNTAB                                                      
         USING ACTD,R1                                                          
*                                  LOOK UP ACTION IN ACTION TABLE               
VALACT2  CLI   0(R1),X'FF'                                                      
         BE    LOAD2               IF INVALID GIVE HELP                         
         CLI   ACTSCAN,2           MATCH ON SHORT IF 2 CHARS                    
         BH    VALACT4                                                          
         CLC   ACTDSHT,ACTSCAN+12                                               
         BE    VALACT5                                                          
VALACT3  LA    R1,ACTDLEN(R1)                                                   
         B     VALACT2                                                          
VALACT4  EX    RE,*+8              OTHERWISE FULL                               
         B     *+10                                                             
         CLC   ACTDNAME(0),ACTSCAN+12                                           
         BNE   VALACT3                                                          
*                                                                               
VALACT5  MVC   ACTNVALS,0(R1)      MATCH FOUND                                  
*        TM    ACTINDS,DDSONLY     CHECK FOR DDS ONLY                           
*        BZ    *+12                                                             
         CLI   TWAOFFC,C'*'                                                     
         BNE   ERROR                                                            
         MVI   FERN,SPECIAL                                                     
         MVC   MSG,SPACES                                                       
         OI    INVACTH+4,X'20'                                                  
         CLI   ACTION,HLP                                                       
         BE    LOAD2                                                            
         B     VALFLT                                                           
         EJECT                                                                  
VALFLT   GOTO1 AFVAL,INVFILTH                                                   
         BZ    LOAD                                                             
         TM    INVFILTH+4,X'20'                                                 
         BO    *+8                                                              
         MVI   OVMODE,INIT                                                      
         XC    ACTSCAN(256),ACTSCAN                                             
         XC    ACTSCAN+256(56),ACTSCAN+256                                      
         GOTO1 VSCANNER,DMCB,FLDH,(7,ACTSCAN)                                   
         MVC   FLAG1,4(R1)                                                      
         MVI   FERN,INVALID                                                     
         CLI   FLAG1,0                                                          
         BE    LOAD2               GIVE HELP IF BAD FIELD                       
         LA    R2,ACTSCAN                                                       
         ZIC   R3,FLAG1                                                         
         LA    R4,1                FOR FNDX                                     
*                                                                               
VLF2     CLC   12(3,R2),=C'BATCH'                                               
         BNE   VLF4                                                             
         MVC   FLTBAT,22(R2)                                                    
         B     VLF50                                                            
VLF4     CLC   12(3,R2),=C'REFERENCE'                                           
         BNE   VLF6                                                             
         MVC   FLTREF,22(R2)                                                    
         MVC   FLTREFL,1(R2)       LENGTH OF REFERENCE INPUT                    
         B     VLF50                                                            
VLF6     CLC   12(2,R2),=C'WORK'                                                
         BE    VLF7                                                             
         CLC   12(2,R2),=C'WC'                                                  
         BNE   VLF8                                                             
VLF7     MVC   FLTWC,22(R2)                                                     
         B     VLF50                                                            
VLF8     CLC   12(3,R2),=C'DATE'                                                
         BNE   VLF10                                                            
         GOTO1 VDATVAL,DMCB,(0,22(R2)),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VLF18                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(1,WORK+6)                                 
         MVC   FLTDTE,WORK+6                                                    
         B     VLF50                                                            
VLF10    CLC   12(3,R2),=C'SDATE'                                               
         BNE   VLF12                                                            
         GOTO1 VDATVAL,DMCB,(0,22(R2)),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VLF18                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(1,WORK+6)                                 
         MVC   FLTSDTE,WORK+6                                                   
         B     VLF50                                                            
VLF12    CLC   12(3,R2),=C'CLI'                                                 
         BNE   VLF14                                                            
         MVC   FLTCLI,22(R2)                                                    
         MVI   FLAG,C'N'                                                        
         BAS   RE,CLIRD            VALIDATE CLIENT                              
         CLI   FLAG,C'Y'                                                        
         BNE   VLF20                                                            
         B     VLF50                                                            
VLF14    CLC   12(3,R2),=C'PRODUCT'                                             
         BNE   VLF16                                                            
         MVC   FLTPRD,22(R2)                                                    
         B     VLF50                                                            
VLF16    CLC   12(3,R2),=C'JOB'                                                 
         BNE   VLF18                                                            
         MVC   FLTJOB,22(R2)                                                    
         B     VLF50                                                            
*                                                                               
VLF18    DS    0H                                                               
         CLC   12(3,R2),=C'SUPPLIER'                                            
         BNE   VLF20                                                            
         MVC   FLTSUP,22(R2)                                                    
         B     VLF50                                                            
*                                                                               
VLF20    DS    0H                                                               
         CLC   12(3,R2),=C'SUBREFERENCE'                                        
         BNE   VLF22                                                            
         MVC   FLTSUBL,1(R2)                                                    
         MVC   FLTSUB,22(R2)                                                    
         B     VLF50                                                            
         SPACE 1                                                                
VLF22    DS    0H                                                               
         CLC   12(3,R2),=C'EDATE'                                               
         BNE   VLF24                                                            
         GOTO1 VDATVAL,DMCB,(0,22(R2)),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VLF24                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(1,WORK+6)                                 
         MVC   FLTEDTE,WORK+6                                                   
         B     VLF50                                                            
VLF24    DS    0H                                                               
         CLC   12(2,R2),=C'AMOUNT'                                              
         BNE   VLF26                                                            
         ZIC   RF,1(R2)            LENGTH OF INPUT                              
         GOTO1 VCASHVAL,DMCB,22(R2),(RF)                                        
         CLI   DMCB,X'FF'                                                       
         BE    VLF26                                                            
         ICM   RF,15,DMCB+4                                                     
         CVD   RF,DUB                                                           
         ZAP   FLTAMT,DUB                                                       
         B     VLF50                                                            
VLF26    DS    0H                                                               
         STC   R4,FNDX                                                          
         MVI   FERN,INVALID                                                     
         B     ERROR                                                            
VLF50    LA    R2,32(R2)                                                        
         LA    R4,1(R4)                                                         
         BCT   R3,VLF2                                                          
         OI    INVFILTH+4,X'20'                                                 
         B     LOAD                                                             
         EJECT                                                                  
LOAD     CLC   ACTSCRN,LSCRN       SAME SCREEN AS LAST                          
         BE    GO                  IF SO DONT LOAD                              
         B     LOAD4                                                            
*                                                                               
LOAD2    L     R1,AACTNTAB         SET SCREEN FOR HELP                          
         MVC   ACTNVALS,0(R1)                                                   
         MVC   HELPFERN,FERN                                                    
*                                                                               
LOAD4    MVC   DMCB+4(3),=X'D90613'                                             
         MVC   DMCB+7(1),ACTSCRN                                                
         GOTO1 VCALLOV,DMCB,INVTABH                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R1,INVHEDH          RETRANSMIT TOP OF SCREEN                     
         SR    RE,RE                                                            
         LA    RF,INVTABH                                                       
         OI    6(R1),X'80'                                                      
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-8                                                        
         MVC   LSCRN,ACTSCRN                                                    
         CLI   ACTION,HLP                                                       
         BE    ERROR                                                            
         CLI   INVKEYH+5,0                                                      
         BNE   ANYKEY                                                           
         LA    R1,INVTABH          PUT CURSOR IN FIRST FIELD                    
         SR    RE,RE                                                            
         LA    RF,2000(R1)                                                      
         TM    1(R1),X'20'                                                      
         BZ    *+14                                                             
         IC    RE,0(R1)                                                         
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         ST    R1,FADR                                                          
         MVC   MSG(9),=C'ENTER KEY'                                             
         CLI   ACTION,CHQ                                                       
         BE    LOAD6                                                            
         CLI   ACTION,UNCHQ                                                     
         BNE   OMSG                                                             
LOAD6    DS    0H                                                               
*&&UK*&& MVC   MSG(28),=C'ENTER KEY AND CHEQUE DETAILS'                         
*&&US*&& MVC   MSG(27),=C'ENTER KEY AND CHECK DETAILS'                          
         B     OMSG                                                             
*                                                                               
ANYKEY   LA    R1,INVKEYH                                                       
         MVI   NOCVB,C'Y'          DO NOT DO CVB                                
         GOTO1 AFVAL                                                            
         BZ    GO                                                               
         MVI   FERN,INVALID                                                     
         GOTO1 VSCUNKEY,DMCB,INVKEYH,INVTABH                                    
         CLI   0(R1),0                                                          
         BNE   ERROR                                                            
         XC    INVKEY,INVKEY                                                    
         OI    INVKEYH+6,X'80'     CLEAR KEY FIELD AND TRANSMIT                 
         XC    LKEY,LKEY                                                        
         MVI   OVMODE,INIT                                                      
         B     GO                                                               
         EJECT                                                                  
GO       MVC   PHASE,ACTOVER       OVERLAY APPLICATION PHASE                    
GO1      GOTO1 VCALLOV,DMCB,(PHASE,0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHASE,0(R1)                                                     
*                                  GO TO APPLICATION WITH ACTMODE               
GOPHASE  MVI   FERN,X'FF'                                                       
         LA    R0,INVACTH                                                       
         ST    R0,FADR                                                          
         GOTO1 APHASE                                                           
         CLI   FERN,X'FF'          CHECK FOR ERRORS                             
         BNE   ERROR                                                            
         B     OKEND                                                            
         EJECT                                                                  
* READ CLIENT RECORD FOR FILTER VALIDATION                                      
*                                                                               
CLIRD    NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         MVC   KEY+3(6),FLTCLI                                                  
         GOTO1 AREAD,AIOAREA1                                                   
         CLI   FERN,OK                                                          
         BNE   EXIT                                                             
         MVI   FLAG,C'Y'                                                        
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT OUTPUT MESSAGE/EXTRA MESSAGE INTO MSG & EXIT.                          
*                                                                               
ERROR    CLI   FERN,SPECIAL                                                     
         BE    OMSG                                                             
         GOTO1 VGETMSG,DMCB1,(FERN,MSG),(FNDX,DMCB),0                           
         CLC   XTRAMESS,SPACES                                                  
         BE    OMSG                                                             
         LA    R1,XTRAMESS+L'XTRAMESS-1                                         
         LA    RE,XTRAMESS-1                                                    
         ZIC   RF,DMCB1                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         SR    R1,RE               R1=L'XTRAMESS                                
         LA    R1,1(RF,R1)         R1=TOTAL MESSAGE LENGTH                      
         LA    RE,L'MSG                                                         
         CR    R1,RE               CHECK MESSAGE FITS                           
         BH    OMSG                                                             
         LA    RF,MSG+1(RF)        AND IF SO TACK ON EXTRA MESSAGE              
         MVC   0(L'XTRAMESS,RF),XTRAMESS                                        
         B     OMSG                                                             
*                                                                               
OKEND    DS    0H                                                               
*                                  MOVE MESSAGE TO TWA & TRANSMIT               
OMSG     MVC   INVHED,MSG                                                       
         OI    INVHEDH+6,X'80'                                                  
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
         B     EXIT                                                             
*                                                                               
OKXIT    SR    RB,RB                                                            
ERRXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
* EXTRACT AND PRE-VALIDATE AN INPUT FIELD.                                      
*                                                                               
* ADDRESS OF FIELD HEADER IS PASSED IN R1. RETURN WITH:-                        
*                                                                               
*              FADR     = A(INPUT FIELD HEADER)                                 
*              FERN     = MISSING INPUT FIELD IF NO INPUT                       
*              FNDX     = ZERO                                                  
*              FLDH     = INPUT FIELD HEADER (FLDH(4) = BINARY VALUE            
*                                             FOR NUMERIC FIELD)                
*              FLD      = EXTRACTED & SPACE FILLED INPUT FIELD                  
*                                                                               
* RETURN WITH CC=EQU IF NO INPUT IN FIELD                                       
*                                                                               
FVAL     NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         MVI   FNDX,0                                                           
         MVI   FERN,NOINPUT                                                     
         ST    R1,FADR                                                          
         XC    FLDH,FLDH                                                        
         MVC   FLDH+4(2),4(R1)                                                  
         MVC   FLD,SPACES                                                       
         ZIC   RE,FLDH+5                                                        
         SH    RE,=H'1'                                                         
         BM    FVALX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         MVI   FERN,OK                                                          
         TM    FLDH+4,X'08'                                                     
         BZ    FVALX                                                            
         CLI   NOCVB,C'Y'          JOB KEY - DO NOT DO CONVERT                  
         BE    FVALX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
         ST    RE,FLDH                                                          
FVALX    MVI   NOCVB,C'N'          RESET CONVERT INDICATOR                      
         CLI   FERN,NOINPUT                                                     
         B     EXIT                                                             
         EJECT                                                                  
* EXTRACT NAME FROM A RECORD INTO WORK.                                         
*                                                                               
* RECORD IS ADDRESSED BY WORD AT R1.                                            
*                                                                               
GETNAME  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R1,0(R1)                                                         
         ST    R1,AIOAREA                                                       
         MVC   WORK,SPACES                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
GETNAME2 CLI   0(R1),0                                                          
         BE    GETNAMEX                                                         
         CLI   0(R1),X'20'                                                      
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETNAME2                                                         
         IC    RF,1(R1)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     GETNAMEX                                                         
         MVC   WORK(0),2(R1)                                                    
GETNAMEX B     EXIT                                                             
         EJECT                                                                  
* FILTER TRANSACTION BASED ON INPUT FILTER VALUES                               
*                                                                               
* RECORD IS ADDRESSED BY WORD AT R1.                                            
*                                                                               
TRNSFLT  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R2,0(R1)                                                         
         USING ACKEYD,R2                                                        
         LA    R3,ACRECORD                                                      
         USING TRANSD,R3                                                        
         TM    TRNSSTAT,X'20'      IGNORE ALL MATCHED ITEMS                     
         BO    TRNOXIT                                                          
         CLI   ACTION,UNAUTH                                                    
         BNE   TRFL1                                                            
         OC    ACDTUSED,ACDTUSED   ON UNAUTH WE DONT WANT PAID ITEMS            
         BNZ   TRNOXIT                                                          
TRFL1    OC    FLTBAT,FLTBAT                                                    
         BZ    TRFL2                                                            
         CLC   TRNSBTCH,FLTBAT                                                  
         BNE   TRNOXIT                                                          
TRFL2    OC    FLTREF,FLTREF                                                    
         BZ    TRFL4                                                            
         ZIC   RF,FLTREFL                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNSREF(0),FLTREF                                                
         BNE   TRNOXIT                                                          
TRFL4    OC    FLTDTE,FLTDTE                                                    
         BZ    TRFL6                                                            
         CLC   TRNSDATE,FLTDTE                                                  
         BNE   TRNOXIT                                                          
TRFL6    OC    FLTSDTE,FLTSDTE                                                  
         BZ    TRFL8                                                            
         CLC   TRNSDATE,FLTSDTE                                                 
         BL    TRNOXIT                                                          
TRFL8    DS    0H                                                               
         OC    FLTEDTE,FLTEDTE                                                  
         BZ    TRFL8A                                                           
         CLC   TRNSDATE,FLTEDTE                                                 
         BH    TRNOXIT                                                          
TRFL8A   DS    0H                                                               
         OC    FLTAMT,FLTAMT                                                    
         BZ    TRFL9                                                            
         CP    TRNSAMNT,FLTAMT     SHOW POSITIVE AND NEGATIVE AMOUNTS           
         BE    TRFL9                                                            
         ZAP   DUB,TRNSAMNT                                                     
         MP    DUB,=P'-1'                                                       
         CP    DUB,FLTAMT                                                       
         BNE   TRNOXIT                                                          
TRFL9    DS    0H                                                               
         MVI   FLAG,C'N'                                                        
         CLI   ACTION,AUTH                                                      
         BE    TRFL10                                                           
         CLI   ACTION,UNAUTH                                                    
         BNE   TRFL30                                                           
TRFL10   CLI   0(R3),0                                                          
         BE    TRFL12                                                           
         CLI   0(R3),X'23'         OTHERS EL FOR SUBREFERENCE ON SV             
         BNE   TRFL11                                                           
         MVI   FLAG,C'Y'                                                        
         OC    FLTSUB,FLTSUB                                                    
         BZ    TRFL11                                                           
         ZIC   RF,FLTSUBL                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R3),FLTSUB                                                   
         BNE   TRNOXIT                                                          
TRFL11   CLI   0(R3),X'4F'                                                      
         BE    TRFL14              USE 4F ELEMENT FOR REST OF FILTERS           
         ZIC   RF,1(R3)            ON SV POSTING                                
         AR    R3,RF                                                            
         B     TRFL10                                                           
TRFL12   SR    R3,R3               NO 4F ELEMENT                                
         USING TRCPJD,R3                                                        
TRFL14   OC    FLTWC,FLTWC                                                      
         BZ    TRFL16                                                           
         LTR   R3,R3                                                            
         BZ    TRNOXIT                                                          
         CLC   FLTWC,TRCPWC                                                     
         BNE   TRNOXIT                                                          
TRFL16   OC    FLTCLI,FLTCLI                                                    
         BZ    TRFL18                                                           
         LTR   R3,R3                                                            
         BZ    TRNOXIT                                                          
         CLC   FLTCLI,TRCPCLI                                                   
         BNE   TRNOXIT                                                          
TRFL18   OC    FLTPRD,FLTPRD                                                    
         BZ    TRFL20                                                           
         LTR   R3,R3                                                            
         BZ    TRNOXIT                                                          
         CLC   FLTPRD,TRCPPROD                                                  
         BNE   TRNOXIT                                                          
TRFL20   OC    FLTJOB,FLTJOB                                                    
         BZ    TRFL22                                                           
         LTR   R3,R3                                                            
         BZ    TRFL22                                                           
         CLC   FLTJOB,TRCPJOB                                                   
         BNE   TRNOXIT                                                          
TRFL22   OC    FLTSUB,FLTSUB       IF A SUBREF FILTER WAS INPUT                 
         BZ    TROKXIT                                                          
         CLI   FLAG,C'Y'           AND NO 23 EL FOUND - THEN DONT WANT          
         BNE   TRNOXIT                                                          
         B     TROKXIT                                                          
         SPACE 2                                                                
TRFL30   DS    0H                  COME HERE IF ACTION IS HOLD/UNHOLD           
         CLI   ACTION,CHQ          OR CHEQUE/UNCHEQUE                           
         BE    TROKXIT                                                          
         CLI   ACTION,UNCHQ                                                     
         BE    TROKXIT                                                          
         OC    FLTWC,FLTWC                                                      
         BZ    TRFL32                                                           
         CLC   FLTWC,ACKEYWRK                                                   
         BNE   TRNOXIT                                                          
TRFL32   OC    FLTSUP,FLTSUP                                                    
         BZ    TROKXIT                                                          
         CLC   FLTSUP,ACKEYCON+1                                                
         BNE   TRNOXIT                                                          
TROKXIT  SR    RB,RB                                                            
TRNOXIT  LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
* ACCOUNT FILE I/O EXECUTIVE.                                                   
*                                                                               
* I/O IS EXECUTED ON KEY INTO I/O AREA ADDRESSED BY R1. COMMAND IS              
* PASSED IN THE HIGH ORDER BYTE OF RF AS FOLLOWS:-                              
*                                                                               
*              BITS 0-3 = COMMAND NUMBER (1-6 SEE IOCMNDS)                      
*                   5ON = PASS BACK DELETED RECORDS                             
*                   6ON = READ KEY WITH LOCK                                    
*                   7ON = SAVE KEY IN KEYSAVE BEFORE I/O                        
*                                                                               
* RETURN WITH CC=NEQ ON I/O ERROR WITH FERN SET TO ERROR MESSAGE NUM.           
*                                                                               
ACCIO    NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         STCM  RF,8,DUB            SAVE COMMAND BYTE                            
         L     R1,0(R1)                                                         
         ST    R1,AIOAREA          SAVE A(I/O AREA)                             
         SRL   RF,28                                                            
         SLL   RF,3                                                             
         LA    RF,IOCMNDS-8(RF)                                                 
         ST    RF,DMCB             SET A(COMMAND)                               
         TM    DUB,X'04'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'08'          SET TO PASS BACK DELETES                     
         TM    DUB,X'02'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'80'          SET TO READ WITH LOCK                        
         TM    DUB,X'01'                                                        
         BZ    *+10                                                             
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,,IOFILE,KEY,AIOAREA                                
         MVI   FERN,OK             SET FIELD ERROR NUMBER                       
         CLI   DMCB+8,0                                                         
         BE    ACCIOX                                                           
         MVI   FERN,NOTFOUND                                                    
         TM    DMCB+8,X'10'        TEST N/F                                     
         BO    ACCIOX                                                           
         MVI   FERN,DELETED                                                     
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BO    ACCIOX                                                           
         MVI   FERN,IOERROR        EOF/ERR/DUP/LOCKS                            
*                                                                               
ACCIOX   CLI   FERN,OK             EXIT WITH CC=EQ IF I/O OK                    
         B     EXIT                                                             
         EJECT                                                                  
*                                  LIST OF I/O COMMANDS/FILES                   
IOCMNDS  DS    0CL8                                                             
         DC    C'DMADD   '                                                      
         DC    C'DMRDHI  '                                                      
         DC    C'DMREAD  '                                                      
         DC    C'DMRSEQ  '                                                      
         DC    C'DMWRT   '                                                      
         DC    C'DMUNLK  '                                                      
IOFILE   DC    C'ACCOUNT '                                                      
         EJECT                                                                  
*              LITERALS ETC.                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
RELO     DS    F                                                                
         SPACE 2                                                                
* TABLE OF A&V-TYPES FOR RELOCATING INTO GLOBAL W/S.                            
*                                                                               
*              BYTE 0-3 = A/V-TYPE ADDRESS                                      
*                   1-N = HIGH ORDER BYTE VALUES DELIMITED BY X'FF'             
*                                                                               
ROUTTAB  DS    0X                                                               
         DC    AL3(ACTNTAB-ACINV00),X'00FF'                                     
         DC    AL3(FVAL-ACINV00),X'00FF'                                        
         DC    AL3(GETNAME-ACINV00),X'00FF'                                     
         DC    AL3(TRNSFLT-ACINV00),X'00FF'                                     
         DC    AL3(ACCIO-ACINV00),X'102527343640425060FF'                       
         DC    X'FF'                                                            
*                                                                               
* TABLE OF INPUT ACTIONS.                                                       
*                                                                               
*              BYTE 0-7 = ACTION NAME                                           
*                   8-9 = SHORT ACTION NAME                                     
*                   10  = ACTION NUMBER                                         
*                   11  = INDICATORS - BIT0ON=DDS-ONLY ACTION                   
*                   12  = OVERLAY PHASE NUMBER                                  
*                   13  = SCREEN                                                
*                                                                               
ACTNTAB  DS    0CL14                                                            
         DC    C'HELP    HE',AL1(HLP,0,0),X'FC'                                 
*&&UK                                                                           
         DC    C'AUTH    AU',AL1(AUTH,0,1),X'FE'                                
         DC    C'UNAUTH  UA',AL1(UNAUTH,0,1),X'FE'                              
*&&                                                                             
         DC    C'HOLD    HO',AL1(HOLD,0,2),X'FD'                                
         DC    C'UNHOLD  UH',AL1(UNHOLD,0,2),X'FD'                              
*&&UK                                                                           
         DC    C'CHEQUE  CH',AL1(CHQ,0,3),X'FB'                                 
         DC    C'UNCHEQUEUC',AL1(UNCHQ,0,3),X'FB'                               
*&&                                                                             
*&&US                                                                           
*        DC    C'CHECK   CH',AL1(CHQ,0,3),X'FB'                                 
*        DC    C'UNCHECK UC',AL1(UNCHQ,0,3),X'FB'                               
*&&                                                                             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE ACINVDSECT                                                     
         ORG   TWAD+1800                                                        
PROGPROF DS    CL16                                                             
KEYTAB   DS    13CL27                                                           
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDACCFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACINV00   05/01/02'                                      
         END                                                                    
