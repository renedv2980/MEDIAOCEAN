*          DATA SET ACBAT10    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T61B10A                                                                  
         TITLE 'ACBAT10 - CASH RECEIPT'                                         
T61B10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT10,CLEAR=YES                                   
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    RD,SAVERD                                                        
         RELOC (R7)                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         XC    INCOMAC,INCOMAC                                                  
         MVC   CREDUNIT,SPACES                                                  
         MVC   CARBILD,SPACES                                                   
         OI    CARBILDH+6,X'80'                                                 
         MVI   OPT1,0                                                           
         LA    R7,COMPEL                                                        
         USING ACCOMPD,R7          R7=A(COMPANY ELEMENT)                        
*                                  VALIDATE BILL NUMBER                         
VALBILNO LA    R2,CARBILLH                                                      
         BAS   RE,ANY                                                           
         MVC   BILLNUM,CARBILL                                                  
         OC    BILLNUM,SPACES                                                   
*                                  VALIDATE BILL DATE                           
VALBILDT LA    R2,CARBLDTH                                                      
         BAS   RE,ANY                                                           
         MVI   ERRNUM,EIDF                                                      
         MVC   BILLDATE,=X'760101'                                              
         CLI   5(R2),1                                                          
         BNE   VALBILD                                                          
         CLI   8(R2),X'6F'                                                      
         BNE   ERROR                                                            
         OI    OPT1,SKIP                                                        
         B     VALBANK                                                          
         SPACE 1                                                                
VALBILD  GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,BILLDATE)                                
         EJECT                                                                  
*              VALIDATE DEBIT ACCOUNT (BANK)                                    
*                                                                               
VALBANK  MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPBANK                                                
         LA    R2,CARBACH                                                       
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         LA    R5,KEY+3                                                         
         CLI   8(R2),C'*'          CAN BE *ULACC                                
         BNE   VALBANK2                                                         
         MVI   ERRNUM,EFTS                                                      
         CLI   5(R2),4                                                          
         BL    ERROR                                                            
         MVI   ERRNUM,EIPL                                                      
         CLC   1(2,R4),=C'SQ'      CHECK UNIT/LEDGER                            
         BE    *+14                                                             
         CLC   1(2,R4),=C'SE'                                                   
         BNE   ERROR                                                            
         BCTR  R3,0                                                             
         LA    R4,9(R2)                                                         
         LA    R5,KEY+1                                                         
VALBANK2 BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE TO KEY                                  
         BAS   RE,GETACC                                                        
         MVC   DRAC,ACCTNUM        SAVE ACCOUNT CODE                            
         MVC   DRNM,ACCTNAME                                                    
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'      SEE IF OK TO POST                            
         BZ    ERROR                                                            
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         TM    CARBACH+4,X'20'                                                  
         BO    VALCRAC                                                          
         MVC   CARBACN,ACCTNAME                                                 
         OI    CARBACNH+6,X'80'                                                 
         OI    CARBACH+4,X'20'                                                  
         B     VALCRAC                                                          
         EJECT                                                                  
*              VALIDATE CREDIT ACCOUNT                                          
*                                                                               
VALCRAC  MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPRECV                                                
         SR    R6,R6                                                            
         LA    R2,CARCACH                                                       
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         LA    R5,KEY+3                                                         
         CLI   8(R2),C'*'          CAN BE *ULACC                                
         BNE   VALCRAC2                                                         
         MVI   ERRNUM,EFTS                                                      
         CLI   5(R2),4                                                          
         BL    ERROR                                                            
         MVI   ERRNUM,EIPL                                                      
         CLI   1(R4),C'S'          CHECK UNIT                                   
         BNE   ERROR                                                            
         MVI   ERRNUM,EIPL                                                      
         CLC   1(2,R4),=C'SJ'      CHECK UNIT/LEDGER                            
         BE    ERROR                                                            
         CLC   1(2,R4),=C'SE'                                                   
         BE    ERROR                                                            
         BCTR  R3,0                                                             
         LA    R4,9(R2)                                                         
         LA    R5,KEY+1                                                         
VALCRAC2 BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE TO KEY                                  
         BAS   RE,GETACC                                                        
         MVC   CRAC,ACCTNUM        SAVE ACCOUNT NUMBER                          
         MVC   CRNM,ACCTNAME                                                    
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'      SEE IF OK TO POST                            
         BZ    ERROR                                                            
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         TM    CARCACH+4,X'20'                                                  
         BO    VALINC                                                           
         MVC   CARCACN,ACCTNAME                                                 
         OI    CARCACNH+6,X'80'                                                 
         OI    CARCACH+4,X'20'                                                  
         B     VALINC                                                           
         EJECT                                                                  
*              VALIDATE INCOME ACCOUNT (CASH DISCOUNT POSTING)                  
*                                                                               
*&&UK                                                                           
VALINC   B     VALSRCE                                                          
*&&                                                                             
*&&US                                                                           
VALINC   LA    R2,CARIACH                                                       
         CLI   5(R2),0                                                          
         BNE   VALINC2                                                          
         OC    CARIACN,CARIACN     CLEAR NAME FIELD IF ANYTHING THERE           
         BZ    VALSRCE                                                          
         XC    CARIACN,CARIACN                                                  
         OI    CARIACNH+6,X'80'                                                 
         B     VALSRCE                                                          
VALINC2  MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SI'                                                  
         ZIC   R3,5(R2)                                                         
         LA    R4,8(R2)                                                         
         LA    R5,KEY+3                                                         
         CLI   8(R2),C'*'          CAN BE *ULACC                                
         BNE   VALINC4                                                          
         MVI   ERRNUM,EFTS                                                      
         CLI   5(R2),4                                                          
         BL    ERROR                                                            
         BCTR  R3,0                                                             
         LA    R4,9(R2)                                                         
         LA    R5,KEY+1                                                         
VALINC4  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(R4)       MOVE TO KEY                                  
         BAS   RE,GETACC                                                        
         MVC   INCOMAC,ACCTNUM                                                  
         MVC   INCOMNM,ACCTNAME                                                 
         MVI   ERRNUM,EIAC                                                      
         TM    ACCTSTAT,X'80'                                                   
         BZ    ERROR                                                            
         MVI   ERRNUM,EAIL                                                      
         TM    ACCTSTAT,X'10'                                                   
         BO    ERROR                                                            
         TM    CARIACH+4,X'20'                                                  
         BO    VALSRCE                                                          
         MVC   CARIACN,ACCTNAME                                                 
         OI    CARIACNH+6,X'80'                                                 
         OI    CARIACH+4,X'20'                                                  
         B     VALSRCE                                                          
*&&                                                                             
         EJECT                                                                  
*              VALIDATE BILLING SOURCE                                          
*                                                                               
VALSRCE  LA    R2,CARDUMH                                                       
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         MVC   BILLSRCE,SPACES                                                  
         CLI   5(R2),4                                                          
         BL    VALSRCE2                                                         
         MVI   ERRNUM,EFTL                                                      
         CLI   5(R2),12                                                         
         BH    ERROR                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     VALAMNT                                                          
         MVC   BILLSRCE+3(0),8(R2)                                              
VALSRCE2 MVC   KEY,SPACES          IF INPUT 3 LONG READ ACCOUNT IN SI           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SI'                                                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)                                                   
         BAS   RE,GETACC                                                        
         GOTO1 CHOPPER,DMCB,(36,ACCTNAME),(12,BILLSRCE+3),1                     
         B     VALAMNT                                                          
*                                                                               
*              VALIDATE RECEIVED AMOUNT                                         
*                                                                               
VALAMNT  LA    R2,CARAMTH                                                       
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 AMTVAL,DMCB,8(R2),(R3)                                           
         MVI   ERRNUM,EICA                                                      
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R1,DMCB+4                                                        
         LA    R1,0(R1)                                                         
         ZAP   RECEIVED,0(8,R1)                                                 
         ZAP   TRANSAMT,0(8,R1)                                                 
         EJECT                                                                  
*              VALIDATE CHECK NUMBER/CHECK DATE /DEPOSIT DATE                   
         SPACE 1                                                                
VALCKN   MVC   CHKNUM,SPACES       CHECK NUMBER                                 
         LA    R2,CARCKNH                                                       
         BAS   RE,ANY                                                           
         ZIC   R3,5(R2)                                                         
         MVI   ERRNUM,INVALID                                                   
         CH    R3,=H'6'            MUST BE 6 LONG                               
         BL    ERROR                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     VALCKD                                                           
         MVC   CHKNUM,8(R2)                                                     
         SPACE 1                                                                
VALCKD   LA    R2,CARCKDH          CHECK DATE                                   
         BAS   RE,ANY                                                           
         MVI   ERRNUM,EIDF                                                      
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKDATE)                                 
         GOTO1 DATECHK,DMCB,CHKDATE                                             
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         SPACE 1                                                                
VALDEPD  LA    R2,CARDPDH          DEPOSIT DATE                                 
         BAS   RE,ANY                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,DEPDATE)                                 
         GOTO1 DATECHK,DMCB,DEPDATE                                             
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         EJECT                                                                  
*              READ ACCOUNT FILE & CHECK INPUT AMOUNT                           
*                                                                               
READMAST LA    R3,KEY                                                           
         USING ACKEYD,R3                                                        
         ZAP   RECVAMNT,=P'0'                                                   
         ZAP   CASHDISC,=P'0'      CHECK FOR SPECIAL KEYWORDS                   
         TM    OPT1,SKIP                                                        
         BO    READM                                                            
         CLC   BILLSRCE+3(12),=C'OTHERS      '                                  
         BE    POST                                                             
         CLC   BILLSRCE+3(12),=C'OVERPAYMENT '                                  
         BE    POST                                                             
         CLC   BILLSRCE+3(12),=C'UNAPPLIED   '                                  
         BE    POST                                                             
*                                  BUILD CONTRA-ACCOUNT KEY & READ              
READM    MVC   KEY,SPACES                                                       
         MVC   ACKEYACC,CRAC                                                    
         MVC   ACKEYCON,BILLSRCE                                                
         BAS   RE,HIGH                                                          
         LA    R2,CARDUMH                                                       
         MVI   ERRNUM,EIBS                                                      
         CLC   KEY(42),KEYSAVE                                                  
         BNE   ERROR                                                            
*                                  MOVE BILL DATE/NUMBER TO KEY & READ          
READM1   MVC   ACKEYDTE,BILLDATE                                                
         MVC   ACKEYREF,BILLNUM                                                 
         MVI   ACKEYSBR,0                                                       
         BAS   RE,HIGH                                                          
         MVI   ERRNUM,ENTM                                                      
         LA    R2,CARBILLH                                                      
         CLC   ACKEYACC(ACKEYDTE-ACKEYACC),KEYSAVE                              
         BNE   ERROR                                                            
         TM    OPT1,SKIP                                                        
         BZ    READM1A                                                          
         MVC   BILLDATE,ACKEYDTE                                                
         CLC   ACKEYREF,BILLNUM                                                 
         BNE   READM2                                                           
         B     READM3                                                           
READM1A  CLC   ACKEYACC(ACKEYSBR-ACKEYACC),KEYSAVE                              
         BNE   ERROR                                                            
         B     READM3                                                           
READM2   BAS   RE,SEQ                                                           
         CLC   KEY(32),KEYSAVE     MATCH POSTINGS ON BILL DATE/NUMBER           
         BNE   READM14                                                          
         CLC   ACKEYDTE,BILLDATE                                                
         BNE   READM2                                                           
         CLC   ACKEYREF,BILLNUM                                                 
         BNE   READM2                                                           
READM3   CLI   ACRECORD,X'44'                                                   
         BNE   READM2                                                           
         MVC   BILLDATE,ACKEYDTE                                                
         LA    R4,ACRECORD                                                      
         USING TRANSD,R4                                                        
         OC    ACDTPEEL,ACDTPEEL   CHECK FOR DELETED TRANSACTION                
         BNZ   READM2                                                           
         ZAP   DUB,TRNSAMNT                                                     
         TM    TRNSSTAT,X'80'      IF THIS IS A DEBIT AND WE ARE                
         BZ    READM10             RECEIVING GROSS LOOK FOR C/D                 
         MVC   CREDUNIT,TRNSANAL                                                
         OC    INCOMAC,INCOMAC                                                  
         BZ    READM12                                                          
         ZIC   R1,1(R4)                                                         
         AR    R1,R4                                                            
         SR    RF,RF                                                            
READM4   CLI   0(R1),0                                                          
         BE    READM12                                                          
         CLI   0(R1),X'50'                                                      
         BE    READM8                                                           
READM6   IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     READM4                                                           
READM8   DS    0H                                                               
         USING TRCASHD,R1                                                       
         CLI   TRCSTYPE,C'D'                                                    
         BNE   READM6                                                           
         CP    TRCSAMNT,=P'0'                                                   
         BE    READM6                                                           
         ZAP   CASHDISC,TRCSAMNT                                                
         DROP  R1                                                               
         B     READM12                                                          
READM10  ZAP   CASHDISC,=P'0'      PARTIAL PAYMENT - CLEAR DISCOUNT             
         MP    DUB,=P'-1'                                                       
READM12  AP    RECVAMNT,DUB                                                     
         B     READM2                                                           
READM14  AP    RECVAMNT,CASHDISC                                                
         LA    R2,CARAMTH                                                       
         MVI   ERRNUM,EICA                                                      
         CLI   CARPART,C'P'                                                     
         BE    *+14                                                             
         CP    RECVAMNT,RECEIVED   CHECK BALANCE IF FULL PAYMENT                
         BNE   READMHI                                                          
         TM    OPT1,SKIP                                                        
         BZ    POST                                                             
         GOTO1 DATCON,DMCB,(1,BILLDATE),(8,CARBILD)                             
         B     POST                                                             
         SPACE 1                                                                
*         IF ITS SKIP AND NOT PARTIAL TRY TO FIND EXACT MATCH                   
READMHI  TM    OPT1,SKIP                                                        
         BZ    ERROR                                                            
         GOTO1 DATCON,DMCB,(1,BILLDATE),(0,WORK)                                
         GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,BILLDATE)                              
         ZAP   RECVAMNT,=P'0'                                                   
         ZAP   CASHDISC,=P'0'                                                   
         B     READM                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
*              MAKE POSTINGS & ADD TO ACCDAY                                    
*                                                                               
POST     LA    R5,IOAREA+2                                                      
         USING DLDESCD,R5                                                       
         XC    DLDSEL(DLDSNARR-DLDSEL),DLDSEL                                   
         MVI   DLDSEL,X'64'        BUILD X'64' ELEMENT                          
         MVC   DLDSREF,BILLNUM                                                  
         MVC   DLDSDATE,BILLDATE                                                
         MVC   DLDSNARR(57),SPACES                                              
*&&UK                                                                           
         MVC   DLDSNARR(13),=C'CHEQUE NUMBER'                                   
         MVC   DLDSNARR+14(L'CHKNUM),CHKNUM                                     
         MVC   DLDSNARR+21(5),=C'DATED'                                         
         GOTO1 DATCON,DMCB,(1,CHKDATE),(8,DLDSNARR+27)                          
         MVC   DLDSNARR+35(12),=C'DEPOSITED ON'                                 
         GOTO1 (RF),(R1),(1,DEPDATE),(8,DLDSNARR+48)                            
*&&                                                                             
*&&US                                                                           
         MVC   DLDSNARR(12),=C'CHECK NUMBER'                                    
         MVC   DLDSNARR+13(L'CHKNUM),CHKNUM                                     
         MVC   DLDSNARR+20(5),=C'DATED'                                         
         GOTO1 DATCON,DMCB,(1,CHKDATE),(8,DLDSNARR+26)                          
         MVC   DLDSNARR+35(12),=C'DEPOSITED ON'                                 
         GOTO1 (RF),(R1),(1,DEPDATE),(8,DLDSNARR+48)                            
*&&                                                                             
         LA    R2,CARNARH                                                       
         LA    R3,DLDSNARR+57                                                   
         XC    DLDSNARR+57(143),DLDSNARR+57                                     
         BAS   RE,NARRSCAN                                                      
         AH    R6,=H'57'                                                        
         LA    R6,DLDSNARR(R6)                                                  
         SR    R6,R5                                                            
         STC   R6,DLDSLEN                                                       
*                                                                               
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         USING DLPOSTD,R5                                                       
         XC    DLPSEL(113),DLPSEL                                               
         MVI   DLPSEL,X'69'        BUILD X'69' ELEMENT - DEBIT CASH             
         MVI   DLPSLEN,113                                                      
         MVC   DLPSDBAC,DRAC                                                    
         MVC   DLPSDBNM,DRNM                                                    
         MVC   DLPSCRAC,CRAC                                                    
         MVC   DLPSCRNM,CRNM                                                    
         ZAP   DLPSAMNT,RECEIVED                                                
         MVC   DLPSANAL,CREDUNIT                                                
*                                                                               
         LA    RF,DLPOSTD         SAVE A(CURRENT POSTING ELEMENT)               
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
         USING RALELD,R5           BUILD ALLOCATION ELEMENT                     
         XC    RALELD(RALALCLQ),RALELD                                          
         MVI   RALEL,RALELQ                                                     
         MVI   RALLN,RALALCLQ                                                   
         MVI   RALTYPE,RALTALC                                                  
         MVC   RALAREF,CHKNUM                                                   
         MVC   RALADAT,CHKDATE                                                  
         MVC   RALADEP,DEPDATE                                                  
*                                                                               
         USING DLPOSTD,R5                                                       
         ZIC   R1,DLPSLEN                                                       
         AR    R5,R1                                                            
         MVC   0(113,R5),0(RF)     DUPLICATE ELEMENT                            
         MVI   DLPSEL,X'6A'        BUILD X'6A' ELEMENT - CREDIT RCVBL           
         MVC   DLPSDBAC,BILLSRCE                                                
         MVC   DLPSDBNM,SPACES                                                  
         ZAP   DLPSAMNT,RECEIVED                                                
         SP    DLPSAMNT,CASHDISC                                                
*                                                                               
         OC    INCOMAC,INCOMAC     ANY CASH DISCOUNT                            
         BZ    POST2                                                            
         CP    CASHDISC,=P'0'                                                   
         BE    POST2                                                            
         ZIC   R1,1(R5)                                                         
         AR    R1,R5                                                            
         MVC   0(113,R1),0(R5)     YES - DUPLICATE ELEMENT                      
         LR    R5,R1                                                            
         MVC   DLPSDBAC,CRAC       BUILD X'6A' ELEMENT - CREDIT INCOME          
         MVC   DLPSDBNM,CRNM                                                    
         MVC   DLPSCRAC,INCOMAC                                                 
         MVC   DLPSCRNM,INCOMNM                                                 
         ZAP   DLPSAMNT,CASHDISC                                                
POST2    ZIC   R1,1(R5)            SET L'RECORD                                 
         AR    R5,R1                                                            
         MVI   0(R5),0                                                          
         LA    R1,IOAREA-1                                                      
         SR    R5,R1                                                            
         ST    R5,DUB                                                           
         MVC   IOAREA(2),DUB+2                                                  
         BAS   RE,PUTDAY           ADD RECORD TO ACCDAY                         
         XC    WORK,WORK                                                        
         MVC   WORK(6),BILLNUM                                                  
         L     R1,DMCB+8                                                        
         MVC   WORK+10(4),0(R1)                                                 
         BAS   RE,ADTWA1           AND TWA1                                     
         LA    R2,CARBILLH                                                      
         MVI   ERRNUM,X'FF'                                                     
         B     EXIT                                                             
         EJECT                                                                  
* ACBATCODE                                                                     
       ++INCLUDE ACBATCODE                                                      
*              LITERALS ETC.                                                    
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* ACBATDSECT                                                                    
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
* ACBATEFD                                                                      
       ++INCLUDE ACBATEFD                                                       
*        ORG   CONHEADH                                                         
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
*              DSECT TO COVER TEMP W/S                                          
*                                                                               
PROGD    DSECT                                                                  
SAVERD   DS    F                                                                
BILLNUM  DS    CL6                                                              
BILLDATE DS    CL3                                                              
BILLSRCE DS    CL15                                                             
DRAC     DS    CL15                                                             
DRNM     DS    CL36                                                             
CRAC     DS    CL15                                                             
CRNM     DS    CL36                                                             
INCOMAC  DS    CL15                                                             
INCOMNM  DS    CL36                                                             
CREDUNIT DS    CL2                                                              
RECEIVED DS    PL6                                                              
RECVAMNT DS    PL6                                                              
CASHDISC DS    PL6                                                              
CHKNUM   DS    CL6                                                              
CHKDATE  DS    CL3                                                              
DEPDATE  DS    CL3                                                              
OPT1     DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
SKIP     EQU   X'80'                                                            
*                                                                               
*              ERROR NUMBER EQUATES                                             
*                                                                               
EMIF     EQU   1                                                                
EIIF     EQU   2                                                                
EIPL     EQU   9                                                                
EIDF     EQU   13                                                               
EIAC     EQU   18                                                               
EAIL     EQU   18                                                               
EICA     EQU   25                                                               
EFTS     EQU   36                                                               
EFTL     EQU   37                                                               
ELCD     EQU   71                                                               
EIBS     EQU   72                                                               
ENTM     EQU   73                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACBAT10   05/01/02'                                      
         END                                                                    
