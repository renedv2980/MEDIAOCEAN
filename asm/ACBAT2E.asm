*          DATA SET ACBAT2E    AT LEVEL 011 AS OF 12/04/12                      
*                                                                               
*PHASE T61B2EA                                                                  
         TITLE 'MULTIPLE BILLABLE/NON-BILLABLE EXPENSE ENTRY'                   
*                                                                               
*        BATCH TYPE 46                                                          
*                                                                               
T61B2E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,T61B2E**,R7,R8,RR=R5,CLEAR=YES                      
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         ST    R5,RELOA                                                         
         L     R1,=A(BLDELSC)                                                   
         AR    R1,R5                                                            
         ST    R1,BLDELS                                                        
         L     R1,=A(ERRCHCK)                                                   
         AR    R1,R5                                                            
         ST    R1,ERRCHK                                                        
         L     R1,=A(CTAXMOD)                                                   
         AR    R1,R5                                                            
         ST    R1,ACTAXMOD                                                      
*                                                                               
* BUILD SCREEN DIRECTORY                                                        
*                                                                               
* CANADIAN SCREEN MATCHES US SCREEN UP TO EXPENSE VENDOR FIELDS                 
* SCREENS DIVERGE BELOW EXPENSE VENDOR CD OVERRIDE                              
*                                                                               
         LA    R1,USTAB                                                         
         CLI   AGYCTRY,CTRYCAN     TEST FOR CANADA                              
         BNE   *+8                                                              
         LA    R1,CANTAB           YES-USE CANADIAN SCREEN TABLE                
*                                                                               
AC1      CLI   0(R1),X'FF'         TEST FOR EOT                                 
         BE    AC2                                                              
         LM    RE,RF,0(R1)                                                      
         LA    RE,TWAD(RE)         FORM READ ADDRESS OF SCREEN FIELD            
         LA    RF,PROGD(RF)        FORM ADDRESS OF ADCON                        
         ST    RE,0(RF)                                                         
         LA    R1,L'USTAB(R1)                                                   
         B     AC1                                                              
*                                                                               
AC2      ZAP   CSHTOT,CSLSTCUR+LSTBCSHA-LSTTABD(L'LSTBCSHA)                     
*                                                                               
AC2EB    LA    R2,BASSRVH          PREPARE BH MSG FLD                           
         OI    1(R2),X'01'         TURN ON MODIFIED BIT                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
*                                                                               
         MVC   TMPMODE,MODE                                                     
         NI    TMPMODE,X'0F'                                                    
         CLI   TMPMODE,1           PROCESSING ANOTHER SCREEN?                   
         BH    PREP10              DON'T CHECK DATE                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         LA    R2,ATRDAT1H         DATE FLD                                     
         LA    R3,SVDATE1                                                       
         CLI   5(R2),0                                                          
         BNE   DAT02                                                            
         BAS   RE,GETODAY          DEFAULT TO CURRENT DAY                       
         B     DAT04                                                            
*                                                                               
DAT02    GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  EDIT DATE INPUT                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*                                                                               
DAT04    GOTO1 DATCON,DMCB,(0,WORK),(1,(R3)) PACK IT Y/M/D                      
         CLI   0(R3),X'70'                                                      
         BL    ERROR                                                            
         GOTO1 DATECHK,DMCB,(R3)                                                
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         OI    4(R2),X'20'         TURN ON VALIDATED BIT                        
         CLI   5(R2),0             WAS THERE INPUT                              
         BNE   PREP10              YES                                          
         OI    6(R2),X'80'         SET TRANS                                    
         GOTO1 DATCON,DMCB,(1,(R3)),(8,8(R2)) DISPLAY DFLT DATE                 
*                                                                               
PREP10   CLI   CSACT,ACTCHA                                                     
         BNE   *+16                                                             
         MVI   CLEAROK,C'N'        DON'T CLEAR GST/PST FOR CHANGE               
         MVI   USERPROV,C'Y'       USER ENTERED PROV FROM CHANGE                
         MVI   USERPST,C'Y'                                                     
*                                                                               
PREP20   CLI   AGYCTRY,CTRYCAN                                                  
         BNE   PREP50                                                           
         CLI   MODE,2              CANNOT CLEAR WHEN IN ANOTHER                 
         BE    PREP50              PROGRAM                                      
         CLI   MODE,3                                                           
         BE    PREP50                                                           
         CLI   CLEAROK,C'N'        NOT OK TO CLEAR?                             
         BE    PREP35                                                           
         L     R2,AGORNH                                                        
         TM    4(R2),X'80'         INPUT THIS TIME?                             
         BO    PREP30              ONLY CLEAR THE OLD ONES                      
         OI    6(R2),X'80'                                                      
         MVI   8(R2),0                                                          
         L     R2,AGSTXH                                                        
         MVC   8(L'ATCGSTX,R2),SPACES        CLEAR INFO DATA TOO                
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(L'ATCTYPN,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
         L     R2,APROVH                                                        
         MVC   8(L'ATCPROV,R2),SPACES                                           
         OI    6(R2),X'80'                                                      
*                                                                               
PREP30   L     R2,ATYPEH                                                        
         TM    4(R2),X'80'                                                      
         BO    *+12                                                             
         MVI   8(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AGAMTH                                                        
         TM    4(R2),X'80'                                                      
         BO    *+18                                                             
         OI    6(R2),X'80'                                                      
         MVI   5(R2),0                                                          
         XC    8(L'ATCGAMT,R2),8(R2)                                            
         XC    XTRAELM,XTRAELM     CLEAR PST FIELDS                             
*                                                                               
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    PREP35                                                           
         L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BO    PREP45                                                           
         MVI   USERPROV,C'N'                                                    
         B     PREP48                                                           
*                                                                               
PREP35   LA    R2,ATRACCH          PROD VENDOR                                  
         TM    4(R2),X'80'                                                      
         BO    PREP38                                                           
         LA    R2,ATRACC2H         CHECK EXPENSE VENDOR TOO                     
         TM    4(R2),X'80'                                                      
         BZ    PREP40                                                           
PREP38   XC    VENDPROV,VENDPROV                                                
         L     R2,APROVH                                                        
         TM    4(R2),X'80'                                                      
         BO    PREP40                                                           
         XC    8(2,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
PREP40   L     R2,APROVH                                                        
         TM    4(R2),X'80'         NEW PROVINCE?                                
         BNO   PREP50              SAVE PROVINCE                                
PREP45   MVI   USERPROV,C'Y'       USER ENTERED PROVINCE                        
PREP48   MVC   KEEPPROV,8(R2)                                                   
         XC    XTRAELM,XTRAELM                                                  
*                                                                               
PREP50   MVI   CLEAROK,C'N'        DON'T CLEAR UNTIL DONE                       
         LA    RF,OFFCLIST                                                      
         ST    RF,AOFFC            POINT TO START OF OFFICE LIST                
*                                                                               
*--------------------------------------------------------------------           
*        MODE=X'00'  ORDER OVERLAY                                              
*        MODE=X'01'  EXIT WITH ORDER DISPLAYED                                  
*        MODE=X'03'  CANADIAN SALES TAX SCREEN                                  
*--------------------------------------------------------------------           
PREP60   CLI   CSACT,ACTCHA        ITEM CHANGE?                                 
         BNE   PREP70                                                           
         CLI   PFKEY,X'FF'         FIRST TIME, IT SETS PFKEY TO FF              
         BNE   *+8                 YES                                          
         MVI   PFKEY,0                                                          
PREP70   CLI   MODE,3              CANDIAN TAX?                                 
         BE    PREP90                                                           
*                                                                               
         CLI   PFKEY,7             USE PF=7 TO LOAD                             
         BNE   PREP80                                                           
         CLI   AGYCTRY,CTRYCAN                                                  
         BE    ATR00                                                            
         MVC   FVMSGNO,=Y(AE$CSNA)                                              
         LA    R2,CONACTH                                                       
         B     ERROR                                                            
*                                                                               
PREP80   CLI   PFKEY,0                                                          
         BE    ATR00                                                            
         MVC   FVMSGNO,=AL2(AE$INVPF)   INVALID PFKEY                           
         L     R2,TIACURS                                                       
         B     ERROR                                                            
*                                                                               
PREP90   CLI   AGYCTRY,CTRYCAN     CANADIAN?                                    
         BNE   ATR00                                                            
         MVI   CSSPROG,2                                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 ACTAXMOD,DMCB,(RC)                                               
         CLI   CSACT,ACTCHA        ARE WE DOING A CHANGE?                       
         BNE   CURSIT              NO, LEAVE                                    
         CLC   FVMSGNO,=AL2(FVFOK) IF ERROR, RETURN                             
         BNE   CURSIT                                                           
         MVC   FVMSGNO,=X'FFFF'    TELL BT61 WE HAVE TO COME BACK               
         B     CURSIT                                                           
         EJECT                                                                  
ATR00    OI    ATRACDH+6,X'80'     CLEAR DISPLAY FLAGS                          
         MVC   ATRACD,SPACES                                                    
         OI    ATRCDDH+6,X'80'                                                  
         MVC   ATRCDD,SPACES                                                    
         OI    ATRAC2DH+6,X'80'                                                 
         MVC   ATRAC2D,SPACES                                                   
         OI    ATRCDD2H+6,X'80'                                                 
         MVC   ATRCDD2,SPACES                                                   
         EJECT                                                                  
*                                                                               
ATR01    LA    R2,ATRDOCH          VALIDATE/SAVE DOC NUMBER AND DATE            
         BAS   RE,ANY                                                           
         MVC   SVDOC,ATRDOC                                                     
         OC    SVDOC,SPACES                                                     
         OI    4(R2),X'20'                                                      
*                                                                               
ATR6     LA    R2,ATRURGH         VALIDATE URGENT                               
         CLI   5(R2),0                                                          
         BE    ATR6A                                                            
         CLI   8(R2),C'U'                                                       
         BE    ATR6A                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERROR                                                            
*                                                                               
ATR6A    OI    4(R2),X'20'        TURN ON VALIDATED BIT                         
         OC    SVCSHNAM(51),SVCSHNAM  VALIDATE CASH ACCT                        
         LA    R2,ATRCSHH                                                       
         CLI   5(R2),0                                                          
         BE    ATR8                                                             
*                                                                               
         ZIC   R3,5(R2)           LENGTH OF CASH ACCT                           
         BCTR  R3,0                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SC'    CASH U/L                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),8(R2)     CASH ACCT CODE                                
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         BAS   RE,CHECKACC        VERIFY ACCT                                   
*                                                                               
         MVC   SVCSHNUM,ACCTNUM   CASH ACCT #                                   
         MVC   SVCSHNAM,ACCTNAME  CASH ACCT NAME                                
         LA    R3,ATRCSDH                                                       
         OI    6(R3),X'80'                                                      
         MVC   8(36,R3),ACCTNAME  DISPLAY CASH ACCT EXPANSION                   
*                                                                               
         LA    R2,ATRCDH                                                        
         CLI   5(R2),0            IS THERE A PROD CD?                           
         BNE   ATR15B             YES - FLAG ERROR                              
*                                                                               
         LA    R2,ATRCD2H                                                       
         CLI   5(R2),0            IS THERE AN EXP CD?                           
         BNE   ATR15B             YES - FLAG ERROR                              
*                                                                               
ATR8     ZAP   DISC,=P'0'         INIT DISCOUNT SAVES                           
         ZAP   DISC2,=P'0'                                                      
         OC    SVCRACCT(102),SVCRACCT CLEAR CREDIT ACCOUNT SAVES                
         MVI   FRSTPASS,0         PASS INDICATION                               
*                                                                               
ATR9     LA    R2,ATRACCH         PRODUCTION VENDOR FIELD                       
         CLI   5(R2),0            IS THERE A PROD VENDOR?                       
         BE    ATR30              NO                                            
         MVI   ADVSW,C'Y'         SET PROD VENDOR SWITCH                        
*                                                                               
ATR10    ZIC   R3,5(R2)           =LEN OF CREDIT ACCOUNT INPUT                  
         BCTR  R3,0               MINUS 1 FOR EX INSTRUCTION                    
         LA    R1,8(R2)                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY     FROM TDA                                      
         CLI   8(R2),C'*'         UNIT LEDGER INPUT OVERRIDE                    
         BE    ATR11              YES                                           
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         CLI   ADVSW,C'Y'         IS IT PROD VENDOR?                            
         BNE   ATR10A             NO                                            
         MVC   KEY+1(2),ACMPSUPP  ASSUME UNLESS OVERRIDDEN                      
         B     ATR14                                                            
*                                                                               
ATR10A   MVC   KEY+1(1),=C'S'                                                   
         MVC   KEY+2(1),ACMPSUPX  ASSUME UNLESS OVERRIDDEN                      
         B     ATR14                                                            
         DROP  RF                                                               
*                                                                               
ATR11    CLI   SVCSHNUM,0         IS THERE A CASH ACCOUNT?                      
         BNE   ATR11A             YES - *SC NOT ALLOWED                         
         CLC   9(2,R2),=C'SC'     IS IT CASH ACCT?                              
         BE    ATR13                                                            
*                                                                               
ATR11A   MVC   FVMSGNO,=AL2(AE$INACP)                                           
         LA    RF,ADVCLIST        LIST OF OVERRIDES FOR PROD VEND               
         CLI   ADVSW,C'Y'         IS IT PRODUCTION VENDOR?                      
         BE    ATR12              YES                                           
         LA    RF,AGYCLIST        LIST OF OVERRIDES FOR EXP VEND                
*                                                                               
ATR12    CLI   0(RF),X'FF'        END OF LIST                                   
         BE    ERROR                                                            
         CLC   9(2,R2),0(RF)      INPUT TO LIST                                 
         BE    ATR13                                                            
         LA    RF,2(RF)           NEXT VALID ENTRY TO CREDIT                    
         B     ATR12                                                            
*                                                                               
ATR13    MVC   KEY+1(2),9(R2)     2 CHAR U/L FROM INPUT                         
         SH    R3,=H'3'           SUBTRACT *U/L FROM INPUT LEN                  
         LA    R1,11(R2)          POINT TO ACCT INPUT                           
ATR14    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R1)     CREDIT ACCT TO KEY                            
*                                                                               
ATR15    CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   ATR15A             NO                                            
         LA    R1,ATRCDH          CD FOR PROD VENDOR                            
         B     *+8                                                              
*                                                                               
ATR15A   LA    R1,ATRCD2H         CD FOR EXP VENDOR                             
         CLI   5(R1),0            INPUT                                         
         BE    ATR16              NO                                            
         CLI   SVCSHNUM,0         IS THERE A CASH ACCOUNT?                      
         BE    ATR15C             NO                                            
*                                                                               
ATR15B   MVC   FVMSGNO,=Y(AE$CDALL)   NO CASH DISCOUNT ALLOWED                  
         B     ERRXIT             ERROR                                         
*                                                                               
ATR15C   CLI   8(R1),C'N'         IS THERE CASH DISCOUNT                        
         BE    ATR16              NO GO GET ACCT                                
         CLI   8(R1),C'Y'                                                       
         BE    ATR16                                                            
         LR    R2,R1                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERROR                                                            
*                                                                               
ATR16    MVC   RKEY,KEY           READ VENDOR FOR CD AND OTHER INFO             
         GOTO1 ARDHI,AIOAREA1                                                   
         L     RE,AIOAREA1                                                      
         CLC   KEY(15),0(RE)      WAS REC RETRIEVED                             
         BE    ATR17                                                            
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     ERROR                                                            
*                                                                               
ATR17    LA    R0,KEY              MOVE RECORD TO LOCAL STORAGE                 
         LA    R1,1000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,IOAREA          FIND DISCOUNT ELEMENT                         
ATR18    CLI   0(R4),0                                                          
         BNE   ATR19              DIDN'T FIND DISCOUNT ELEMENT                  
         CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   ATR18A             NO                                            
         OI    ATRCDH+6,X'80'     PROD VENDOR CD                                
         MVI   ATRCD,C' '                                                       
         B     ATR21              SKIP CD FOR PROD VENDOR                       
*                                                                               
ATR18A   OI    ATRCD2H+6,X'80'    EXP VENDOR CD                                 
         MVI   ATRCD2,C' '                                                      
         B     ATR21              SKIP CD FOR EXP VENDOR                        
*                                                                               
ATR19    CLI   0(R4),X'38'        IS THIS A DIS ELEM?                           
         BE    ATR20              YES                                           
         CLI   0(R4),ITCELQ        TEST FOR INPUT TAX DEFAULT                   
         BE    ATR20C                                                           
ATR19A   ZIC   R3,1(R4)           LEN OF CURRENT EL                             
         AR    R4,R3              TO NEXT EL                                    
         B     ATR18                                                            
*                                                                               
ATR20    CLI   SVCSHNUM,0         TEST FOR CASH ACCOUNT                         
         BNE   ATR19A              YES-NO CASH DISCOUNT ALLOWED                 
         LA    RE,ATRCDH           SET POINTER TO CD FIELD                      
         CLI   ADVSW,C'Y'          TEST PROD OR EXP VENDOR                      
         BE    *+8                                                              
         LA    RE,ATRCD2H                                                       
         CLI   8(RE),C'N'          TEST CD SUPPRESSED                           
         BE    ATR19A              YES                                          
*                                                                               
         MVC   HALF,2(R4)         PLACE CD IN DISC SAVE FLD                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   ATR20A             NO                                            
         ZAP   DISC,DUB           CD VALUE FOR PROD VENDOR                      
         LA    R5,DISC                                                          
         LA    R1,ATRCDH                                                        
         LA    R6,ATRCDD                                                        
         B     ATR20B             OUTPUT CD                                     
*                                                                               
ATR20A   ZAP   DISC2,DUB          CD VALUE FOR EXP VENDOR                       
         LA    R5,DISC2                                                         
         LA    R1,ATRCD2H                                                       
         LA    R6,ATRCDD2                                                       
*                                                                               
ATR20B   OI    6(R1),X'80'                                                      
         MVI   8(R1),C'Y'                                                       
         EDIT  (P3,(R5)),(6,(R6)),2                                             
         B     ATR19A                                                           
*                                                                               
         USING ITCELD,R4                                                        
ATR20C   LA    RE,PVENTYPE         SET POINTER TO DEFAULT TAX                   
         CLI   ADVSW,C'Y'          TEST FOR PRODUCTION VENDOR                   
         BE    *+8                 YES                                          
         LA    RE,EVENTYPE                                                      
         CLC   SVDATE1,ITCEFFD     TEST TRANS DATE >= EFF DATE                  
         BL    ATR19A                                                           
         OC    ITCPROV,ITCPROV     NO PROV=GST                                  
         BZ    ATR20D                                                           
         MVC   VENDPSTT,ITCTYPE    SAVE PST TYPE & PROV                         
         MVC   VENDPROV,ITCPROV                                                 
         B     ATR19A                                                           
*                                                                               
ATR20D   MVC   0(1,RE),ITCTYPE     SET DEFAULT TYPE                             
         MVC   VENDTYPE,ITCTYPE                                                 
         B     ATR19A                                                           
*                                                                               
ATR21    SR    R6,R6              CLEAR FOR GETACC USE                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 AGETACC,DMCB,KEY,(R6) DONE MYSELF TO HANDLE DFLT                 
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERROR                                                            
*                                                                               
ATR22    MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'     BALANCE ELEMENT                               
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'30'     LOCKED OR CLOSED                              
         BNZ   ERROR                                                            
*                                                                               
ATR23    CLI   USERPROV,C'Y'       DID USER ENTERED PROVINCE?                   
         BE    ATR23H                                                           
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   ATR23H                                                           
         OC    VENDPROV,VENDPROV                                                
         BZ    ATR23H                                                           
         L     R2,APROVH                                                        
         MVI   5(R2),2                                                          
         OI    6(R2),X'80'         MODIFIED                                     
         L     RF,BCAUTL                                                        
         TM    TSTAT6-UTLD(RF),TST6STRO        STEREO?                          
         BZ    ATR23A                                                           
         CLC   VENDPROV,SPACES                                                  
         BNH   ATR23H                                                           
ATR23A   MVC   8(2,R2),VENDPROV                                                 
*                                                                               
ATR23H   CLI   ADVSW,C'Y'         IS THIS A PROD VENDOR?                        
         BNE   ATR28              NO                                            
         CLI   ATRACCH+5,0        WAS PROD VENDOR INPUT?                        
         BE    ATR30              NO                                            
         MVC   SVCRACCT,ACCTNUM   SAVE PROD VEN KEY                             
         MVC   SVACCTNM,ACCTNAME  SAVE PROD VEN NAME                            
         MVC   PRSTAT,ACCTSTAT    PRO 2C BIT(X'04')                             
*                                                                               
ATR24    DS    0H                                                               
         CLI   SVCSHNUM,0         IS THERE A CASH ACCT?                         
         BE    ATR24A             NO - DON'T WANT 2C OR 27                      
         TM    PRSTAT,X'04'       DO WE NEED 2C FOR PROD VENDOR?                
         BZ    ATR24A             NO.                                           
         MVC   KEY(15),SVCRACCT   ACCT #                                        
         MVC   KEY+1(2),=C'2C'    U/L                                           
         BAS   RE,GETACC                                                        
         MVI   PCONSULT,C'Y'      SET 2C SWITCH                                 
         MVC   P2CNUM,ACCTNUM                                                   
         MVC   P2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'  BUILD 27 ACCT                            
         BAS   RE,GETACC                                                        
         MVC   PROTROL,ACCTNUM                                                  
         MVC   PROTROLN,ACCTNAME                                                
*                                                                               
ATR24A   LA    R3,ATRACDH                                                       
         OI    6(R3),X'80'        SET TRANSBIT                                  
         MVC   8(36,R3),SVACCTNM  PRODUCTION NAME                               
         B     ATR30                                                            
         EJECT                                                                  
*                                                                               
ATR28    CLI   ATRACC2H+5,0       WAS EXP VENDOR INPUT?                         
         BE    ATR30                                                            
         MVC   SVCRACC2,ACCTNUM   EXP VENDOR ACCT #                             
         MVC   SVACCTN2,ACCTNAME  EXP VENDOR NAME                               
         MVC   EXSTAT,ACCTSTAT    EXP 2C BIT (X'04')                            
         MVI   V29SW,0                                                          
         MVC   KEYSAVE(15),KEY                                                  
         MVC   KEY(15),SVCRACC2   EXP VENDOR.                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',KEY,KEY .                
         MVC   FVMSGNO,=AL2(AE$NOERR)                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   BADRTRN                                                          
         LA    R1,IOAREA                                                        
         SR    RE,RE                                                            
*                                                                               
ATR28A   CLI   0(R1),0            IS IT END OF RECORD?                          
         BE    ATR28Z                                                           
         CLI   0(R1),ACSTELQ      IS IT X'30'?                                  
         BE    ATR28B                                                           
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     ATR28A                                                           
*                                                                               
         USING ACSTATD,R1         STATUS DSECT                                  
ATR28B   TM    ACSTSTX,X'80'      DO WE NEED VENDOR AS CONTRA?                  
         BZ    ATR28Z             NO.                                           
ATR28C   MVI   V29SW,C'Y'         YES.                                          
*                                                                               
ATR28Z   MVC   KEY(15),KEYSAVE    SAVE KEY                                      
*                                                                               
ATR29    CLI   SVCSHNUM,0         IS THERE A CASH ACCT?                         
         BE    ATR29Z             NO - DON'T WANT 2C OR 27                      
         TM    EXSTAT,X'04'       DO WE NEED 2C FOR EXP VENDOR?                 
         BZ    ATR29Z             NO.                                           
*                                                                               
         MVC   KEY(15),SVCRACC2                                                 
         MVC   KEY+1(2),=C'2C'                                                  
         BAS   RE,GETACC                                                        
         MVI   ECONSULT,C'Y'                                                    
         MVC   E2CNUM,ACCTNUM                                                   
         MVC   E2CNAM,ACCTNAME                                                  
         MVC   KEY+1(14),=CL14'27999'   BUILD 27                                
         BAS   RE,GETACC          GET ACCT                                      
         MVC   EXPTROL,ACCTNUM                                                  
         MVC   EXPTROLN,ACCTNAME                                                
*                                                                               
ATR29Z   DS    0H                                                               
         LA    R3,ATRAC2DH                                                      
         OI    6(R3),X'80'         SET TRANS                                    
         MVC   8(36,R3),SVACCTN2   DISPLAY EXP VEND EXPANSION                   
*                                                                               
ATR30    CLI   FRSTPASS,0          IS THIS THE SECOND PASS                      
         BNZ   ATR31               YES                                          
         LA    R1,ATRACC2H                                                      
         CLI   5(R1),0             IS THERE AN EXP VENDOR?                      
         BE    ATR31               NO                                           
         LR    R2,R1                                                            
         MVI   ADVSW,0                                                          
         MVI   AGYSW,C'Y'          YES - WE HAVE EXP VENDOR                     
         MVI   FRSTPASS,1          FINISHED 1ST PASS                            
         B     ATR10               NOW VERIFY EXP VENDOR                        
*                                                                               
ATR31    TM    COMPSTA2,X'04'      IS VENDOR REQUIRED?                          
         BZ    ATR35               NO                                           
         CLI   SVCRACCT,0          WAS PROD VENDOR ENTERED?                     
         BNE   ATR35               YES                                          
         CLI   SVCRACC2,0          WAS EXP VENDOR ENTERED?                      
         BNE   ATR35               YES                                          
         LA    R2,ATRACCH          POINT TO PROD VENDOR FIELD                   
         MVC   FVMSGNO,=Y(AE$PEVIR)  MUST HAVE AT LEAST 1 VENDOR                
         B     ERRXIT              ERROR                                        
*                                                                               
ATR35    MVI   FRSTPASS,1          NOTE SECOND PASS                             
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   *+8                                                              
         MVI   FRSTPASS,0          YES-USE TWO PASS METHOD                      
         EJECT                                                                  
*                                                                               
ATR40    LA    RE,AMTTAB           SET AMOUNT TABLE POINTER                     
         ST    RE,AAMT                                                          
         CLI   AGYCTRY,CTRYCAN     TEST CANADA                                  
         BNE   ATR40X              NO                                           
*                                                                               
         L     RE,AIOA                                                          
         ST    RE,APOST                                                         
*                                                                               
         BAS   RE,BLDAMTS          BUILD AMOUNTS                                
         BNE   BADRTRN                                                          
*                                                                               
         MVC   FLD,SPACES                                                       
         L     R2,ATYPNH                                                        
         BAS   RE,MOVEFLD          CLEAR PROTECTED GST FIELDS                   
         OI    6(R2),X'80'                                                      
         L     R2,AGSTXH                                                        
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
ATR40A   CLI   FRSTPASS,1          CALL GST EDIT ON SECOND PASS                 
         BNE   ATR40X                                                           
*                                                                               
         CLI   PFKEY,7                                                          
         BNE   ATR40A1                                                          
         CLI   MODE,1                                                           
         BH    ATR40A1                                                          
         B     PREP90                                                           
*                                                                               
ATR40A1  BAS   RE,EDTAX                                                         
         BE    ATR40B                                                           
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERROR                                                            
*                                                                               
ATR40B   MVI   MODE,4                                                           
         MVC   MSG,SPACES                                                       
         GOTO1 ACTAXMOD,DMCB,(RC)                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   ATR40C                                                           
         MVI   FVOMTYP,C'E'                                                     
         MVC   FVMSGNO,CTXMSGNO                                                 
         L     R2,FVADDR                                                        
         B     ERROR                                                            
*                                                                               
ATR40C   ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
         ZAP   TOTGST,TMPGST                                                    
         ZAP   TOTPST,TMPPST                                                    
*                                                                               
ATR40X   L     R2,AWCH             POINT TO FIRST WORK CODE FLD                 
         USING SCRNCED,R2          MY SCREEN DSECT                              
*                                                                               
ATR41    L     R3,ATABH            BOTTOM OF SCREEN                             
         ST    R2,SVCURLN          SAVE A(START OF DTL LINE)                    
         LA    R1,WCEXLN(R2)       POINT TO CURRENT EXPENSE FLD                 
         LA    R6,6                CHECK 6 FLDS ON LINE FOR INPUT               
         LR    R4,R2                                                            
*                                                                               
ATR42    TM    4(R4),X'20'         VALIDATED BEFORE?                            
         BZ    ATR43               NO, NEW INPUT OR BLANK LINE                  
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   R6,ATR42                                                         
*                                                                               
         MVC   AMNTD+8(11),SPACES                                               
         OI    AMNTD+6,X'80'                                                    
*                                                                               
         LA    R2,LNTOLN(R2)       ADVANCE TO NEXT DTL LINE                     
         CR    R2,R3               ARE WE AT BOTTOM                             
         BNE   ATR41                                                            
         B     ATR48                                                            
*                                                                               
ATR43    CLI   5(R1),0             ANY INPUT IN DEBIT ACCOUNT                   
         BNE   ATR50               YES, GO EDIT                                 
         LA    R0,6                SCAN THIS LINE FOR INPUT WITHOUT EXP         
*                                                                               
ATR44    CLI   5(R2),0                                                          
         BE    ATR45                                                            
         LR    R2,R1               CURSOR POS                                   
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     ERROR                                                            
*                                                                               
ATR45    ZIC   R4,0(R2)                                                         
         AR    R2,R4               NEXT FLD                                     
         BCT   R0,ATR44                                                         
         L     R2,SVCURLN          RESTORE                                      
         LA    R2,LNTOLN(R2)       TO NEXT LINE                                 
         CR    R2,R3               BOTTOM OF SCREEN                             
         BE    ATR48               YES                                          
*                                                                               
ATR46    TM    1(R2),X'20'         SCAN REMAINDER OF FIELDS                     
         BO    ATR47                                                            
         CLI   5(R2),0                                                          
         BE    ATR47                                                            
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     ERROR                                                            
*                                                                               
ATR47    ZIC   R4,0(R2)                                                         
         AR    R2,R4                                                            
         CR    R2,R3                                                            
         BNE   ATR46                                                            
*                                                                               
ATR48    CLI   ACTV,C'Y'           WERE FIELDS PREV VALIDATED?                  
         BE    ATR60                                                            
         L     R2,AWCH                                                          
         L     R3,ATABH                                                         
         TWAXC (R2),(R3),PROT=Y                                                 
         L     R2,AWCH             NO DETAIL INPUT FOR THIS ENTER               
         MVC   FVMSGNO,=Y(AE$DEBIT)  PLEASE INPUT DEBIT INFORMATION             
         B     ERRXIT                                                           
         EJECT                                                                  
*                                                                               
ATR50    ZAP   SVAMNT,=P'0'        INIT AMOUNT FIELDS                           
         ZAP   CDAMNT,=P'0'                                                     
         XC    ADVSW,ADVSW         CLEAR EXPENSE TYPE INDICATORS                
         XC    AGYSW,AGYSW                                                      
         XC    COMSW,COMSW         CLEAR COMMISSION INDICATOR                   
***************************************************************                 
*DETERMINE BILLABLE NON/BILLABLE EXPENSE                      *                 
*BILLABLE=EXPENSES CHARGED TO ADVERTISERS                     *                 
*NON/BILL=EXPENSES INCURRED BY AGENCY                         *                 
*ANALYSE WORK CODE FLD. FOR INPUT-PRESENCE INDICATES ADV. EXP.*                 
*NO INPUT INDICATES AGY. EXP.                                 *                 
***************************************************************                 
         ST    R1,SAVEEXP          SAVE A(CURRENT EXPENSE FLD HEADER)           
         LA    R3,WCWCLN(R2)       POINT R3 TO 2ND W.C.                         
         LA    R1,SCELN(R2)        CLEAR DISPLAY FLD FOR CURRENT LN             
         OI    6(R1),X'80'                                                      
         MVC   8(75,R1),SPACES                                                  
         DROP  R2                                                               
*                                                                               
*        DETERMINE COMMISSSIONABLE OR NOT BY COLUMN INPUT                       
*                                                                               
         CLC   8(2,R2),=C'99'      W/C 99 NOT ALLOWED                           
         BE    ATR51                                                            
         CLC   8(2,R3),=C'99'      W/C 99 NOT ALLOWED                           
         BNE   ATR52                                                            
         LA    R2,0(R3)            SET CURSOR FOR ERROR POSITION                
ATR51    MVC   FVMSGNO,=Y(AE$WC99)                                              
         B     ERRXIT                                                           
*                                                                               
ATR52    CLI   5(R2),0             INPUT IN 1ST COLM=COMMISSIONABLE             
         BE    ATR54               NO                                           
         CLI   5(R3),0             INPUT IN BOTH MUTUALLY EXCLUSIVE             
         BE    ATR53                                                            
         MVC   FVMSGNO,=Y(AE$MEWC)                                              
         B     ERRXIT                                                           
*                                                                               
ATR53    MVI   COMSW,C'Y'          INDICATE COM                                 
         LR    R3,R2                                                            
         B     ATR55                                                            
*                                                                               
ATR54    CLI   5(R3),0             INPUT IN 2ND W.C.                            
         BE    ATR56               NO INPUT = EXP. VENDOR PROCESS               
*                                                                               
ATR55    LA    R2,ATRACCH                                                       
         CLI   5(R2),0             IS THERE AN PROD VENDOR?                     
         BNE   ATR55A              YES                                          
         TM    COMPSTA2,X'04'      IS PROD VEND REQUIRED?                       
         BNZ   ATR56A              YES - FLAG ERROR                             
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BE    ATR56A              NO - NEED PROD VENDOR TRAP ERROR             
*                                                                               
ATR55A   ST    R3,SAVEWC           A(WORK CODE INPUT)                           
         MVI   ADVSW,C'Y'          INDICATE PRODUCTION EXP                      
         BAS   RE,ADVEXP           YES, EDIT LINE FOR ADV. EXP.                 
         BZ    ATR58               GOOD RETURN SET PVALS FOR LINE               
         L     R2,CURSPOS          POSITION CURSOR ON ERROR RETURN              
         B     ERROR                                                            
*                                                                               
ATR56    LA    R2,ATRACC2H                                                      
         CLI   5(R2),0             IS THERE AN EXP VENDOR?                      
         BNE   ATR57               YES                                          
         TM    COMPSTA2,X'04'      IS EXP VENDOR REQUIRED?                      
         BNZ   ATR56A              YES - FLAG ERROR                             
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT                         
         BNE   ATR57               YES                                          
*                                                                               
ATR56A   MVC   FVMSGNO,=Y(AE$VMEXP)  TRAP ERROR                                 
         B     ERRXIT                                                           
*                                                                               
ATR57    MVI   AGYSW,C'Y'          INDICATE AGY ACCRUED EXP.                    
         BAS   RE,AGYEXP           EDIT LINE FOR AGY. EXP.                      
         BZ    ATR58               GOOD RETURN SET PVALS FOR LINE               
         L     R2,CURSPOS          POSITION CURSOR ON ERROR RETURN              
         B     ERROR                                                            
*                                                                               
ATR58    CLI   FRSTPASS,0          ONLY TURN BITS ON DURING SECOND PASS         
         BE    ATR59A                                                           
         LA    R0,6                                                             
         L     R1,SVCURLN          SET PVALS FOR LINE JUST PROCESSED            
ATR59    OI    4(R1),X'20'                                                      
         ZIC   R3,0(R1)                                                         
         AR    R1,R3               NEXT FLD                                     
         BCT   R0,ATR59                                                         
ATR59A   MVI   ACTV,C'Y'           INDICATE ACTIVITY ON DETAIL LINE             
         L     R2,SVCURLN                                                       
         LA    R2,LNTOLN(R2)       BUMP AHEAD TO NEXT LINE                      
         C     R2,ATABH            TEST UP TO TAB FIELD                         
         BL    ATR41               YES                                          
*                                                                               
ATR60    CLI   FRSTPASS,1          TEST SECOND PASS LOGIC DONE                  
         BE    ATR62               YES                                          
         MVI   FRSTPASS,1          NO-RESET FOR IT                              
         B     ATR40                                                            
*                                                                               
ATR62    MVC   FLD,SPACES                                                       
         L     R2,ATOTH                                                         
         MVC   FLD(11),=C'BATCH TOTAL'                                          
         LA    R3,FLD+12                                                        
         EDIT  (P6,CSHTOT),(13,(R3)),2,MINUS=YES                                
         GOTO1 SQUASHER,DMCB,FLD,L'FLD                                          
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'         SEND IT BACK                                 
*                                                                               
         LA    R2,ATRDOCH                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
         MVI   ACTV,C'N'           INDICATE DATA DISPLAYED ONCE                 
         B     EXIT                RETURN TO BASE                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SUB-ROUTINE TO EDIT TAX FIELDS AND DISPLAY DATA                        
*        ON EXIT, CC=EQ IF OK, NEQ IF ERROR AND R2 SET TO ERR FLD               
*--------------------------------------------------------------                 
EDTAX    NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         L     R2,ATYPEH                                                        
         CLI   8(R2),C'*'          GST/PST NOT APPLICABLE                       
         BE    EDTAXX                                                           
*                                                                               
         L     R2,AGORNH           POINT TO GROSS/NET                           
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         CLI   5(R2),0                                                          
         BE    EDTAXX                                                           
*                                                                               
         MVC   GORN,8(R2)                                                       
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         CLI   GORN,C'G'                                                        
         BE    *+12                                                             
         CLI   GORN,C'N'                                                        
         BNE   EDTAXX                                                           
*                                                                               
         MVI   CTXMODE,C'G'                                                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   CTAXBEF,C'Y'        HAVE TO HAVE USE CTAX SCREEN                 
         BNE   EDTAXX                                                           
*                                                                               
EDTAX50  ZAP   TOTNET,TMPNET                                                    
         ZAP   TOTGRS,TMPGRS                                                    
*                                                                               
EDTAXX   CLC   FVMSGNO,=AL2(FVFOK)  S-RESET VENDOR AMT=GROSS                    
XITR2    XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                                                                               
ADVEXP   NTR1                      VALIDATE C/P/J INPUT                         
         L     R2,SAVEEXP          A(CURRENT EXPENSE FLD)                       
         CLI   8(R2),C'*'          CHECK FOR REPEAT FEATURE                     
         BNE   ADV0                                                             
         BAS   RE,DUPRTN           REPEAT FEATURE ROUTINE                       
*                                                                               
         USING ACCOMPD,RF                                                       
ADV0     LA    RF,COMPEL                                                        
         MVC   KEY(42),SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),ACMPJOB    U/L FOR CLI/PRD/JOB                          
         LA    R6,KEY+3            R4=CLIENT POSITION                           
         DROP  RF                                                               
*                                                                               
         SR    R3,R3               ACCUM CLIENT INPUT LENGTH IN R3              
         LA    R4,8(R2)            R4 POINTS TO EXPENSE INPUT                   
         LA    R1,10               MAXIMUM CLIENT CODE LENGHT                   
ADV1     CLI   0(R4),C','          SCAN FOR DILIMETER                           
         BE    ADV2                                                             
         LA    R3,1(R3)            ACCUM LENGTH OF CLIENT INPUT                 
         LA    R4,1(R4)            BUMP A(INPUT)                                
         BCT   R1,ADV1                                                          
CLTERR   MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV2     CH    R1,=H'10'           DID INPUT BEGIN WITH A COMMA                 
         BE    CLTERR              YES                                          
         BCTR  R3,0                FOR EX INST.                                 
         EX    R3,ADVMVC                                                        
         B     ADV3                                                             
ADVMVC   MVC   0(0,R6),8(R2)       CLT CODE TO KEY                              
*                                                                               
ADV3     DS    0H                                                               
*                                                                               
ADV6     XC    CLIPROF,CLIPROF     CLEAR CLIENT PROFILE FLD                     
         LA    R6,CLIPROF                                                       
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'10'                                                   
         BO    BADRTRN                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         OC    TWAACCS,TWAACCS     TEST LIMIT ACCESS                            
         BZ    ADV7                                                             
         CLC   TWAACCS,SPACES                                                   
         BE    ADV7                                                             
         CLI   TWAACCS,C'*'        OFFICE -CODE                                 
         BE    ADV7                                                             
         CLI   TWAACCS,C'$'        AND LIST WILL BE CHECK BY BASE               
         BE    ADV7                                                             
         CLC   TWAACCS(2),KEY+3    2 CHARACTER CLIENT MATCH                     
         BNE   BADRTRN                                                          
*                                                                               
ADV7     MVC   SVCLINUM,KEY                                                     
         MVC   SVCLINM,ACCTNAME                                                 
*                                                                               
         LA    R6,KEY+3            EDIT PRODUCT                                 
         ZIC   R0,CLILNGTH         LEVA LENGTH                                  
         AR    R6,R0               POINT TO PRODUCT POSITION                    
         ZIC   R0,5(R2)            TOTAL LENGTH OF INPUT                        
         LA    R3,2(R3)            ADD 2 TO ADV LN FOR COMMA & EX INST.         
         SR    R0,R3               ANYMORE INPUT                                
         BNZ   ADV8                YES, R1=REMAINING LEN OF INPUT               
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV8     LA    R4,8(R2)            POINT TO INPUT                               
         AR    R4,R3               TO START POINT PROD INPUT                    
         LR    R5,R4               SAVE                                         
         LA    R1,0                ACCUM PROD LEN                               
         SH    R0,=H'2'            SUB. 1 CHAR. MIN. JOB CODE AND COMMA         
         BNP   ADV9A               NO JOB ERROR                                 
         ST    R0,FULL             SAVE                                         
ADV9     CLI   0(R5),C','          SCAN FOR DILIMETER BETWEEN PRD,JOB           
         BE    ADV10                                                            
         LA    R1,1(R1)            ACCUM LEN OF PRD INPUT                       
         LA    R5,1(R5)            BUMP A(INPUT)                                
         BCT   R0,ADV9                                                          
ADV9A    MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV10    ST    R5,FULL1            SAVE FOR START OF JOB INPUT                  
         L     R5,FULL             DID INPUT BEGIN WITH A COMMA                 
         CR    R0,R5                                                            
         BE    ADV9A               YES                                          
         BCTR  R1,0                FOR EX INST.                                 
         ST    R1,PRDLEN           SAVE LENGTH OF PRD INPUT FOR POSTING         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)       PRD CODE TO KEY                              
         XC    PRODPROF,PRODPROF                                                
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC           READ FOR PRD REC                             
         MVC   SVPRDNM,ACCTNAME    SAVE PRD NAME                                
         MVC   SVPRDNUM,KEY        SAVE PRD KEY                                 
*                                                                               
         LA    R6,KEY+3                                                         
         ZIC   R0,PRDLNGTH                                                      
         AR    R6,R0               POINT TO JOB LOCATION                        
         LA    R1,2(R1)            ADD FOR COMMA AND EX INST.                   
         AR    R3,R1               R3=CLT/PRD INPUT PLUS 2 COMMAS               
         ZIC   R1,5(R2)            =TOTAL INPUT                                 
         SR    R1,R3               =REMAINING INPUT                             
         BP    ADV11A                                                           
ADV11AA  MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     BADRTRN                                                          
*                                                                               
ADV11A   L     R5,FULL1            RESTORE A(JOB INPUT)                         
         LA    R5,1(R5)            POINT TO JOB CODE                            
         BCTR  R1,0                FOR EX INSTR.                                
         ST    R1,JOBLEN           SAVE LENGTH OF JOB INPUT FOR POSTING         
         CLC   JOBLEN,=F'5'                                                     
         BH    ADV11AA                                                          
         EX    R1,*+8              R1=REMAINING INPUT WHICH = JOBLEN            
         B     *+10                                                             
         MVC   0(0,R6),0(R5)       JOB CODE TO KEY                              
ADV12    XC    JOBPROF,JOBPROF     CLEAR                                        
         LA    R6,JOBPROF                                                       
         BAS   RE,GETACC           READ FOR JOB                                 
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'                                                   
         BZ    BADRTRN                                                          
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADRTRN                                                          
         MVC   FVMSGNO,=AL2(AE$ACTCL)                                           
         TM    ACCTSTAT,X'20'      CLOSED JOB?                                  
         BO    BADRTRN                                                          
         MVC   SVJOBNM,ACCTNAME                                                 
         MVC   SVJOBNUM,KEY                                                     
*                                                                               
         BAS   RE,PROFMERG         PICK UP ANALYSIS FILTER                      
         LA    R4,PROFILE          FROM A PROFILE                               
         USING ACPROFD,R4                                                       
         MVC   PSCLINUM,ACPRCOST   SAVE FOR STAFF POSTING                       
         MVC   CURROFFC,ACPROFFC                                                
         CLI   FRSTPASS,1          DON'T WANT DOUBLES                           
         BE    ADV13                                                            
         L     RF,AOFFC                                                         
         MVC   0(2,RF),ACPROFFC                                                 
         LA    RF,2(RF)                                                         
         ST    RF,AOFFC                                                         
         DROP  R4                                                               
*                                                                               
ADV13    MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         L     R2,SAVEWC           =A(CURRENT WORK CODE INPUT)                  
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   ADV13A                                                           
         BAS   RE,DUPRTN           EDIT FOR REPEAT                              
*                                                                               
ADV13A   CLI   5(R2),2             CHECK FOR 2 CHARACTER INPUT                  
         BNE   BADRTRN                                                          
*                                                                               
*CONFIRM THAT IT IS A BILLABLE WORK CODE FOR THIS CLT/PRD/JOB                   
*                                                                               
         LA    R4,SVJOBNUM                                                      
         GOTO1 ASETJOB,DMCB,(R4)                                                
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB ?                           
         BZ    *+18                NO                                           
         L     R2,SAVEEXP          YES, STOP THEM                               
         MVC   FVMSGNO,=AL2(AE$BXJOB)                                           
         B     BADRTRN                                                          
*                                                                               
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
         MVI   PASSCD,C'N'                                                      
         CLI   COCDPASS,C'N'       IS COMP. KEEPING ALL CD'S                    
         BE    *+10                YES                                          
         MVC   PASSCD,GOCLICD                                                   
         LA    RF,GOUWLIST                                                      
         BAS   RE,VALW01                                                        
         B     VALW03                                                           
         DROP  R4                                                               
*                                                                               
VALW01   LA    R0,6                                                             
VALW02   CLC   8(2,R2),0(RF)       CHK FOR MATCH ON NON-BILLABLE WC'S           
         BE    BADRTRN                                                          
         LA    RF,2(RF)                                                         
         BCT   R0,VALW02                                                        
         BR    RE                                                               
*                                                                               
*READ ANALYSIS RECORD FOR VALID WORK CODE                                       
*                                                                               
VALW03   GOTO1 AGETWC,8(R2)                                                     
         BNE   BADRTRN                                                          
         LA    R3,SVWRKNAM                                                      
         MVC   0(L'ACANDESC,R3),WORK                                            
         MVC   SVWRKCD(2),8(R2)                                                 
*                                                                               
*ADVANCE TO COINCIDING AMOUNT FLD CHECKING FOR NONAPPLICABLE INPUT              
*IN OFF/DEPT/STAFF AND CLI/PRD FIELDS                                           
*                                                                               
         L     R2,SAVEEXP          RESTORE R2 TO A(CURRENT EXP. FLD.)           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         LA    R0,2                                                             
VALW06   ZIC   R1,0(R2)            TOTAL LENGTH OF FIELD                        
         AR    R2,R1               NEXT FIELD                                   
         CLI   5(R2),0                                                          
         BNE   BADRTRN             NOT APPLICABLE                               
         BCT   R0,VALW06                                                        
*                                                                               
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         ZIC   R1,0(R2)            TOTAL LENGTH OF FIELD                        
         AR    R2,R1               TO AMOUNT FLD HEADER                         
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   VALW07                                                           
         BAS   RE,DUPRTN           EDIT FOR REPEAT                              
*                                                                               
VALW07   CLI   5(R2),0                                                          
         BE    BADRTRN             MISSING INPUT FIELD                          
         ST    R2,SAVEAMNT         SAVE A(CURRENT AMOUNT FLD)                   
*                                                                               
         ZIC   R0,5(R2)            LENGTH OF AMOUNT INPUT                       
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         CLI   0(R1),0                                                          
         BNE   BADRTRN                                                          
         ZAP   SVAMNT,4(8,R1)      SAVE CURRENT AMOUNT                          
         ZAP   NETPOST,SVAMNT                                                   
         ZAP   GRSPOST,SVAMNT      ALWAYS INIT WITH INPUT AMOUNT                
         CLI   FRSTPASS,1          TEST FOR SECOND PASS                         
         BNE   VALW09                                                           
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   VALW08                                                           
         CLI   GSTSW,C'Y'          TEST GST APPLICABLE                          
         BNE   VALW08                                                           
         L     R3,AAMT             GET A(AMOUNT TABLE ENTRY)                    
         USING AMTD,R3                                                          
         ZAP   GRSPOST,AMTGRS                                                   
         ZAP   NETPOST,AMTNET                                                   
         ZAP   GSTPOST,AMTGST                                                   
         DROP  R3                                                               
*                                                                               
VALW08   AP    CSHTOT,GRSPOST      ACCUM AMMOUNT                                
*                                                                               
VALW09   LA    RE,SVWRKCD                                                       
         LA    R4,SVJOBNUM                                                      
         GOTO1 ASETJOB,DMCB,(X'80',(R4)),(RE)                                   
         GOTO1 AOPTVAL                                                          
         BNE   BADRTRN             ERROR                                        
*                                                                               
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    VALW13              NO                                           
         MVC   FVMSGNO,=AL2(AE$AEEWC)                                           
         LA    R3,SVWRKCD                                                       
         GOTO1 AWRKVAL,DMCB,(R3)                                                
         BH    BADRTRN                                                          
*                                                                               
VALW13   CP    DISC,=P'0'          SAVED DISCOUNT                               
         BE    VALW14                                                           
         ZAP   DUB,NETPOST         CALCULATE CD                                 
         MP    DUB,DISC                                                         
         SRP   DUB,64-4,5          ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,DUB                                                       
*                                                                               
VALW14   ZAP   DUB,SVAMNT                                                       
         SP    DUB,CDAMNT                                                       
VALW15   CP    DISC,=P'0'          IS THERE CASH DISC                           
         BE    BLD                 NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   BLD                 YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         PASSING CD TO CLIENT                         
         BNE   BLD                 YES - REDUCE PAYABLE                         
*                                                                               
VALW16   DS    0H                                                               
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'           READ MEDIA RECORD                            
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),SVJOBNUM+9 1ST BYTE OF JOB IS MEDIA CODE                
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    VALW17                                                           
         MVC   FVMSGNO,=Y(AE$MMR)   MISSING MEDIA RECORD'                       
         B     BADRTRN                                                          
*                                                                               
VALW17   MVI   ELCODE,ACMDELQ                                                   
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACMEDIAD,R3                                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACMDCSHD),ACMDCSHD                                         
         CLC   ACMDCSHD,SPACES                                                  
         BH    VALW22                                                           
*                                                                               
VALW18   DS    0H                                                               
         MVC   KEY,SPACES          READ LEDGER RECORD                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SJ'     PRODUCTION LEDGER                            
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    VALW19                                                           
         MVC   FVMSGNO,=Y(AE$MSJL)                                              
         B     BADRTRN                                                          
*                                                                               
VALW19   MVI   ELCODE,ACLTELQ      GET COMMISSION ACCT FROM LEDGER REC          
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BNE   VALW20              USE DEFAULT OF SIMD                          
         USING ACLEDGD,R3                                                       
         CLC   ACLTCDAC,SPACES                                                  
         BNH   VALW20                                                           
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACLTCDAC                                               
         B     VALW22                                                           
*                                                                               
VALW20   MVC   KEY,SPACES          ELSE POST TO INCOME                          
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(4),=C'SIMD'                                                
*                                                                               
VALW22   L     R2,SAVEAMNT         A(CURRENT AMOUNT HEADER)                     
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SIMDAC,ACCTNUM                                                   
         MVC   SIMDACN,ACCTNAME                                                 
*                                                                               
BLD      CLI   FRSTPASS,1          TEST SECOND PASS LOGIC                       
         BNE   BLD1                NO                                           
         GOTO1 BLDELS,DMCB,(RC)    BUILD ELEMENTS FOR EDITED LINE               
BLD1     BAS   RE,DISPNMS          DISPLAY EXPANSIONS                           
         L     RE,AAMT             BUMP AMOUNT POINTER                          
         LA    RE,AMTLNQ(RE)                                                    
         ST    RE,AAMT                                                          
         CLI   GSTSW,C'Y'                                                       
         BNE   BLD1A                                                            
         L     RE,APOST            BUMP TO NEXT POSTINGS                        
         ZICM  R0,0(RE),2                                                       
         AR    RE,R0                                                            
         ST    RE,APOST                                                         
BLD1A    SR    R0,R0               SET GOOD CC                                  
RTRN     XMOD1                                                                  
BADRTRN  ST    R2,CURSPOS          ERROR CURSOR POSITION                        
         LTR   RB,RB               SET BAD CC                                   
         B     RTRN                                                             
*                                                                               
*                                                                               
DLIST    DS    0H U/L'S VALID TO DEBIT                                          
         DC    C'SASBSFSLSC'       GIDEON - CASH OK                             
         DC    X'FF'                                                            
*                                                                               
ADVCLIST DS    0H    PRD. U/L'S VALID TO CREDIT                                 
         DC    C'SXSWSYSBSA'       SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
*                                                                               
AGYCLIST DS    0H    EXP. U/L'S VALID TO CREDIT                                 
         DC    C'SVSWSYSBSASFSL'   SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        PREPARE FOR CTAX                                                       
*-------------------------------------------------------------                  
*                                                                               
BLDAMTS  NTR1                                                                   
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         SR    R6,R6               R6=# OF WORKCODES                            
         LA    R3,AMTTAB           BUILD AMOUNT TABLE                           
         USING AMTD,R3                                                          
         L     R4,AWCH                                                          
         USING SCRNCED,R4                                                       
         L     R5,ATABH            LAST FIELD                                   
*                                                                               
BLDAMT10 LA    R1,6                                                             
         LR    R2,R4                                                            
BLDAMT15 TM    4(R2),X'20'         VALIDATED BEFORE?                            
         BZ    BLDAMT50            NO, KEEP AMOUNT                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,BLDAMT15                                                      
         B     BLDAMT80            WHOLE LINE VALIDATED BEFORE, NEXT            
*                                                                               
BLDAMT50 LA    R2,AMNTD                                                         
         CLI   5(R2),0             AMOUNT?                                      
         BE    BLDAMT90                                                         
*                                                                               
         ZIC   R0,5(R2)                                                         
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   0(R1),0             TEST OK                                      
         BNE   BLDAMT99                                                         
*                                                                               
         ZAP   AMTNET,4(8,R1)      INIT AMOUNTS                                 
         ZAP   AMTGRS,=P'0'                                                     
         ZAP   AMTGST,=P'0'                                                     
         ZAP   AMTPST,=P'0'                                                     
*                                                                               
         LA    R6,1(R6)            INCREMENT COUNTER                            
         LA    R3,AMTLNQ(R3)                                                    
BLDAMT80 LA    R4,LNTOLN(R4)                                                    
         CR    R4,R5                                                            
         BL    BLDAMT10                                                         
*                                                                               
BLDAMT90 STC   R6,NAMTS                                                         
         SR    RE,RE                                                            
BLDAMT99 LTR   RE,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
*-------------------------------------------------------------                  
*        TOTAL AMOUNTS                                                          
*-------------------------------------------------------------                  
TOTAMTS  NTR1                                                                   
         LA    R3,AMTTAB                                                        
         L     R4,AWCH                                                          
         ZAP   TOTPROD,=P'0'                                                    
         ZAP   TOTEXPN,=P'0'                                                    
         SR    R6,R6                                                            
         IC    R6,NAMTS                                                         
*                                                                               
TOTA100  LA    RF,TOTPROD                                                       
         CLI   WC1D+5,0            ANY WORKCODE?                                
         BNZ   TOTA200                                                          
         CLI   WC2D+5,0                                                         
         BNZ   TOTA200                                                          
         LA    RF,TOTEXPN          HAS TO BE EXPENSE THEN                       
TOTA200  CLI   AGYCTRY,CTRYCAN                                                  
         BE    TOTA250                                                          
         AP    0(L'TOTPROD,RF),AMTNET                                           
         B     *+10                                                             
TOTA250  AP    0(L'TOTPROD,RF),AMTGRS                                           
         LA    R3,AMTLNQ(R3)                                                    
         LA    R4,LNTOLN(R4)                                                    
         BCT   R6,TOTA100                                                       
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        VALIDATE AGENCY ACCOUNT                                                
*-------------------------------------------------------------                  
*                                                                               
AGYEXP   NTR1                                                                   
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         L     R2,SAVEEXP          A(CURRENT EXPENSE FLD)                       
         MVC   KEY(42),SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   AGY05                                                            
         ZIC   RE,5(R2)                                                         
         CH    RE,=H'1'                                                         
         BNH   AGY0C               MORE THAN 2 INDICATES DEFAULT ACT            
*                                                                               
AGY0     DS    0H                                                               
         LA    RF,DLIST            A(LIST OF VALID U/L'S TO DEBIT)              
AGY00    CLI   0(RF),X'FF'         END OF LIST                                  
         BE    BADRTRN2                                                         
         CLC   9(2,R2),0(RF)       INPUT TO LIST                                
         BE    AGY0A                                                            
         LA    RF,2(RF)                                                         
         B     AGY00                                                            
*                                                                               
AGY0A    LR    RF,RE                                                            
         MVC   KEY+1(2),9(R2)      DEFAULT U/L TO KEY                           
         SH    RF,=H'3'            DEDUCT FOR *UL                               
         LTR   RF,RF                                                            
         BM    BADRTRN2                                                         
         LA    R5,11(R2)           A(REST OF DFLT ACCT)                         
         B     AGY06                                                            
*                                                                               
AGY0C    BAS   RE,DUPRTN           EDIT FOR REPEAT                              
         ZIC   RE,5(R2)            LEN FOR DFLT ACT                             
         CLI   8(R2),C'*'          CHK IF ITS DFLT ACT INPUT                    
         BE    AGY0                                                             
*                                                                               
AGY05    MVC   KEY+1(2),=C'SE'     U/L DEBIT                                    
         ZIC   RF,5(R2)            INPUT LEN                                    
         LA    R5,8(R2)                                                         
*                                                                               
AGY06    BCTR  RF,0                SUB 1 FOR EXECUTE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R5)      EXP ACC TO KEY                               
         SR    R6,R6               NO PROFILES                                  
         BAS   RE,GETACC                                                        
         MVC   POSTACC,ACCTNUM                                                  
         MVC   POSTACCN,ACCTNAME                                                
*                                                                               
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADRTRN2                                                         
         TM    ACCTSTAT,X'30'      LOCKED OR CLOSED                             
         BNZ   BADRTRN2                                                         
         MVI   STFSW,C'N'                                                       
         MVI   DEPSW,C'N'                                                       
         MVI   OFFSW,C'N'                                                       
         TM    COMPSTAT,X'20'      MANDATORY OFFICE                             
         BZ    AGY30                                                            
         MVI   OFFSW,C'Y'                                                       
AGY30    TM    ACCTSTAT,X'40'      PERSONAL EXPENSE (STAFF=Y)                   
         BZ    AGY35                                                            
         MVI   STFSW,C'Y'                                                       
AGY35    TM    ACCTSTAT,X'08'      DEPT EXPENSE (DEPT=Y)                        
         BZ    AGY40                                                            
         MVI   DEPSW,C'Y'                                                       
AGY40    MVC   COSTANAL(1),ACCTCOST   (ANALYSIS=)                               
*        OI    COSTBYTE,X'40'                                                   
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    AGY45                                                            
*                                                                               
         L     R2,SAVEEXP                                                       
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         ZIC   R6,5(R2)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BM    BADRTRN2            ERROR                                        
*                                                                               
AGY45    CLI   DEPSW,C'Y'          SET UP 2/8 ACCOUNT                           
         BNE   AGY50                                                            
         MVC   KEY+1(2),=C'28'                                                  
         SR    R6,R6               CLEAR FOR NO PROFILE                         
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   CRDSNUM,ACCTNUM                                                  
         MVC   CRDSNAME,ACCTNAME                                                
         B     AGY50                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SCAN OFFICE/DEPT/STAFF FIELD                                           
*--------------------------------------------------------------                 
*                                                                               
AGY50    DS    0H                  OFF/DEPT/STAFF                               
         ZIC   RF,0(R2)            POINT TO OFF/DEPT/STAFF                      
         AR    R2,RF                                                            
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   AGY55                                                            
         BAS   RE,DUPRTN           EDIT FOR REPEAT                              
*                                                                               
AGY55    DS    0H                                                               
         MVC   OFFICE,SPACES       INITIALIZE                                   
         MVC   DEPT,SPACES                                                      
         MVC   STAFF,SPACES                                                     
         XC    OFFICEL,OFFICEL                                                  
         XC    DEPTL,DEPTL                                                      
         XC    STAFFL,STAFFL                                                    
*                                                                               
         ST    R2,SAVEODS          SAVE ADDRESS OF OFF/DEPT/STAFF               
         XC    BLOCK(96),BLOCK                                                  
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),C',=,,'                              
         CLI   DMCB+4,3                                                         
         BH    BADRTRN2            MORE THAN 3 (OFF,DEPT,STAFF)                 
*                                                                               
         MVC   OFFICEL,BLOCK       SAVE OFFICE AND L'OFFICE ENTERED             
         MVC   OFFICE(L'OFFICE),BLOCK+12                                        
         OC    OFFICE,SPACES                                                    
         MVC   CURROFFC,OFFICE                                                  
         CLI   FRSTPASS,1          DON'T WANT DOUBLES                           
         BE    AGY58                                                            
         L     RF,AOFFC                                                         
         MVC   0(2,RF),OFFICE      INPUT TO OFFICE PRECEDES CLIENT'S            
         LA    RF,2(RF)                                                         
         ST    RF,AOFFC                                                         
*                                                                               
AGY58    MVC   DEPTL,BLOCK+32      SAVE DEPT AND L'DEPT ENTERED                 
         MVC   DEPT(L'DEPT),BLOCK+44                                            
         OC    DEPT,SPACES                                                      
*                                                                               
         MVC   STAFFL,BLOCK+64     SAVE STAFF AND L'STAFF ENTERED               
         MVC   STAFF(L'STAFF),BLOCK+76                                          
         OC    STAFF,SPACES                                                     
*                                                                               
         GOTO1 ERRCHK,DMCB,(RC)    CHECK INPUT TO OFF/DPT/STAFF                 
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   BADRTRN2                                                         
*                                                                               
         GOTO1 AVALOFFC,DMCB,(X'80',OFFICE)                                     
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    AGY80                                                            
         MVC   FVMSGNO,=Y(AE$IVOFF)                                             
         B     BADRTRN2                                                         
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SCAN CLI/PRO FIELD                                                     
*--------------------------------------------------------------                 
*                                                                               
AGY80    XC    COSTNUM,COSTNUM                                                  
         XC    CLIPRON,CLIPRON                                                  
         XC    DEPSTFN,DEPSTFN                                                  
         ZIC   RF,0(R2)            POINT TO CLI/PROD                            
         AR    R2,RF                                                            
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   AGY83                                                            
         BAS   RE,DUPRTN           EDIT FOR REPEAT                              
*                                                                               
AGY83    ST    R2,SAVCLPR          SAVE ADDRESS OF CLI/PRO                      
         XC    SCAN(64),SCAN                                                    
         CLI   5(R2),0             IF NOTHING THERE,DON'T BOTHER                
         BE    AGY90               WITH SCANNER                                 
         CLI   STFSW,C'Y'                                                       
         BE    AGY85                                                            
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BO    AGY85                                                            
         CLI   COSTANAL,C' '       IF NO STAFF OR COSTING,                      
         BNE   AGY85                                                            
         MVC   FVMSGNO,=Y(AE$ANFAN) INPUT TO CLIENT NOT ALLOWED                 
         B     ERRXIT                                                           
*                                                                               
AGY85    MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         GOTO1 SCANNER,DMCB,(R2),(4,SCAN),C',=,,'                               
         CLI   DMCB+4,2                                                         
         BH    BADRTRN2            MORE THAN CLI,PROD                           
         LA    R6,CLIPROF                                                       
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         MVC   KEY+1(48),SPACES                                                 
         MVC   KEY+1(2),ACMPJOB    PRODUCTION LEDGER                            
         DROP  RF                                                               
         ZIC   R3,SCAN             LENGTH OF CLI INPUT                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),SCAN+12    CLIENT TO KEY                                
         BAS   RE,GETACC                                                        
         MVC   CLIPRON,ACCTNAME                                                 
         MVC   CLIPRO,ACCTNUM                                                   
*                                                                               
         LA    R1,FFTELEM          BUILD FFTEL                                  
         USING FFTELD,R1                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,CLIPRO+3                                                 
         OC    FFTCLPRA,SPACES                                                  
         DROP  R1                                                               
                                                                                
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         CLI   SCAN+32,0           IS PRODUCT INPUT                             
         BE    AGY87                                                            
         LA    R4,KEY+3                                                         
         IC    R3,CLILNGTH         LEVEL A LENGTH                               
         AR    R4,R3                                                            
         ZIC   R3,SCAN+32          PRODUCT LENGTH                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SCAN+44     PRODUCT                                      
         LA    R6,PRODPROF                                                      
         BAS   RE,GETACC                                                        
         MVC   CLIPRON,ACCTNAME                                                 
         MVC   CLIPRO,ACCTNUM                                                   
                                                                                
         LA    R1,FFTELEM          UPDATE FFTEL                                 
         USING FFTELD,R1                                                        
         MVC   FFTPRAC,CLIPRO+6                                                 
         OC    FFTCLPRA,SPACES                                                  
         DROP  R1                                                               
                                                                                
AGY87    BAS   RE,PROFMERG                                                      
         SR    R6,R6               NO MORE PROFILES                             
         LA    R4,PROFILE          FIND 1/C ACCOUNT                             
         USING ACPROFD,R4                                                       
         MVC   COSTNUM,ACPRCOST                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),ACPRCOST                                                 
         BAS   RE,GETACC                                                        
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADACC                                                           
         TM    ACCTSTAT,X'10'      LOCKED                                       
         BO    BADACC                                                           
         MVC   COSTNAME,ACCTNAME                                                
         DROP  R4                                                               
*                                                                               
AGY90    LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BO    AGY95                                                            
         CLI   COSTANAL,C' '       IS ANALYSIS REQUIRED (ANALYSIS=)             
         BE    AGY150              NO                                           
         CLI   SCAN,0              IF ANALYSIS REQUIRED,                        
         BNE   AGY95               CLIENT IS REQUIRED                           
         CLI   STFSW,C'Y'                                                       
         BNE   AGY95                                                            
         MVC   FVMSGNO,=Y(AE$ICLPQ)                                             
         B     ERRXIT                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 1P ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
AGY95    DS    0H                                                               
         XC    CR13NUM,CR13NUM                                                  
         L     R2,SAVEODS          CURSOR TO OFFICE/DEP/STF FIELD               
*                                                                               
         TM    COMPSTAT,X'10'                                                   
         BNO   *+8                                                              
         OI    COSTSW,COSTACC                                                   
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   AGY96                                                            
         MVI   COSTSW,0                                                         
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         MVC   CATDMGR,DATAMGR     BUILD CONTROL BLOCK                          
         MVC   CATSEAC,POSTACC     DEBIT ACCOUNT                                
         MVC   CATOFF,OFFICE       OFFICE                                       
         MVC   CATDPT,DEPT         DEPARTMENT                                   
         GOTO1 VCATCALL,CATD                                                    
         L     R2,SAVEEXP          POINT TO CURRENT EXPENSE ACCOUNT             
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),CATACC3                                                  
         CLI   CATERR,0                                                         
         BE    AGY95A                                                           
         MVC   FVMSGNO,=AL2(AE$IANAL)                                           
         B     ERROR                                                            
*                                                                               
AGY95A   CLI   CATPST,C'N'         NO COST POSTING                              
         BE    AGY96                                                            
         OI    COSTSW,COSTACC+COSTNEW                                           
         MVC   COSTANAL,CATCDE                                                  
         MVC   CR13NUM,CATACC3     SAVE 13 ACCOUNT                              
*                                                                               
AGY96    CLI   COSTANAL,X'40'                                                   
         BE    *+12                IF NO COSTING REQUIRED                       
         TM    COSTSW,COSTACC                                                   
         BO    AGY96C                                                           
         CLI   STFSW,C'Y'          OR STAFF IS NOT REQUIRED                     
         BE    AGY125                                                           
         L     RE,SAVCLPR                                                       
         CLI   5(RE),0             THEN CLIENT INPUT NOT ALLOWED                
         BE    AGY96A                                                           
         L     R2,SAVCLPR                                                       
         B     AGY96B                                                           
*                                                                               
AGY96A   L     R2,SAVCLPR          AND PRODUCT INPUT NOT ALLOWED                
         CLI   5(R2),0                                                          
         BE    AGY125                                                           
*                                                                               
AGY96B   MVC   FVMSGNO,=Y(AE$ANFAN)  ACCT NOT FLAGGED FOR ANALYSIS              
         B     ERRXIT                                                           
*                                                                               
AGY96C   OI    COSTSW,COSTACC      COST ACCOUNTING                              
         L     R2,SAVCLPR                                                       
         CLI   FVILEN-FVIHDR(R2),0                                              
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     ERRXIT                                                           
*                                                                               
* SAME STUPID TEST REGARDLESS                                                   
*        L     R2,SAVCLPR                                                       
*        LA    RF,BCCPYEL          TEST PRODUCT REQUIRED FOR EXPENSE            
*        TM    BCCPYST5-BCCPYEL(RF),CPYSEXPP EXPENSE PRODUCT                    
*        BNO   *+8                                                              
*        BAS   RE,ANY                                                           
         L     R2,SAVEODS                                                       
         MVC   KEY+1(14),SPACES                                                 
         MVC   KEY+1(2),=C'1P'    1P/OF/DPT/CATEGORY                            
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   AGY99                                                            
         MVC   KEY+3(12),=C'999999999999' SAYS VANESSA                          
         B     AGY110                                                           
*                                                                               
*                                                                               
AGY99    MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1P'     SET UP 1/P ACCOUNT                           
         LA    RF,KEY+3                                                         
*                                                                               
         CLI   OFFSW,C'N'                                                       
         BE    AGY100                                                           
         ZIC   R1,OFCLNGTH                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),OFFICE      OFFICE                                       
         LA    RF,1(R1,RF)                                                      
*                                                                               
AGY100   DS    0H                                                               
         LA    R3,=C'9999'         DEFAULT DEPT                                 
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                                                              
         LA    R3,DEPT             OR REAL DEPT                                 
*                                                                               
         ZIC   R1,DPTLNGTH                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)                                                    
         LA    RF,1(R1,RF)                                                      
         MVC   0(1,RF),COSTANAL                                                 
*                                                                               
AGY110   BAS   RE,GETACC                                                        
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADACC                                                           
         MVC   CRCNUM,ACCTNUM                                                   
         MVC   CRCNAME,ACCTNAME                                                 
         B     AGY125                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 13 ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
AGY125   DS    0H                                                               
         OC    CR13NUM,CR13NUM                                                  
         BNZ   AGY150                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'13'                                                  
         MVC   KEY+3(1),COSTANAL                                                
         BAS   RE,GETACC                                                        
         MVC   CR13NUM,ACCTNUM                                                  
         MVC   CR13NAME,ACCTNAME                                                
         B     AGY150                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 2D ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
AGY150   DS    0H                                                               
         CLI   DEPSW,C'N'          NO 2D POSTING IF SWITCH NOT=Y                
         BE    AGY180                                                           
*                                                                               
         L     R2,SAVEODS          POINT TO OFF/DEPT/STAFF                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2D'                                                  
         LA    R1,KEY+3                                                         
*                                                                               
         CLI   OFFSW,C'Y'          OFFICE IN ACCT                               
         BNE   AGY155                                                           
         ZIC   RF,OFCLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),OFFICE      OFFICE                                       
         LA    R1,1(RF,R1)                                                      
*                                                                               
AGY155   DS    0H                                                               
         ZIC   RF,DPTLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT        DEPARTMENT                                   
*                                                                               
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   DEPNAME,ACCTNAME                                                 
         MVC   DEPNUM,ACCTNUM                                                   
         MVC   DEPSTFN,ACCTNAME                                                 
         B     AGY180                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 2P ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
AGY180   DS    0H                                                               
         CLI   STFSW,C'Y'                                                       
         BNE   AGYAMNT                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2P'                                                  
         LA    R1,KEY+3                                                         
*                                                                               
         CLI   LEVEL,3             ONLY MOVE OFFICE FOR 3 LEVEL ACCTS           
         BL    AGY186                                                           
         ZIC   RF,OFCLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),OFFICE      MOVE OFFICE INTO KEY                         
         LA    R1,1(RF,R1)         BUMP TO NEXT SPOT IN KEY                     
*                                                                               
AGY186   DS    0H                                                               
         CLI   LEVEL,2                                                          
         BL    AGY188                                                           
         ZIC   RF,DPTLNGTH                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),DEPT                                                     
         LA    R1,1(RF,R1)                                                      
*                                                                               
AGY188   DS    0H                                                               
         CLI   STAFFL,0                                                         
         BE    AGY190                                                           
         ZIC   RF,STAFFL                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),STAFF       MOVE STAFF INTO KEY                          
*                                                                               
AGY190   SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   STAFFNUM,ACCTNUM                                                 
         MVC   STAFFNAM,ACCTNAME                                                
         MVC   DEPSTFN,ACCTNAME                                                 
         B     AGY200                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 29 ACCOUNT                                                       
*--------------------------------------------------------------                 
*                                                                               
AGY200   DS    0H                                                               
         ZIC   RF,0(R2)            POINT TO CLI/PROD                            
         AR    R2,RF                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'29'     SET UP 2/9 ACCOUNT                           
         MVC   KEY+3(3),=C'999'                                                 
*                                                                               
         LA    RF,BCCPYEL                                                       
         TM    BCCPYST5-BCCPYEL(RF),CPYSNCST TEST NEW COSTING                   
         BNO   AGY205                                                           
         MVC   KEY+3(12),=C'999999999999'                                       
         B     AGY210                                                           
*                                                                               
AGY205   CLI   OFFSW,C'Y'                                                       
         BNE   AGY210                                                           
         MVC   KEY+3(46),SPACES                                                 
         ZIC   R1,OFCLNGTH                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),OFFICE                                                  
         LA    RF,KEY+3                                                         
         LA    RF,1(R1,RF)                                                      
         MVC   0(4,RF),=C'9999'    MOVE IN 9999 AS DEFAULT                      
*                                                                               
AGY210   OC    COSTNUM,COSTNUM                                                  
         BZ    *+10                                                             
         MVC   KEY+3(12),COSTNUM+3 OR USE COSTING                               
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         TM    ACCTSTAT,X'80'      BALANCE ELEMENT                              
         BZ    BADACC                                                           
         TM    ACCTSTAT,ACSLOCK    LOCKED?                                      
         BO    BADACC                                                           
         MVC   CRPSNUM,ACCTNUM                                                  
         MVC   CRPSNAME,ACCTNAME                                                
         EJECT                                                                  
*--------------------------------------------------------------                 
*                                                                               
*--------------------------------------------------------------                 
*                                                                               
AGYAMNT  DS    0H                                                               
         L     R2,SAVCLPR                                                       
         ZIC   RF,0(R2)            POINT TO AMOUNT                              
         AR    R2,RF                                                            
         CLI   8(R2),C'*'          REPEAT FEATURE                               
         BNE   AA03                                                             
         BAS   RE,DUPRTN           EDIT FOR REPEAT                              
*                                                                               
AA03     ST    R2,SAVEAMNT                                                      
         CLI   5(R2),0             MUST INPUT AMOUNT                            
         BNE   AA05                                                             
         MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     BADRTRN2                                                         
*                                                                               
AA05     ZIC   R3,5(R2)                                                         
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   DMCB,X'FF'                                                       
         BE    BADRTRN2                                                         
         ZAP   SVAMNT,4(8,R1)                                                   
         ZAP   NETPOST,SVAMNT                                                   
         ZAP   GRSPOST,SVAMNT                                                   
         CLI   FRSTPASS,1          ONLY ADD TO BATCH TOTAL                      
         BNE   AA05AA              ON SECOND PASS                               
*                                                                               
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   AA05A                                                            
         CLI   GSTSW,C'Y'                                                       
         BNE   AA05A                                                            
         L     R3,AAMT                                                          
         USING AMTD,R3                                                          
         ZAP   GRSPOST,AMTGRS                                                   
         ZAP   NETPOST,AMTNET                                                   
         ZAP   GSTPOST,AMTGST                                                   
         DROP  R3                                                               
*                                                                               
AA05A    AP    CSHTOT,GRSPOST                                                   
         ZAP   TRANSAMT,GRSPOST                                                 
*                                                                               
AA05AA   CP    DISC2,=P'0'         SAVED DISCOUNT                               
         BE    BLD2                                                             
         ZAP   DUB,NETPOST         CALCULATE CD                                 
         MP    DUB,DISC2                                                        
         SRP   DUB,64-4,5          ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,DUB                                                       
*                                                                               
         L     R2,SAVEAMNT         DEFAULT COMMISSION ACCT                      
*                                                                               
AA05B    MVC   KEY,SPACES          READ LEDGER RECORD                           
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(1),=C'S'                                                   
         LA    RF,COMPEL                                                        
         USING ACCOMPD,RF                                                       
         MVC   KEY+2(1),ACMPSUPX   EXPENSE VENDOR FROM CO REC                   
         DROP  RF                                                               
*                                                                               
         CLC   ATRACC2(1),=C'*'                                                 
         BNE   *+10                                                             
         MVC   KEY+1(2),ATRACC2+1  OVERRIDE ACCT                                
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    AA05C                                                            
         MVC   FVMSGNO,=Y(AE$MEL)  MISSING EXPENSE LEDGER RECORD'               
         MVC   MSG+28(2),0(R1)                                                  
         B     BADRTRN                                                          
*                                                                               
AA05C    MVI   ELCODE,ACLTELQ      GET COMMISSION ACCT FROM LEDGER REC          
         L     R3,AIOAREA1                                                      
         BAS   RE,GETEL                                                         
         BNE   AA05D                                                            
         USING ACLEDGD,R3                                                       
         CLC   ACLTCDAC,SPACES                                                  
         BNH   AA05D                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),ACLTCDAC                                               
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SIMDAC,ACCTNUM      CD ACCT TAKEN FROM LEDGER RECORD             
         MVC   SIMDACN,ACCTNAME                                                 
         B     BLD2                                                             
         DROP  R3                                                               
*                                                                               
AA05D    MVC   KEY(15),SPACES                                                   
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(4),=C'SIMD'                                                
         SR    R6,R6                                                            
         BAS   RE,GETACC                                                        
         BAS   RE,CHECKACC                                                      
         MVC   SIMDAC,ACCTNUM                                                   
         MVC   SIMDACN,ACCTNAME                                                 
*                                                                               
BLD2     CLI   FRSTPASS,1          TEST SECOND PASS                             
         BNE   BLD3                NO                                           
         GOTO1 BLDELS,DMCB,(RC)    BUILD ELEMENT S FOR EDITED LINE              
BLD3     BAS   RE,DISPNMS          DISPLAY EXPANSIONS                           
         L     RE,AAMT                                                          
         LA    RE,AMTLNQ(RE)                                                    
         ST    RE,AAMT                                                          
         CLI   GSTSW,C'Y'                                                       
         BNE   BLD3A                                                            
         L     RE,APOST            BUMP TO NEXT GST/PST POSTINGS                
         ZICM  R0,0(RE),2                                                       
         AR    RE,R0                                                            
         ST    RE,APOST                                                         
BLD3A    SR    R0,R0               SET GOOD CC                                  
RTRN2    XMOD1                                                                  
*                                                                               
BADRTRN2 ST    R2,CURSPOS          ERROR CURSOR POSITION                        
         LTR   RB,RB               SET BAD CC                                   
         B     RTRN                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        DISPLAY EXPANSIONS FOR DETAIL LINE JUST PROCESSED                      
*--------------------------------------------------------------                 
*                                                                               
DISPNMS  NTR1                                                                   
         L     R2,SAVEAMNT         RESTORE A(CURRENT AMOUNT FLD HDR)            
         ZIC   R1,0(R2)            TOTAL FLD LEN                                
         AR    R2,R1               POINT TO PROTECTED DISPLAY LINE              
         OI    6(R2),X'80'         SET TRANSMIT OF PROTECTED FLD.               
         L     R1,SAVEWC           RESTORE A(WORK CODE HEADER)                  
         CLI   AGYSW,C'Y'          IS IT AN AGENCY EXP. LINE                    
         BE    AGYDISP             YES, DISPLAY FOR AGY EXP                     
*                                                                               
         GOTO1 SQUASHER,DMCB,SVWRKNAM,15                                        
         MVC   8(15,R2),SVWRKNAM   WORK CODE-AS MUCH AS POSSIBLE                
         MVI   23(R2),C' '                                                      
*                                                                               
         GOTO1 SQUASHER,DMCB,SVCLINM,36                                         
         GOTO1 SQUASHER,DMCB,SVPRDNM,36                                         
         GOTO1 SQUASHER,DMCB,SVJOBNM,36                                         
*                                                                               
         LA    R2,24(R2)           START POINT FOR CLI,PRD,JOB NAMES            
         LA    R6,3                LOOP FOR THREE                               
*                                                                               
DISP1    LA    R4,0                ACCUM LEN OF INDIVIDUAL MOVE                 
         CH    R6,=H'3'                                                         
         BNE   DISP1A                                                           
         LA    R1,SVCLINM                                                       
         B     DISP1C                                                           
*                                                                               
DISP1A   CH    R6,=H'2'                                                         
         BNE   DISP1B                                                           
         LA    R1,SVPRDNM                                                       
         B     DISP1C                                                           
*                                                                               
DISP1B   LA    R1,SVJOBNM                                                       
DISP1C   LA    R0,19               MAXIMUM LEN OF A DISPLAY                     
DISP2    CLI   0(R1),C' '          SCAN FOR TWO BLANKS                          
         BNE   DISP3                                                            
         LA    R1,1(R1)            BUMP A(NAME FLD)                             
         CLI   0(R1),C' '          TWO BLANKS INDICATES END OF NAME             
         BE    DISP4                                                            
         LA    R4,1(R4)            ADD TO LEN OF MOVE                           
         SH    R0,=H'1'            CHECK AGAINST MAXIMUM MOVE                   
         BZ    DISP4                                                            
*                                                                               
DISP3    LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,DISP2                                                         
*                                                                               
DISP4    CH    R6,=H'3'                                                         
         BNE   DISP5                                                            
         LA    R1,SVCLINM                                                       
         B     DISP7                                                            
*                                                                               
DISP5    CH    R6,=H'2'                                                         
         BNE   DISP6                                                            
         LA    R1,SVPRDNM                                                       
         B     DISP7                                                            
*                                                                               
DISP6    LA    R1,SVJOBNM                                                       
*                                                                               
DISP7    BCTR  R4,0                FOR EX INSTRUCTION                           
         EX    R4,*+8              MOVE NAME FOR CALCULATED LEN                 
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         CH    R6,=H'1'            NO COMMA LAST PASS                           
         BE    DISPEXT                                                          
         LA    R4,1(R4)            RESTORE CORRECT LEN                          
         AR    R2,R4               ADVANCE A(OUTPUT FLD)                        
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         BCT   R6,DISP1            LOOP FOR REMAINING NAMES                     
         B     DISPEXT                                                          
*                                                                               
AGYDISP  MVC   14(5,R2),=C'ACCT='                                               
         MVC   19(22,R2),POSTACCN  DEBIT ACCOUMT NAME                           
         L     R3,SAVEODS          OUTPUT DEPT/STAFF NAME                       
         CLI   0(R3),1             JUST OFFICE                                  
         BNH   DISP9               YES                                          
         MVC   42(18,R2),DEPSTFN   DEPT,STAFF NAME                              
*                                                                               
DISP9    L     R3,SAVCLPR          OUTPUT CLIENT/PRODUCT NAME                   
         CLI   0(R3),0                                                          
         BE    DISPEXT             NO INPUT IN CLI/PRD                          
         MVC   62(20,R2),CLIPRON                                                
DISPEXT  XMOD1                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        REPEAT FEATURE ROUTINE-R2=A(CURRENT FLD HDR WITH '*' INPUT)            
*--------------------------------------------------------------                 
*                                                                               
DUPRTN   NTR1                                                                   
         LR    R1,R2                                                            
         USING SCRNCED,R1          DSECT FOR MY SCREEN                          
         LA    R4,LNTOLN           SCREEN LINE LENGTH                           
         SR    R1,R4               DECREMENT TO A(OF FLD ABOVE)                 
         L     R3,AWCH             A(1ST DETAIL HDR)                            
         CR    R3,R1                                                            
         BNH   DUPOK                                                            
         MVC   FVMSGNO,=Y(AE$AITL) INVALID '*' ON TOP LINE                      
         B     ERRXIT                                                           
DUPOK    CLI   5(R1),0             ANY INPUT ABOVE                              
         BNE   OKTODUP                                                          
         MVC   FVMSGNO,=Y(AE$NODUP)                                             
         B     ERRXIT                                                           
OKTODUP  ZIC   R3,5(R1)            LENGTH OF INPUT TO REPEAT                    
         STC   R3,BYTE                                                          
         MVC   5(1,R2),BYTE        MOVE LEN TO CURRENT HDR                      
         OI    6(R2),X'80'         SET TRANSMIT                                 
         BCTR  R3,0                MINUS 1 FOR EXECUTE INSTRUCTION              
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R1)                                                    
         XMOD1                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        DISPLAY GST DATA                                                       
*--------------------------------------------------------------                 
*                                                                               
DISTAX   NTR1  ,                                                                
         L     R1,AVTC             R1=A(VATICAN BLOCK)                          
         USING VTCD,R1                                                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(1),VTCTYPE                                                   
         MVC   FLD+2(L'VTCTTYPE),VTCTTYPE                                       
         MVC   FLD+14(5),=C'RATE='                                              
         SR    R0,R0                                                            
         ICM   R0,3,VTCRATE                                                     
*                                                                               
         TM    VTCINDS,VTCIDC3                                                  
         BZ    DISTAX2                                                          
         EDIT  (R0),(5,FLD+18),3,ALIGN=LEFT                                     
         B     DISTAX4                                                          
                                                                                
DISTAX2  EDIT  (R0),(5,FLD+19),2,ALIGN=LEFT                                     
*                                                                               
DISTAX4  GOTO1 SQUASHER,DMCB,FLD,L'FLD                                          
*                                                                               
         L     R2,ATYPNH                                                        
         BAS   RE,MOVEFLD                                                       
*                                                                               
         CLI   NGST,0              TEST IF GST INPUT                            
         BNE   DISTAXX             YES-DO NOT DISPLAY VALUE                     
         L     R2,AGSTXH                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(4),=C'GST='                                                  
         EDIT  (P6,TOTGST),(11,FLD+4),2,ALIGN=LEFT,MINUS=YES                    
         BAS   RE,MOVEFLD                                                       
*                                                                               
DISTAXX  XIT1                                                                   
         EJECT                                                                  
         GETEL R3,DATADISP,ELCODE                                               
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                                          
*                                                                               
MOVEFLD  ST    RE,FULL                                                          
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
*                                                                               
MOVEFLDX L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* US SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
USTAB    DS    0D                                                               
         DC    AL4(ATRNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(ATRWCH-TWAD),AL4(AWCH-PROGD)                                 
         DC    AL4(ATRTABH-TWAD),AL4(ATABH-PROGD)                               
         DC    AL4(ATRTOTH-TWAD),AL4(ATOTH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* CA SCREEN TABLE  ( BYTES 0-3=DISP TO FIELD, BYTES 4-7=DISP TO ADCON )         
*                                                                               
CANTAB   DS    0D                                                               
         DC    AL4(ATCTYPEH-TWAD),AL4(ATYPEH-PROGD)                             
         DC    AL4(ATCTYPNH-TWAD),AL4(ATYPNH-PROGD)                             
         DC    AL4(ATCGORNH-TWAD),AL4(AGORNH-PROGD)                             
         DC    AL4(ATCGSTXH-TWAD),AL4(AGSTXH-PROGD)                             
         DC    AL4(ATCPROVH-TWAD),AL4(APROVH-PROGD)                             
         DC    AL4(ATCGAMTH-TWAD),AL4(AGAMTH-PROGD)                             
         DC    AL4(ATCNARH-TWAD),AL4(ANARH-PROGD)                               
         DC    AL4(ATCWCH-TWAD),AL4(AWCH-PROGD)                                 
         DC    AL4(ATCTABH-TWAD),AL4(ATABH-PROGD)                               
         DC    AL4(ATCTOTH-TWAD),AL4(ATOTH-PROGD)                               
         DC    X'FF'                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATCODE                                                              
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
NOCTAX   DC    C'** ERROR - CANADIAN SCREEN NOT AVAILABLE'                      
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ERROR CHECK ROUTINE FOR OFFICE/DEPT/STAFF                              
*--------------------------------------------------------------                 
*                                                                               
ERRCHCK  DS    0D                                                               
         NMOD1 0,*ERCH*,R7                                                      
         L     RC,0(R1)                                                         
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   DEPTL(1),DPTLNGTH                                                
         BNH   ERRB                                                             
         MVC   FVMSGNO,=Y(AE$INVDL)    INVALID DEPT LENGTH                      
         B     ERRX                                                             
*                                                                               
ERRB     DS    0H                                                               
         CLI   STAFFL,8                                                         
         BNH   ERRC                                                             
         MVC   FVMSGNO,=Y(AE$INVSL)    INVALID STAFF LENGTH                     
         B     ERRX                                                             
*                                                                               
ERRC     DS    0H                                                               
         MVI   ODSSW,0             CLEAR REQUIRED FLAG                          
         CLI   OFFSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,OFFCBIT       IF OFFICE ENTERED THEN X'04' ON              
         CLI   DEPSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,DEPTBIT       IF DEPT ENTERED THEN X'02' ON                
         CLI   STFSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    ODSSW,STFFBIT       IF STAFF ENTERED THEN X'01' ON               
*                                                                               
         CLI   STFSW,C'Y'                                                       
         BNE   ERRD                ONLY READ 2P IF STAFF IS ON                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'2P'     SET UP 2/P ACCOUNT                           
*                                                                               
         MVC   RKEY,KEY                                                         
         GOTO1 AREAD,AIOAREA1      READ LEDGER FOR STRUCTURE                    
         BE    *+6                                                              
         DC    H'0'                MISSING 2P LEDGER                            
*                                                                               
         SR    RE,RE                                                            
         L     R3,AIOAREA                                                       
         AH    R3,DATADISP                                                      
         CLI   0(R3),ACHRELQ       MUST HAVE HEIRARCHY ELEMENT                  
         BE    *+14                                                             
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     *-14                                                             
*                                                                               
         USING ACHEIRD,R3                                                       
         MVI   LEVEL,3                                                          
         CLI   ACHRLEVC,12         3 LEVEL ACCT?                                
         BE    ERRC1                                                            
*                                                                               
         MVI   LEVEL,2                                                          
         CLI   ACHRLEVB,12         2 LEVEL ACCT?                                
         BE    ERRC1                                                            
*                                                                               
         MVI   LEVEL,1                                                          
         CLI   ACHRLEVA,12         1 LEVEL ACCT?                                
         BE    ERRC1                                                            
         DC    H'0'                BAD ACCT LEVEL                               
*                                                                               
ERRC1    CLI   LEVEL,1             FOR 2 & 3 LEVEL, NEED DEPT                   
         BE    ERRD                                                             
         OI    ODSSW,DEPTBIT       MAKE DEPT REQUIRED AS INPUT                  
*                                                                               
ERRD     DS    0H                  'ODSSW' HAS CORRECT BIT SETTINGS             
         MVI   INPUTSW,0           'INPUT' WILL HAVE BITS FOR WHAT WAS          
         CLI   OFFICEL,0            ENTERED IN THE OF/DP/ST FIELD.              
         BE    *+8                                                              
         OI    INPUTSW,OFFCBIT                                                  
         CLI   DEPTL,0                                                          
         BE    *+8                                                              
         OI    INPUTSW,DEPTBIT                                                  
         CLI   STAFFL,0                                                         
         BE    *+8                                                              
         OI    INPUTSW,STFFBIT                                                  
*                                                                               
         XC    INPUTSW(1),ODSSW    IF ALL IS OK, INPUT SHOULD BE 0              
         CLI   OFFSW,C'Y'                                                       
         BE    *+8                                                              
         NI    INPUTSW,X'03'       ALLOW OFFICE IF OFFSW=N                      
         CLI   INPUTSW,0                                                        
         BE    ERROK                                                            
*                                                                               
         MVI   ERRNUM,SPECIAL                                                   
         L     R2,SAVEODS          POINT TO OFFICE/DPT/STFF FIELD               
         MVC   MSG,SPACES                                                       
         TM    ODSSW,OFFCBIT       SHOULD OFFICE BE ENTERED?                    
         BZ    ERRE                                                             
         MVC   FVMSGNO,=Y(AE$CFO)                                               
*                                                                               
ERRE     TM    ODSSW,DEPTBIT       SHOULD DEPT BE ENTERED?                      
         BZ    ERRF                                                             
         MVC   FVMSGNO,=Y(AE$CFOD)                                              
*                                                                               
ERRF     TM    ODSSW,STFFBIT       SHOULD STAFF BE ENTERED?                     
         BZ    ERRG                                                             
         MVC   FVMSGNO,=Y(AE$CFODS)                                             
         B     ERRX                                                             
*                                                                               
ERRG     CLI   ODSSW,0                                                          
         BNE   ERRX                                                             
         MVC   FVMSGNO,=Y(AE$NOIPP)  NO INPUT                                   
         B     ERRX                                                             
*                                                                               
ERROK    MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
ERRX     XMOD1                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 64 ELEMENT                                                       
*--------------------------------------------------------------                 
*                                                                               
BLDELSC  DS    0D                                                               
         NMOD1 0,*BLDL*,R7                                                      
         L     RC,0(R1)                                                         
         LA    R2,IOAREA           CLEAR FOR CURRENT DTL LINE ELEMENTS          
         LA    R3,1000                                                          
         LA    R1,0                                                             
         MVCL  R2,R0                                                            
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVC   DLDSREF,SVDOC                                                    
         MVC   DLDSDATE,SVDATE1                                                 
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         OI    DLDSSTAT,X'08'      AUTHORIZED INVOICE INDICATION                
         CLI   ATRURG,C'U'                                                      
         BNE   BLD04                                                            
         OI    DLDSSTAT,X'40'                                                   
*                                                                               
BLD04    L     R2,ANARH            PUT NARRATIVE INTO ELEMENT                   
         LA    R3,DLDSNARR                                                      
         GOTO1 ANARRSCN,DMCB,(R2),(R3)                                          
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               LENGTH OF ELEMENT-NARRATIVE                  
         AR    R5,R6               0 OR LEN RETURNED FROM NARRSCAN              
         STH   R5,HALF                                                          
         MVC   DLDSLEN,HALF+1                                                   
         ZIC   R5,DLDSLEN                                                       
         AR    R8,R5               ADVANCE A(IOWORK LENGTH OF ELEMENT)          
*                                                                               
         DS    0H'0'                                                            
         CLI   AGYSW,C'Y'          AGY EXP. PASS                                
         BE    AGYELS                                                           
*                                                                               
         CP    CDAMNT,=P'0'        IS THERE DISCOUNT                            
         BE    BLD06               NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   BLD06               YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         SKIP CD IF WE KEEP IT                        
         BE    BLD06                                                            
*                                                                               
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR JOB                           
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R5,TRCSLEN                                                       
         AR    R8,R5                                                            
*                                                                               
         USING PAKEL,R8                                                         
BLD06    CLI   SVCSHNUM,0          IS THERE A CASH ACCOUNT?                     
         BNE   BLD08               YES, NO PAYABLE POSTING THEN                 
         MVI   PAKEL,PAKELQ                                                     
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,SVCRACCT                                                  
         MVC   PAKOFF,CURROFFC                                                  
         MVC   PAKCON,SVCLINUM                                                  
         CLC   SVCRACCT+1(2),=C'SX'                                             
         BNE   *+10                                                             
         MVC   PAKCON,SVJOBNUM                                                  
         MVC   PAKDATE,SVDATE1                                                  
         MVC   PAKREF,SVDOC                                                     
         ZIC   R5,PAKLN                                                         
         AR    R8,R5                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 69 DEBIT ELEMENT                                                 
*--------------------------------------------------------------                 
*                                                                               
         USING DLPOSTD,R8                                                       
BLD08    MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSDBAC,SVJOBNUM   DEBIT JOB - SJ                               
         MVC   DLPSDBNM,SVJOBNM                                                 
         MVC   DLPSCRAC,SVCRACCT   CONTRA = PROD VENDOR                         
         MVC   DLPSCRNM,SVACCTNM                                                
*                                                                               
         CLI   SVCRACCT,0          IS THERE A VENDOR?                           
         BE    BLD10               NO - MUST USE CASH                           
         CLI   SVCSHNUM,0          DO WE HAVE CASH ACCT?                        
         BE    BLD12               NO.                                          
         TM    COMPSTA2,X'02'      DO WE WANT CONTRA TO BE CASH?                
         BZ    BLD12               NO                                           
*                                                                               
BLD10    MVC   DLPSCRAC,SVCSHNUM   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,SVCSHNAM   CASH ACCT NAME.                              
*                                                                               
BLD12    MVI   DLPSTYPE,0                                                       
         CLI   COMSW,C'Y'                                                       
         BE    BLD14                                                            
         OI    DLPSTYPE,X'40'      SET FOR NO COMMISSION                        
*                                                                               
BLD14    MVC   DLPSANAL,SVWRKCD                                                 
         ZAP   DLPSAMNT,NETPOST    NET                                          
         CP    CDAMNT,=P'0'        IS DISCOUNT = 0                              
         BE    BLD16               YES                                          
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   BLD16               YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         DO WE PASS CD ALONG?                         
         BE    BLD16               NO                                           
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
*                                                                               
BLD16    ZIC   R4,DLPSLEN                                                       
         ZAP   EXPOST,DLPSAMNT     SAVE AMT FOR OTHER POSTINGS.                 
         AR    R8,R4                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD GST POSTING IF NEEDED                                            
*--------------------------------------------------------------                 
*                                                                               
VAL30    CLI   AGYCTRY,CTRYCAN                                                  
         BNE   VAL31                                                            
         CLI   GSTSW,C'Y'                                                       
         BNE   VAL31                                                            
*                                                                               
         L     R2,APOST                                                         
         ZICM  RF,0(R2),2          GET LENGTH OF POSTING                        
         LTR   RF,RF               NOTHING?                                     
         BZ    VAL31                                                            
         SH    RF,=H'2'            SUBTRACT LENGTH                              
         LA    RE,2(R2)            COPY POSTINGS                                
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R8,R0                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD OTHERS ELEMENT FOR PRD AND JOB                                   
*--------------------------------------------------------------                 
*                                                                               
         USING ACOTHERD,R8                                                      
VAL31    MVC   ACOTEL(2),=X'230F'  BUILD 'OTHERS' ELEMENT FOR                   
         MVC   ACOTNUM(13),SPACES  PRODUCT AND JOB                              
         L     R2,SAVEEXP          A(CURRENT EXPENSE FLD HDR)                   
         LA    R2,8(R2)            ADVANCE TO INPUT                             
*                                                                               
VAL32    CLI   0(R2),C','          SCAN FOR CLIENT,PROD DILIMETER               
         BE    VAL33                                                            
         LA    R2,1(R2)            BUMP INPUT ADDR                              
         B     VAL32                                                            
*                                                                               
VAL33    L     R1,PRDLEN           =LENGTH OF PROD INPUT                        
         LA    R2,1(R2)            BUMP PAST COMMA                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM(0),0(R2)                                                 
*                                                                               
VAL34    CLI   0(R2),C','          SCAN FOR PROD,JOB DILIMETER                  
         BE    VAL35                                                            
         LA    R2,1(R2)            BUMP INPUT ADDR                              
         B     VAL34                                                            
*                                                                               
VAL35    L     R1,JOBLEN           =LENGTH OF JOB INPUT                         
         LA    R2,1(R2)            BUMP PAST COMMA                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM+6(0),0(R2)                                               
         ZIC   R3,ACOTLEN                                                       
         AR    R8,R3                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        BUILD 50 CD EL FOR SUPPLIER                                            
*--------------------------------------------------------------                 
*                                                                               
AGYEL00  CP    CDAMNT,=P'0'                                                     
         BE    VAL37               CD FOR SUPPLIER                              
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   VAL37               YES - NO CD ALLOWED                          
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR SUPPLIER                      
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R3,TRCSLEN                                                       
         AR    R8,R3                                                            
                                                                                
VAL37    BAS   RE,ADDADB                                                        
                                                                                
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSDBAC,SVCLINUM   CONTRA = PROD VENDOR                         
         MVC   DLPSDBNM,SVCLINM                                                 
         MVC   DLPSCRAC,SVCRACCT   CR JOB - SJ                                  
         MVC   DLPSCRNM,SVACCTNM                                                
         CLC   SVCRACCT+1(2),=C'SX'                                             
         BNE   *+16                                                             
         MVC   DLPSDBAC,SVJOBNUM   OR CLI/PRD/JOB FOR EXPENSE VENDORS           
         MVC   DLPSDBNM,SVJOBNM                                                 
         CLC   SVCRACCT+1(2),=C'SA'  OR SOMETHING ELSE FOR ADVANCES             
         BNE   VAL38                                                            
         OC    SAVNUM,SAVNUM                                                    
         BZ    VAL38                                                            
         MVC   DLPSDBAC,SAVNUM                                                  
         MVC   DLPSDBNM,SAVNAM                                                  
*                                                                               
VAL38    CLI   SVCSHNUM,0          IS THERE A CASH ACCOUNT?                     
         BE    VAL38A              NO.                                          
*                                                                               
         MVC   DLPSDBAC+6(3),SVPRDNUM+6      IF CASH WE NEED PROD               
         MVC   DLPSCRAC,SVCSHNUM   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,SVCSHNAM   CASH ACCT NAME.                              
*                                                                               
VAL38A   MVC   DLPSANAL,CURROFFC                                                
         ZAP   DLPSAMNT,GRSPOST                                                 
         CLI   SVCSHNUM,0          IS THERE A CASH ACCOUNT?                     
         BNE   VAL38B              YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT                                                  
VAL38B   ZIC   R3,DLPSLEN                                                       
         LR    R6,R8               BUILD CREDIT POSTING TO CD-INCOME AC         
         AR    R8,R3                                                            
*                                                                               
         CP    CDAMNT,=P'0'        DID VENDOR HAVE DISCT                        
         BE    VAL39A              NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   VAL39A              YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         CD FOR SUPPLIER-CLIENT MODE                  
         BNE   VAL39A                                                           
         BCTR  R3,R0               SUBTRACT 1 FROM R3                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R6)                                                    
         LA    R3,1(R3)                                                         
         MVC   DLPSCRAC,SIMDAC                                                  
         MVC   DLPSCRNM,SIMDACN                                                 
         MVC   DLPSDBAC,SVPRDNUM   CLI/PROD AS CONTRA                           
         MVC   DLPSDBNM,SVPRDNM                                                 
         ZAP   DLPSAMNT,CDAMNT                                                  
         ZIC   R3,DLPSLEN                                                       
         LR    R6,R8                                                            
         AR    R8,R3                                                            
*                                                                               
VAL39A   CLI   PCONSULT,C'Y'       SHOULD WE POST TO 2C?                        
         BNE   VAL39               NO.                                          
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     DB & CR                                      
         MVI   DLPSLEN,DLPSLNQ     LENGTH                                       
         MVC   DLPSDBAC,P2CNUM     DB ACCT #                                    
         MVC   DLPSDBNM,P2CNAM     DB ACCT NAME                                 
         MVC   DLPSCRAC,PROTROL    CR ACCT #                                    
         MVC   DLPSCRNM,PROTROLN   CR ACCT NAME                                 
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,CURROFFC   OFF                                          
         ZAP   DLPSAMNT,GRSPOST    AMT                                          
         ZIC   R4,DLPSLEN          LEN                                          
         AR    R8,R4               INCREMENT                                    
         B     VAL39               END                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ELS FOR AGENCY ACCRUED EXPENSES                                        
*--------------------------------------------------------------                 
*                                                                               
AGYELS   DS    0H                                                               
         CP    CDAMNT,=P'0'                                                     
         BE    AGYEL01             CD FOR SUPPLIER                              
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   AGYEL01             YES - NO CD ALLOWED                          
*        TM    COMPSTAT,X'08'      PASS CD TO CLIENT? (CD IN COMP)              
*        BZ    AGYEL01             NO                                           
*                                                                               
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR SUPPLIER                      
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R4,TRCSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
AGYEL01  BAS   RE,ADDADB                                                        
                                                                                
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT                                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC       DEBIT ACCOUNT - DEF EXP               
         MVC   DLPSCRAC(51),SVCRACC2      CONTRA ACCOUNT - DEF VENDOR           
*                                                                               
         CLI   SVCRACC2,0          IS THERE A EXP VENDOR?                       
         BE    AGYEL012            NO - MUST USE CASH                           
*                                                                               
         CLI   SVCSHNUM,0          CASH ACCOUNT?                                
         BE    AGYEL01A            NO                                           
*                                                                               
         TM    COMPSTA3,X'08'      CASH ACCT REQ?                               
         BZ    AGYEL01A            NO..                                         
*                                                                               
AGYEL012 MVC   DLPSCRAC(51),SVCSHNUM      CASH ACCT # & NAME                    
*                                                                               
AGYEL01A MVC   DLPSANAL,OFFICE                                                  
         ZAP   DLPSAMNT,NETPOST                                                 
         CP    CDAMNT,=P'0'        IS DIC = 0                                   
         BE    AGYEL01B            YES                                          
         CLI   SVCSHNUM,0          DO WE HAVE A CASH ACCT?                      
         BNE   AGYEL01B            YES - NO CD ALLOWED                          
         TM    COMPSTAT,X'08'      PASS CD TO CLIENT?                           
         BNZ   AGYEL01B            NO                                           
         SP    DLPSAMNT,CDAMNT     NET LESS C.D                                 
AGYEL01B ZAP   EXPOST,DLPSAMNT     SAVE EXPENSE POSTING                         
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------                 
*        BUILD GST POSTING IF NEEDED                                            
*--------------------------------------------------------------                 
*                                                                               
AGYEL01C CLI   AGYCTRY,CTRYCAN                                                  
         BNE   AGYEL01D                                                         
         CLI   GSTSW,C'Y'                                                       
         BNE   AGYEL01D                                                         
*                                                                               
         L     R2,APOST                                                         
         ZICM  RF,0(R2),2          GET LENGTH OF POSTING                        
         LTR   RF,RF               NOTHING?                                     
         BZ    AGYEL01D                                                         
         SH    RF,=H'2'            SUBTRACT LENGTH                              
         LA    RE,2(R2)            COPY POSTINGS                                
         LR    R0,R8                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         LR    R8,R0                                                            
*                                                                               
AGYEL01D CP    CDAMNT,=P'0'                                                     
         BE    AGYEL02             CD FOR SUPPLIER                              
         CLI   SVCSHNUM,0          DO WE HAVE CASH ACCT?                        
         BNE   AGYEL02             YES - NO CD ALLOWED                          
*        TM    COMPSTAT,X'08'      PASS CD TO CLIENT?                           
*        BZ    AGYEL02             NO                                           
         USING TRCASHD,R8                                                       
         ZAP   TRCSAMNT,CDAMNT     CD ELEMENT FOR SUPPLIER                      
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZIC   R4,TRCSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
AGYEL02  BAS   RE,ADDADB                                                        
                                                                                
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT                                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC        CONTRA ACCOUNT - EXP                 
         MVC   DLPSCRAC(51),SVCRACC2       CREDIT ACCOUNT - VENDOR              
         MVC   DLPSANAL,OFFICE                                                  
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BE    AGYEL03             NO.                                          
*                                                                               
         MVC   DLPSCRAC,SVCSHNUM   CASH ACCT #                                  
         MVC   DLPSCRNM,SVCSHNAM   CASH ACCT NAME                               
*                                                                               
AGYEL03  ZAP   DLPSAMNT,GRSPOST    NET                                          
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   AGYEL04             YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
*                                                                               
AGYEL04  ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
         CP    CDAMNT,=P'0'        DID VENDOR HAVE DISCT                        
         BE    AGYEL05             NO                                           
         CLI   SVCSHNUM,0          IS THERE A CASH ACCT?                        
         BNE   AGYEL05             YES - NO CD ALLOWED                          
         TM    COMPSTAT,X'08'      DO WE PASS CD TO CLIENT?                     
         BZ    AGYEL05             YES                                          
                                                                                
         BAS   RE,ADDADB                                                        
                                                                                
         MVI   DLPSEL,DLPSECRQ     CREDIT EXP WITH CD AMOUNT                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,SVCRACC2                                                
         MVC   DLPSDBNM,SVACCTN2                                                
         MVC   DLPSCRAC,SIMDAC                                                  
         MVC   DLPSCRNM,SIMDACN                                                 
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,CDAMNT                                                  
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,OFFICE                                                  
         ZIC   R4,DLPSLEN                                                       
         LR    R6,R8                                                            
         AR    R8,R4                                                            
         B     AGYEL05                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2P WITH 29 CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
AGYEL05  CLI   STFSW,C'Y'                                                       
         BNE   AGYEL10                                                          
         BAS   RE,ADDADB                                                        
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT STAFF                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,STAFFNUM   DEBIT 2P                                     
         MVC   DLPSDBNM,STAFFNAM                                                
         MVC   DLPSCRNM,CRPSNAME   CONTRA 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         L     R2,SAVEEXP          CREDIT SE ACCT                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                FOR EX INST.                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),8(R2)                                              
         MVC   DLPSANAL,OFFICE                                                  
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    AGYEL07                                                          
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         L     R2,SAVEEXP                                                       
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         LA    R6,1(R1)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BNM   *+6                                                              
         DC    H'0'                SHOULD HAVE CAUGHT THIS EARLIER              
         LA    R3,8(R6,R2)         R3=A(DESIRED PORTION ACCT INPUT)             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(R3) = * +DESIRED PORTION ACCT INPUT              
         LR    R1,R4                                                            
*                                                                               
AGYEL07  STC   R1,BYTE             LEN OF ACCT INPUT DESIRED                    
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CRPSNUM+3   2/9 ACCT KEY                                 
         MVC   WORK(15),DLPSCRAC   SAVE FOR DEBIT SIDE OF CRD CLI EL            
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
                                                                                
         BAS   RE,ADDADB                                                        
         EJECT                                                                  
*--------------------------------------------------------------                 
*        29 WITH 2P CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBNM,STAFFNAM   CONTRA 2P                                    
         MVC   DLPSCRNM,CRPSNAME   CREDIT 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,X'80'      SUBSIDIARY FROM HERE                         
         MVC   DLPSDBAC(15),WORK   SAVED FROM DEBIT ELEMENT                     
         MVC   DLPSCRAC,CRPSNUM                                                 
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         LA    RF,DLPSDBAC+1(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFFNUM+3  CONTRA IS *EXPENSE-STAFF                     
         MVC   DLPSANAL,OFFICE                                                  
*                                                                               
         CLI   V29SW,C'Y'          SHOULD CONTRA BE VENDOR?                     
         BNE   AGYEL07A            NO                                           
         MVC   DLPSDBAC,SVCRACC2   EXP VENDOR                                   
         MVC   DLPSDBNM,SVACCTN2   EXP VENDOR NAME                              
*                                                                               
AGYEL07A ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
         B     AGYEL10                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2D WITH 28 CONTRA POSTING  DR                                          
*        28 WITH 2D CONTRA POSTING  CR                                          
*--------------------------------------------------------------                 
*                                                                               
AGYEL10  DS    0H                                                               
         CLI   DEPSW,C'Y'                                                       
         BNE   AGYEL11                                                          
*                                                                               
         BAS   RE,ADDADB                                                        
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DEPNUM     DEBIT 2D   CONTRA = 28                       
         MVC   DLPSDBNM,DEPNAME                                                 
         MVC   DLPSCRAC,CRDSNUM    CREDIT 28  CONTRA = 2D                       
         MVC   DLPSCRNM,CRDSNAME                                                
         ZAP   DLPSAMNT,EXPOST                                                  
         MVC   DLPSANAL,SPACES                                                  
         MVC   DLPSANAL,OFFICE                                                  
         OI    DLPSTYPE,X'80'      IN CASE WE MISSED IT ON ELEMENT 2            
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
         B     AGYEL11                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        2C WITH 27 CONTRA POSTING                                              
*        27 WITH 2C CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
AGYEL11  CLI   ECONSULT,C'Y'       DO WE NEED EXP 2C?                           
         BNE   AGYEL15             NO.                                          
         BAS   RE,ADDADB                                                        
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     DB & CVR                                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,E2CNUM     ACCT #                                       
         MVC   DLPSDBNM,E2CNAM     ACCT NAME                                    
         MVC   DLPSCRAC,EXPTROL    27                                           
         MVC   DLPSCRNM,EXPTROLN   27                                           
         ZAP   DLPSAMNT,GRSPOST    AMT                                          
         MVC   DLPSANAL,OFFICE                                                  
         OI    DLPSTYPE,X'80'                                                   
         ZIC   R4,DLPSLEN          LEN                                          
         AR    R8,R4               INCREMENT                                    
         B     AGYEL15                                                          
         EJECT                                                                  
*--------------------------------------------------------------                 
*        1C WITH 1P CONTRA POSTING                                              
*--------------------------------------------------------------                 
*                                                                               
AGYEL15  CLI   COSTANAL,C' '                                                    
         BE    VAL39                                                            
         TM    COSTSW,COSTACC                                                   
         BZ    VAL39                                                            
         BAS   RE,ADDADB                                                        
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT DEPT C/A CLIENT                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CONTRA  1C                                   
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CRCNUM     DEBIT 1P                                     
         MVC   DLPSDBNM,CRCNAME                                                 
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,OFFICE                                                  
         ZIC   R4,DLPSLEN                                                       
         AR    R8,R4                                                            
*                                                                               
         BAS   RE,ADDADB           ADD CLIENT/PRODUCT DETAILS                   
                                                                                
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT CLIENT                                
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CR 1C                                        
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CR13NUM    CONTRA 13                                    
         MVC   DLPSDBNM,CR13NAME                                                
         OI    DLPSTYPE,X'80'                                                   
         MVC   DLPSANAL,OFFICE                                                  
         ZAP   DLPSAMNT,EXPOST                                                  
         ZIC   R4,DLPSLEN                                                       
         LR    R6,R8                                                            
         AR    R8,R4                                                            
         B     VAL39                                                            
         EJECT                                                                  
*--------------------------------------------------------------                 
*        PUT TO ACCDAY FILE                                                     
*--------------------------------------------------------------                 
*                                                                               
VAL39    MVI   0(R8),0             MARK END                                     
         LA    R8,1(R8)            END ADDR                                     
         LA    R3,IOAREA           START ADDR                                   
         SR    R8,R3                                                            
         STH   R8,HALF             TOTAL LEN                                    
         MVC   IOAREA(2),HALF      TO START OF REC                              
         BAS   RE,PUTDAYX          ADD RECORDS TO TRANSACTION FILE              
*                                                                               
         XC    WORK,WORK           ADD ENTRY TO TWA1                            
         ZIC   R3,ATRDOCH+5                                                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ATRDOC      REF                                          
         L     R3,DMCB+8           RETURNED FROM PUTDAY                         
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         ZAP   TRANSAMT,GRSPOST                                                 
         BAS   RE,ADSCRINF                                                      
*                                                                               
         MVI   USERPROV,C'N'                                                    
         MVI   USERPST,C'N'                                                     
         MVI   CLEAROK,C'Y'                                                     
         MVI   ACTV,C'N'                                                        
         SR    R0,R0               GOOD CC RETURN                               
ELEX     XMOD1 1                   RETURN                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ADD A RECORD TO THE DAILY TRANSACTION FILE                             
*--------------------------------------------------------------                 
*                                                                               
PUTDAYX  LA    R1,BCFULL                                                        
         XC    0(4,R1),0(R1)       CLEAR DISK ADDRESS                           
         ST    R1,BOPARM+8         SET A(DISK ADDRESS)                          
         BR    RE                                                               
*UTDAYX  NTR1                                                                   
*        MVI   ERRNUM,OK                                                        
*        GOTO1 AADACDAY,DMCB,IOAREA,,DUB                                        
*        CLI   ERRNUM,OK                                                        
*        BNE   ERRXITX                                                          
*        B     ELEX                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
* ADSCRINF - POST & SAVE 2ND SCREEN'S INFO, LIKE ADTWA1                         
*--------------------------------------------------------------                 
*                                                                               
ADSCRINF NTR1                                                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         LA    RF,XTRAELM                                                       
         GOTO1 AADACDAY,BOPARM,(X'80',IOAREA),BOPL61,BOWORK1,(RF)               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   ERRXITX                                                          
         B     ELEX                                                             
*                                                                               
ERRXITX  L     RD,AWORK            RETURN TO BASE                               
         L     RD,8(RD)            AT POINT OF OVERLAY CALL                     
         ST    R2,FADR                                                          
         B     ELEX                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ADD FREEFORM ELEMENT FOR CLIENT AND PRODUCT                            
*--------------------------------------------------------------                 
ADDADB   CLI   FFTELEM,0           DO WE HAVE A CLIENT AND PRODUCT?             
         BER   RE                  NO, DONE                                     
         LA    R1,FFTELEM                                                       
         USING FFTELD,R1                                                        
         SR    RF,RF                                                            
         IC    RF,FFTLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),FFTELEM                                                  
         LA    R8,1(RF,R8)         R8 TO NEXT AREA                              
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        LITERAL DECLARATIONS                                                   
*--------------------------------------------------------------                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
CTAXMOD  DS    0D                                                               
         NMOD1 0,**CTAX**                                                       
         L     RC,0(R1)                                                         
         USING PROGD,RC                                                         
         USING TWAD,RA                                                          
         USING GWS,R9                                                           
         MVI   CTXMODE,C'E'                                                     
         CLI   MODE,3                                                           
         BE    CTAX50                                                           
         MVI   CTXMODE,C'B'                                                     
         CLI   MODE,4                                                           
         BNE   *+8                                                              
         MVI   CTXMODE,C'G'        GO THROUGH                                   
*                                                                               
         CLI   CSACT,ACTCHA                                                     
         BNE   CTAX10                                                           
         CLI   CTAXBEF,C'Y'        DID WE READ BEFORE?                          
         BE    *+8                 YES, DON'T RESET AGAIN                       
         BAS   RE,EXTRAELM                                                      
CTAX10   MVC   CTXXTELM,XTRAELM                                                 
*                                                                               
         LA    R0,8                CLEAR AMOUNT TABLE                           
         LA    R3,AMTBLK                                                        
CTAX11   MVC   0(2,R3),SPACES                                                   
         ZAP   2(6,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R0,CTAX11                                                        
*                                                                               
         LA    R3,AMTBLK                                                        
         L     R4,AWCH                                                          
         USING SCRNCED,R4                                                       
         L     R5,ATABH            A(LAST FIELD)                                
*                                                                               
CTAX11A  LA    R1,6                                                             
         LR    R2,R4                                                            
CTAX11B  TM    4(R2),X'20'         VALIDATED BEFORE?                            
         BZ    CTAX12              NO, KEEP AMOUNT                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R1,CTAX11B                                                       
         B     CTAX13X             WHOLE LINE VALIDATED BEFORE, NEXT            
*                                                                               
CTAX12   LA    R2,AMNTD                                                         
         CLI   5(R2),0             TEST FOR AN AMOUNT                           
         BE    CTAX14X                                                          
*                                                                               
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R0)                                  
         CLI   0(R1),0             TEST OK                                      
         BE    *+6                                                              
         DC    H'0'                MUST BE OK ON SECOND PASS                    
*                                                                               
         ZAP   2(6,R3),4(8,R1)     CONVERTED AMOUNT                             
         LA    R2,WC1D                                                          
         CLI   5(R2),0             WORK CODE                                    
         BNZ   CTAX13                                                           
         LA    R2,WC2D             NON-COMMISION WORK-CODE                      
         CLI   5(R2),0                                                          
         BNZ   CTAX13                                                           
         LA    R2,ODSD             EXPENSE ACCOUNT, USE OFF/DEPT/STAFF          
CTAX13   MVC   0(2,R3),8(R2)       WORKCODE                                     
         LA    R3,8(R3)                                                         
CTAX13X  LA    R4,LNTOLN(R4)                                                    
         CR    R4,R5                                                            
         BL    CTAX11A                                                          
*                                                                               
CTAX14X  MVC   CTXACC(1),COMPANY                                                
         MVC   CTXOFF,CURROFFC                                                  
         MVC   CTAX46OF,OFFCLIST                                                
         L     R2,AGORNH                                                        
         MVC   CTXGORN,8(R2)                                                    
         L     R2,APROVH                                                        
         MVC   CTXPROV,8(R2)                                                    
         MVC   CTXDATE,SVDATE1                                                  
         MVC   CTXVENGT,VENDTYPE                                                
         MVC   CTXVENPT,VENDPSTT                                                
         L     R2,ATYPEH                                                        
         MVC   CTXGSTT,8(R2)                                                    
         L     R2,ATYPNH                                                        
         MVC   CTXGSTTN,8(R2)                                                   
         L     R2,AGAMTH                                                        
         MVC   CTXLGSTA,5(R2)                                                   
         MVC   CTXGSTA,8(R2)                                                    
         MVC   CTXUPSTT,USERPST                                                 
         MVC   CTXUPROV,USERPROV                                                
*                                                                               
CTAX15   MVC   CTXCNTRA,SVCRACCT   VENDOR                                       
         MVC   CTXCNTRN,SVACCTNM                                                
         CLI   SVCRACCT,0                                                       
         BNE   CTAX50                                                           
         MVC   CTXCNTRA,SVCRACC2   EXPENSE VENDOR                               
         MVC   CTXCNTRN,SVACCTN2                                                
         CLI   SVCRACC2,0                                                       
         BNE   CTAX50                                                           
         MVC   CTXCNTRA,SVCSHNUM   CASH ACCOUNT                                 
         MVC   CTXCNTRN,SVCSHNAM                                                
*                                                                               
CTAX50   LA    R3,X'41'            CTAX SCREEN                                  
         GOTO1 CALLOV,DMCB,((R3),0),(0,0)                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(X'2E',CTXDATA),(R9),AMTBLK,AMTTAB                     
         MVI   MODE,3                                                           
         CLI   CTXMODE,C'E'                                                     
         BE    CTAXXIT                                                          
         CLI   CTXMODE,C'G'        GOOD 1-PASS                                  
         BE    CTAX60                                                           
         CLI   CTXMODE,C'Z'        ERROR ON 1-PASS                              
         BE    CTAX60                                                           
         CLI   CTXMODE,C'X'                                                     
         BNE   CTAXXIT                                                          
CTAX60   MVI   MODE,0                                                           
         CLI   CTXMODE,C'Z'                                                     
         BE    *+8                                                              
         MVI   CTAXBEF,C'Y'        USED CTAX BEFORE                             
*                                                                               
         MVC   GSTSW,CTXAPPL       SEE IF GST/PST APPLICABLE                    
*                                                                               
         MVC   TMPNET,CTXNET                                                    
         MVC   TMPGST,CTXGST                                                    
         MVC   TMPPST,CTXPST                                                    
         MVC   TMPGRS,CTXGRS                                                    
*                                                                               
         MVC   USERPST,CTXUPSTT                                                 
         CLI   CTXUPROV,C'Y'                                                    
         BNE   CTAX61                                                           
         MVI   USERPROV,C'Y'                                                    
         MVC   KEEPPROV,CTXPROV                                                 
*                                                                               
CTAX61   L     R2,AGORNH           SHOW CHANGES                                 
         MVC   8(1,R2),CTXGORN                                                  
         OI    6(R2),X'80'                                                      
         L     R2,APROVH                                                        
         MVC   8(2,R2),CTXPROV                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPEH                                                        
         MVC   8(1,R2),CTXGSTT                                                  
         OI    6(R2),X'80'                                                      
         L     R2,ATYPNH                                                        
         MVC   8(21,R2),CTXGSTTN                                                
         OI    6(R2),X'80'                                                      
         L     R2,AGAMTH                                                        
         MVC   8(10,R2),SPACES                                                  
         MVC   5(1,R2),CTXLGSTA                                                 
         CLI   CTXLGSTA,0                                                       
         BZ    CTAX65                                                           
         ZIC   R1,CTXLGSTA                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),CTXGSTA                                                  
CTAX65   OI    6(R2),X'80'                                                      
*                                                                               
         L     R2,AGSTXH                                                        
         XC    8(L'ATCGSTX,R2),8(R2)                                            
         CLI   GSTSW,C'Y'                                                       
         BNE   CTAX69                                                           
         OC    CTXGST,CTXGST       DO WE HAVE GST?                              
         BZ    CTAX70                                                           
         MVC   8(4,R2),=C'GST='                                                 
         EDIT  CTXGST,(14,12(R2)),2,ZERO=NOBLANK,ALIGN=LEFT                     
CTAX69   OI    6(R2),X'80'                                                      
*                                                                               
CTAX70   MVC   XTRAELM,CTXXTELM                                                 
         L     R2,ATYPEH           PUT A * IN AMT WHEN WE RETURN                
         ST    R2,FADR                                                          
         CLI   CTXMODE,C'G'        EXCEPT FOR 1 PASS                            
         BE    CTAXXIT                                                          
         OI    6(R2),X'01'         MODIFIED FIELD, TO HIT ENTER                 
*                                                                               
         CLI   CTXMODE,C'Z'                                                     
         BNE   CTAXXIT                                                          
         L     R2,AGORNH                                                        
         CLI   CTXERR,1                                                         
         BNE   *+8                                                              
         L     R2,ATYPEH                                                        
         CLI   CTXERR,2                                                         
         BNE   *+8                                                              
         L     R2,AGAMTH                                                        
         CLI   CTXERR,3                                                         
         BNE   *+8                                                              
         L     R2,APROVH                                                        
         ST    R2,FVADDR                                                        
*                                                                               
CTAXXIT  XMOD1                                                                  
         EJECT                                                                  
*------------------------------------------------------------------             
* EXTRAELM - EXTRACTS THE SPECIAL 2ND SCREEN FROM ITEM RECORD                   
*------------------------------------------------------------------             
EXTRAELM NTR1                                                                   
         LA    R2,BCITECUR         HAVE TO READ ITEM RECORD                     
         USING LSTTABD,R2                                                       
         MVC   IODAOVER,LSTTDA                                                  
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING TBARECD,R2                                                       
         LA    R2,TBARFST          DISP TO 1ST ELEMENT                          
         USING SFSELD,R2                                                        
EXTRA10  CLI   SFSEL,0             END OF ELEMENTS                              
         BE    EXTRAXIT                                                         
         CLI   SFSEL,SFSELQ        SCREEN FIELD SAVE ELEMENT                    
         BNE   EXTRA50                                                          
         CLI   SFSFLDN,128         1ST FIELD # SAVED                            
         BNE   EXTRA50                                                          
         MVC   XTRAELM,0(R2)       COPY CHUNK                                   
         B     EXTRAXIT                                                         
*                                                                               
EXTRA50  ZIC   R0,SFSLN                                                         
         AR    R2,R0                                                            
         B     EXTRA10                                                          
EXTRAXIT XIT1                                                                   
*                                                                               
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        WORKING STORAGE DSECT                                                  
*--------------------------------------------------------------                 
*                                                                               
PROGD    DSECT                                                                  
*                                                                               
RELOA    DS    F                                                                
BLDELS   DS    A                                                                
ERRCHK   DS    A                   ERROR CHECK ROUTINE                          
ACTAXMOD DS    A                   GST/PST EDIT ROUTINE                         
ANARH    DS    A                   A(FIRST NARRATIVE FIELD HEADER)              
AWCH     DS    A                   A(FIRST WORKCODE FIELD  HEADER)              
ATABH    DS    A                   A(TAB FIELD HEADER)                          
ATOTH    DS    A                   A(TOTALS FIELD HEADER)                       
ATYPEH   DS    A                   A(GST TYPE FIELD HEADER)                     
ATYPNH   DS    A                   A(GST NAME HEADER)                           
AGORNH   DS    A                   A(GROSS OR NET HEADER)                       
APROVH   DS    A                   A(PROVINCE CODE)                             
AGSTXH   DS    A                   A(GST EXTRA DATA HEADER)                     
AGAMTH   DS    A                   A(GST AMOUNT FIELD HEADER)                   
*                                                                               
ELCODE   DS    XL1                 USE FOR ELEMENT                              
*                                                                               
SVCRACCT DS    CL15                PROD VEN KEY                                 
SVACCTNM DS    CL36                PROD VEN NAME                                
SVCRACC2 DS    CL15                EXP VEN KEY                                  
SVACCTN2 DS    CL36                EXP VEN NAME                                 
DISC     DS    PL3                 CD VAL FOR PROD VENDS                        
DISC2    DS    PL3                 CD VAL FOR EXP VENDS                         
PVENTYPE DS    CL1                 DEFAULT TAX TYPE                             
EVENTYPE DS    CL1                 DEFAULT TAX TYPE                             
*                                                                               
SVJOBNUM DS    CL15                                                             
SVJOBNM  DS    CL36                                                             
SVDATE1  DS    CL3                                                              
SAVNUM   DS    CL15                                                             
SAVNAM   DS    CL36                                                             
*                                                                               
SVSTAT   DS    CL1                                                              
SVSTAT2  DS    CL1                                                              
*                                                                               
SIMDSW   DS    CL1                                                              
SIMDAC   DS    CL15                                                             
SIMDACN  DS    CL36                                                             
*                                                                               
SVCLINUM DS    CL15                                                             
SVCLINM  DS    CL36                                                             
SVPRDNUM DS    CL15                                                             
SVPRDNM  DS    CL36                                                             
AOFFC    DS    A                   ADDRESS OF CURRENT OFFC                      
CURROFFC DS    CL2                                                              
OFFCLIST DS    CL8                 OFFICE LIST                                  
*                                                                               
STFSW    DS    CL1                                                              
PERSNUM  DS    CL15                                                             
PERSNAME DS    CL36                                                             
SVWRKCD  DS    CL2                                                              
*                                                                               
GRSPOST  DS    PL6                                                              
NETPOST  DS    PL6                                                              
GSTPOST  DS    PL6                                                              
PSTPOST  DS    PL6                                                              
SVAMNT   DS    PL6                                                              
EXPOST   DS    PL6                                                              
CSHTOT   DS    PL6                 ACCUMULATING ITEM TOTAL                      
CDAMNT   DS    PL6                                                              
*                                                                               
SVDOC    DS    CL6                                                              
CONSULT  DS    CL1                                                              
VENDSW   DS    CL1                                                              
VENDTYPE DS    CL1                 VENDOR'S GST TYPE                            
VENDPSTT DS    CL1                 VENDOR'S PST TYPE                            
VENDPROV DS    CL2                 VENDOR'S PROVINCE, VNDR REC (LFM)            
*                                                                               
COUNT    DS    H                                                                
DEPSW    DS    CL1                                                              
LEN      DS    H                                                                
OFFSW    DS    CL1                                                              
SAVENARR DS    CL120                                                            
SAVEEXP  DS    A                   A(CURRENT EXPENSE FIELD HEADER)              
SAVEWC   DS    A                   A(CURRENT WORK CODE FIELD HEADER)            
SAVEAMNT DS    A                   A(CURRENT AMOUNT FIELD HEADER)               
PRDLEN   DS    F                   LENGTH OF PRD INPUT                          
JOBLEN   DS    F                   LENGTH OF JOB INPUT                          
*                                                                               
PSCLINUM DS    CL15                RETURNED FROM PROFMERGE                      
SVWRKNAM DS    CL35                WORK CODE EXPANSION                          
SANUM    DS    CL15                S/A CONTRA ACCT DETAILS                      
SANAM    DS    CL36                                                             
CURSPOS  DS    A                   CURSOR POSITIONING SAVE                      
ADVSW    DS    CL1                 PRODUCTION EXP                               
AGYSW    DS    CL1                 AGY EXP                                      
PASSCD   DS    CL1                 PASS CD TO CLIENT                            
COMSW    DS    CL1                 COMMISSIONABLE WORK CODE INDICATOR           
DFLTSW   DS    CL1                 DEFAULT CREDIT ACCT READ INDICATOR           
*                                                                               
COSTSW   DS    XL1                                                              
COSTACC  EQU   X'80'               COST ACCOUNTING                              
COSTNEW  EQU   X'40'               NEW COSTING                                  
COSTANAL DS    CL5                 COST CODE FROM SE ACCOUNT                    
*                                                                               
SVCURLN  DS    A                   START A(CURRENT DTL LINE)                    
POSTACC  DS    CL15                AGY EXP KEY                                  
POSTACCN DS    CL36                AGY EXP NAME                                 
CRDSNUM  DS    CL15                2/8 ACCT KEY                                 
CRDSNAME DS    CL36                NAME                                         
COSTNUM  DS    CL15                1/C ACCT KEY                                 
COSTNAME DS    CL36                NAME                                         
CRCNUM   DS    CL15                1/P ACCT KEY                                 
CRCNAME  DS    CL36                NAME                                         
CR13NUM  DS    CL15                1/3 ACCT KEY                                 
CR13NAME DS    CL36                NAME                                         
CRPSNUM  DS    CL15                2/9 ACCT KEY                                 
CRPSNAME DS    CL36                NAME                                         
STAFFNUM DS    CL15                2/P STAFF ACCT KEY                           
STAFFNAM DS    CL36                STAFF NAME                                   
SVCSHNUM DS    CL15                SC CASH ACCT #                               
SVCSHNAM DS    CL36                SC CASH ACCT NAME                            
P2CNUM   DS    CL15                2C NUM  PROD                                 
P2CNAM   DS    CL36                2C NAME PROD                                 
E2CNUM   DS    CL15                2C NUM  EXP                                  
E2CNAM   DS    CL36                2C NAME EXP                                  
PROTROL  DS    CL15                27 NUM  PROD                                 
PROTROLN DS    CL36                27 NAME PROD                                 
EXPTROL  DS    CL15                27 NUM  EXP                                  
EXPTROLN DS    CL36                27 NAME EXP                                  
PCONSULT DS    CL1                 1099(2C) VENDOR SWITCH PROD VENDOR.          
ECONSULT DS    CL1                 1099(2C) VENDOR SWITCH EXP VENDOR.           
PRSTAT   DS    CL1                 PROD VENDOR 1099 STATUS                      
EXSTAT   DS    CL1                 EXP VENDOR 1099 STATUS                       
V29SW    DS    CL1                 EXPENSE VENDOR CONTRA.                       
FRSTPASS DS    CL1                 FIRST PASS SWITCH                            
SAVEODS  DS    A                   SAVED A(OFF,DEPT,STAFF HDR)                  
SAVCLPR  DS    A                   SAVED A(CLI,PRO HDR)                         
BLOCK    DS    6CL32               SCANNER AREA OFF,DEPT,STAFF                  
CLIPRO   DS    CL15                CLI,PRO ACCT KEY                             
CLIPRON  DS    CL36                CLI,PRD NAME FOR DISP                        
DEPNUM   DS    CL15                DEPT ACCT KEY                                
DEPNAME  DS    CL36                DEPT NAME                                    
DEPSTFN  DS    CL36                OFF,DEPT,STAFF NAME FOR DISP                 
*                                                                               
OFFICE   DS    CL2                                                              
OFFICEL  DS    XL1                                                              
DEPT     DS    CL4                                                              
DEPTL    DS    XL1                                                              
STAFF    DS    CL7                                                              
STAFFL   DS    XL1                                                              
*                                                                               
LEVEL    DS    XL1                                                              
ODSSW    DS    XL1                                                              
INPUTSW  DS    XL1                                                              
*                                                                               
OFFCBIT  EQU   X'04'               OFFICE BIT                                   
DEPTBIT  EQU   X'02'               DEPARTMENT BIT                               
STFFBIT  EQU   X'01'               STAFF BIT                                    
*                                                                               
SCAN     DS    4CL32               SCANNER AREA CLI,PRO                         
BYTE     DS    CL1                 TENO DIRECTED EXP ACCT IPT LEN               
*                                                                               
TOTNET   DS    PL6                                                              
TOTGRS   DS    PL6                                                              
TOTGST   DS    PL6                                                              
TOTPST   DS    PL6                                                              
SVTAXCOD DS    CL1                                                              
GORN     DS    CL1                                                              
NAMTS    DS    XL1                                                              
NGST     DS    XL1                                                              
AMTTAB   DS    6CL(AMTLNQ)                                                      
ALARGEST DS    A                                                                
AAMT     DS    A                   AMOUNT TABLE POINTER                         
APOST    DS    A                   ADDRESS OF POSTINGS                          
TOTPROD  DS    PL6                 PRODUCTION TOTAL                             
TOTEXPN  DS    PL6                 EXPENSE TOTAL                                
CATBLK   DS    CL(CATLNQ)          CATEGORY BLOCK                               
FFTELEM  DS    XL(FFTLN1Q+L'FFTDLEN+L'FFTCLPRA) CLIENT PRODUCT ELEMENT          
*                                                                               
       ++INCLUDE ACBATCTAX                                                      
*                                                                               
TMPMODE  DS    CL1                                                              
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
*                                                                               
PROGDX   DS    0C                                                               
         EJECT                                                                  
*--------------------------------------------------------------                 
*        DSECT TO COVER SCREEN LINE                                             
*--------------------------------------------------------------                 
*                                                                               
SCRNCED  DSECT                                                                  
WC1D     DS    CL18                FIRST WORK CODE                              
WCWCLN   EQU   *-SCRNCED           DISPLACEMENT FROM 1ST WC TO 2ND              
WC2D     DS    CL18                SECOND WORK CODE                             
WCEXLN   EQU   *-SCRNCED           DISPLACEMENT FROM 1ST WC TO EXP              
EXPD     DS    CL31                EXPENSE                                      
ODSD     DS    CL30                OFF/DEPT/STF                                 
CPD      DS    CL23                CLI/PRD                                      
AMNTD    DS    CL27                AMNT                                         
SCELN    EQU   *-SCRNCED           SCREEN LINE LENGTH                           
PROTFLD  DS    CL91                PROTECTED DISPLAY FIELD+HDR                  
LNTOLN   EQU   *-SCRNCED           DISP FROM DTL LN TO NEXT                     
         SPACE 2                                                                
*--------------------------------------------------------------                 
*        KEY DSECT                                                              
*--------------------------------------------------------------                 
*                                                                               
KEYD     DSECT                                                                  
KVENNUM  DS    CL15                                                             
         DS    CL17                                                             
KDATE    DS    XL3                                                              
KDOC     DS    CL6                                                              
*                                                                               
KEYSAVED DSECT                                                                  
KSVENNUM DS    CL15                                                             
         DS    CL17                                                             
KSDATE   DS    XL3                                                              
KSDOC    DS    CL6                                                              
         SPACE 2                                                                
*--------------------------------------------------------------                 
*        DSECT TO COVER AMOUNT TABLE                                            
*--------------------------------------------------------------                 
*                                                                               
AMTD     DSECT                                                                  
AMTNET   DS    PL6                                                              
AMTGRS   DS    PL6                                                              
AMTGST   DS    PL6                                                              
AMTPST   DS    PL6                                                              
AMTLNQ   EQU   *-AMTD                                                           
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATDSECT                                                             
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATCED                                                               
*--------------------------------------------------------------                 
*                                                                               
       ++INCLUDE ACBATCED                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        STORAGE ATTATCHED TO TWA                                               
*--------------------------------------------------------------                 
*                                                                               
         ORG   TWAHOLE                                                          
SAVECAC  DS    CL15                CONTRA ACCOUNT                               
SAVECACN DS    CL36                                                             
CLCDPASS DS    CL1                 OPTION BY CLIENT TO KEEP CD                  
ACTV     DS    CL1                 ACTIVITY INDICATORAR                         
*                                                                               
LCSHTOT  DS    PL6                                                              
AMTBLK   DS    CL64                                                             
GSTSW    DS    CL1                 FLAG FOR CTAX                                
CTAXBEF  DS    CL1                 USED CTAX BEFORE                             
KEEPPROV DS    CL2                 SAVE PROVINCE                                
TMPNET   DS    PL6                                                              
TMPGRS   DS    PL6                                                              
TMPGST   DS    PL6                                                              
TMPPST   DS    PL6                                                              
XTRAELM  DS    CL71                                                             
CLEAROK  DS    CL1                 FLAG FOR CLEAR GST/PST FIELD                 
USERPROV DS    CL1                 FLAG, USER ENTERED PROVINCE                  
USERPST  DS    CL1                 FLAG, USER ENTERED PST TYPE                  
         EJECT                                                                  
*--------------------------------------------------------------                 
*        ACBATC5D                                                               
*--------------------------------------------------------------                 
*                                                                               
         ORG   CONTABH                                                          
       ++INCLUDE ACBATC5D                                                       
         EJECT                                                                  
*--------------------------------------------------------------                 
*        OTHER DSECTS USED                                                      
*--------------------------------------------------------------                 
*                                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACBAT2E   12/04/12'                                      
         END                                                                    
