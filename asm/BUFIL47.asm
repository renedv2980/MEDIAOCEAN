*          DATA SET BUFIL47    AT LEVEL 007 AS OF 05/01/02                      
*PHASE T50247A                                                                  
         TITLE 'T50247 - BUDGET EXTRACT - TALENT'                               
T50247   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 (TALWKX-TALWKD),T50247,RA,RR=R8,CLEAR=YES                        
         ST    R8,RELO                                                          
         SPACE 1                                                                
         LR    R7,RC                                                            
         USING TALWKD,R7                                                        
         SPACE 1                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         SPACE 1                                                                
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE 1                                                                
         L     R0,VADUMMY                                                       
         ST    R0,NEXTADDR         SET NEXT AVAILABLE CORE ADDRESS              
         SPACE 1                                                                
         L     R3,ATWA                                                          
         USING T502FFD,R3                                                       
         SPACE 1                                                                
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 TWAVBUFF,DMCB,=C'SET',BUFFBUFF                                   
         SPACE 1                                                                
         L     R2,ARULDATA                                                      
         USING QRD,R2                                                           
         BAS   RE,TSTRUL                                                        
         ICM   R2,15,QRNEXT                                                     
         BNZ   *-8                                                              
         SPACE 1                                                                
TAL2     LA    RE,ACIOS            RE=A(RECORD SAVE AREA)                       
         LA    RF,ADLIST           RF=A(ADCON LIST)                             
         LA    R0,ADCONS           R0=N'ADCONS                                  
         ST    RE,0(RF)                                                         
         LA    RE,1000(RE)         NEXT SAVE AREA                               
         LA    RF,4(RF)            NEXT ADCON                                   
         BCT   R0,*-12                                                          
         SPACE 1                                                                
         BAS   RE,BLDMOS           BUILD MOS DATES                              
         LA    R1,SVDTYPES                                                      
         USING SVDTD,R1                                                         
         LA    R0,MAXDTYP                                                       
         SR    R4,R4               R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS                                                        
*                                                                               
TAL3     CLI   SVDTEX,0            TEST FOR EOT                                 
         BE    TAL6                YES                                          
         LA    RE,EXTTBL           RE=A(VALID DATA TYPE TABLE)                  
         LA    RF,EXTYPES          RF=N'VALID DATA TYPES                        
*                                                                               
TAL4     CLC   SVDTEX,0(RE)        TEST IF VALID FOR PROD EXTRACT               
         BE    *+16                YES                                          
         LA    RE,L'EXTTBL(RE)                                                  
         BCT   RF,TAL4                                                          
         B     TAL5                                                             
*                                                                               
         OC    DATAINDS,1(RE)       UPDATE CUMULATIVE MASK                      
         MVC   0(1,R5),0(RE)                                                    
         LA    R4,1(R4)            INCREMENT EXTRACT TYPE COUNT                 
         LA    R5,1(R5)            AND TABLE POINTER                            
*                                                                               
TAL5     LA    R1,SVDTL(R1)        NEXT DATA TYPE                               
         BCT   R0,TAL3                                                          
         DROP  R1                                                               
         EJECT                                                                  
TAL6     LTR   R4,R4                                                            
         BZ    EXIT                NO DATA TYPE                                 
         STC   R4,NEXTYPS                                                       
         SPACE 1                                                                
         L     R2,ARULDATA         GET ADDRESS OF FIRST RULE                    
         MVC   ACCCOMP,QRAGY        SAVE COMPANY CODE                           
         CLC   =C'FC',QRAGYC       TEST FOR FCB                                 
         BNE   EXIT                NO-GET OUT                                   
*                                                                               
         USING TLCOMD,R4                                                        
TAL8     LA    R4,KEY                                                           
         MVC   TLCMKEY,SPACES        GET AGENCY RECORD                          
         MVC   TLCMCMP,ACCCOMP                                                  
         BAS   RE,ACHIGH                                                        
         CLC   TLCMKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVI   TLCMUNT,C'S'                                                     
         BAS   RE,ACHIGH                                                        
         CLC   TLCMKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BAD UNIT RECORD                              
         SPACE 1                                                                
         MVI   TLCMLDG,C'M'                                                     
         BAS   RE,ACHIGH                                                        
         CLC   TLCMKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BAD LEDGER RECORD                            
*                                                                               
TAL10    MVC   TLCMCLI,QRCLT       READ THE CLIENT                              
         BAS   RE,ACHIGH                                                        
         CLC   TLCMKEY,KEYSAVE                                                  
         BNE   BADCLI                                                           
         LA    R5,ADCLI                                                         
         BAS   RE,SAVREC                                                        
         LA    RE,TLCMPRD-TLCMKEY                                               
         CLC   =C'ALL',QRPRD                                                    
         BE    *+8                                                              
         LA    RE,L'TLCMPRD(RE)                                                 
         BCTR  RE,0                                                             
         STC   RE,COMPLEN                                                       
         CLC   =C'ALL',QRPRD       TEST FOR ALL PRODUCTS                        
         BE    TAL12               YES                                          
         MVC   TLCMPRD,QRPRD       VALIDATE THE PRODUCT                         
         BAS   RE,ACHIGH                                                        
         CLC   TLCMKEY,KEYSAVE     TEST PRODUCT FOUND                           
         BNE   BADPRD                                                           
*                                                                               
TAL12    BAS   RE,ACSEQ                                                         
*                                                                               
TAL15    ZIC   R1,COMPLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TLCMKEY(0),KEYSAVE  TEST SAME CLI/(PRD)                          
         BNE   TAL900              ALL DONE-WRAP UP                             
         CLC   LASTCOM,TLCMKEY     TEST FOR CHANGE IN COMMERCIAL                
         BE    TAL20               NO                                           
         LA    R5,ADCOM            YES SAVE NEW RECORD                          
         BAS   RE,SAVREC                                                        
         MVC   LASTCOM,TLCMKEY     SAVE LAST COMMERCIAL                         
         BAS   RE,FILTCOM          FILTER COMMERCIAL                            
         CLI   FILTSW,C'Y'                                                      
         BE    TAL18                                                            
*                                                                               
         MVC   TLCMWRK,XFF         SKIP TO NEXT COMMERCIAL                      
         MVC   TLCMPH,SPACES                                                    
         BAS   RE,ACHIGH                                                        
         B     TAL15                                                            
*                                                                               
TAL18    MVC   TLCMBCDE,=C'999'    SKIP TO BILLING RECORDS                      
         BAS   RE,ACHIGH                                                        
         B     TAL15                                                            
*                                                                               
TAL20    CLC   TLCMBCDE,=C'999'    TEST FOR BILLING RECORD                      
         BNE   *+18                                                             
         CLC   TLCMBDTE,SPACES     TEST FOR BILLING HEADER RECORD               
         BE    *+8                 YES-SKIP IT                                  
         BAS   RE,PROCBL                                                        
         B     TAL12               GET NEXT RECORD                              
         EJECT                                                                  
* WRITE WORKER RECORDS *                                                        
         SPACE 1                                                                
TAL900   L     R2,ARULDATA         R2=A(RULE ENTRY)                             
*                                                                               
TAL902   ZIC   R4,NEXTYPS          R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
         XC    BUFFREC,BUFFREC                                                  
         ST    R2,BUFFRULE                                                      
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         GOTO1 VDATAHD                                                          
*                                                                               
TAL904   MVC   BUFFTYPE,0(R5)      SET EXTRACT TYPE                             
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
TAL906   MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         BAS   RE,BUFFGET                                                       
         CLI   DMCB+8,0            TEST IF ITEM FOUND                           
         BE    TAL908              YES                                          
         ZAP   BUFFGRS,=P'0'       NO-MANUFACTURE ZERO RECORD                   
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
TAL908   MVI   DMCB,BUPPUT                                                      
         TM    WHEN,X'18'          TEST OVERNIGHT                               
         BNZ   TAL910              YES                                          
         MVI   DMCB,BUPADD                                                      
         TM    WHEN,X'20'          TEST SOON                                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TAL910   GOTO1 VBUPPER                                                          
         LA    R6,SVEXL(R6)        NEXT PERIOD                                  
         OC    SVEXPER,SVEXPER     TEST FOR LAST PERIOD                         
         BNZ   TAL906              NO                                           
*                                                                               
         LA    R5,1(R5)            NEXT EXTRACT TYPE                            
         BCT   R4,TAL904                                                        
*                                                                               
         MVI   DMCB,BUPFINAL       FINAL CALL FOR OUTLINE                       
         GOTO1 VBUPPER                                                          
         ICM   R2,15,QRNEXT                                                     
         BNZ   TAL902                                                           
         B     EXIT                NO MORE RULES                                
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FILTER AGAINST COMMERCIAL RECORD                               
*                                                                               
FILTCOM  ST    RE,SAVERE                                                        
         MVI   FILTSW,C'Y'                                                      
         GOTO1 GETL,DMCB,(X'72',ADCOM),0                                        
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,ELADDR                                                        
         USING ACCOMMD,RE                                                       
         CLI   QRMED,0             TEST FOR MEDIA FILTER                        
         BE    FILTCOMX            NO                                           
         CLC   QRMED,ACCOMED       APPLY MEDIA FILTER                           
         BE    FILTCOMX                                                         
         MVI   FILTSW,C'N'                                                      
*                                                                               
FILTCOMX L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS A BILLING RECORD                                       
*                                                                               
PROCBL   NTR1                                                                   
         BAS   RE,GETPER                                                        
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         GOTO1 GETL,DMCB,(X'84',(R4)),0                                         
         CLI   ELERR,0             TEST IF BILLING HIST EL FOUND                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ABILLEL,ELADDR      YES-SAVE ITS ADDRESS                         
         LA    R6,ACRECORD                                                      
         USING TRANSD,R6                                                        
         XC    BUFFRULE,BUFFRULE                                                
         L     R2,ARULDATA                                                      
*                                                                               
PROCBL2  BAS   RE,COMFILT                                                       
         CLI   FILTSW,C'N'                                                      
         BE    PROCBL3                                                          
         BAS   RE,ENFILT                                                        
         CLI   FILTSW,C'Y'                                                      
         BE    PROCBL4                                                          
*                                                                               
PROCBL3  ICM   R2,15,QRNEXT                                                     
         BNZ   PROCBL2                                                          
         B     EXIT                                                             
*                                                                               
PROCBL4  ZAP   ACGROSS,TRNSAMNT    GET FEES                                     
         L     RE,ABILLEL                                                       
         USING ACBID,RE                                                         
         ICM   R0,15,ACBIPNW       GET P+W                                      
         ICM   R1,15,ACBITAX       GET TAXES AND INSURANCE                      
         AR    R0,R1               ADD THEM                                     
*                                                                               
         CLI   ACBILEN,ACBILNQ     TEST IF NEW ELEMENT                          
         BNE   PROCBL4A            NO                                           
         LA    RF,3                RF=COUNTER                                   
         LA    R5,ACBIHNW          R5=A(ADDITIONAL FIELDS)                      
         ICM   R1,15,0(R5)         GET H+W,INS+RET,CONTR FEE                    
         AR    R0,R1               UPDATE GROSS BUCKET                          
         LA    R5,4(R5)                                                         
         BCT   RF,*-10                                                          
*                                                                               
PROCBL4A CVD   R0,DUB                                                           
         AP    ACGROSS,DUB                                                      
         ICM   R0,15,ACBICOMM      GET AGENCY COMMISSION ON BILL                
         CVD   R0,DUB                                                           
         AP    ACGROSS,DUB         ADD IT IN TO GET FULL GROSS                  
         ZAP   ACNET,ACGROSS       COMPUTE THE NET BY                           
         SP    ACNET,DUB           DEDUCTING COMMISSION FROM GROSS              
*                                                                               
PROCBL5  TM    DATAIND,EXBADVB+EXBNADVB TEST FOR BADV                           
         BNZ   *+12                                                             
         TM    DATAIND1,EXBAADVB                                                
         BZ    PROCBL6                                                          
         OC    BADVPER,BADVPER     TEST IF BADV PER WI/IN PLAN                  
         BZ    PROCBL6             NO                                           
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,BADVPER                                                  
         ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
*                                                                               
         TM    DATAIND,EXBADVB                                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBADV                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBAADVB    ACTUAL BILL                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBAADV                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNADVB                                                 
         BZ    PROCBL6                                                          
         ZAP   BUFFGRS,BUFFNET     SET GROSS=NET                                
         MVI   BUFFTYPE,EXBNADV                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
PROCBL6  TM    DATAIND,EXBDATB+EXBNDATB TEST FOR BDAT                           
         BNZ   *+12                                                             
         TM    DATAIND1,EXBADATB                                                
         BZ    PROCBL8                                                          
         OC    BDATPER,BDATPER     TEST IF BDAT PER WI/IN PLAN                  
         BZ    PROCBL8             NO                                           
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,BDATPER                                                  
         ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
*                                                                               
         TM    DATAIND,EXBDATB                                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBDATE                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBADATB    ACTUAL BILL                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBADAT                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNDATB                                                 
         BZ    PROCBL8                                                          
         ZAP   BUFFGRS,BUFFNET     SET GROSS=NET                                
         MVI   BUFFTYPE,EXBNDATE                                                
         BAS   RE,BUFFPUT                                                       
*                                                                               
PROCBL8  TM    DATAIND,EXBINVB+EXBNINVB TEST FOR BINV                           
         BNZ   *+12                YES                                          
         TM    DATAIND1,EXBAINVB                                                
         BZ    PROCBL9                                                          
         OC    BINVPER,BINVPER     TEST IF BINV PER WI/IN PLAN                  
         BZ    PROCBL9             NO                                           
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,BINVPER                                                  
         ZAP   BUFFSPTS,=P'0'                                                   
         ZAP   BUFFGRS,ACGROSS                                                  
         ZAP   BUFFNET,ACNET                                                    
*                                                                               
         TM    DATAIND,EXBINVB     GROSS                                        
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBINV                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBAINVB    ACTUAL BILL                                  
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBAINV                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNINVB                                                 
         BZ    PROCBL9                                                          
         ZAP   BUFFGRS,BUFFNET     SET GROSS=NET                                
         MVI   BUFFTYPE,EXBNINV                                                 
         BAS   RE,BUFFPUT                                                       
*                                                                               
PROCBL9  OC    BUFFRULE,BUFFRULE   TEST IF POSTED AGAINST RULE                  
         BZ    PROCBL3             NO-GO TO NEXT RULE                           
         ICM   R2,15,QRJUMP        YES-TRY FOR ANOTHER RULE                     
         BNZ   PROCBL2                                                          
         B     EXIT                                                             
         DROP  R6,RE                                                            
         EJECT                                                                  
* FILTER BILLING RECORD AGAINST COMMERCIAL                                      
*                                                                               
COMFILT  NTR1                                                                   
         MVI   FILTSW,C'Y'                                                      
         SR    R5,R5                                                            
         ICM   R5,3,QRCOMDSP                                                    
         LA    R5,0(R2,R5)         R2=A(COMMERCIAL LIST)                        
         ZIC   R0,0(R5)            NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    EXIT                NO COMMERCIAL FILTERS                        
         LA    R5,1(R5)            R5=A(RULE LIST)                              
         L     R4,AIO              R4=A(RECORD)                                 
         USING TLCOMD,R4                                                        
         BAS   RE,SETSIGN                                                       
         CLI   SIGN,C'+'           TEST FOR POSITIVE RULE                       
         BE    *+8                 YES                                          
         MVI   FILTSW,C'N'         SET DEFAULT FILTER TO NO                     
         SPACE 1                                                                
COMFILT2 MVC   DUB(L'TLCMCOM),0(R5) EXTRACT RULE                                
         OI    DUB,X'40'           RESTORE LOWER CASE BIT                       
         CLC   TLCMCOM,DUB                                                      
         BE    EXIT                MATCHES SO RETURN OK                         
         LA    R5,LRUCOM(R5)                                                    
         BCT   R0,COMFILT2                                                      
         MVI   FILTSW,C'N'                                                      
         CLI   SIGN,C'+'                                                        
         BE    *+8                                                              
         MVI   FILTSW,C'Y'         NO MATCHES MEANS YES FOR NEG RULE            
         B     EXIT                DID NOT MATCH ANY COMMERCIAL RULE            
         EJECT                                                                  
* FILTER BILLING RECORD AGAINST ESTIMATE NUMBER                                 
*                                                                               
ENFILT   NTR1                                                                   
         MVI   FILTSW,C'Y'                                                      
         SR    R5,R5                                                            
         ICM   R5,3,QRESNDSP                                                    
         LA    R5,0(R2,R5)         R2=A(EST NUMBER LIST)                        
         ZIC   R0,0(R5)            NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    EXIT                NO EST NUMBER FILTERS                        
         LA    R5,1(R5)            R5=A(RULE LIST)                              
         L     R6,ABILLEL                                                       
         USING ACBID,R6                                                         
         BAS   RE,SETSIGN                                                       
         CLI   SIGN,C'+'           TEST FOR POSITIVE RULE                       
         BE    *+8                 YES                                          
         MVI   FILTSW,C'N'         SET DEFAULT FILTER TO NO                     
         SPACE 1                                                                
ENFILT2  MVC   DUB(L'ACBIEST),0(R5) EXTRACT RULE                                
         OI    DUB,X'40'           RESTORE LOWER CASE BIT                       
         CLC   ACBIEST,DUB         ESTIMATE NUMBER AGAINST RULE                 
         BE    EXIT                MATCHES SO RETURN OK                         
         LA    R5,LRUESN(R5)                                                    
         BCT   R0,ENFILT2                                                       
         MVI   FILTSW,C'N'                                                      
         CLI   SIGN,C'+'                                                        
         BE    *+8                                                              
         MVI   FILTSW,C'Y'         NO MATCHES MEANS YES FOR NEG RULE            
         B     EXIT                DID NOT MATCH ANY ESTIMATE NUMBER            
         EJECT                                                                  
* SUB-ROUTINE TO SET PERIODS FOR A BILLING RECORD                               
* AT ENTRY, AIO POINTS TO RECORD                                                
*                                                                               
GETPER   NTR1                                                                   
         L     R4,AIO                                                           
         USING TLCOMD,R4                                                        
         GOTO1 DATCON,DMCB,(1,TLCMBDTE),(3,FULL)                                
         GOTO1 SETPER,BDATPER                                                   
*                                                                               
         USING ACKEYD,R4                                                        
GETPER2  LA    R6,ACRECORD                                                      
         USING TRANSD,R6                                                        
         XC    BADVPER,BADVPER                                                  
         LA    R1,MOSTAB                                                        
         LA    R0,12                                                            
         CLC   TRNSBTCH(2),2(R1)   MATCH MONTH OF SERVICE                       
         BE    *+16                                                             
         LA    R1,4(R1)                                                         
         BCT   R0,*-14                                                          
         B     *+10                NOT THE MONTH I WANT                         
         MVC   BADVPER,0(R1)       SET PERIOD                                   
         SPACE 1                                                                
GETPER4  GOTO1 DATCON,DMCB,(1,TRNSDATE),(3,FULL)                                
         GOTO1 SETPER,BINVPER                                                   
*                                                                               
GETPERX  B     EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SET PERIOD FOR TRANSACTION                                     
* AT ENTRY, R1=A(PERIOD) AND FULL(3) CONTAINS BINARY YMD                        
*                                                                               
SETPER   ST    RE,SAVERE                                                        
         LR    RE,R1                                                            
         XC    0(2,RE),0(RE)                                                    
         CLC   FULL(3),SVEXTSTB    TEST RUN ON DATE W/IN PLAN                   
         BL    SETPERX                                                          
         CLC   FULL(3),SVEXTNDB                                                 
         BH    SETPERX                                                          
*                                                                               
         LA    R1,SVEXTDTS                                                      
         USING SVEXD,R1                                                         
*                                                                               
SETPER2  CLC   FULL(3),SVEXSTB     MATCH ON PERIOD START DATE                   
         BL    SETPER3                                                          
         CLC   FULL(3),SVEXENDB    AND ON END DATE                              
         BH    SETPER3                                                          
         MVC   0(2,RE),SVEXPER                                                  
         B     SETPERX                                                          
*                                                                               
SETPER3  LA    R1,SVEXL(R1)                                                     
         B     SETPER2                                                          
*                                                                               
SETPERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR CONSISTENT RULES                                      
*                                                                               
TSTRUL   ST    RE,SAVERE                                                        
         LA    R6,SIGNTAB                                                       
         SPACE 1                                                                
TSTRUL2  CLI   0(R6),X'FF'         TEST FOR EOT                                 
         BE    TSTRULX                                                          
         SR    RF,RF                                                            
         ICM   RF,3,0(R6)          GET DISPLACEMENT INTO RULE                   
         LA    RF,0(R2,RF)                                                      
         SR    R5,R5                                                            
         ICM   R5,3,0(RF)          PICK UP DSP TO RULE ENTRIES                  
         LA    R5,0(R2,R5)                                                      
         ZIC   R0,0(R5)                                                         
         LTR   R0,R0               TEST FOR ANY RULES                           
         BZ    TSTRUL4                                                          
         LA    R5,1(R5)            POINT AT FIRST JOB                           
         BAS   RE,SETSIGN          SET SIGN OF RULE FROM 1ST JOB                
         ZIC   R1,2(R6)            R1=L'RULE ENTRY                              
         GOTO1 CHKRUL,(R1)                                                      
         BE    TSTRUL4                                                          
         ICM   RF,15,3(R6)         GET ERROR BRANCH POINT                       
         A     RF,RELO                                                          
         BR    RF                                                               
         SPACE 1                                                                
TSTRUL4  LA    R6,L'SIGNTAB(R6)                                                 
         B     TSTRUL2                                                          
         SPACE 1                                                                
TSTRULX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
SETSIGN  MVI   SIGN,C'+'                                                        
         TM    0(R5),X'40'                                                      
         BO    *+8                                                              
         MVI   SIGN,C'-'                                                        
         BR    RE                                                               
         SPACE 1                                                                
* SUB-ROUTINE TO CHECK FOR CONSISTENT RULES (POSITIVE OR NEGATIVE)              
* AT ENTRY, R1=L'RULE ENTRY, R0=N'RULES, R5=A(FIRST RULE)                       
* ON EXIT, CC=EQ IF OK, NEQ IF ERROR                                            
*                                                                               
CHKRUL   CLI   SIGN,C'+'                                                        
         BNE   CHKRUL2                                                          
         TM    0(R5),X'40'                                                      
         BZ    CHKRULR                                                          
         LA    R5,0(R1,R5)                                                      
         BCT   R0,*-12                                                          
         B     CHKRULOK                                                         
         SPACE 1                                                                
CHKRUL2  TM    0(R5),X'40'                                                      
         BO    CHKRULR                                                          
         LA    R5,0(R1,R5)                                                      
         BCT   R0,CHKRUL2                                                       
         SPACE 1                                                                
CHKRULOK CR    RB,RB                                                            
         B     CHKRULX                                                          
         SPACE 1                                                                
CHKRULR  LTR   RB,RB                                                            
         SPACE 1                                                                
CHKRULX  BR    RE                                                               
         SPACE 2                                                                
SIGNTAB  DS    0XL7                                                             
         DC    AL2(QRCOMDSP-QRD),AL1(LRUCOM),AL4(BADCOM)                        
         DC    AL2(QRESNDSP-QRD),AL1(LRUESN),AL4(BADESN)                        
         DC    X'FF'                                                            
         DS    0H                                                               
         EJECT                                                                  
*              BUILD MONTH OF SERVICE LIST                                      
BLDMOS   NTR1                                                                   
         LA    R0,12                                                            
         LA    R4,SVEXTDTS                                                      
         USING SVEXD,R4                                                         
         LA    R2,MOSTAB                                                        
BLDMOS2  CLI   SVEXPER,0           TEST FOR EOT                                 
         BE    BLDMOS6                                                          
         MVC   WORK(2),SVEXPER                                                  
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(1,WORK+3)                                  
         MVC   0(2,R2),WORK        YYMM BINARY                                  
         ZIC   R3,WORK+3           YEAR                                         
         SLL   R3,28                                                            
         SRL   R3,28               STRIP OFF DECADE                             
         STC   R3,2(R2)                                                         
         OI    2(R2),X'F0'         YEAR IN CHARACTER                            
         MVC   3(1,R2),WORK+4        MONTH                                      
         OI    3(R2),X'F0'                                                      
         CLI   WORK+4,X'10'          10=A,11=B,12=C                             
         BL    BLDMOS4                                                          
         MVI   3(R2),C'A'                                                       
         CLI   WORK+4,X'11'                                                     
         BL    BLDMOS4                                                          
         MVI   3(R2),C'B'                                                       
         CLI   WORK+4,X'12'                                                     
         BL    BLDMOS4                                                          
         MVI   3(R2),C'C'                                                       
BLDMOS4  LA    R4,SVEXL(R4)                                                     
         LA    R2,L'MOSTAB(R2)                                                  
         BCT   R0,BLDMOS2                                                       
BLDMOS6  SH    R2,=Y(L'MOSTAB)     BACK UP TO LAST ENTRY                        
         MVC   ENDPER,0(R2)        EXTRACT END PERIOD                           
         MVC   STARTPER,MOSTAB     EXTRACT START PERIOD                         
         B     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO SAVE RECORDS                                          
*                                                                               
*              AT ENTRY, R5=A(DESTINATION ADCON) AND AIO=A(SOURCE)              
*                                                                               
SAVREC   NTR1                                                                   
         L     R2,0(R5)            R2=A(DESTINATION)                            
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         SR    R5,R5                                                            
         ICM   R5,3,ACLENGTH                                                    
         LA    R3,1000             FULL IO AREA LENGTH                          
         MVCL  R2,R4                RECORD FROM IO TO ACIOS                     
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETL     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     EXIT                                                             
         EJECT                                                                  
ACHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'ACCOUNT'                                               
         TM    SVTRACE,X'80'                                                    
         BZ    ACGET                                                            
         LA    R0,KEYSAVE          SET TRACE PARAMETERS                         
         ST    R0,TRIO1                                                         
         MVI   TRIO1,42                                                         
         MVC   TRIO2,AIO                                                        
         MVI   TRIO2,42                                                         
         B     ACGET                                                            
*                                                                               
ACSEQ    NTR1                                                                   
         MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'ACCOUNT'                                               
         TM    SVTRACE,X'80'                                                    
         BZ    ACGET                                                            
         MVC   TRIO1,AIO                                                        
         MVI   TRIO1,42                                                         
         XC    TRIO2,TRIO2                                                      
         SPACE 1                                                                
ACGET    DC    0H'0'                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,AIO                    
         L     R2,AIO                                                           
         MVC   KEY(42),0(R2)                                                    
         TM    SVTRACE,X'80'                                                    
         BZ    *+8                                                              
         BAS   RE,ACTRACE                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              ERROR ROUTINES                                                   
         SPACE 1                                                                
BADCLI   GOTO1 VPRTRULE,PARAS,0    PRINT ALL RULES                              
         MVC   P+1(30),=CL30'** ERROR - BAD CLIENT CODE **'                     
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADPRD   GOTO1 VPRTRULE,PARAS,0                                                 
         MVC   P+1(30),=CL30'** ERROR - BAD PRODUCT CODE **'                    
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADCOM   GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(36),=CL36'** ERROR - INCONSISTENT COM RULES **'              
         B     ACTRAC2                                                          
         SPACE 1                                                                
BADESN   GOTO1 VPRTRULE,PARAS,(R2)                                              
         MVC   P+1(41),=CL36'** ERROR - INCONSISTENT EST NUM RULES **'          
         B     ACTRAC2                                                          
         EJECT                                                                  
ACTRACE  NTR1                                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RE,DMCB                                                          
         MVC   P(6),0(RE)                                                       
         L     RE,DMCB+4                                                        
         MVC   P+8(7),0(RE)                                                     
*                                                                               
         LA    R4,P+16                                                          
         ZIC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
*        A     R4,DMCB+16                                                       
*        LA    R4,2(R4)                                                         
*                                                                               
         OC    TRIO2,TRIO2                                                      
         BZ    ACTRAC2             FOR SEQUENTIAL NO SET PARAM                  
         LA    R4,P2+16                                                         
         ZIC   R0,TRIO2            GET OUTPUT REC LEN                           
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
*                                                                               
ACTRAC2  GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                RETURN TO CALLER                             
         DROP  R8                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
BUFFPUT  MVC   COMMAND(8),=CL8'BPUT'                                            
         B     GOBUFF                                                           
*                                                                               
BUFFGET  MVC   COMMAND(8),=CL8'BGET'                                            
*                                                                               
GOBUFF   LR    R0,RE               SAVE CALLING REG                             
         L     R3,ATWA                                                          
         L     RF,TWAVBUFF                                                      
         GOTO1 (RF),DMCB,COMMAND+1,BUFFBUFF,BUFFREC,1                           
         LR    RE,R0               RESTORE CALLING REG                          
*                                                                               
         TM    SVTRACE,X'40'       TEST TRACE BUFFALO                           
         BZR   RE                                                               
*                                                                               
BUFFTRC  NTR1                                                                   
         MVC   WORK(16),DMCB       SAVE DMCB                                    
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVC   P(5),COMMAND                                                     
*                                                                               
         L     R4,BUFFRULE                                                      
         LA    R5,QRNODE-QRD(R4)                                                
         GOTO1 HEXOUT,DMCB,(R5),P+7,4,=C'TOG'                                   
*                                                                               
         LA    R5,QRCODE-QRD(R4)                                                
         MVC   P+17(8),0(R5)                                                    
*                                                                               
         GOTO1 HEXOUT,DMCB,BUFFREC,P+27,32,=C'TOG'                              
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+8,P+92,1,=C'TOG'                                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   DMCB(16),WORK       RESTORE DMCB                                 
         B     EXIT                                                             
*                                                                               
BUFFBUFF DS    A                                                                
         SPACE 1                                                                
* TABLE OF VALID EXTRACT TYPES FOR PRODUCTION EXTRACT                           
*                                                                               
EXTTBL   DS    0CL3                                                             
         DC    AL1(EXBADV),AL1(EXBADVB),AL1(0)                                  
         DC    AL1(EXBDATE),AL1(EXBDATB),AL1(0)                                 
         DC    AL1(EXBNADV),AL1(EXBNADVB),AL1(0)                                
         DC    AL1(EXBNDATE),AL1(EXBNDATB),AL1(0)                               
         DC    AL1(EXBINV),AL1(EXBINVB),AL1(0)                                  
         DC    AL1(EXBNINV),AL1(EXBNINVB),AL1(0)                                
         DC    AL1(EXBAADV),AL1(0),AL1(EXBAADVB)                                
         DC    AL1(EXBADAT),AL1(0),AL1(EXBADATB)                                
         DC    AL1(EXBAINV),AL1(0),AL1(EXBAINVB)                                
EXTYPES  EQU   (*-EXTTBL)/L'EXTTBL                                              
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
XFF      DC    16X'FF'                                                          
RELO     DC    A(0)                                                             
*                                                                               
* EQUATES                                                                       
*                                                                               
         SPACE 2                                                                
         BUFF  LINES=250,ROWS=1,COLUMNS=3,FLAVOR=PACKED,               X        
               KEYLIST=(8,A)                                                    
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
TALWKD   DSECT                                                                  
ADLIST   DS    0A                  ADCON LIST                                   
ADCLI    DS    A                   A(CLIENT RECORD)                             
ADCOM    DS    A                   A(COMMERCIAL RECORD)                         
ADCONS   EQU   (*-ADLIST)/4                                                     
         SPACE 1                                                                
AFSTRULE DS    A                                                                
ABILLEL  DS    A                   A(BILLING DETAILS ELEMENT)                   
         SPACE 1                                                                
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
         SPACE 1                                                                
DATAINDS DS    0XL2                                                             
DATAIND  DS    X                                                                
DATAIND1 DS    X                                                                
NEXTYPS  DS    X                                                                
EXTYPS   DS    CL(MAXDTYP)                                                      
         SPACE 1                                                                
MOSTAB   DS    12CL4            2 BYTE BINARY YYMM/2 BYTE MOS                   
STARTPER DS    XL2                 START PERIOD (YYMM)                          
ENDPER   DS    XL2                 END PERIOD (YYMM)                            
BADVPER  DS    XL2                 BADV PERIOD (Y/M)                            
BDATPER  DS    XL2                                                              
BINVPER  DS    XL2                                                              
         SPACE 1                                                                
ACCCOMP  DS    CL1                 ACC AGENCY COMPANY CODE                      
*                                                                               
FILTSW   DS    CL1                                                              
SIGN     DS    C                                                                
ACNET    DS    PL8                                                              
ACGROSS  DS    PL8                                                              
COMPLEN  DS    X                   KEY COMPARE LENGTH - SAME CLI/(PRD)          
LASTCOM  DS    CL(L'TLCMACC)       LAST CLIENT/PRODUCT/COMMERCIAL               
         SPACE 1                                                                
         DS    0D                                                               
ACIOS    DS    (ADCONS*1000)C                                                   
TALWKX   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
       ++INCLUDE BUEXTWORKD                                                     
         SPACE 2                                                                
*  BUPPERD                                                                      
*  DDCOMFACS                                                                    
*  ACGENBOTH                                                                    
*  ACTALENT                                                                     
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACTALENT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007BUFIL47   05/01/02'                                      
         END                                                                    
