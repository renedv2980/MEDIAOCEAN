*          DATA SET ACREPAF02  AT LEVEL 001 AS OF 03/02/20                      
*PHASE ACAF02A                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE RIGHT                                                                  
         TITLE 'SAP - CREATE SPECIAL RECORDS TO STORE ACC KEYS'                 
ACAF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAF**,R9       BASE REGISTERS 11, 9                         
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACFUD,RC            RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
                                                                                
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,RUNLAST        RUN LAST                                     
         BE    RUNL                                                             
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     L     RF,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(RF)                                                         
         L     RF,ABXHOOK                                                       
         ST    RF,HEADHOOK                                                      
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
         B     XIT                                                              
         DROP  R4,RF                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
REQF     MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         MVI   OPTN,0                                                           
                                                                                
         CLI   QOPT1,C'D'          DUMP OUTPUT                                  
         BNE   *+8                                                              
         OI    OPTN,OPTDUMP                                                     
                                                                                
         USING INXMLD,R3                                                        
         LA    R3,INXREC                                                        
         L     R2,AINFIL           SET FILE DCB                                 
         OPEN  ((R2),(INPUT))      OPEN INPUT FILE                              
                                                                                
         OI    OPTN,OPTFRST                                                     
                                                                                
REQF10   DS    0H                                                               
         GET   (R2),(R3)                                                        
                                                                                
         CLC   2(8,R3),=C'<Header>'                                             
         BNE   REQF20                                                           
         TM    OPTN,OPTFRST                                                     
         BZ    REQF15                                                           
         NI    OPTN,X'FF'-OPTFRST                                               
         B     REQF20                                                           
                                                                                
REQF15   BRAS  RE,ADDXREC          ADD PREVIOUSLY BUILT RECORD                  
                                                                                
REQF20   CLC   4(12,R3),=C'<Voucher_No>'                                        
         BNE   REQF30                                                           
         BRAS  RE,BLDVCH                                                        
         XC    SVXITMTX,SVXITMTX   CLEAR SAVE ARE FOR NEXT BLOCK                
         B     REQF10                                                           
                                                                                
REQF30   CLC   4(14,R3),=C'<Gross_Amount>'                                      
         BNE   REQF40                                                           
         BRAS  RE,BLDGRS                                                        
         B     REQF10                                                           
                                                                                
REQF40   CLC   6(10,R3),=C'<Material>'                                          
         BNE   REQF50                                                           
         CLC   INXMATCD,=C'M00029'                                              
         BE    REQF45                                                           
         CLC   INXMATCD,=C'M00031'                                              
         BE    REQF45                                                           
         CLC   INXMATCD,=C'M00035'                                              
         BE    REQF45                                                           
         B     REQF10                                                           
REQF45   OI    OPTN,OPTSKIP                                                     
         B     REQF10                                                           
                                                                                
REQF50   CLC   6(11,R3),=C'<Item_Text>'                                         
         BNE   REQF10                                                           
         OI    OPTN,OPTKEY                                                      
         CLC   INXITMTX,SVXITMTX   IF KEY IS THE SAME DO NOT ADD                
         BE    REQF10                                                           
         BRAS  RE,BLDELEM                                                       
         B     REQF10                                                           
                                                                                
REQF80   DS    0H                                                               
         TM    OPTN,OPTKEY                                                      
         BZ    REQF90                                                           
         BRAS  RE,ADDXREC          ADD FINAL RECORD                             
                                                                                
REQF90   DS    0H                                                               
         CLOSE ((R2))              CLOSE INPUT FILE                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RUNL     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD NEW KEY FOR RECORD ADD                                        *         
***********************************************************************         
                                                                                
BLDCOMP  NTR1  ,                                                                
         L     R4,AIO2                                                          
         USING SPKRECD,R4                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD VOUCHER TO KEY                                                  *         
***********************************************************************         
                                                                                
BLDVCH   NTR1  ,                                                                
         L     RE,AIO2             CLEAR IO                                     
         LA    RF,2000                                                          
         XCEF                                                                   
                                                                                
         L     R4,AIO2                                                          
         USING SPKRECD,R4                                                       
*MN      MVC   SPKKEY,SPACES                CLEAR KEY                           
         XC    SPKKEY,SPKKEY                CLEAR KEY                           
         MVI   SPKTYP,SPKTYPQ               RECORD TYPE                         
         MVI   SPKSUB,SPKSUBQ               RECORD SUB TYPE                     
         MVC   SPKCPY,RCCOMPFL              COMPANY CODE                        
         MVC   SPKRLEN,=Y(SPKRFST-SPKRECD)  INITIALIZE RECORD LENGTH            
         XC    SPKRSTA,SPKRSTA              CLEAR STATUS BYTES                  
         XC    SPKRFST(2),SPKRFST                                               
*note : XSD defines voucher as 16 but we generate voucher                       
*note : and it is always 14 alphanumeric                                        
         MVC   SPKVOUCH,INXVCHNM            ADD VOUCHER NUMBER                  
                                                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD GROSS TO KEY                                                    *         
***********************************************************************         
                                                                                
BLDGRS   NTR1  ,                                                                
         L     R4,AIO2                                                          
         USING SPKRECD,R4                                                       
*        MVC   GRSWORK,SPACES                                                   
         XC    GRSWORK,GRSWORK                                                  
                                                                                
         LA    R5,GRSWORK                                                       
         LA    R6,18(R3)                                                        
         LA    R7,20                                                            
BLDGR10  CLI   0(R6),C'<'                                                       
         BE    BLDGR30                                                          
         MVC   0(1,R5),0(R6)                                                    
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R7,BLDGR10                                                       
                                                                                
BLDGR30  DS    0H                                                               
         GOTO1 RIGHT,DMCB,GRSWORK,L'GRSWORK                                     
         MVC   SPKGRAMT,GRSKEY                                                  
                                                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD NEW ELEMENT ENT                                                 *         
***********************************************************************         
                                                                                
BLDELEM  NTR1  ,                                                                
         MVC   SAPKWRK,SPACES                                                   
         MVC   ELEMENT,SPACES                                                   
                                                                                
         USING FFTELD,R5                                                        
         LA    R5,ELEMENT          BUILD ADDRESS ELEMENT                        
         MVI   FFTEL,FFTELQ        ELEMENT CODE                                 
         MVI   FFTLN,FFTXMLQ2      ELEMENT LENGTH                               
         MVI   FFTTYPE,FFTTXMLK    ELEMENT TYPE                                 
         MVI   FFTSEQ,0            SEQUENCE NUMBER                              
         MVI   FFTDLEN,FFTXMLQ1    DATA LENGTH                                  
                                                                                
         MVC   SVXITMTX,INXITMTX   SAVE KEY FOR NEXT COMPARE                    
         MVC   SAPKWRK(L'INXITMTX),INXITMTX   KEY FROM XML                      
                                                                                
         BAS   RE,BLDKEY           CONVERT KEY                                  
                                                                                
         MVC   FFTXMLKY,TKEYWORK   CONVERTED TRANSACTION KEY                    
         BAS   RE,ADDL                                                          
                                                                                
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT KEY                                                         *         
***********************************************************************         
                                                                                
BLDKEY   NTR1  ,                                                                
         USING SAPITMD,R6                                                       
         LA    R6,SAPKWRK                                                       
         USING TRNRECD,R7                                                       
         LA    R7,TKEYWORK                                                      
                                                                                
         GOTO1 HEXIN,DMCB,SAPMCPY,TRNKCPY,2                                     
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   TRNKULA,SAPMULA    MOVE U/L/ACCOUNT                              
         MVC   TRNKOFF,SAPMOFFC                                                 
                                                                                
         GOTO1 HEXIN,DMCB,SAPMCCPY,TRNKCCPY,2                                   
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   TRNKULC,SAPMCNAC                                                 
                                                                                
         GOTO1 DATCON,DMCB,SAPMDATE,WORK                                        
         PACK  DUB(4),WORK(7)                                                   
         MVC   TRNKDATE,DUB                                                     
                                                                                
         MVC   TRNKREF,SAPMREF                                                  
                                                                                
         GOTO1 HEXIN,DMCB,SAPMSEQ,TRNKSBR,2                                     
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         B     XIT                                                              
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* ADD RECORD TO THE FILE                                              *         
***********************************************************************         
                                                                                
ADDXREC  NTR1  ,                                                                
         USING SPKRECD,R4                                                       
         L     R4,AIO2                                                          
                                                                                
         TM    OPTN,OPTDUMP        DUMP OUTPUT                                  
         BNO   *+16                                                             
         LA    R2,MSGBLT                                                        
         L     R3,AIO2                                                          
         BAS   RE,DUMP                                                          
                                                                                
         MVC   DKEY,SPKKEY         CHECK IF RECORD ALREADY EXISTS               
         BAS   RE,DMHGH            READ FOR RECORD                              
                                                                                
         SR    R0,R0               R0=0 RECORD NOT FOUND                        
         CLC   DKEY,DIR            CHECK AGAINST DIR RECORD RETURNED            
         BNE   ADDR5               NOT EQUAL / NOT ON FILE                      
         LA    R0,1                                                             
         CLI   QOPT2,C'R'          REPLACE EXISTING RECORDS?                    
         BE    ADDR5                                                            
         TM    SPKKSTAT,ACTSDELT   TEST DELETED                                 
         BNO   ADDR13              ACTIVE RECORD EXISTS                         
                                                                                
ADDR5    LTR   R0,R0               TEST RECORD ON FILE                          
         BNZ   ADDR7                                                            
         BAS   RE,DMADDR           ADD TO FILE  (IN AIO2)                       
                                                                                
         TM    OPTN,OPTDUMP        DUMP OUTPUT                                  
         BNO   *+16                                                             
         LA    R2,MSGADD                                                        
         L     R3,AIO2                                                          
         BAS   RE,DUMP                                                          
         B     ADDRXIT                                                          
                                                                                
ADDR7    DS    0H                                                               
         BAS   RE,DMGETR           GET OLD RECORD (IN AIO)                      
                                                                                
         TM    OPTN,OPTDUMP        DUMP OUTPUT                                  
         BNO   *+16                                                             
         LA    R2,MSGFILE                                                       
         L     R3,AIO                                                           
         BAS   RE,DUMP                                                          
                                                                                
         L     R4,AIO                                                           
         MVC   HALF,SPKRLEN-SPKKEY(R4)                                          
         SR    R5,R5                                                            
         LH    R5,HALF                                                          
         L     RE,AIO2                                                          
         MVC   HALF,SPKRLEN-SPKKEY(RE)                                          
         SR    RF,RF                                                            
         LH    RF,HALF                                                          
         CLCL  RE,R4                                                            
         BE    ADDRXIT             IF REC HAS NOT CHANGED DO NOT WRITE          
                                                                                
*        L     R4,AIO2                                                          
*        NI    SPKRSTAT,X'FF'-X'80'  TURN OFF DELETE BIT                        
         BAS   RE,DMPUT            REPLACE WITH NEW REC IN AIO2                 
         LA    R4,DIR                                                           
         NI    SPKKSTA,X'FF'-X'80'                                              
         BAS   RE,DMWRTR           REPLACE DIRECTORY                            
                                                                                
         TM    OPTN,OPTDUMP        DUMP OUTPUT                                  
         BNO   *+16                                                             
         LA    R2,MSGWRT                                                        
         L     R3,AIO2                                                          
         BAS   RE,DUMP                                                          
         B     ADDRXIT                                                          
                                                                                
ADDR13   MVI   ERRNUM,ERRNACCX     ACCOUNT EXISTS                               
         BAS   RE,ERROR                                                         
         LTR   RE,RE               RETURN ERROR                                 
ADDRXIT  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT RECORD DETAILS EACH TIME A RECORD IS ADDED                    *         
***********************************************************************         
*&&DO                                                                           
REPT     NTR1  ,                                                                
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         ST    R6,AXPLN                                                         
         MVI   LNXPO,0                                                          
         MVI   OPTNUM,0            NUMBER OF OPTIONS FIELDS                     
         MVC   OPWK,SPACES                                                      
         L     R2,AIO2                                                          
         USING SPKRECD,R2                                                       
         MVC   PLACC,ACTKULA       ACCOUNT CODE                                 
         LA    R3,ACTRFST          PROCESS RECORD ELEMENTS                      
         SR    R0,R0                                                            
         DROP  R2                                                               
                                                                                
REPT3    CLI   0(R3),NAMELQ                                                     
         BNE   REPT5                                                            
         USING NAMELD,R3           RECORD NAME                                  
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLNME(0),NAMEREC                                                 
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
                                                                                
REPT31   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   REPT3                                                            
                                                                                
         BAS   RE,ACRPT                                                         
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,XSPACES                                                  
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,ACRPT                                                         
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* SET UP HEADLINES AND GO TO ACREPORT                                 *         
***********************************************************************         
                                                                                
ACRPT    NTR1  ,                                                                
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ERROR ITEM TO TABLE                                             *         
***********************************************************************         
                                                                                
ERROR    NTR1  ,                                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
                                                                                
DMHGH    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
                                                                                
DMSEQ    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
                                                                                
DMGETR   LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
                                                                                
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
*        GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',DIR,DIR                
*        GOTO1 DATAMGR,DMCB,DMWRTR,ACCDIR,DIR,DIR                               
         B     DMERR                                                            
                                                                                
DMADDR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,AIO2,DMWORK                        
         B     DMERR                                                            
                                                                                
DMPUT    CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO2,DMWORK                        
                                                                                
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ADD AND ELEMENT TO THE RECORD                                       *         
***********************************************************************         
                                                                                
ADDL     LR    R0,RE                                                            
         L     R2,AIO2                                                          
         LA    R3,ELEMENT                                                       
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
                                                                                
DUMP     CP    DMPCNT,DMPMAX       TEST MAXIMUM DUMPS                           
         BHR   RE                                                               
         AP    DMPCNT,=P'1'                                                     
         NTR1  ,                                                                
         LA    R0,L'MSGBLT                                                      
*        LA    R2,MSG1                                                          
*        L     R3,AIO2                                                          
         SR    R4,R4                                                            
         ICM   R4,3,ACCRLEN-ACCRECD(R3)                                         
DUMP3    LA    R5,=C'2D'                                                        
         GOTO1 ,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)            
         GOTO1 PRNTBL                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
                                                                                
MXRLNQ   EQU   2000                MAX RECORD SIZE                              
                                                                                
MXERR    EQU   50                  MAX ERROR TABLE ENTRIES                      
ERRLNQ   EQU   20                  SIZE OF ERROR ENTRY                          
                                                                                
DATVAL   DC    V(DATVAL)           DATVAL                                       
HELLO    DC    V(HELLO)            HELLO                                        
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
RIGHT    DC    V(RIGHT)            RIGHT                                        
HEXIN    DC    V(HEXIN)            HEXIN                                        
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
                                                                                
AIO      DC    A(IO)               IO AREA                                      
AIO2     DC    A(IO2)              IO AREA 2                                    
AERRS    DC    A(ERRS)             A(ERROR MESSAGES)                            
AINFIL   DC    A(INFIL)            A(INPUT FILE DCB)                            
                                                                                
OPTN     DC    X'00'               RUN OPTIONS                                  
OPTTAPE  EQU   X'80'               TAPE IS OPEN                                 
OPTFRST  EQU   X'40'               FIRST HEADER RECORD IN                       
OPTDUMP  EQU   X'20'               DUMP OUTPUT RECORDS                          
OPTSKIP  EQU   X'10'               SKIP NEXT KEY                                
OPTKEY   EQU   X'08'               AT LEAST ONE KEY FOUND                       
ALL      EQU   X'FF'                                                            
                                                                                
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
                                                                                
                                                                                
MSGBLT   DC    C'SAP KEY RECORD - BUILT'                                        
MSGFILE  DC    C'SAP KEY RECORD - FILE '                                        
MSGADD   DC    C'SAP KEY RECORD - ADDED'                                        
MSGWRT   DC    C'SAP KEY RECORD - WRITE'                                        
                                                                                
DMPCNT   DC    PL2'0'              DUMP COUNT - ERRORS                          
DMPMAX   DC    PL2'500'             MAX DUMP COUNT                              
                                                                                
BILLT    DC    C'CPTESU1',X'FF'                                                 
CZ       DC    15X'F0'                                                          
                                                                                
CNTS     DS    0XL25               RECORD COUNTS                                
CNTIN    DC    PL5'0',CL20'RECORDS IN'                                          
CNTADD   DC    PL5'0',CL20'RECORDS ADDED'                                       
CNTERR   DC    PL5'0',CL20'RECORDS WITH ERRORS'                                 
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
                                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXCOLS+(PLL-PLD),C'L'                                           
         MVI   BOXCOLS+(PLC1-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC2-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC3-PLD),C'C'                                          
         MVI   BOXCOLS+(PLR-PLD),C'R'                                           
                                                                                
BX200    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
                                                                                
BXXIT    XMOD1 1                                                                
                                                                                
BOXRC    DC    A(0)                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
                                                                                
         DC    C'**IO****'                                                      
         DC    F'0'                      IOAREA #1                              
IO       DC    (MXRLNQ)X'00'                                                    
                                                                                
         DC    C'**IO2***'                                                      
         DC    F'0'                      IOAREA #2                              
IO2      DC    (MXRLNQ)X'00'                                                    
                                                                                
                                                                                
ERRS     DS    0CL(ERRLNQ)                                                      
ERRN1    EQU   1                                                                
         DC    CL(ERRLNQ)'                  '                                   
ERRN2    EQU   2                                                                
         DC    CL(ERRLNQ)'                  '                                   
ERRNACCX EQU   15                                                               
         DC    CL(ERRLNQ)'ACCOUNT EXISTS'                                       
         EJECT                                                                  
***********************************************************************         
* DCB'S                                                               *         
***********************************************************************         
                                                                                
INFIL    DCB   DDNAME=INFIL,DSORG=PS,MACRF=(GM),                       X        
               EODAD=REQF80                                                     
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT FOR WORKING STORAGE                                           *         
***********************************************************************         
                                                                                
ACFUD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
                                                                                
GRSWORK  DS    0CL20               WORK AREA TO RIGHT JUSTIFY GROSS             
         DS    CL4                 BUFFER                                       
GRSKEY   DS    CL16                KEY LENGTH FOR GROSS AMT                     
                                                                                
SAPKWRK  DS    CL(L'INXITMTX)                                                   
SVXITMTX DS    CL(L'INXITMTX)      SAVE LAST PROCESSED KEY                      
TKEYWORK DS    CL42                BUILD CONVERTED TRANSACTION KEY              
                                                                                
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
HKEY     DS    CL(L'ACCKEY)        LAST HIGH LEVEL ADDED                        
                                                                                
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
                                                                                
ELEMENT  DS    XL255                                                            
ERRNUM   DS    X                   CURRENT ERROR NUMBER                         
ERRCNT   DS    X                   NUMBER OF ERRORS                             
ERRLST   DS    XL(MXERR)           LIST OF ERROR NUMBERS                        
                                                                                
OPTNUM   DS    X                   NUMNER OF OPTION FIELDS                      
AXPLN    DS    A                   A(OF THE CURRENT LINE FOR OPTIONS)           
LNXPO    DS    X                   LENGTH OF DATA ON THIS LINE                  
OPWK     DS    CL32                OPTIONS WORK AREA                            
                                                                                
INXREC   DS    CL(INXLNQ)          INPUT RECORD                                 
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER THE INPUT FILE RECORD                                *         
***********************************************************************         
                                                                                
INXMLD   DSECT                                                                  
INXXML   DS    CL80                                                             
                                                                                
         ORG   INXXML                                                           
         DS    CL4                 SPACES                                       
INXVCH1  DS    CL12                VOUCHER TAG PRECEDING                        
INXVCHNM DS    CL14                VOUCHER NUMBER                               
INXVCH2  DS    CL13                VOUCHER TAG TRAILING                         
                                                                                
         ORG   INXXML                                                           
         DS    CL4                 SPACES                                       
INXGRS1  DS    CL14                GROSS AMOUNT TAG PRECEDING                   
INXGRSAM DS    CL20                GROSS AMOUNT VARIABLE LENGTH                 
                                                                                
         ORG   INXXML                                                           
         DS    CL6                 SPACES                                       
INXITMX1 DS    CL11                ITEM TEXT TAG PRECEDING                      
INXITMTX DS    CL48                ITEM TEXT KEY                                
INXITMX2 DS    CL12                ITEM TEXT TAG TRAILING                       
                                                                                
         ORG   INXXML                                                           
         DS    CL6                 SPACES                                       
INXMATC1 DS    CL10                MATERIAL CODE TAG PRECEDING                  
INXMATCD DS    CL6                 MATERIAL CODE                                
INXMATC2 DS    CL11                MATERIAL CODE TAG TRAILING                   
                                                                                
INXLNQ   EQU   *-INXMLD                                                         
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ITEM TEXT KEY                                        *         
***********************************************************************         
SAPITMD  DSECT                                                                  
SAPITMTX DS    0CL48                                                            
SAPMCPY  DS    CL2                                                              
SAPMULA  DS    CL14                                                             
SAPMOFFC DS    CL2                                                              
SAPMCCPY DS    XL2                                                              
SAPMCNAC DS    CL14                                                             
SAPMDATE DS    CL6                                                              
SAPMREF  DS    CL6                                                              
SAPMSEQ  DS    XL2                                                              
SAPITLEN EQU   *-SAPITMTX                                                       
***********************************************************************         
* DSECT TO COVER REPORT PRINT LINE                                    *         
***********************************************************************         
                                                                                
PLD      DSECT                                                                  
PLL      DS    X                                                                
PLVCHER  DS    CL14                SAP VOUCHER NUMER                            
PLC1     DS    X                                                                
PLGRAMT  DS    CL20                HEADER GROSS AMOUNT                          
PLC2     DS    X                                                                
PLXKEY   DS    CL50                TRANSACTION KEY/DETAIL RECORD                
PLC3     DS    X                                                                
PLR      DS    X                                                                
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPAF02 03/02/20'                                      
         END                                                                    
