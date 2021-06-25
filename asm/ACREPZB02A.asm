*          DATA SET ACREPZB02A AT LEVEL 057 AS OF 01/14/00                      
*PHASE ACZB02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'CHECK PEELED TRANSACTIONS'                                      
ACZB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZB**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZBD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
*        CLI   MODE,SBACFRST             CONTRA ACCOUNT FIRST                   
*        BE    SUBF                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                      LOOKUP TRANSACTION INFO                
         CLI   MODE,REQLAST              REQUEST LAST                           
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         XC    SVBINKY,SVBINKY                                                  
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
         ZAP   PKDMPMAX,=P'100'                                                 
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                     *           
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FCRDTRNS,C'Y'                                                    
         ZAP   PKDMPCNT,=P'0'                                                   
*                                                                               
         USING BIND,R2                                                          
         L     R2,ATRNTTAB         TRANSACTION TABLE                            
         XC    BININ,BININ         CLEAR BIN TABLE                              
         DROP  R2                                                               
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS TRANSACTION                                                *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNELD,R4                                                        
PTRN     DS    0H                                                               
         L     R4,ADTRANS                                                       
*                                                                               
         LR    R2,R4                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                GET ADDRESABILTY TO TRAN RECD          
*                                                                               
*        MVC   MSG,=CL10'SVBINKY'                                               
*        GOTO1 ADUMP,DMCB,(RC),SVBINKY,L'SVBINKY                                
*        MVC   MSG,=CL10'TRNKEY'                                                
*        GOTO1 ADUMP,DMCB,(RC),TRNKEY,L'TRNKEY                                  
*                                                                               
         OC    SVBINKY,SVBINKY                                                  
         BZ    *+14                                                             
         CLC   SVBINKY,TRNKEY                                                   
         BH    PTRNX                                                            
*                                                                               
         LR    RE,R2                                                            
         LA    RE,ACCOPEEL(RE)     BUMP TO PEEL DATE                            
         OC    0(ACCOPLEN,RE),0(RE) WAS THIS PEELED?                            
         BZ    PTRNX                                                            
*                                                                               
         USING TRNTD,R3                                                         
         LA    R3,TRNTWRK          TRANSACTION TABLE WORK AREA                  
         XC    TRNTWRK,TRNTWRK                                                  
         ZAP   TRNTDR,=P'0'                                                     
         ZAP   TRNTCR,=P'0'                                                     
*                                                                               
         MVC   TRNTCULA(TRNTKLNQ),TRNKEY          SAVE OFF KEY                  
         TM    TRNSTAT,TRNSDR      IS THIS A DEBIT?                             
         BNO   *+14                                                             
         ZAP   TRNTDR,TRNAMNT                   SAVE DEB AMOUNT                 
         B     *+10                                                             
         ZAP   TRNTCR,TRNAMNT                   SAVE CRD AMOUNT                 
*                                                                               
         USING TRSELD,R6                                                        
         LR    R6,R2                                                            
         MVI   ELCODE,TRSELQ      X'60'-TRANSACTION STATUS ELEMENT              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TRNTMOS,TRSPMOS                    SAVE OFF MONTH                
         GOTO1 ABINADD,DMCB,(RC),TRNTWRK,ATRNTTAB ADD TABLE ENTRY               
*                                                                               
PTRNX    B     EXIT                                                             
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R1                                                          
REQL     DS    0H                                                               
         ZAP   PKCOUNT,=P'0'                                                    
         L     R1,ATRNTTAB         R1=A(TRANSACTION TABLE)                      
         ICM   R0,15,BININ                                                      
         BZ    REQLX                                                            
         USING TRNTD,R2                                                         
         LA    R2,BINTAB                                                        
*                                                                               
         ST    R0,FULL                                                          
         MVC   MSG,=CL10'BININ'                                                 
         GOTO1 ADUMP,DMCB,(RC),FULL,L'FULL                                      
         MVC   MSG,=CL10'TRNTKEY'                                               
         GOTO1 ADUMP,DMCB,(RC),(R2),TRNTKEYL                                    
         DROP  R1                                                               
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         USING CHDRECD,R3                                                       
REQL10   LA    R3,SVKEY                                                         
         XC    SVKEY,SVKEY                                                      
         MVI   FLAG,0              INITIALIZE FLAG                              
*                                                                               
         MVC   CHDKEY(TRNTKLNQ),TRNTCULA  MOVE IN THE KEY                       
         MVC   CHDKSPCS(L'CHDKSPCS+L'CHDKBTYP),SPACES                           
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         CLC   SVKEY(CHDKEND),IOKEY      SAME KEY??                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET RECORD                            
         LA    R3,IO                                                            
REQL20   LA    R4,CHDRFST                                                       
*                                                                               
         MVC   MSG,=CL10'CO REC IN'                                             
         SR    R6,R6                                                            
         ICM   R6,3,CHDRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         USING BUKELD,R4                                                        
REQL30   CLI   0(R4),0             EOR, DON'T HAVE BUCKET ELEMENT               
         BNE   REQL40              SHOULD HAVE ONE                              
         CLI   QOPT1,C' '          DID THEY REQUEST ANY SPECIFIC OUTPUT         
         BE    *+12                                                             
         CLI   QOPT1,C'E'          DO THEY WANT ONLY ONES MISSING?              
         BNE   REQL80                                                           
         MVC   PNARR,=CL30'BUCKET DID NOT EXIST - ADDED'                        
         BAS   RE,ADDELM                                                        
         OI    FLAG,FLGUPD         UPDATE  RECORD                               
         B     REQL70                                                           
*                                                                               
REQL40   CLI   BUKEL,BUKELQ        BUCKET ELEMENT?                              
         BE    REQL60                                                           
REQL50   SR    R1,R1                                                            
         IC    R1,BUKLN            NO, BUMP UNTIL WE FIND IT                    
         AR    R4,R1                                                            
         B     REQL30                                                           
*                                                                               
REQL60   CLC   BUKMOS,TRNTMOS      RIGHT BUCKET?                                
         BNE   REQL50              NO-FIND NEXT ONE                             
         CP    BUKDR,TRNTDR        DO DEBITS MATCH?                             
         BNE   *+14                                                             
         CP    BUKCR,TRNTCR        DO CREDITS MATCH?                            
         BE    REQL80                                                           
         CLI   QOPT1,C' '          DID THEY REQUEST ANY SPECIFIC OUTPUT         
         BE    *+12                                                             
         CLI   QOPT1,C'M'          ONLY ONES THAT DONT MATCH                    
         BNE   REQL80                                                           
         MVC   PNARR,=CL30'BUCKET DID NOT MATCH - FIXED'                        
*                                                                               
         OI    FLAG,FLGUPD         UPDATE RECORD                                
         ZAP   BUKDR,TRNTDR        FIX DEBITS                                   
         ZAP   BUKCR,TRNTCR        FIX CREDITS                                  
*                                                                               
         USING TRNRECD,RE                                                       
REQL70   LA    RE,TRNTCULA                                                      
         MVC   PACC,TRNKULA                                                     
         MVC   POFF,TRNKOFF                                                     
         MVC   PCONTRA,SPACES                                                   
*                                                                               
         LA    R1,L'PCONTRA                                                     
         LA    RF,TRNKCCPY                                                      
         CLC   TRNKCPY,0(RF)       DOES CONTRA BEGIN WITH COMP                  
         BNE   *+10                                                             
         BCTR  R1,0                DECREMENT LENGTH                             
         LA    RF,L'TRNKCCPY(RF)   BUMP PAST COMPANY                            
         BCTR  R1,0                DECREMENT FOR EX MVC                         
         EX    R1,*+4                                                           
         MVC   PCONTRA(0),0(RF)                                                 
         DROP  RE                                                               
*                                                                               
         MVC   WORK(2),TRNTMOS                                                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+6)                                  
         MVC   PMOS,WORK+6                                                      
         CURED (P8,TRNTDR),(20,PDR),2,ZERO=NOBLANK,MINUS=YES                    
         CURED (P8,TRNTCR),(20,PCR),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
         AP    PKCOUNT,=P'1'                                                    
*                                                                               
REQL80   CHI   R0,1                                                             
         BE    REQL90                                                           
         LA    RE,TRNTLNQ(,R2)                                                  
         CLC   0(TRNTMOS-TRNTKEY,RE),0(R2)    SAME RECORD AS BEFORE?            
         BNE   REQL90                                                           
         LR    R2,RE                                                            
         BCT   R0,REQL20                                                        
*                                                                               
REQL90   TM    FLAG,FLGUPD         ANYTHING TO UPDATE?                          
         BNO   *+8                                                              
         BAS   RE,PUTIT                                                         
*                                                                               
         LA    R2,TRNTLNQ(R2)                                                   
         BCT   R0,REQL10                                                        
*                                                                               
         CP    PKCOUNT,=P'0'                                                    
         BE    REQLX                                                            
         MVC   PACC(35),=CL35'THE NUMBER OF ENTRIES IN ERROR :'                 
         EDIT  (P8,PKCOUNT),(16,PMOS)                                           
         GOTO1 ACREPORT                                                         
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* UPDATE BUCKET ELEMENT                                              *          
*        R2 - BINTABLE ENTRY                                         *          
*        R3 - OFFICE CONTRA RECORD                                   *          
*        R4 - X'45' ELEMENT                                          *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNTD,R2                                                         
         USING CHDRECD,R3                                                       
         USING BUKELD,R4                                                        
ADDELM   NTR1                                                                   
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM           BUILD BUCKET ELEMENT                         
         MVI   BUKEL,BUKELQ        BUCKET ELEMENT                               
         MVI   BUKLN,BUKLNQ        LENGTH OF ELEMENT                            
         MVC   BUKMOS,TRNTMOS      MONTH OF SERVICE                             
         ZAP   BUKDR,TRNTDR        DEBITS IN MONTH                              
         ZAP   BUKCR,TRNTCR        CREDITS IN MONTH                             
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),IO,ELEM                                
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
*                                                                               
ADDEX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* WRITE RECORD BACK                                                  *          
*       R3 - OFFICE CONTRA RECORD                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING CHDRECD,R3                                                       
PUTIT    NTR1                                                                   
         TM    CHDRSTA,CHDSDELT                                                 
         BNO   *+8                                                              
         NI    CHDRSTA,X'FF'-CHDSDELT     MARK ACTIVE KEY UNDELETED             
*                                                                               
         MVC   MSG,=CL10'CO REC OUT'                                            
         SR    R6,R6                                                            
         ICM   R6,3,CHDRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         CLI   QOPT2,C'Y'          DO WE WANT TO UPDATE?                        
         BNE   PUTITX                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    PUTITX                                                           
         GOTO1 =A(DMPUTREC),DMCB,(RC)     WRITE RECORD BACK                     
         CLI   DMCB+8,0                   DUMP ON ERROR                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTITX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ATRNTTAB DC    A(TRNTTAB)          TRANSACTION TABLE                            
ABINADD  DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(HELLO)            HELLO MODULE                                 
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   BINA10                                                           
         CLC   ALPHAID,=C'HK'      SPECIAL CODE FOR HK                          
         BE    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   SVBINKY,0(R3)                                                    
         MVI   FCRDTRNS,C'N'       DON'T READ ANY MORE TRANS                    
         MVC   MSG,=CL10'SVBINKY'                                               
         GOTO1 ADUMP,DMCB,(RC),SVBINKY,L'SVBINKY                                
         B     BINAX                                                            
*                                                                               
BINA10   MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINAX                                                            
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINAX                 NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
BINA20   AP    0(TRNTBKLN,R4),0(TRNTBKLN,R3) ADD TO BUCKET                      
         LA    R3,TRNTBKLN(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,TRNTBKLN(R4)     BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA20                                                        
*                                                                               
BINAX    XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    PKDMPCNT,PKDMPMAX                                                
         BH    DUMPX                                                            
         AP    PKDMPCNT,=P'1'                                                   
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
        SPACE 1                                                                 
*                                                                               
* BINTABLE 1 - TRANSACTION TABLE                                                
*                                                                               
         DC    C'***TRNT***'                                                    
TRNTTAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TRNTLNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TRNTKLNQ)           KEY LENGTH                               
         DC    AL4(TRNTMAX)            MAX IN TABLE                             
         DC    AL1(TRNTBKCT)           NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(TRNTDR-TRNTD)       DISPLACEMENT TO FIRST BUCKET             
         DS    (TRNTMAX*TRNTLNQ)XL1    TABLE                                    
*                                                                               
TRNTMAX  EQU   99999                                                            
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZBD    DSECT                                                                  
VTYPES   DS    0A                                                               
ADUMP    DS    A                   ROUTINE TO DITTO                             
PRNTBL   DS    V                   PRINT DATA                                   
VHELLO   DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SVDA     DS    F                   SAVED AREA FOR DISK ADDRESS                  
SVKEY    DS    CL49                SAVED AREA FOR KEY                           
*                                                                               
SVBINKY  DS    CL(TRNTKEYL)                                                     
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO KEY                          
ELCODE   DS    CL1                 ELEMENT CODE                                 
MSG      DS    CL10                DUMP MESSAGE                                 
PKCOUNT  DS    PL8                 NUMBER OF ENTRIES FIXED/ADDED                
PKDMPCNT DS    PL8                 NUMBER OF DUMPS                              
PKDMPMAX DS    PL4                 MAXIMUM NUMBER OF DUMPS                      
*                                                                               
FLAG     DS    CL1                 UPDATE FLAG                                  
FLGUPD   EQU   X'80'               ONLY UPDATE BUCKET DONT ADD                  
*                                                                               
TRNTWRK  DS    CL(TRNTLNQ)         BINSEARCH WORK AREA - TRNS TABLE             
ELEM     DS    CL255               ELEMENT WORK AREA                            
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL2                                                              
PACC     DS    CL12                                                             
         DS    CL2                                                              
POFF     DS    CL2                                                              
         DS    CL2                                                              
PCONTRA  DS    CL15                                                             
         DS    CL5                                                              
PMOS     DS    CL4                                                              
         DS    CL5                                                              
PDR      DS    CL20                                                             
         DS    CL5                                                              
PCR      DS    CL20                                                             
         DS    CL5                                                              
PNARR    DS    CL30                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN TRANSACTION TABLE                                *         
***********************************************************************         
         SPACE 1                                                                
TRNTD    DSECT                                                                  
TRNTKEY  DS    0CL12                 TRNS KEY                                   
TRNTCULA DS    CL15                  TRANSACTION ACCOUNT                        
TRNTTOFF DS    CL2                   TRANSACTION OFFICE                         
TRNTCNT  DS    CL15                  TRANSACTION CONTRA                         
TRNTKEYL EQU   *-TRNTD               LENGTH OF ACCOUNT INFO FOR SVBINKY         
TRNTMOS  DS    PL2                   POSTING MOS                                
TRNTKLNQ EQU   *-TRNTD               LENGTH OF KEY                              
TRNTDR   DS    PL8                   DEBIT BUCKET                               
TRNTBKLN EQU   *-TRNTDR              BUCKET LENGTH                              
TRNTCR   DS    PL8                   CREDIT BUCKET                              
TRNTBKCT EQU   (*-TRNTDR)/TRNTBKLN   NUMBER OF BUCKETS                          
TRNTLNQ  EQU   *-TRNTD               LENGTH OF ENTRY                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
*ACREPWORKD                                                                     
*ACGENFILE                                                                      
*ACGENMODES                                                                     
*DDLOGOD                                                                        
*ACMASTD                                                                        
*DDMASTD                                                                        
*DDBIGBOX                                                                       
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057ACREPZB02A01/14/00'                                      
         END                                                                    
