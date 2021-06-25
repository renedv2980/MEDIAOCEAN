*          DATA SET ACREPBD02  AT LEVEL 010 AS OF 08/16/00                      
*PHASE ACBD02A,+0                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DMDMGRL                                                                
         TITLE 'BBDO BALANCE RESTORE'                                           
ACBD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBD**,R9,R8    BASE REGISTERS 11, 9 AND 8                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING LWKSD,RC            RC=A(SAVE W/S)                               
         MVC   ADMGR,DATAMGR       DEFAULT IS MONACC DATAMGR                    
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,PROCACC        ACCOUNT                                      
         BE    PRAC                                                             
         CLI   MODE,PROCTRNS       TRANSACTIONS                                 
         BE    PTRN                                                             
         CLI   MODE,ACCLAST        TRANSACTIONS                                 
         BE    ACCL                                                             
         CLI   MODE,REQLAST        REQUEST LAST                                 
         BE    REQL                                                             
         CLI   MODE,RUNLAST        RUN LAST                                     
         BE    RUNL                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     L     R2,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         GOTO1 VDMGR,DMCB,OPEN,ACCOUNT,ACFILEL    OPEN TEST FILE                
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         LA    R1,ANYACC                                                        
         CLC   QSELECT(2),=C'NY'                                                
         BE    REQF03                                                           
         LA    R1,ACHACC                                                        
         CLC   QSELECT(2),=C'CH'                                                
         BE    REQF03                                                           
         LA    R1,AATACC                                                        
         CLC   QSELECT(2),=C'AT'                                                
         BE    REQF03                                                           
         DC    H'0'                                                             
*                                                                               
REQF03   MVC   ACCLIST(8),0(R1)                                                 
         L     R4,ADCMPNAM                                                      
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
         AP    REQNUM,=P'1'                                                     
         CP    REQNUM,=P'1'                                                     
         BNE   *+8                                                              
         BAS   RE,BLDGL            BUILD TABLE OF G/L POINTERS                  
         CLI   QOPT5,C'Y'          CREATE CONVERSION FILE                       
         BNE   XIT                                                              
         L     R3,ADCNVF                                                        
         OPEN  ((R3),(OUTPUT))                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING ACLELD,R4                                                        
LDGF     L     R4,ADLDGHIR         HEIRARCHY ELEMENT                            
         MVC   LEVELA,ACLVLEN      LEVEL LENGTHS                                
         MVC   LEVELB,ACLVLEN+(L'ACLVALS)                                       
         MVC   LEVELC,ACLVLEN+(L'ACLVALS*2)                                     
         MVC   LEVELD,ACLVLEN+(L'ACLVALS*3)                                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PRAC     MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   STATUS,0                                                         
         MVI   BINFLG,0                                                         
         LA    RF,CTOTS            CLEAR CONTROL TOTALS                         
         LA    R0,CTOTN                                                         
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         L     R5,ADWRK                                                         
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR NUMBER OF ENTRIES                      
         LA    R0,BINTABLE         AND THE TABLE                                
         L     R1,=AL4(MXWRK*WRKLNQ)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,ADSUM                                                         
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR NUMBER OF ENTRIES                      
         LA    R0,BINTABLE         AND THE TABLE                                
         L     R1,=AL4(MXSUM*SUMLNQ)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,ADCNV                                                         
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR NUMBER OF ENTRIES                      
         LA    R0,BINTABLE         AND THE TABLE                                
         L     R1,=AL4(MXCNV*CNVLNQ)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,ADACC                                                         
         MVC   ACCODE,0(R1)        SAVE ACCOUNT CODE                            
         MVC   ACNAME,SPACES       AND NAME                                     
         L     RF,ADACCNAM                                                      
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   ACNAME(0),NAMEREC                                                
         BAS   RE,SUBPTR           GET LIST OF SUBLEDGER POINTERS               
         DROP  RF                                                               
*                                                                               
         LA    R2,DKEY                                                          
         USING OFARECD,R2                                                       
         MVC   OFAKEY,SPACES       READ ALL OFFICE ACCOUNT RECORDS              
         MVC   OFAKCULA,ACCODE                                                  
*                                                                               
PRAC5    SR    R1,R1                                                            
         IC    R1,OFAKOFF+1        BUMP TO NEXT OFFICE                          
         AH    R1,=H'1'                                                         
         STC   R1,OFAKOFF+1                                                     
         BAS   RE,HIGH                                                          
         LA    R2,DIR                                                           
         CLC   OFAKCULA,ACCODE     SAME ACCOUNT                                 
         BNE   XIT                                                              
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,PBALF                                                         
         MVC   DKEY,DIR                                                         
         LA    R2,DKEY                                                          
         B     PRAC5                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF SUBLEDGER POINTERS FOR ACCOUNT                        *         
***********************************************************************         
         SPACE 1                                                                
SUBPTR   NTR1  ,                                                                
         MVI   SUBLST,X'FF'        END OF LIST                                  
         XC    SUBACC,SUBACC                                                    
         LA    R0,MXSBL                                                         
         SH    R0,=H'1'            DON'T OVERFLOW TABLE                         
         LA    R1,SUBLST                                                        
         L     R6,AGLPTAB          G/L POINTER TABLE                            
         USING GLPTD,R6                                                         
SUBPTR3  CLC   GLPTTO,ACCODE+1     FIND ALL TO POINTERS                         
         BNE   SUBPTR7                                                          
         MVC   0(14,R1),GLPTFR     SAVE FROM ACCOUNT                            
         OC    SUBACC,SUBACC       ANY SB ACCOUNT YET                           
         BNZ   SUBPTR5                                                          
         CLC   GLPTFR(2),=C'SB'                                                 
         BNE   SUBPTR5                                                          
         MVC   SUBACC,GLPTFR       SAVE FIRST SB ACCOUNT                        
SUBPTR5  LA    R1,14(R1)                                                        
         MVI   0(R1),X'FF'                                                      
         BCT   R0,*+6                                                           
         DC    H'0'                TOO MANY POINTERS                            
SUBPTR7  LA    R6,GLPTLNQ(R6)                                                   
         CLI   0(R6),X'FF'                                                      
         BNE   SUBPTR3                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* POST OFFICE/ACCOUNT BALANCE                                         *         
***********************************************************************         
         SPACE 1                                                                
PBALF    NTR1  ,                                                                
         BAS   RE,INITWK           INITIALIZE WORK LINE                         
         MVI   WRKSEC,WRKSGL                                                    
         MVI   WRKTYP,WRKTNEW      SET OFFICE/ACCOUNT BAL.FRWD                  
         MVC   WRKOFF,OFAKOFF                                                   
         L     R2,AIO1                                                          
         USING ACTRECD,R2                                                       
         BAS   RE,GET              GET THE RECORD                               
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING ABLELD,R4                                                        
PBALF3   CLI   0(R4),ABLELQ        X'32' ELEMENT                                
         BE    PBALF7                                                           
         CLI   0(R4),0                                                          
         BE    XIT                 EOR                                          
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     PBALF3                                                           
*                                                                               
PBALF7   CP    ABLFRWD,=P'0'       TEST ANY BBF                                 
         BE    XIT                                                              
         ZAP   WRKBBF,ABLFRWD                                                   
         ZAP   WRKADJ,ABLFRWD                                                   
         MP    WRKADJ,=P'-1'                                                    
         OI    STATUS,STBBF                                                     
         BAS   RE,ADDWRK                                                        
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
         SPACE 1                                                                
PTRN     CLI   QOPT5,C'Y'                                                       
         BE    XIT                                                              
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         LR    R2,R4                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         CLC   ACMMDTE,=X'9512'                                                 
         BNH   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,INITSUM                                                       
         MVC   SUMMOA,ACMMDTE      GET TRANSACTIONS BY MONTH                    
         MVC   SUMOFF,TRNOFFC      OFFICE                                       
         MVC   SUMCON,TRNKCUNT     SOURCE ACCOUNT                               
         ZAP   DUB,TRNAMNT                                                      
         MP    DUB,=P'-1'          REDUCE BUCKETS IF ON FILE                    
         LA    R1,SUMDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,SUMCR                                                         
         ZAP   0(8,R1),DUB                                                      
         BAS   RE,ADDSUM                                                        
*                                                                               
         BAS   RE,INITWK                                                        
         MVI   WRKSEC,WRKSGL                                                    
         MVI   WRKTYP,WRKTNEW      ACCOUNT                                      
         MVC   WRKOFF,TRNOFFC      OFFICE                                       
         MVC   WRKACC,TRNKCUNT     SOURCE ACCOUNT                               
         ZAP   WRKACT,TRNAMNT      ACTIVITY AMOUNT                              
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+10                                                             
         MP    WRKACT,=P'-1'                                                    
*                                                                               
         MVC   WRKNME,SPACES                                                    
         L     RF,ADSUBAC                                                       
         USING CACELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,CACLN            ELEMENT LENGTH                               
         SH    R1,=H'18'                                                        
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   WRKNME(0),CACNAME                                                
         BAS   RE,ADDWRK           ADD A ROW TO THE TABLE                       
         B     XIT                                                              
         DROP  R2,R4,RF                                                         
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
         SPACE 1                                                                
ACCL     DS    0H                                                               
         MVC   ADMGR,VDMGR                                                      
         BAS   RE,OLD              GET OLD DATA                                 
         MVC   ADMGR,DATAMGR                                                    
         CLI   QOPT5,C'Y'                                                       
         BE    ACCL9                                                            
         BAS   RE,SUBL             GET THE SUB LEDGER DATA                      
*                                                                               
ACCL5    BAS   RE,SETSUB           SET SUBLEDGER FLAG ON G/L ACCOUNTS           
         CLI   QOPT1,C' '                                                       
         BE    ACCL7                                                            
         MVI   BYTE,0                                                           
         CLI   QOPT1,C'B'          BAL FRW ACCOUNTS                             
         BNE   *+8                                                              
         OI    BYTE,STBBF                                                       
         CLI   QOPT1,C'M'                                                       
         BNE   *+8                                                              
         OI    BYTE,STMIA          MULTIPLE INPUT ACCOUNTS                      
         CLI   QOPT1,C'O'                                                       
         BNE   *+8                                                              
         OI    BYTE,STOCA          OLD CONTRA ACCOUNTS                          
         CLI   QOPT1,C'*'                                                       
         BNE   *+8                                                              
         OI    BYTE,STOCA+STMIA+STBBF                                           
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    STATUS,0                                                         
         BZ    XIT                                                              
*                                                                               
ACCL7    BAS   RE,CONO                                                          
ACCL9    BAS   RE,RPT                                                           
         CLI   QOPT5,C'Y'          CREATE CONVERSION FILE                       
         BNE   XIT                                                              
         BAS   RE,FOUT                                                          
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
         SPACE 1                                                                
REQL     DS    0H                                                               
         L     R5,ADWRK                                                         
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR TABLE                                  
         CLI   QOPT5,C'Y'          CREATE CONVERSION FILE                       
         BNE   XIT                                                              
         L     R3,ADCNVF                                                        
         CLOSE ((R3))                                                           
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
         SPACE 1                                                                
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   ACCODE,SPACES                                                    
         MVC   ACNAME,SPACES                                                    
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7         R7=ADDRESSES WIDE PRINT                      
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         LA    R4,RUNTOT                                                        
RUNL3    EDIT  (P4,0(R4)),PLBBF                                                 
         MVC   PLACC(26),4(R4)                                                  
         BAS   RE,PRTL                                                          
         LA    R4,L'RUNTOT(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   RUNL3                                                            
         B     XIT                                                              
         DROP  R6,R7                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NEW OFFICE/CONTRA RECORDS                                     *         
***********************************************************************         
         SPACE 1                                                                
CONO     NTR1  ,                                                                
         XC    NXTSUM,NXTSUM       SET FOR FIRST GET                            
         XC    SUBACC,SUBACC                                                    
         L     R2,AIO3                                                          
CONO3    BAS   RE,GETSUM           GET A RECORD                                 
         BNE   XIT                 TEST EOF                                     
         CLI   WRKSEC,WRKSGL                                                    
         BNE   CONO3                                                            
         TM    WRKSTAT,WRKSADJ     TEST ALREADY ADJUSTED                        
         BO    CONO3                                                            
         L     RF,ADRWRK           RF =  CURRENT ENTRY                          
         OI    WRKSTAT-WRKR(RF),WRKSADJ  SET ADJUSTED                           
         CP    WRKADJ,=P'0'        SKIP ADJUSTMENTS                             
         BNE   CONO3                                                            
         CLC   SUMOFF,=C'**'       SKIP THE BAD ONE                             
         BE    CONO3                                                            
         CLC   SUMOFF,=C'ZA'                                                    
         BE    CONO3                                                            
         OC    SUBACC,SUBACC       TEST FIRST FOR OFF/CONTRA                    
         BNZ   *+8                                                              
         BAS   RE,NEWR             BUILD A NEW RECORD                           
         CLI   SUMMOA,X'FF'        TOTAL OFF/CONTRA                             
         BNE   CONO7                                                            
         BAS   RE,WRTR             WRITE THE LAST RECORD                        
         BAS   RE,PNEW                                                          
         XC    SUBACC,SUBACC                                                    
         B     CONO3                                                            
*                                                                               
CONO7    CP    SUMDR,=P'0'         BUILD NEW ELEMENT                            
         BNE   *+14                                                             
         CP    SUMCR,=P'0'                                                      
         BE    CONO3                                                            
         LA    R3,ELEMENT                                                       
         USING BUKELD,R3                                                        
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKYEAR(2),SUMMOA                                                
         ZAP   BUKDR,SUMDR                                                      
         ZAP   BUKCR,SUMCR                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3),0                             
         L     R4,AIO3                                                          
         BAS   RE,CONMX            CREATE PRIOR BUCKET IF NECESSARY             
         B     CONO3                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A NEW CONTRA RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
NEWR     NTR1  ,                                                                
         MVC   SUBACC,SUMCON                                                    
         MVC   SUBNME,SPACES                                                    
         LA    R3,SUBACC                                                        
         LA    R4,SUBNME                                                        
         BAS   RE,GETNME                                                        
*                                                                               
         XC    0(255,R2),0(R2)                                                  
         USING CHDRECD,R2                                                       
         MVC   CHDKEY,SPACES                                                    
         MVC   CHDKCPY(15),ACCODE  C/U/L ACCOUNT                                
         MVC   CHDKOFF,SUMOFF      OFFICE                                       
         MVC   CHDKCCPY,RCCOMPFL                                                
         MVC   CHDKULC,SUMCON      CONTRA                                       
         MVC   CHDKSPCS,SPACES                                                  
         MVI   CHDKBTYP,C' '       BUCKET TYPE                                  
         XC    CHDKNULL,CHDKNULL                                                
         MVC   CHDRLEN,=Y(CHDRFST-CHDKEY)                                       
         XC    CHDRSTA,CHDRSTA                                                  
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING CACELD,R3                                                        
         MVI   CACEL,CACELQ                                                     
         MVC   CACCNT,CHDKCCPY                                                  
         MVC   CACNAME,SUBNME                                                   
         LA    R1,CACNAME+L'CACNAME-1                                           
         LA    RF,L'CACNAME                                                     
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
         LA    RF,CACLN1Q(RF)                                                   
         STC   RF,CACLN                                                         
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3),0                             
         LA    R3,CHDRFST                                                       
         CLI   CACEL,CACELQ        TEST CONTRA ELEMENT STILL THERE              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE A NEW RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
WRTR     NTR1  ,                                                                
         MVC   ADMGR,DATAMGR       SET FOR REAL FILE                            
         MVC   DKEY,0(R2)          GET THE OLD RECORD                           
         BAS   RE,READ                                                          
         CLC   DKEY,DIR                                                         
         BE    WRTR5                                                            
         BAS   RE,ADD              NO PREVOIUS - ADD IT                         
         AP    NEWREC,=P'1'                                                     
         B     XIT                                                              
*                                                                               
WRTR5    L     R2,AIO2             AIO2 -OLD, AIO3 = NEW                        
         BAS   RE,GET                                                           
         USING CHDRECD,R2                                                       
         MVC   CHDRLEN,=Y(CHDRFST-CHDKEY)                                       
         XC    CHDRSTA,CHDRSTA                                                  
         XC    CHDRFST(10),CHDRFST CLEAR OLD                                    
         BAS   RE,CONB             MERGE CONTRA BUCKETS                         
         BAS   RE,PUT                                                           
         AP    CHAREC,=P'1'                                                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* MERGE CONTRA BUCKETS                                                *         
***********************************************************************         
                                                                                
CONB     NTR1  ,                                                                
         L     R2,AIO3             R2 = A(NEW RECORD - IO3)                     
R        USING CACRECD,R2                                                       
         LA    R3,R.CACRFST                                                     
E        USING BUKELD,R3                                                        
         SR    R0,R0                                                            
*                                                                               
CONB3    CLI   E.BUKEL,0              FIND BUCKET ELEMENTS                      
         BE    CONB21                                                           
         CLI   E.BUKEL,BUKELQ                                                   
         BE    CONB7                                                            
         CLI   0(R3),CACELQ                                                     
         BE    CONB15              ADD THE 43 ELEMENT                           
CONB5    IC    R0,E.BUKLN                                                       
         AR    R3,R0                                                            
         B     CONB3                                                            
*                                                                               
CONB7    L     R4,AIO2             R4 = A(OLD RECORD - IO2)                     
         USING CACRECD,R4                                                       
         LA    R5,CACRFST                                                       
         USING BUKELD,R5                                                        
*                                                                               
CONB9    CLI   BUKEL,0                MATCH TO ELEMENT IN RECORD 1              
         BE    CONB15                                                           
         CLI   BUKEL,BUKELQ                                                     
         BNE   CONB11                                                           
         CLC   BUKYEAR(2),E.BUKYEAR    TEST SAME MONTH                          
         BNE   CONB11                                                           
         AP    BUKDR,E.BUKDR                                                    
         AP    BUKCR,E.BUKCR                                                    
         B     CONB5                                                            
CONB11   IC    R0,BUKLN                                                         
         AR    R5,R0                                                            
         B     CONB9                                                            
*                                                                               
CONB15   GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO2,E.BUKELD                           
         CLI   12(R1),0                                                         
         BE    CONB5                                                            
         DC    H'0'                                                             
         DROP  R,E,R4,R5                                                        
         EJECT                                                                  
***********************************************************************         
* MERGE CONTRA PRIOR BUCKETS                                          *         
***********************************************************************         
                                                                                
CONB21   L     R2,AIO3             R2 = A(NEW RECORD - IO3)                     
R        USING CACRECD,R2                                                       
         LA    R3,R.CACRFST                                                     
E        USING PBKEL,R3                                                         
         SR    R0,R0                                                            
*                                                                               
CONB23   CLI   E.PBKEL,0             FIND BUCKET ELEMENTS                       
         BE    CONB41                                                           
         CLI   E.PBKEL,PBKELQ                                                   
         BE    CONB25                                                           
         IC    R0,E.PBKLN                                                       
         AR    R3,R0                                                            
         B     CONB23                                                           
*                                                                               
CONB25   L     R4,AIO2             R4 = A(OLD RECORD - IO2)                     
         USING CACRECD,R4                                                       
         LA    R5,CACRFST                                                       
         USING PBKELD,R5                                                        
*                                                                               
CONB27   CLI   PBKEL,0                MATCH TO ELEMENT IN RECORD 1              
         BE    CONB31                                                           
         CLI   PBKEL,PBKELQ                                                     
         BE    CONB29                                                           
         IC    R0,PBKLN                                                         
         AR    R5,R0                                                            
         B     CONB27                                                           
*                                                                               
CONB29   CLC   PBKLOW,E.PBKLOW        TEST LOW MONTH                            
         BL    *+10                                                             
         MVC   PBKLOW,E.PBKLOW                                                  
         CLC   PBKHI,E.PBKHI          TEST HIGH MONTH                           
         BH    *+10                                                             
         MVC   PBKHI,E.PBKHI                                                    
         AP    PBKDR,E.PBKDR                                                    
         AP    PBKCR,E.PBKCR                                                    
         B     CONB41                                                           
*                                                                               
CONB31   GOTO1 HELLO,DMCB,(C'P',ACCMST),AIO2,E.PBKELD                           
         CLI   12(R1),0                                                         
         BE    CONB41                                                           
         DC    H'0'                                                             
         DROP  R,E,R4,R5                                                        
         EJECT                                                                  
***********************************************************************         
* CREATE PRIOR BUCKET ELEMENT - IF RECORD IS TOO BIG                  *         
***********************************************************************         
                                                                                
CONB41   L     R4,AIO2                                                          
         BAS   RE,CONMX                                                         
         B     XIT                                                              
*                                                                               
CONMX    NTR1  ,                                                                
         LR    R7,R4               SAVE ADDRESS OF RECORD                       
         USING CACRECD,R4                                                       
CONMX1   LR    R4,R7                                                            
         CLC   CACRLEN,=Y(1000)     TEST RECORD LENGTH                          
         BL    XIT                                                              
         LA    R5,CACRFST                                                       
         USING PBKELD,R5                                                        
*                                                                               
CONMX3   CLI   PBKEL,0             FIND PRIOR BUCKET ELEMENT                    
         BE    CONMX5                                                           
         CLI   PBKEL,PBKELQ                                                     
         BE    CONMX7                                                           
         IC    R0,PBKLN                                                         
         AR    R5,R0                                                            
         B     CONMX3                                                           
*                                                                               
CONMX5   LA    R5,ELEMENT          NO PRIOR BUCKET                              
         MVI   PBKEL,PBKELQ        ADD ONE                                      
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,EFFS                                                      
         ZAP   PBKDR,=P'0'                                                      
         ZAP   PBKCR,=P'0'                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R4),ELEMENT                            
         B     CONB41              LOOP BACK TO THE BEGINNING                   
*                                                                               
CONMX7   LA    R3,CACRFST          FIRST BUCKET ELEMENT                         
         SR    R0,R0                                                            
         USING BUKELD,R3                                                        
         CLI   BUKEL,BUKELQ        TEST BUCKET ELEMENT                          
         BE    CONMX9                                                           
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
CONMX9   CLC   PBKLOW,BUKYEAR      GET LOW MONTH                                
         BL    *+10                                                             
         MVC   PBKLOW,BUKYEAR                                                   
         CLC   PBKHI,BUKYEAR       GET HIGH MONTH                               
         BH    *+10                                                             
         MVC   PBKHI,BUKYEAR                                                    
         AP    PBKDR,BUKDR         ADD BUCKETS                                  
         AP    PBKCR,BUKCR                                                      
         MVI   BUKEL,X'FF'         DELETE TO BUCKET                             
         GOTO1 HELLO,DMCB,(C'D',ACCMST),(X'FF',(R4)),0,0                        
         CLI   12(R1),0                                                         
         BE    CONMX1              LOOP BACK TO BEGINNING                       
         DC    H'0'                                                             
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*  POST NEW RECORD TO WORK TABLE                                                
***********************************************************************         
         SPACE 1                                                                
PNEW     NTR1  ,                                                                
         USING CACRECD,R2                                                       
         LA    R3,CACRFST                                                       
         SR    R0,R0                                                            
*                                                                               
PNEW7    CLI   0(R3),0             TEST EOR                                     
         BE    XIT                                                              
         CLI   0(R3),BUKELQ        GET BUCKET ELEMENTS                          
         BE    PNEW11                                                           
         CLI   0(R3),PBKELQ                                                     
         BE    PNEW15                                                           
PNEW8    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PNEW7                                                            
*                                                                               
         USING BUKELD,R3                                                        
PNEW11   CLC   BUKYEAR(2),=X'9512'                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         CP    BUKDR,=P'0'         SKIP ZERO BUCKETS                            
         BNE   *+14                                                             
         CP    BUKCR,=P'0'                                                      
         BE    PNEW8                                                            
         ZAP   DUB,BUKDR           ADD BUCKETS                                  
         SP    DUB,BUKCR                                                        
*                                                                               
PNEW12   DS    0H                                                               
         BAS   RE,INITWK                                                        
         MVC   WRKOFF,CACKOFF      OFFICE                                       
         MVC   WRKACC,CACKULC      NEW ACCOUNT                                  
         MVC   WRKNME,SUBNME                                                    
         ZAP   WRKADJ,DUB                                                       
         MVI   WRKSEC,WRKSGL       POST TO G/L BY NEW CONTRA                    
         MVI   WRKTYP,WRKTNEW                                                   
         BAS   RE,ADDWRK                                                        
         B     PNEW8                                                            
*                                                                               
         USING PBKELD,R3                                                        
PNEW15   CP    PBKDR,=P'0'         SKIP ZERO BUCKETS                            
         BNE   *+14                                                             
         CP    PBKCR,=P'0'                                                      
         BE    PNEW8                                                            
         ZAP   DUB,PBKDR           ADD BUCKETS                                  
         SP    DUB,PBKCR                                                        
         B     PNEW12                                                           
         EJECT                                                                  
***********************************************************************         
* SET SUB-SUBLEDGER FLAG ON GL ACCOUNTS                               *         
***********************************************************************         
         SPACE 1                                                                
SETSUB   NTR1  ,                                                                
         XC    NXTWRK,NXTWRK       SET FOR FIRST GET                            
         SR    R0,R0                                                            
SETSUB1  BAS   RE,GETWRK           GET A RECORD                                 
         BNE   SETSUB2                                                          
         TM    WRKSEC,WRKSGL       TEST G/L                                     
         BNO   SETSUB1A                                                         
         CLC   WRKACC,SPACES       TEST OLD CONTRA                              
         BH    SETSUB1                                                          
         CP    WRKOLD,=P'0'                                                     
         BE    *+8                                                              
         OI    STATUS,STOCA        SET OLD CONTRA FLAG                          
         B     SETSUB1                                                          
SETSUB1A TM    WRKSEC,WRKSRC       SOURCE                                       
         BNO   SETSUB1                                                          
         CP    WRKOLD,=P'0'                                                     
         BE    SETSUB1                                                          
         AH    R1,=H'1'                                                         
         CH    R1,=H'1'                                                         
         BNH   SETSUB1                                                          
         OI    STATUS,STMIA        SET MULTIPLE INPUT ACCOUNTS                  
         B     SETSUB1                                                          
*                                                                               
SETSUB2  XC    NXTWRK,NXTWRK       SET FOR FIRST GET                            
SETSUB3  BAS   RE,GETWRK           GET A RECORD                                 
         BNE   XIT                                                              
         TM    WRKSEC,WRKSSB       GET FIRST SUBLEDGER                          
         BNO   SETSUB3                                                          
*                                                                               
         L     R3,ADRWRK           R3 =  ADDRESS OF FIRST SUBLEDGER             
SETSUB5  XC    NXTWRK,NXTWRK       START AGAIN AT TOP                           
SETSUB7  BAS   RE,GETWRK           GET A RECORD                                 
         BNE   SETSUB11                                                         
         TM    WRKSEC,WRKSGL       TEST G/L RECORD                              
         BNO   SETSUB7                                                          
         CLC   WRKOFF,WRKOFF-WRKR(R3)   TEST SAME OFFICE                        
         BNE   SETSUB7                                                          
         CLC   WRKACC,WRKACC-WRKR(R3)   TEST SAME ACCOUNT                       
         BNE   SETSUB7                                                          
         L     RF,ADRWRK           RF =  CURRENT ENTRY                          
         OI    WRKSTAT-WRKR(RF),WRKSGLP     MATCHING G/L POINTER                
*                                                                               
SETSUB11 LA    R3,WRKLNQ(R3)       NEXT SUBLEDGER                               
         CLI   0(R3),0             EOF                                          
         BE    XIT                                                              
         CLI   WRKTYP-WRKR(R3),X'FF'                                            
         BE    SETSUB11                                                         
         B     SETSUB5                                                          
         EJECT                                                                  
***********************************************************************         
* GET OLD DATA FROM TEST FILE                                         *         
***********************************************************************         
         SPACE 1                                                                
OLD      NTR1  ,                                                                
         L     R1,ACCLIST          LOOK UP OLD ACCOUNT                          
OLD3     CLC   14(14,R1),ACCODE+1  FIND ALL TO ACCOUNTS                         
         BNE   *+8                                                              
         BAS   RE,POLD             POST OLD DATA                                
         LA    R1,L'NYACC(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   OLD3                                                             
*                                                                               
         L     R6,AGLPTAB          G/L POINTER TABLE                            
         USING GLPTD,R6                                                         
OLD5     CLC   GLPTTO,ACCODE+1     FIND ALL 'SB' ACCOUNTS                       
         BNE   OLD9                                                             
         CLC   GLPTFR(2),=C'SB'                                                 
         BNE   OLD9                                                             
         L     R1,ACCLIST          LOOK UP OLD ACCOUNT                          
*                                                                               
OLD7     CLC   14(14,R1),GLPTFR    FIND ALL TO 'SB' ACCOUNTS                    
         BNE   OLD8                                                             
         CLC   0(2,R1),=C'GB'      TEST OLD 'GB' ACCOUNT                        
         BNE   OLD8                                                             
         CLI   QOPT5,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,POLD             POST OLD 'GB' CONVERTRD TO 'SB'              
OLD8     LA    R1,L'NYACC(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   OLD7                                                             
*                                                                               
OLD9     LA    R6,GLPTLNQ(R6)                                                   
         CLI   0(R6),X'FF'                                                      
         BNE   OLD5                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* POST OLD BUCKETS                                                    *         
***********************************************************************         
         SPACE 1                                                                
POLD     NTR1  ,                                                                
         MVC   OLDNME,SPACES                                                    
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),RCCOMPFL                                                 
         CLI   DKEY,X'F5'          BDTEST FOR NY                                
         BNE   *+8                                                              
         MVI   DKEY,X'F6'                                                       
         MVC   DKEY+1(14),0(R1)    OLD ACCOUNT                                  
         BAS   RE,HIGH                                                          
*                                                                               
POLD3    CLC   DIR(15),DKEY        TEST CORRECT ACCOUNT                         
         BNE   XIT                                                              
         BAS   RE,INITSUM                                                       
         BAS   RE,INITWK                                                        
         L     R2,AIO2                                                          
         BAS   RE,GET                                                           
         USING CACRECD,R2                                                       
         CLC   CACKULC,SPACES      TEST ACCOUNT RECORD                          
         BE    POLD5                                                            
         BAS   RE,CONOLD           CONVERT OLD CONTRA AND OFFICE                
         CLC   NEWCON,SPACES                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   SUMOFF,NEWOFF       OFFICE                                       
         MVC   SUMCON,NEWCON       NEW ACCOUNT                                  
*                                                                               
POLD5    LA    R3,CACRFST                                                       
         SR    R0,R0                                                            
*                                                                               
POLD7    CLI   0(R3),0             TEST EOR                                     
         BE    POLD21                                                           
         CLI   0(R3),BUKELQ        GET BUCKET ELEMENTS                          
         BE    POLD11                                                           
         CLI   0(R3),PBKELQ                                                     
         BE    POLD15                                                           
         CLI   0(R3),NAMELQ                                                     
         BE    POLD19                                                           
POLD8    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     POLD7                                                            
*                                                                               
         USING BUKELD,R3                                                        
POLD11   CLI   QOPT5,C'Y'                                                       
         BE    *+14                                                             
         CLC   BUKYEAR(2),=X'9512'                                              
         BH    POLD8                                                            
         MVC   SUMMOA,BUKYEAR                                                   
         ZAP   SUMDR,BUKDR                                                      
         ZAP   SUMCR,BUKCR                                                      
         BAS   RE,ADDSUM                                                        
*        CP    BUKDR,=P'0'         SKIP ZERO BUCKETS                            
*        BNE   *+14                                                             
*        CP    BUKCR,=P'0'                                                      
*        BE    POLD8                                                            
         ZAP   DUB,BUKDR           ADD BUCKETS                                  
         SP    DUB,BUKCR                                                        
*                                                                               
POLD12   DS    0H                                                               
         MVI   WRKTYP,WRKTOLD                                                   
*        MVC   WRKOFF,NEWOFF       OFFICE                                       
         MVC   WRKACC,NEWCON       NEW CONTRA                                   
         MVC   WRKNME,CONNME                                                    
         ZAP   WRKOLD,DUB                                                       
         MVI   WRKSEC,WRKSGL       POST TO G/L BY NEW CONTRA                    
         BAS   RE,ADDWRK                                                        
*                                                                               
         MVI   WRKSEC,WRKSRC       POST TO OLD SOURCE                           
         MVC   WRKOFF,SPACES                                                    
         MVC   WRKACC,DIR+1                                                     
         MVC   WRKNME,OLDNME                                                    
         ZAP   WRKOLD,DUB                                                       
         BAS   RE,ADDWRK                                                        
         CLI   QOPT5,C'Y'          CREATE CONVERSION TABLE                      
         BNE   POLD8                                                            
         MVC   CNVR(CNVLNQ),SPACES                                              
         MVC   CNVOAC,CACKULA      OLD ACCOUNT                                  
         MVC   CNVOCA,CACKULC          CONTRA                                   
         MVC   CNVNAC,ACCODE+1     NEW ACCOUNT                                  
         MVC   CNVNCA,NEWCON           CONTRA                                   
         MVC   CNVNOF,NEWOFF           OFFICE                                   
         CLC   NEWOFF,=C'**'                                                    
         BE    POLD8                                                            
         GOTO1 BINADD,DMCB,CNVR,ADCNV                                           
         B     POLD8                                                            
*                                                                               
         USING PBKELD,R3                                                        
POLD15   MVC   SUMMOA,PBKLOW                                                    
         ZAP   SUMDR,PBKDR                                                      
         ZAP   SUMCR,PBKCR                                                      
         BAS   RE,ADDSUM                                                        
         CP    PBKDR,=P'0'         SKIP ZERO BUCKETS                            
         BNE   *+14                                                             
         CP    PBKCR,=P'0'                                                      
         BE    POLD8                                                            
         ZAP   DUB,PBKDR           ADD BUCKETS                                  
         SP    DUB,PBKCR                                                        
         B     POLD12                                                           
*                                                                               
         USING NAMELD,R3                                                        
POLD19   SR    R1,R1               NAME                                         
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   OLDNME(0),NAMEREC                                                
         B     POLD8                                                            
*                                                                               
POLD21   BAS   RE,SEQ                                                           
         B     POLD3                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CONVERT OLD CONTRA AND OFFICE                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CACRECD,R2                                                       
CONOLD   NTR1  ,                                                                
         MVC   NEWCON,CACKULC     DEFAULT IS OLD                                
         MVC   CONNME,SPACES                                                    
         L     R1,ACCLIST                                                       
*                                                                               
CONOLD3  CLC   0(14,R1),CACKULC    FIND OLD CONTRA IN FROM LIST                 
         BE    CONOLD5                                                          
         LA    R1,L'NYACC(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   CONOLD3                                                          
         B     CONOLD6                                                          
*                                                                               
CONOLD5  MVC   NEWCON,14(R1)       SAVE NEW ACCOUNT                             
CONOLD6  BAS   RE,FIXCON                                                        
         LA    R3,NEWCON                                                        
         LA    R4,CONNME                                                        
         MVC   SVDIR,DIR           SAVE                                         
         BAS   RE,GETNME                                                        
         MVC   DKEY,SVDIR          RE ESTABLISH SEQUENCE                        
         BAS   RE,HIGH                                                          
*                                                                               
         L     R1,OFFLIST          NOW DO OFFICE                                
*                                                                               
CONOLD7  CLC   0(1,R1),CACKACT+6    FIND OLD OFFICE                             
         BE    CONOLD9                                                          
         LA    R1,L'NYOFF(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   CONOLD7                                                          
         MVC   NEWOFF,=C'**'                                                    
         B     XIT                                                              
*                                                                               
CONOLD9  MVC   NEWOFF,1(R1)         SAVE NEW OFFICE                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET THE SUB LEDGER DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
SUBL     NTR1  ,                                                                
         L     R6,AGLPTAB          G/L POINTER TABLE                            
         USING GLPTD,R6                                                         
SUBL3    CLC   GLPTTO,ACCODE+1     FIND ALL 'TO' ACCOUNTS                       
         BNE   SUBL5                                                            
         MVC   SBAC,GLPTFR         SAVE SUB LEDGER ACCOUNT                      
         BAS   RE,PSUBL            POST SUB LEDGER DATA                         
SUBL5    LA    R6,GLPTLNQ(R6)                                                   
         CLI   0(R6),X'FF'                                                      
         BNE   SUBL3                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* POST SUB LEDGER DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
PSUBL    NTR1  ,                                                                
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),RCCOMPFL                                                 
         MVC   DKEY+1(14),SBAC     OLD ACCOUNT                                  
         BAS   RE,HIGH                                                          
*                                                                               
PSUBL3   CLC   DIR(15),DKEY        TEST CORRECT ACCOUNT                         
         BNE   XIT                                                              
         BAS   RE,INITWK                                                        
         MVI   WRKSEC,WRKSSB                                                    
         MVI   WRKTYP,WRKTNEW      ACCOUNT                                      
         MVC   WRKACC,GLPTFR       SOURCE ACCOUNT                               
         MVC   WRKNME,SPACES                                                    
         L     R2,AIO2                                                          
         BAS   RE,GET                                                           
         USING ACTRECD,R2                                                       
         LA    R3,ACTRFST                                                       
         SR    R0,R0                                                            
*                                                                               
PSUBL5   CLI   0(R3),0             TEST EOR                                     
         BE    PSUBL17                                                          
         CLI   0(R3),TRNELQ        TRANSACTION                                  
         BE    PSUBL7                                                           
         CLI   0(R3),NAMELQ        NAME                                         
         BE    PSUBL9                                                           
         CLI   0(R3),ABLELQ        BALANCE                                      
         BE    PSUBL11                                                          
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PSUBL5                                                           
*                                                                               
         USING TRNRECD,R2                                                       
PSUBL7   CLC   TRNRSMOS,=X'9512'                                                
         BH    PSUBL17                                                          
         USING TRNELD,R3                                                        
         CP    TRNAMNT,=P'0'                                                    
         BE    PSUBL17                                                          
         MVC   WRKOFF,TRNOFFC      OFFICE                                       
         ZAP   WRKACT,TRNAMNT      ACTIVITY AMOUNT                              
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+10                                                             
         MP    WRKACT,=P'-1'                                                    
         B     PSUBL16                                                          
*                                                                               
         USING NAMELD,R3                                                        
PSUBL9   SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+4                                                           
         MVC   WRKNME(0),NAMEREC                                                
         B     PSUBL17                                                          
*                                                                               
         USING OFARECD,R2                                                       
PSUBL11  CLC   OFAKOFF,SPACES                                                   
         BNH   PSUBL17                                                          
         USING ABLELD,R3                                                        
         CP    ABLFRWD,=P'0'                                                    
         BE    PSUBL17                                                          
         ZAP   WRKBBF,ABLFRWD      BALANCE FORWARD                              
         MVC   WRKOFF,OFAKOFF      OFFICE                                       
*                                                                               
PSUBL16  DS    0H                                                               
         BAS   RE,ADDWRK                                                        
*                                                                               
PSUBL17  BAS   RE,SEQ                                                           
         B     PSUBL3                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF GENERAL LEDGER POINTER                               *         
***********************************************************************         
                                                                                
BLDGL    NTR1  ,                                                                
         L     R6,AGLPTAB          G/L POINTER TABLE                            
         USING GLPTD,R6                                                         
         SR    R3,R3               NUMBERS OF ENTRIES                           
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY       READ ALL UNIT 'S' ACCOUNTS                   
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVI   ACTKUNT,C'S'                                                     
         MVC   ACTKLDG,QOPT3       OPTION TO USE ONLY ONE LEDGER                
*                                                                               
BLDGL1   BAS   RE,HIGH                                                          
         CLC   DIR(2),DKEY                                                      
         BNE   XIT                                                              
         CLI   QOPT3,C' '          TEST ONE LEDGER OPTION                       
         BNH   *+14                                                             
         CLC   DIR+2(1),QOPT3                                                   
         BNE   XIT                                                              
         CLC   DIR+1(2),=C'SJ'     SKIP SJ                                      
         BNE   BLDGL2                                                           
         MVC   DKEY+1(14),=CL14'SK'                                             
         B     BLDGL1                                                           
BLDGL2   LA    R2,DIR                                                           
         TM    ACTKSTAT,ACTSABLP   TEST BALANCE ELEMENT                         
         BNO   BLDGL15                                                          
*                                                                               
BLDGL3   L     R2,AIO1                                                          
         BAS   RE,GET              GET THE RECORD                               
         LA    R4,ACTRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING GLPELD,R4                                                        
BLDGL5   CLI   0(R4),GLPELQ        X'15' ELEMENT                                
         BE    BLDGL9                                                           
         CLI   0(R4),0                                                          
         BE    BLDGL15             EOR                                          
BLDGL7   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDGL5                                                           
*                                                                               
BLDGL9   MVC   GLPTTO,SPACES                                                    
         CLC   GLPACC1(2),=C'GB'                                                
         BNE   BLDGL15                                                          
         MVC   GLPTTO(10),GLPACC1  G/L ACCOUNT                                  
         CLI   GLPLN,26            NEW LENGTH                                   
         BL    *+10                                                             
         MVC   GLPTTO,GLPACC1      TO ACCOUNT                                   
         MVC   GLPTFR,ACTKULA      FROM ACCOUNT                                 
         LA    R6,GLPTLNQ(R6)                                                   
         MVI   0(R6),X'FF'                                                      
         AH    R3,=H'1'            BUMP COUNT                                   
         CH    R3,=Y(GLPTMAX)                                                   
         BL    *+6                                                              
         DC    H'0'                TABLE FULL                                   
*                                                                               
BLDGL15  MVC   DKEY,SPACES         GET NEXT ACCOUNT                             
         MVC   DKEY(15),DIR                                                     
         LA    R2,DKEY                                                          
         SR    R1,R1                                                            
         IC    R1,ACTKACT+L'ACTKACT-1                                           
         AH    R1,=H'1'                                                         
         STC   R1,ACTKACT+L'ACTKACT-1                                           
         B     BLDGL1                                                           
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT NAME                                                    *         
***********************************************************************         
                                                                                
GETNME   NTR1 ,                                                                 
         MVC   ADMGR,DATAMGR       SET FOR LIVE FILE                            
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(1),RCCOMPFL                                                 
         MVC   DKEY+1(14),0(R3)    SET ACCOUNT KEY                              
         BAS   RE,HIGH             READ HIGH FOR ACCOUNT                        
         CLC   DKEY,DIR                                                         
         BNE   GETNMX                                                           
         L     R2,AIO3                                                          
         BAS   RE,GET              GET THE RECORD                               
         USING ACTRECD,R2                                                       
         SR    R1,R1                                                            
         LA    R5,ACTRFST                                                       
*                                                                               
         USING NAMELD,R5                                                        
GETNME3  IC    R1,NAMLN                                                         
         CLI   0(R5),0                                                          
         BE    GETNMX                                                           
         CLI   0(R5),NAMELQ                                                     
         BE    GETNME4                                                          
         AR    R5,R1                                                            
         B     GETNME3                                                          
*                                                                               
GETNME4  SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,R4),NAMEREC                                                  
GETNMX   MVC   ADMGR,VDMGR         RESET FOR  TEST FILE                         
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
         SPACE 1                                                                
RPT      NTR1  ,                                                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7         R7=ADDRESSES WIDE PRINT                      
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         MVI   RCSUBPRG,0          SET CORRECT HEADINGS                         
         MVI   REPTS,0                                                          
         MVI   OFFCNT,0                                                         
         OI    BINFLG,BINFTOT      PASS TOTAL RECORDS                           
         XC    NXTWRK,NXTWRK       SET FOR FIRST RECORD                         
*                                                                               
RPT3     BAS   RE,GETWRK           GET FIRST/NEXT RECORD                        
         BNE   XIT                                                              
         CLI   WRKOFF,X'FF'        TEST ACCOUNT TOTAL                           
         BE    RPT19                                                            
         CLI   WRKTYP,X'FF'        TEST OFFICE TOTAL                            
         BE    RPT17                                                            
         TM    WRKSEC,WRKSRC       IS IT A SOURCE ACCOUNT                       
         BNO   *+14                                                             
         CP    WRKOLD,=P'0'        WITH NO BALANCE                              
         BE    RPT3                                                             
*                                                                               
         LA    RE,REPSUB                                                        
RPT5     CLC   0(1,RE),WRKSEC      GET SUB-HEADING                              
         BE    RPT7                                                             
         LA    RE,L'REPSUB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   RPT5                                                             
         B     RPT12                                                            
RPT7     SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    REPTS,0             TEST ALREADY PRINTED                         
         BO    RPT12                                                            
         OC    REPTS,1(RE)         SET IT ON                                    
         MVC   PLNME(23),2(RE)     PRINT THE SUB TOTLE                          
         BAS   RE,PRTL                                                          
                                                                                
*                                                                               
RPT12    MVC   PLOFF(2),WRKOFF     OFFICE                                       
         MVC   PLACC,WRKACC        ACCOUNT                                      
         MVC   PLNME,WRKNME        NAME                                         
         LA    R4,WRKBBF                                                        
         BAS   RE,EDIT                                                          
         BAS   RE,PRTL                                                          
         SR    R1,R1                                                            
         IC    R1,OFFCNT           COUNT NUMBER OF LINES FOR OFFICE             
         AH    R1,=H'1'                                                         
         STC   R1,OFFCNT                                                        
         B     RPT3                                                             
*                                                                               
RPT17    BAS   RE,POFFT            PRINT OFFICE TOTALS                          
         B     RPT3                                                             
*                                                                               
RPT19    BAS   RE,PACCT            PRINT ACCOUNT TOTALS                         
         B     RPT3                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT OFFICE / ACCOUNT TOTALS                                       *         
***********************************************************************         
         SPACE 1                                                                
POFFT    NTR1  ,                                                                
         CLI   OFFCNT,1            CHECK MORE THAN ONE OFFICE                   
         BNH   POFFT3                                                           
         MVC   PLNME+2(20),=CL20'* OFFICE TOTAL *'                              
         LA    R4,WRKBBF           PRINT THE OFFICE TOTALS                      
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
POFFT3   BAS   RE,PRTL                                                          
         MVI   OFFCNT,0                                                         
         B     XIT                                                              
*                                                                               
PACCT    NTR1  ,                                                                
         MVC   PLNME+1(20),=CL20'** ACCOUNT TOTAL **'                           
         LA    R4,WRKBBF           PRINT THE ACCOUNT TOTALS                     
         BAS   RE,EDIT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,PRTL                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EDIT A ROW OF ACCUMS                                                *         
***********************************************************************         
         SPACE 1                                                                
EDIT     NTR1  ,                                                                
         LA    R3,WRKBKN                                                        
         LA    R5,PLBBF                                                         
EDIT1    MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),0(R4)                                              
         MVC   0(L'PLBBF,R5),WORK+1                                             
         LA    R4,8(R4)                                                         
         LA    R5,L'PLBBF+1(R5)                                                 
         BCT   R3,EDIT1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GO TO ACREPORT                                                      *         
***********************************************************************         
         SPACE 1                                                                
PRTL     NTR1  ,                                                                
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7         R7=ADDRESSES WIDE PRINT                      
         MVC   XHEAD5+10(L'ACCODE-1),ACCODE+1                                   
         MVC   XHEAD6+10(L'ACNAME),ACNAME                                       
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD A ROW AND TOTALS TO WRK TABLE                                   *         
***********************************************************************         
         SPACE 1                                                                
ADDWRK   NTR1  ,                                                                
         AP    WRKNEW,WRKBBF                                                    
         AP    WRKNEW,WRKACT                                                    
         AP    WRKNEW,WRKADJ                                                    
         BAS   RE,ADDRALL                                                       
         LA    R1,WRKBK            CLEAR ACCUMS                                 
         LA    R0,WRKBKN                                                        
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     XIT                                                              
*                                                                               
ADDRALL  LR    R0,RE                                                            
         GOTO1 BINADD,DMCB,WRKR,ADWRK                                           
         XC    TOTWRK,TOTWRK                                                    
         MVC   TOTWRK(WRKACC-WRKR),WRKR                                         
         MVC   TOTWRK+(WRKBK-WRKR)(L'WRKBK),WRKBK                               
         MVI   TOTWRK+(WRKTYP-WRKR),X'FF'                                       
         GOTO1 BINADD,DMCB,TOTWRK,ADWRK   OFFICE TOTAL                          
         MVC   TOTWRK+(WRKOFF-WRKR)(L'WRKOFF),=X'FFFF'                          
         GOTO1 BINADD,DMCB,TOTWRK,ADWRK   ACCOUNT TOTAL                         
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ADD A ROW AND TOTALS TO SUM TABLE                                   *         
***********************************************************************         
         SPACE 1                                                                
ADDSUM   NTR1  ,                                                                
         GOTO1 BINADD,DMCB,SUMR,ADSUM                                           
         XC    TOTSUM,TOTSUM                                                    
         MVC   TOTSUM(SUMMOA-SUMR),SUMR                                         
         MVC   TOTSUM+(SUMBK-SUMR)(L'SUMBK),SUMBK                               
         MVI   TOTSUM+(SUMMOA-SUMR),X'FF'                                       
         GOTO1 BINADD,DMCB,TOTSUM,ADSUM     OFFICE/CONTRA TOTAL                 
         LA    R1,SUMBK                                                         
         LA    R0,SUMBKN                                                        
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIX CONTRA                                                          *         
***********************************************************************         
         SPACE 1                                                                
FIXCON   NTR1  ,                                                                
         LA    R2,DIR                                                           
         USING CACRECD,R2                                                       
         CLC   NEWCON(2),SPACES    SPECIAL MEDIA CONTRA                         
         BE    XIT                                                              
         CLC   NEWCON(2),=C'SR'    ALL SR'S BECOME 'SR'                         
         BNE   FIXCON11                                                         
         CLC   QSELECT(2),=C'CH'                                                
         BNE   FIXCON09                                                         
         CLC   CACKULA(6),=C'GBAL40'                                            
         BE    FIXCON20                                                         
         CLC   CACKULA(6),=C'GBAL45'                                            
         BE    FIXCON20                                                         
FIXCON09 MVC   NEWCON+2(12),SPACES                                              
         B     XIT                                                              
*                                                                               
FIXCON11 CLC   NEWCON(2),=C'SB'    ALREADY SET FOR SB                           
         BE    FIXCON19                                                         
*                                                                               
FIXCON13 LA    R1,SUBLST           R1=LIST OF G/L POINTERS                      
FIXCON15 CLI   0(R1),X'FF'         END OF LIST                                  
         BE    FIXCON17                                                         
         CLC   NEWCON,0(R1)                                                     
         BE    FIXCON19                                                         
         LA    R1,14(R1)                                                        
         B     FIXCON15                                                         
FIXCON17 MVC   NEWCON,SUBACC       USE DEFAULT SUBACC                           
*                                                                               
FIXCON19 CLC   NEWCON(2),=C'SB'                                                 
         BNE   XIT                                                              
         CLC   QSELECT(2),=C'NY'   NEW YORK                                     
         BNE   FIXCON20                                                         
         LA    RF,GBAE                 GBAE                                     
         CLC   CACKULA(4),=C'GBAE'                                              
         BE    FIXCON23                                                         
         LA    RF,GBAL40               GBAL40                                   
         CLC   CACKULA(6),=C'GBAL40'                                            
         BE    FIXCON23                                                         
         LA    RF,GBAL41               GBAL41                                   
         CLC   CACKULA(6),=C'GBAL41'                                            
         BE    FIXCON23                                                         
         LA    RF,GBAR56               GBAR56                                   
         CLC   CACKULA(6),=C'GBAR56'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLB04               GBLB04                                   
         CLC   CACKULA(6),=C'GBLB04'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLQ30               GBLQ30                                   
         CLC   CACKULA(6),=C'GBLQ30'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLB07               GBLB07                                   
         CLC   CACKULA(6),=C'GBLB07'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLM24               GBLM24                                   
         CLC   CACKULA(6),=C'GBLM24'                                            
         BE    FIXCON27                                                         
         LA    RF,GBLG14               GBLG14                                   
         CLC   CACKULA(6),=C'GBLG14'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLR31               GBLR31                                   
         CLC   CACKULA(6),=C'GBLR31'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLR32               GBLR32                                   
         CLC   CACKULA(6),=C'GBLR32'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLT40               GBLT40                                   
         CLC   CACKULA(6),=C'GBLT40'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLT41               GBLT41                                   
         CLC   CACKULA(6),=C'GBLT41'                                            
         BE    FIXCON23                                                         
         LA    RF,GBLV50               GBLV50                                   
         CLC   CACKULA(6),=C'GBLV50'                                            
         BE    FIXCON23                                                         
         B     XIT                                                              
*                                                                               
FIXCON20 CLC   QSELECT(2),=C'CH'      CHICAGO                                   
         BNE   FIXCON21                                                         
         LA    RF,CHGBAL40              GBAL40                                  
         CLC   CACKULA(6),=C'GBAL40'                                            
         BE    FIXCON27                                                         
         LA    RF,CHGBAL45                                                      
         CLC   CACKULA(6),=C'GBAL45'    GBAL45                                  
         BE    FIXCON27                                                         
         B     XIT                                                              
*                                                                               
FIXCON21 CLC   QSELECT(2),=C'AT'        ATLANTA                                 
         BNE   XIT                                                              
         LA    RF,ATGBAE15              GBAE15                                  
         CLC   CACKULA(6),=C'GBAE15'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBAE14              GBAE14                                  
         CLC   CACKULA(6),=C'GBAE14'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBAG28              GBAG28                                  
         CLC   CACKULA(6),=C'GBAG28'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBAI30              GBAI30                                  
         CLC   CACKULA(6),=C'GBAI30'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBLA01              GBLA01                                  
         CLC   CACKULA(6),=C'GBLA01'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBLA02              GBLA02                                  
         CLC   CACKULA(6),=C'GBLA02'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBLM24              GBLM24                                  
         CLC   CACKULA(6),=C'GBLM24'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBLE11              GBLE11                                  
         CLC   CACKULA(6),=C'GBLE11'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBLR31              GBLR31                                  
         CLC   CACKULA(6),=C'GBLR31'                                            
         BE    FIXCON23                                                         
         LA    RF,ATGBLR32              GBLR32                                  
         CLC   CACKULA(6),=C'GBLR32'                                            
         BE    FIXCON23                                                         
         B     XIT                                                              
*                                                                               
FIXCON23 CLI   0(RF),X'FF'                                                      
         BE    XIT                                                              
         CLC   CACKULA+6(2),0(RF)                                               
         BE    FIXCON25                                                         
         CLC   0(2,RF),=C'**'                                                   
         BE    FIXCON25                                                         
         LA    RF,L'GBAE(RF)                                                    
         B     FIXCON23                                                         
FIXCON25 MVC   NEWCON+8(2),2(RF)                                                
         B     XIT                                                              
*                                                                               
FIXCON27 CLI   0(RF),X'FF'         BIG TABLE                                    
         BE    XIT                                                              
         CLC   CACKULA+6(2),0(RF)                                               
         BE    FIXCON29                                                         
         CLC   0(2,RF),=C'**'                                                   
         BE    FIXCON29                                                         
         LA    RF,L'GBLM24(RF)                                                  
         B     FIXCON27                                                         
FIXCON29 MVC   NEWCON,2(RF)                                                     
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
GBAE     DS    0CL4                                                             
         DC    C'0202'                                                          
         DC    C'0303'                                                          
         DC    C'0404'                                                          
         DC    C'0505'                                                          
         DC    C'0606'                                                          
         DC    C'0707'                                                          
         DC    C'0808'                                                          
         DC    C'0909'                                                          
         DC    C'1110'                                                          
         DC    C'1211'                                                          
         DC    C'1312'                                                          
         DC    C'1613'                                                          
         DC    C'1714'                                                          
         DC    C'1816'                                                          
         DC    C'2002'                                                          
         DC    C'3003'                                                          
         DC    C'3104'                                                          
         DC    C'4005'                                                          
         DC    X'FF'                                                            
*                                                                               
GBAL40   DS    0CL4                                                             
         DC    C'1001'                                                          
         DC    C'1113'                                                          
         DC    C'1406'                                                          
         DC    C'1906'                                                          
         DC    C'1502'                                                          
         DC    C'3616'                                                          
         DC    C'**50'                                                          
         DC    X'FF'                                                            
*                                                                               
GBAL41   DS    0CL4                                                             
         DC    C'1010'                                                          
         DC    C'5302'                                                          
         DC    X'FF'                                                            
*                                                                               
GBAR56   DS    0CL4                                                             
         DC    C'0104'                                                          
         DC    C'0305'                                                          
         DC    C'0406'                                                          
         DC    C'0507'                                                          
         DC    C'0608'                                                          
         DC    C'0709'                                                          
         DC    C'0810'                                                          
         DC    C'0911'                                                          
         DC    C'1012'                                                          
         DC    C'1213'                                                          
         DC    C'1314'                                                          
         DC    C'1515'                                                          
         DC    C'2016'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLB04   DS    0CL4                                                             
         DC    C'0101'                                                          
         DC    C'0202'                                                          
         DC    C'0303'                                                          
         DC    C'0404'                                                          
         DC    C'0505'                                                          
         DC    C'0606'                                                          
         DC    C'0707'                                                          
         DC    C'0808'                                                          
         DC    C'0909'                                                          
         DC    C'1010'                                                          
         DC    C'1110'                                                          
         DC    C'1313'                                                          
         DC    C'1413'                                                          
         DC    C'2014'                                                          
         DC    C'3115'                                                          
         DC    C'9016'                                                          
         DC    C'9918'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLQ30   DS    0CL4                                                             
         DC    C'0501'                                                          
         DC    C'1102'                                                          
         DC    C'2503'                                                          
         DC    C'2904'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLB07   DS    0CL4                                                             
         DC    C'0101'                                                          
         DC    C'0202'                                                          
         DC    C'0303'                                                          
         DC    C'0404'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLM24   DS    0CL16                                                            
         DC    C'02',CL14'SBLA700102'                                           
         DC    C'03',CL14'SBLA700103'                                           
         DC    C'04',CL14'SBLA700107'                                           
         DC    C'05',CL14'SBLA700104'                                           
         DC    C'07',CL14'SBLB100106'                                           
         DC    C'10',CL14'SBLB100108'                                           
         DC    C'12',CL14'SBLB100109'                                           
         DC    C'13',CL14'SBLB100110'                                           
         DC    C'14',CL14'SBLB100111'                                           
         DC    C'15',CL14'SBLB100112'                                           
         DC    C'16',CL14'SBLB100119'                                           
         DC    C'18',CL14'SBLB100120'                                           
         DC    C'20',CL14'SBLB100105'                                           
         DC    C'30',CL14'SBLB100113'                                           
         DC    C'32',CL14'SBLB100115'                                           
         DC    C'06',CL14'SBLB100150'                                           
         DC    C'09',CL14'SBLB100118'                                           
         DC    C'31',CL14'SBLB100114'                                           
         DC    X'FF'                                                            
*                                                                               
GBLG14   DS    0CL4                                                             
         DC    C'0201'                                                          
         DC    C'0302'                                                          
         DC    C'0503'                                                          
         DC    C'0604'                                                          
         DC    C'0705'                                                          
         DC    C'0806'                                                          
         DC    C'0907'                                                          
         DC    C'1208'                                                          
         DC    C'1509'                                                          
         DC    C'1810'                                                          
         DC    C'1911'                                                          
         DC    C'2212'                                                          
         DC    C'2413'                                                          
         DC    C'2614'                                                          
         DC    C'2815'                                                          
         DC    C'3016'                                                          
         DC    C'3417'                                                          
         DC    C'4018'                                                          
         DC    C'4220'                                                          
         DC    C'4421'                                                          
         DC    C'4622'                                                          
         DC    C'4823'                                                          
         DC    C'5224'                                                          
         DC    C'6025'                                                          
         DC    C'6226'                                                          
         DC    C'8027'                                                          
         DC    C'8228'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLR31   DS    0CL4                                                             
         DC    C'1001'                                                          
         DC    C'1102'                                                          
         DC    C'1404'                                                          
         DC    C'1505'                                                          
         DC    C'2008'                                                          
         DC    C'3009'                                                          
         DC    C'5310'                                                          
         DC    C'6012'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLR32   DS    0CL4                                                             
         DC    C'0402'                                                          
         DC    C'0604'                                                          
         DC    C'0805'                                                          
         DC    C'0201'                                                          
         DC    C'1406'                                                          
         DC    C'1507'                                                          
         DC    C'2508'                                                          
         DC    C'3409'                                                          
         DC    C'4310'                                                          
         DC    C'4811'                                                          
         DC    C'5012'                                                          
         DC    C'0503'                                                          
         DC    C'3113'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLT40   DS    0CL4                                                             
         DC    C'2003'                                                          
         DC    C'4005'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLT41   DS    0CL4                                                             
         DC    C'1201'                                                          
         DC    C'2002'                                                          
         DC    X'FF'                                                            
*                                                                               
GBLV50   DS    0CL4                                                             
         DC    C'0101'                                                          
         DC    C'0202'                                                          
         DC    X'FF'                                                            
*                                                                               
CHGBAL40 DS    0CL16                                                            
         DC    C'01',CL14'SRRCAYATC01'                                          
         DC    C'20',CL14'SRRCAYNYC01'                                          
         DC    C'32',CL14'SRRCAYTLC01'                                          
         DC    C'35',CL14'SRRCAYMNC01'                                          
         DC    C'40',CL14'SRRCAYLAC01'                                          
         DC    X'FF'                                                            
*                                                                               
*                                CHICAG0                                        
CHGBAL45 DS    0CL16                                                            
         DC    C'12',CL14'SRSCA2AMC00'                                          
         DC    C'13',CL14'SRQCA2IMC00'                                          
         DC    C'14',CL14'SRSCA2FHC01'                                          
         DC    C'28',CL14'SRSCA2HLC00'                                          
         DC    C'36',CL14'SRSCA2GYC00'                                          
         DC    C'37',CL14'SRSCA2TIC00'                                          
         DC    C'38',CL14'SRSCA2HKC00'                                          
         DC    C'15',CL14'SRSCA2ITC00'                                          
         DC    X'FF'                                                            
*                                                                               
ATGBAE15 DS    0CL4                                                             
         DC    C'0417'                                                          
         DC    C'0508'                                                          
         DC    C'0606'                                                          
         DC    C'0718'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBAE14 DS    0CL4                                                             
         DC    C'0202'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBAG28 DS    0CL4                                                             
         DC    C'0017'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBAI30 DS    0CL4                                                             
         DC    C'0209'                                                          
         DC    C'0308'                                                          
         DC    C'0506'                                                          
         DC    C'0709'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBLA01 DS    0CL4                                                             
         DC    C'0101'                                                          
         DC    C'1102'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBLA02 DS    0CL4                                                             
         DC    C'0910'                                                          
         DC    C'0103'                                                          
         DC    C'0204'                                                          
         DC    C'0305'                                                          
         DC    C'0406'                                                          
         DC    C'0507'                                                          
         DC    C'0608'                                                          
         DC    C'0809'                                                          
         DC    C'0910'                                                          
         DC    C'1011'                                                          
         DC    C'1112'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBLM24 DS    0CL4                                                             
         DC    C'0801'                                                          
         DC    C'0207'                                                          
         DC    C'0301'                                                          
         DC    C'0502'                                                          
         DC    C'0617'                                                          
         DC    C'1021'                                                          
         DC    C'1101'                                                          
         DC    C'0922'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBLE11 DS    0CL4                                                             
         DC    C'0202'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBLR31 DS    0CL4                                                             
         DC    C'0103'                                                          
         DC    C'1501'                                                          
         DC    C'1205'                                                          
         DC    C'0706'                                                          
         DC    C'1108'                                                          
         DC    C'1011'                                                          
         DC    C'0506'                                                          
         DC    C'1703'                                                          
         DC    X'FF'                                                            
*                                                                               
ATGBLR32 DS    0CL4                                                             
         DC    C'0219'                                                          
         DC    C'0308'                                                          
         DC    C'0709'                                                          
         DC    C'0813'                                                          
         DC    C'1005'                                                          
         DC    C'2314'                                                          
         DC    C'2417'                                                          
         DC    C'0507'                                                          
         DC    C'2214'                                                          
         DC    C'2916'                                                          
         DC    C'2815'                                                          
         DC    C'2607'                                                          
         DC    C'2708'                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZE WORK AREA                                                *         
***********************************************************************         
         SPACE 1                                                                
INITWK   NTR1  ,                                                                
         XC    WRKR(WRKLNQ),WRKR                                                
         MVC   WRKOFF,SPACES                                                    
         MVC   WRKACC,SPACES                                                    
         MVC   WRKNME,SPACES                                                    
         LA    R1,WRKBK                                                         
         LA    R0,WRKBKN                                                        
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     XIT                                                              
*                                                                               
INITSUM  NTR1  ,                                                                
         XC    SUMR(SUMLNQ),SUMR                                                
         MVC   SUMOFF,SPACES                                                    
         MVC   SUMCON,SPACES                                                    
         XC    SUMMOA,SUMMOA                                                    
         LA    R1,SUMBK                                                         
         LA    R0,SUMBKN                                                        
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINARY TABLE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1  ,                                                                
         OI    BINFLG,BINFNEW      SET NEW ITEM ADDED                           
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   PARM+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,PARM,(1,(R3)),(R2)                                       
         OC    PARM(4),PARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,PARM+8        UPDATE COUNT                                 
         CLI   PARM,1                                                           
         BE    XIT                 NOT FOUND - ADDED                            
         NI    BINFLG,ALL-BINFNEW  TURNOFF NEW ITEM ADDED                       
         L     R4,PARM             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         IC    R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         SR    R7,R7                                                            
         IC    R7,BINNUMB          NUMBER OF BUCKETS                            
         LTR   R7,R7                                                            
         BZ    XIT                                                              
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R7,*-14                                                          
         B     XIT                                                              
*                                                                               
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R4)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R7,BINBIN                                                        
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT WRK RECORD FROM TABLE                                     *          
***********************************************************************         
         SPACE 1                                                                
GETWRK   NTR1  ,                                                                
         L     R5,ADWRK                                                         
         USING BIND,R5                                                          
         ICM   R0,15,BININ         NUMBER IN TABLE                              
         BZ    GETWRKN                                                          
GETWRK3  ICM   R1,15,NXTWRK        NEXT IN TABLE                                
         AH    R1,=H'1'                                                         
         STCM  R1,15,NXTWRK                                                     
         CR    R1,R0               TEST PAST END                                
         BH    GETWRKN                                                          
         BCTR  R1,0                                                             
         MH    R1,=Y(WRKLNQ)                                                    
         LA    RF,BINTABLE(R1)                                                  
         ST    RF,ADRWRK           SAVE ADDRESS OF THIS ONE                     
         MVC   WRKR(WRKLNQ),0(RF)  MOVE RECORD TO WORK AREA                     
         TM    BINFLG,BINFTOT      PASS TOTAL REOCRDS                           
         BO    GETWRKY                                                          
         CLI   WRKOFF,X'FF'        DON'T RETURN TOTAL RECORDS                   
         BE    GETWRK3                                                          
         CLI   WRKTYP,X'FF'                                                     
         BE    GETWRK3                                                          
GETWRKY  CR    RB,RB                                                            
         B     XIT                                                              
GETWRKN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT SUM RECORD FROM TABLE                                     *          
***********************************************************************         
         SPACE 1                                                                
GETSUM   NTR1  ,                                                                
         L     R5,ADSUM                                                         
         USING BIND,R5                                                          
         ICM   R0,15,BININ         NUMBER IN TABLE                              
         BZ    GETSUMN                                                          
GETSUM3  ICM   R1,15,NXTSUM        NEXT IN TABLE                                
         AH    R1,=H'1'                                                         
         STCM  R1,15,NXTSUM                                                     
         CR    R1,R0               TEST PAST END                                
         BH    GETSUMN                                                          
         BCTR  R1,0                                                             
         MH    R1,=Y(SUMLNQ)                                                    
         LA    RF,BINTABLE(R1)                                                  
         ST    RF,ADRSUM           SAVE ADDRESS OF THIS ONE                     
         MVC   SUMR(SUMLNQ),0(RF)  MOVE RECORD TO WORK AREA                     
GETSUMY  CR    RB,RB                                                            
         B     XIT                                                              
GETSUMN  LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CREATE OUTPUT FILE                                                 *          
***********************************************************************         
         SPACE 1                                                                
FOUT     NTR1  ,                                                                
         L     R5,ADCNV                                                         
         USING BIND,R5                                                          
         ICM   R4,15,BININ         NUMBER IN TABLE                              
         BZ    XIT                                                              
         LA    R3,BINTABLE                                                      
         L     R5,ADCNVF                                                        
FOUT3    PUT   (R5),(R3)                                                        
         LA    R3,CNVLNQ(R3)                                                    
         BCT   R4,FOUT3                                                         
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     NTR1  ,                                                                
         GOTO1 ADMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                                
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     XIT                                                              
*                                                                               
READ     NTR1  ,                                                                
         GOTO1 ADMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                                
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     XIT                                                              
*                                                                               
SEQ      NTR1  ,                                                                
         GOTO1 ADMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                                
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     XIT                                                              
*                                                                               
WRT      NTR1  ,                                                                
         GOTO1 ADMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                  
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
GET      NTR1  ,                                                                
         GOTO1 ADMGR,DMCB,GETREC,ACCMST,DA,(R2),DMWORK                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
PUT      NTR1  ,                                                                
         GOTO1 ADMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                          
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QOPT2,C'D'                                                       
         BNE   XIT                                                              
         LA    R3,PUTREC                                                        
         SR    R8,R8                                                            
         ICM   R8,3,ACTRLEN-ACTKEY(R2)                                          
         GOTO1 PRNTBL,DMCB,(6,(R3)),(R2),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
         B     XIT                                                              
*                                                                               
ADD      NTR1  ,                                                                
         GOTO1 ADMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                          
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   QOPT2,C'D'                                                       
         BNE   XIT                                                              
         LA    R3,ADDREC                                                        
         SR    R8,R8                                                            
         ICM   R8,3,ACTRLEN-ACTKEY(R2)                                          
         GOTO1 PRNTBL,DMCB,(6,(R3)),(R2),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
*                                                                               
HELLO    DC    V(HELLO)            HELLO                                        
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
VDMGR    DC    V(DATAMGR)          DATAMGR FOR TEST FILE                        
*                                                                               
ADSUM    DC    A(SUMTAB)                                                        
ADWRK    DC    A(WRKTAB)                                                        
ADCNV    DC    A(CNVTAB)                                                        
AGLPTAB  DC    A(GLPTAB)           G/L POINTERS                                 
ADCNVF   DC    A(CNVFILE)          CONVERSION FILE                              
*                                                                               
AIO1     DC    A(IO1)              IO AREA #1                                   
AIO2     DC    A(IO2)              IO AREA #2                                   
AIO3     DC    A(IO3)              IO AREA #3                                   
*                                                                               
ACCLIST  DC    A(0)                                                             
OFFLIST  DC    A(0)                                                             
*                                                                               
ANYACC   DC    A(NYACC)            NY ACCOUNT LIST                              
ANYOFF   DC    A(NYOFF)            NY OFFICE LIST                               
*                                                                               
ACHACC   DC    A(CHACC)            CH ACCOUNT LIST                              
ACHOFF   DC    A(CHOFF)            CH OFFICE LIST                               
*                                                                               
AATACC   DC    A(ATACC)            AT ACCOUNT LIST                              
AATOFF   DC    A(ATOFF)            AT OFFICE LIST                               
*                                                                               
REQNUM   DC    PL3'0'              REQUEST NUMBER                               
EFFS     DC    48X'FF'                                                          
ALL      EQU   X'FF'                                                            
ALLOFF   DC    X'FEFE'                                                          
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
OPEN     DC    CL8'OPEN    '                                                    
*                                                                               
EDMSK    DC    X'40202020202020202020202020214B202060'                          
*                                                                               
ACCOUNT  DC    C'ACCOUNT'                                                       
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
*                                                                               
REPTS    DS    XL1                 REPORT CONTROL                               
REPTSGL  EQU   X'80'                                                            
REPTSGLA EQU   X'40'                                                            
REPTSRC  EQU   X'20'                                                            
REPTSSB  EQU   X'10'                                                            
*                                                                               
REPSUB   DS    0XL25                                                            
         DC    AL1(WRKSGL),AL1(REPTSGL),CL23'   ** GENERAL LEDGER **'           
         DC    AL1(WRKSGLA),AL1(REPTSGLA),CL23'   ** ALL OFFICES  **'           
         DC    AL1(WRKSRC),AL1(REPTSRC),CL23'       ** SOURCE **    '           
         DC    AL1(WRKSSB),AL1(REPTSSB),CL23'      ** SUB LEDGER ** '           
         DC    X'FF'                                                            
*                                                                               
         ENTRY UTL                                                              
UTL      DC    F'0',X'E6'                                                       
         ORG   UTL+4                                                            
SE       DS    X                                                                
*                                                                               
PRODUL   DC    C'SJ'                                                            
PERSUL   DC    C'1R'                                                            
COSTUL   DC    C'1C'                                                            
NONCUL   DC    C'1N'                                                            
INCMUL   DC    C'SI'                                                            
SUSPUL   DC    C'SK'                                                            
ANALUL   DC    C'12'                                                            
PCNTUL   DC    C'1J'                                                            
*                                                                               
RUNTOT   DS    0CL30                                                            
CHAREC   DC    PL4'0',CL26'RECORDS CHANGED'                                     
NEWREC   DC    PL4'0',CL26'RECORDS ADDED'                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC                                                         
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         MVI   BOXROWS+06,C'T'                                                  
         MVI   BOXROWS+09,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+(PLCL-PLD),C'L'                                          
         MVI   BOXCOLS+(PLC1-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC2-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC3-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC4-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC5-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC6-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC7-PLD),C'C'                                          
         MVI   BOXCOLS+(PLCR-PLD),C'R'                                          
         B     BX200                                                            
*                                                                               
BX200    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DC    C'*GLPTP**'          G/L POINTERS                                
GLPTAB   DC    (GLPTMAX*GLPTLNQ)X'00'                                           
GLPTMAX  EQU   15000                                                            
*                                                                               
         DC    C'**IO1***'                                                      
         DC    F'0'                      IOAREA #1                              
IO1      DC    (MAXRLNQ)X'00'                                                   
MAXRLNQ  EQU   1990                INSTEAD OF 2000 TO BE SAFE                   
*                                                                               
         SPACE 1                                                                
         DC    C'**IO2***'                                                      
         DC    F'0'                      IOAREA #2                              
IO2      DC    (MAXRLNQ)X'00'                                                   
*                                                                               
         SPACE 1                                                                
         DC    C'**IO3***'                                                      
         DC    F'0'                      IOAREA #3                              
IO3      DC    (MAXRLNQ)X'00'                                                   
         SPACE 1                                                                
MXSUM    EQU   5000                                                             
         DS    0D                                                               
         DC    CL8'**SUMTAB*'                                                   
SUMTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(SUMLNQ)         RECORD LENGTH                                
         DC    AL4(L'SUMKEY)       DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXSUM)          MAX. IN TABLE                                
         DC    AL1(SUMBKN)         NUMBER OF BUCKETS                            
         DC    AL1(SUMBK-SUMR)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXSUM*SUMLNQ)C     TABLE                                        
         SPACE 1                                                                
MXWRK    EQU   1000                                                             
         DS    0D                                                               
         DC    CL8'**WRKTAB*'                                                   
WRKTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(WRKLNQ)         RECORD LENGTH                                
         DC    AL4(L'WRKKEY)       DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXWRK)          MAX. IN TABLE                                
         DC    AL1(WRKBKN)         NUMBER OF BUCKETS                            
         DC    AL1(WRKBK-WRKR)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXWRK*WRKLNQ)C     TABLE                                        
         SPACE 1                                                                
MXCNV    EQU   1000                                                             
         DS    0D                                                               
         DC    CL8'**CNVTAB*'                                                   
CNVTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(CNVLNQ)         RECORD LENGTH                                
         DC    AL4(L'CNVKEY)       DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXCNV)          MAX. IN TABLE                                
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXCNV*CNVLNQ)C     TABLE                                        
*                                                                               
CNVFILE  DCB   DDNAME=CNVFILE,DSORG=PS,MACRF=(PM),                     X        
               RECFM=FB,LRECL=62,BLKSIZE=6200                                   
         EJECT                                                                  
         ENTRY SSB                                                              
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOSTAT2                                                         
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
***********************************************************************         
* ACCOUNT - OFFICE TABLES                                             *         
***********************************************************************         
*                                                                               
* ACREPBD03                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPBD03                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
*                                                                               
LWKSD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
ADMGR    DS    A                   DATAMGR(MONACC)/VDATAMGR(INCLUDED)           
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
SVDIR    DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
SVKEY    DS    CL(L'ACCKEY)        KEY SAVE AREA                                
*                                                                               
CMPNAME  DS    CL36                COMPANY NAME                                 
ACCODE   DS    CL15                ACCOUNT CODE                                 
ACNAME   DS    CL36                ACCOUNT NAME                                 
*                                                                               
NEWCON   DS    CL14                NEW CONTRA                                   
NEWOFF   DS    CL2                 NEW OFFICE                                   
CONNME   DS    CL36                OLD CONTRA NAME                              
OLDNME   DS    CL36                OLD ACCOUNT NAME                             
SBAC     DS    CL14                SUB ACC                                      
*                                                                               
PARM     DS    6F                                                               
BINFLG   DS    XL1                                                              
BINFNEW  EQU   X'80'               NEW ITEM ADDED                               
BINFTOT  EQU   X'40'               RETURN TOTAL RECORDS                         
*                                                                               
LEVELS   DS    0H                                                               
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
*                                                                               
CTOTS    DS    0PL8                CONTROL TOTALS                               
GNEW     DS    PL8                 UNIT G - NEW TOTAL                           
GOLD     DS    PL8                 UNIT G - OLD TOTAL                           
SOLD     DS    PL8                 UNIT S - OLD TOTAL                           
CTOTN    EQU   (*-CTOTS)/8                                                      
*                                                                               
DUB1     DS    D                                                                
ELEMENT  DS    XL255                                                            
STATUS   DS    XL1                 ACCOUNT STATUS                               
STBBF    EQU   X'80'               BALANCE BROUGHT FORWARD                      
STOCA    EQU   X'40'               OLD CONTRA ACCOUNTS                          
STMIA    EQU   X'20'               MULTI INPUT ACCOUNTS                         
*                                                                               
OFFCNT   DS    X                   NUMBER OF OFFICE LINES PRINTED               
*                                                                               
NXTWRK   DS    F                   NUMBER OF NEXT WORK RECORD                   
ADRWRK   DS    F                   ADDRESS OF THIS WORK RECORD                  
NXTSUM   DS    F                   NUMBER OF NEXT WORK RECORD                   
ADRSUM   DS    F                   ADDRESS OF THIS WORK RECORD                  
OFFN     DS    XL1                 NUMBER OF OFFICES                            
OFFL     DS    20CL2               OFFICE LIST                                  
*                                                                               
OFFICE   DS    CL2                 CURRENT OFFICE                               
SUBACC   DS    CL14                SUBACCOUNT                                   
SUBNME   DS    CL36                SUBACCOUNT NAME                              
*                                                                               
SUBLST   DS    CL(MXSBL*14)                                                     
MXSBL    EQU   200                                                              
         EJECT                                                                  
*              WORK RECORD FOR REPORT                                           
*                                                                               
WRKR     DS    0C                                                               
WRKKEY   DS    0CL18                                                            
WRKSEC   DS    XL1                 REPORT SECTION                               
WRKSGL   EQU   X'10'               G/L SECTION                                  
WRKSGLA  EQU   X'11'               G/L SECTION ALL OFFICES                      
WRKSRC   EQU   X'20'               SOURCE SECTION                               
WRKSSB   EQU   X'40'               SUB ACCOUNT                                  
WRKOFF   DS    CL2                 OFFICE CODE                                  
WRKTYP   DS    XL1                                                              
WRKTOLD  EQU   1                   OLD (SOURCE) DATA                            
WRKTNEW  EQU   2                   NEW ACCOUNT DATA                             
WRKACC   DS    CL14                ACCOUNT CODE                                 
*                                                                               
WRKDATA  DS    0CL36                                                            
WRKNME   DS    CL36                NAME                                         
WRKSTAT  DS    XL1                 STATUS                                       
WRKSADJ  EQU   X'80'               ADJUSTED                                     
WRKSGLP  EQU   X'20'               ACCOUNT HAS G/L POINTER                      
*                                                                               
WRKBK    DS    0XL(5*8)                                                         
WRKBBF   DS    PL8                 BALANCE FORWARD                              
WRKACT   DS    PL8                 ACTIVITY                                     
WRKADJ   DS    PL8                 ADJUSTMENTS                                  
WRKNEW   DS    PL8                 BALANCE                                      
WRKOLD   DS    PL8                 OLD BALANCE                                  
WRKBKN   EQU   (*-WRKBK)/8                                                      
WRKLNQ   EQU   *-WRKR                                                           
*                                                                               
TOTWRK   DS    XL(WRKLNQ)          WORK AREA FOR TOTAL                          
SVWRK    DS    XL(L'WRKKEY)        SAVE KEY                                     
         EJECT                                                                  
*              MONTH SUMMARY RECORD                                             
*                                                                               
SUMR     DS    0C                                                               
SUMKEY   DS    0CL18                                                            
SUMOFF   DS    CL2                 OFFICE                                       
SUMCON   DS    CL14                CONTRA ACCOUNT                               
SUMMOA   DS    CL2                 MOA YEAR/MONTH                               
*                                                                               
SUMBK    DS    0XL(2*8)                                                         
SUMDR    DS    PL8                 DEBITS                                       
SUMCR    DS    PL8                 CREDITS                                      
SUMBKN   EQU   (*-SUMBK)/8                                                      
SUMLNQ   EQU   *-SUMR                                                           
*                                                                               
TOTSUM   DS    XL(SUMLNQ)          WORK AREA FOR TOTAL                          
         EJECT                                                                  
*              CONVERSION TABLE                                                 
*                                                                               
CNVR     DS    0C                                                               
CNVKEY   DS    0CL(CNVKLNQ)                                                     
CNVBGN   DS    0C                                                               
CNVNAC   DS    CL14                NEW ACCOUNT                                  
         DS    CL1                                                              
CNVNOF   DS    CL2                 NEW OFFICE                                   
         DS    CL1                                                              
CNVNCA   DS    CL14                NEW CONTRA                                   
         DS    CL1                                                              
CNVOCA   DS    CL14                OLD CONTRA                                   
         DS    CL1                                                              
CNVOAC   DS    CL14                OLD ACCOUNT                                  
CNVKLNQ  EQU   *-CNVBGN                                                         
*                                                                               
CNVBK    DS    0X                                                               
CNVLNQ   EQU   *-CNVR                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GENERAL LEDGER POINTER TABLE                         *         
***********************************************************************         
         SPACE 1                                                                
GLPTD    DSECT                                                                  
GLPTTO   DS    CL14                TO ACCOUNT                                   
GLPTFR   DS    CL14                FROM ACCOUNT                                 
GLPTLNQ  EQU   *-GLPTD                                                          
         SPACE 2                                                                
***********************************************************************         
* DSECT TO BINSRCH CONTROL TABLE                                      *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER REPORT PRINT LINE                                    *         
***********************************************************************         
         SPACE 1                                                                
PLD      DSECT                                                                  
PLCL     DS    CL1                                                              
PLOFF    DS    CL6                 OFFICE                                       
PLC1     DS    CL1                                                              
PLACC    DS    CL14                CONTRA ACCOUNT                               
PLC2     DS    CL1                                                              
PLNME    DS    CL36                NAME                                         
PLC3     DS    CL1                                                              
PLBBF    DS    CL17                BALANCE FORWARD                              
PLC4     DS    CL1                                                              
PLACT    DS    CL17                ACTIVITY                                     
PLC5     DS    CL1                                                              
PLADJ    DS    CL17                ADJUSTMENT                                   
PLC6     DS    CL1                                                              
PLNEW    DS    CL17                BALANCE                                      
PLC7     DS    CL1                                                              
PLOLD    DS    CL17                OLD BALANCE                                  
PLCR     DS    CL1                                                              
         ORG   PLD+164                                                          
         EJECT                                                                  
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
**PAN#1  DC    CL21'010ACREPBD02 08/16/00'                                      
         END                                                                    
