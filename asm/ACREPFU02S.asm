*          DATA SET ACREPFU02S AT LEVEL 012 AS OF 08/17/00                      
*PHASE ACFU02A                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'ACCOUNT FILE UPLOAD PROGRAM'                                    
ACFU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACFU**,R9       BASE REGISTERS 11, 9                         
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
         NI    OPTN,ALL-OPTTAPE                                                 
         CLI   QOPT1,C'D'          DUMP OUTPUT                                  
         BNE   *+8                                                              
         OI    OPTN,OPTDUMP                                                     
         CLI   QOPT3,C'S'          SKIP ACCOUNTS ALREADY ON FILE                
         BNE   *+8                                                              
         OI    OPTN,OPTSKIP                                                     
         CLI   QOPT2,C'T'          TAPE OUTPUT                                  
         BNE   REQF1                                                            
         TM    OPTN,OPTTAPE        TAPE ALREADY OPENED                          
         BO    REQF1                                                            
         OI    OPTN,OPTTAPE                                                     
         L     R3,AOUTFIL                                                       
         OPEN  ((R3),(OUTPUT))                                                  
                                                                                
REQF1    L     R2,AINFIL           SET FILE DCB                                 
*        USING IHADCB,R2                                                        
         OPEN  ((R2),(INPUT))      OPEN INPUT FILE                              
         LA    R1,INFLNQ           RECORD LENGTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         LA    R1,INFKEY-INFD+1      DISP. TO KEY                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(3),DUB+6(2)                                          
         LA    R1,INFKLNQ           LENGTH OF KEY                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+17(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
         LA    R5,INREC                                                         
         USING INFD,R5                                                          
                                                                                
REQF3    GET   (R2),(R5)                                                        
         OC    INFD(150),XSPACES                                                
         OC    INFD+150(150),XSPACES                                            
         CLC   QUNIT,SPACES        FILTER UNIT/LEDGER                           
         BE    REQF5                                                            
         CLC   INFUL,QUNIT                                                      
         BNE   REQF3                                                            
                                                                                
REQF5    GOTO1 ADSORTER,DMCB,=C'PUT',(R5)                                       
         B     REQF3                                                            
                                                                                
REQF7    CLOSE ((R2))                                                           
         L     R4,ADCMPNAM                                                      
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
                                                                                
REQF9    GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   RE,15,DMCB+4        GET SORTED JOBS                              
         BZ    REQF11              ALL DONE                                     
         AP    CNTIN,=P'1'                                                      
         LA    R0,INREC            SAVE CURRENT SORT RECORD                     
         LA    R1,INFLNQ                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,BLDR             BUILD AND ADD NEW RECORD                     
         B     REQF9                                                            
                                                                                
REQF11   MVI   FORCEHED,C'Y'                                                    
         LA    R2,CNTS                                                          
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         LA    R3,PLACC                                                         
                                                                                
REQF13   EDIT  (P5,0(R2)),(6,0(R3))                                             
         MVC   PLNME(20),5(R2)                                                  
         BAS   RE,ACRPT                                                         
         LA    R2,L'CNTS(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   REQF13                                                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RUNL     TM    OPTN,OPTTAPE        TEST OUTPUT TAPE OPENED                      
         BNO   XIT                                                              
         L     R3,AOUTFIL                                                       
         CLOSE ((R3))                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD NEW ACCOUNT RECORD                                            *         
***********************************************************************         
                                                                                
BLDR     NTR1  ,                                                                
         CLC   INFUL,SVUL          TEST SAME U/L                                
         BE    BLDR3                                                            
         BAS   RE,LDGR             GET NEW LEDGER LENGTHS                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   SVUL,INFUL                                                       
                                                                                
BLDR3    L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKULA,INFUL       ULA                                          
         MVC   ACTRLEN,=Y(ACTRFST-ACTRECD)                                      
         XC    ACTRSTA,ACTRSTA                                                  
         XC    ACTRFST(2),ACTRFST                                               
         MVI   ERRCNT,0            ERROR FLAG                                   
         BAS   RE,LEVL             GET LEVEL                                    
         BAS   RE,NAME             ADD NAME                                     
         BAS   RE,ADRS             ADD ADDRESS                                  
         BAS   RE,BALC             BALANCE/PEEL/DRAFT TRANSACTION               
         BAS   RE,STAT             STATUS ELEMENT                               
         BAS   RE,GENP             GENERAL LEDGER POSTING RULES                 
         BAS   RE,ANAL             ANALYSIS ACCOUNT                             
         BAS   RE,INCM             INCOME ACCOUNT                               
         BAS   RE,WOFF             INCOME ACCOUNT                               
         BAS   RE,DISC             DISCOUNT                                     
         BAS   RE,NUMB             NUMBER                                       
         BAS   RE,ONLM             ON-LINE MEMO                                 
         BAS   RE,HIRD             HIRED DATE                                   
         BAS   RE,PROD             PRODUCTION ELEMENTS                          
                                                                                
         CLI   ERRCNT,0            TEST ANY ERRORS                              
         BNE   BLDR7                                                            
         BAS   RE,ADDR             ADD IT                                       
         BNE   BLDR5               ERROR ON ADD                                 
         BAS   RE,REPT             PRINT IT                                     
         AP    CNTADD,=P'1'                                                     
         B     XIT                                                              
                                                                                
BLDR5    TM    OPTN,OPTSKIP        SKIP ACCOUNTS ALREADY ON FILE                
         BO    XIT                                                              
                                                                                
BLDR7    BAS   RE,REPT             PRINT IT                                     
         AP    CNTERR,=P'1'                                                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET LEDGER LENGTHS                                                  *         
***********************************************************************         
                                                                                
LDGR     NTR1  ,                                                                
         LA    R2,DKEY                                                          
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,RCCOMPFL    COMPANY                                      
         MVC   LDGKUNT(2),INFUL    UNIT LEDGER                                  
         BAS   RE,DMHGH                                                         
         CLC   DKEY(L'LDGKEY),DIR                                               
         BE    *+6                                                              
         DC    H'0'                NO LEDGER                                    
                                                                                
         L     R2,AIO                                                           
         BAS   RE,DMGETR           GET THE LEDGER RECORD                        
         LA    R4,LDGRFST                                                       
         SR    R0,R0                                                            
                                                                                
LDGR5    CLI   0(R4),ACLELQ        X'16' ELEMENT                                
         BE    LDGR9                                                            
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     LDGR5                                                            
                                                                                
         USING ACLELD,R4                                                        
LDGR9    MVC   LEVELA,ACLVLEN      LEVEL LENGTHS                                
         MVC   LEVELB,ACLVLEN+(L'ACLVALS)                                       
         MVC   LEVELC,ACLVLEN+(L'ACLVALS*2)                                     
         MVC   LEVELD,ACLVLEN+(L'ACLVALS*3)                                     
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET LEVEL FOR THIS ACCOUNT                                          *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
LEVL     NTR1  ,                                                                
         LA    R1,12               GET LENGTH OF ACCOUNT                        
         LA    RF,INFACC+L'INFACC-1                                             
                                                                                
LEVL3    CLI   0(RF),C' '                                                       
         BH    LEVL5                                                            
         BCTR  RF,0                                                             
         BCT   R1,LEVL3                                                         
         B     LEVL9                                                            
                                                                                
LEVL5    STC   R1,BYTE                                                          
         LA    RF,LEVELA           GET LEVEL FOR THIS ACCOUNT                   
         LA    R0,4                                                             
         CLC   0(1,RF),BYTE        LEDGER LENGTH TO ACCOUNT LENGTH              
         BNL   LEVL7                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,*-14                                                          
         B     LEVL9                                                            
                                                                                
LEVL7    MVC   LEVEL,0(RF)         CURRENT LEVEL LENGTH                         
         CH    R0,=H'4'            TEST ONE LEVEL LEDGER                        
         BE    XIT                                                              
         BCTR  RF,0                GET PREVIOUS LENGTH                          
         MVC   DKEY,SPACES                                                      
         SR    R1,R1                                                            
         IC    R1,0(RF)            LENGTH OF PREVIOUS                           
         LA    R1,2(R1)            PLUS CUL                                     
         LR    R3,R1               SAVE FOR LATER                               
         EX    R1,*+4                                                           
         MVC   DKEY(0),ACTKEY      KEY FOR PREVIOUS                             
         BAS   RE,DMHGH                                                         
         CLC   DKEY(L'ACTKEY),DIR                                               
         BE    XIT                                                              
         CLI   RCWRITE,C'N'        FOR WRITE=NO                                 
         BNE   LEVL8                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   DKEY(0),HKEY        TEST AGAINST LAST HIGH KEY ADDED             
         BE    XIT                                                              
                                                                                
LEVL8    MVI   ERRNUM,ERRNLVL      NO HIGH LEVEL                                
         B     *+8                                                              
                                                                                
LEVL9    MVI   ERRNUM,ERRNACC      NO ACCOUNT FIELD                             
         BAS   RE,ERROR            POST ERROR                                   
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD NAME ELEMENT                                                    *         
***********************************************************************         
                                                                                
NAME     NTR1  ,                                                                
         LA    R1,36               GET LENGTH OF NAME                           
         LA    RF,INFNAME+L'INFNAME-1                                           
                                                                                
NAME3    CLI   0(RF),C' '                                                       
         BH    NAME5                                                            
         BCTR  RF,0                                                             
         BCT   R1,NAME3                                                         
         MVI   ERRNUM,ERRNNME      NO NAME FIELD                                
         BAS   RE,ERROR            POST ERROR                                   
         B     XIT                                                              
                                                                                
NAME5    LA    R3,ELEMENT          BUILD NAME ELEMENT                           
         USING NAMELD,R3                                                        
         MVI   NAMEL,NAMELQ                                                     
         LA    R1,2(R1)                                                         
         STC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   NAMEREC(0),INFNAME                                               
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ADDRESS ELEMENT                                                 *         
***********************************************************************         
                                                                                
ADRS     NTR1  ,                                                                
         LA    R3,ELEMENT          BUILD ADDRESS ELEMENT                        
         USING ADRELD,R3                                                        
         MVI   ADREL,ADRELQ                                                     
         LA    RE,ADRADD1-ADRELD   MINIMUM LENGTH                               
         LA    R0,4                MAX OF 4 LINES                               
         SR    RF,RF               COUNT LINES                                  
         LA    R1,INFADD1                                                       
         LA    R5,ADRADD1                                                       
                                                                                
ADRS3    OC    0(L'INFADD1,R1),SPACES                                           
         CLC   0(L'INFADD1,R1),SPACES SKIP BLANK LINES                          
         BE    ADRS5                                                            
         MVC   0(L'ADRADD1,R5),0(R1)                                            
         LA    R5,L'ADRADD1(R5)                                                 
         AH    RE,=Y(L'ADRADD1)    ADJUST LENGTH                                
         LA    RF,1(RF)            COUNT LINES                                  
                                                                                
ADRS5    LA    R1,L'INFADD1(R1)                                                 
         BCT   R0,ADRS3                                                         
                                                                                
         LTR   RF,RF                                                            
         BZ    XIT                 NO ADDRESS                                   
         STC   RE,ADRLN            LENGTH                                       
         STC   RF,ADRNUM           NUMBER OF LINES                              
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ADD BALANCE/PEEL/DRAFT COUNT                                        *         
***********************************************************************         
                                                                                
BALC     CLI   LEVEL,12            TEST POSTING LEVEL                           
         BNER  RE                                                               
                                                                                
         USING ACTRECD,R2                                                       
BALC1    NTR1  ,                                                                
         OI    ACTRSTAT,ACTSABLP                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING ASTELD,R3           ACCOUNT STATUS                               
         MVI   ASTEL,ASTELQ                                                     
         MVI   ASTLN,ASTLN1Q                                                    
         BAS   RE,ADDL                                                          
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         USING ABLELD,R3           BALANCE ELEMENT                              
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN1Q                                                    
         ZAP   ABLFRWD,=P'0'                                                    
         ZAP   ABLDR,=P'0'                                                      
         ZAP   ABLCR,=P'0'                                                      
         BAS   RE,ADDL                                                          
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         USING APOELD,R3           PEEL ELEMENT                                 
         MVI   APOEL,APOELQ                                                     
         MVI   APOLN,APOLN2Q                                                    
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ADD RECORD STATUS ELEMENT                                           *         
***********************************************************************         
                                                                                
STAT     NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING RSTELD,R3           ACCOUNT STATUS                               
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVC   RSTBDATE,TODAYP                                                  
         MVC   RSTTDATE,TODAYP                                                  
         MVC   RSTCOSTG,SPACES                                                  
         MVC   RSTFILT1,INFILT1                                                 
         MVC   RSTFILT2,INFILT2                                                 
         MVC   RSTFILT3,INFILT3                                                 
         MVC   RSTFILT4,INFILT4                                                 
         MVC   RSTFILT5,INFILT5                                                 
                                                                                
         USING ACTRECD,R2                                                       
         MVC   ACTRSAF1,INFILT1                                                 
         MVC   ACTRSAF2,INFILT2                                                 
         MVC   ACTRSAF3,INFILT3                                                 
         MVC   ACTRSAF4,INFILT4                                                 
         MVC   ACTRSAF5,INFILT5                                                 
                                                                                
         CLI   INFOFIL,C'Y'        OUTFILE=Y                                    
         BNE   *+8                                                              
         OI    RSTSTAT1,RSTSOFBR                                                
         CLI   INFDEPT,C'Y'        DEPT=Y                                       
         BNE   *+8                                                              
         OI    RSTSTAT1,RSTSEADD                                                
         CLI   INFSTAF,C'Y'        STAFF=Y                                      
         BNE   *+8                                                              
         OI    RSTSTAT1,RSTSGPEI                                                
         CLI   INFVND2,C'Y'        VEND2C=Y                                     
         BNE   *+8                                                              
         OI    RSTSTAT1,RSTSVB2C                                                
         CLI   INFNBIZ,C'Y'        NEWBIZ=Y                                     
         BNE   *+8                                                              
         OI    RSTSTAT5,RSTSNBIZ                                                
         CLI   INFBONO,C'Y'        PROBONO=Y                                    
         BNE   *+8                                                              
         OI    RSTSTAT5,RSTSBONO                                                
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GENERAL LEDGER POSTING ELEMENT                                      *         
***********************************************************************         
                                                                                
GENP     CLC   INFGEN(2),=C'GB'                                                 
         BE    GENP1                                                            
         CLC   INFGEN(2),=C'GP'                                                 
         BNER  RE                                                               
                                                                                
GENP1    NTR1  ,                                                                
         MVC   DKEY,SPACES         MAKE SURE ACCOUNT EXISTS                     
         MVC   DKEY(1),RCCOMPFL                                                 
         MVC   DKEY+1(14),INFGEN                                                
         LA    R0,12                                                            
         LA    R3,DKEY+14                                                       
                                                                                
GENP3    CLI   0(R3),C'*'          FIND TRAILING *                              
         BNE   *+12                                                             
         MVI   0(R3),C' '                                                       
         B     GENP5                                                            
         BCTR  R3,0                                                             
         BCT   R0,GENP3                                                         
                                                                                
GENP5    BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'ACTKEY),DIR                                               
         BNE   GENP13              ERROR                                        
         BAS   RE,DMGETR                                                        
         L     R2,AIO                                                           
         USING ACTRECD,R2                                                       
         LA    R3,ACTRFST                                                       
         USING ABLELD,R3                                                        
         SR    R0,R0                                                            
                                                                                
GENP7    CLI   0(R3),0                                                          
         BE    GENP13                                                           
         CLI   ABLEL,ABLELQ        TEST VALID FOR POSTING                       
         BE    GENP11                                                           
         IC    R0,ABLLN                                                         
         AR    R3,R0                                                            
         B     GENP7                                                            
                                                                                
GENP11   XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING GLPELD,R3                                                        
         MVI   GLPEL,GLPELQ                                                     
         MVI   GLPLN,GLPLN1Q       NO OPTIONAL SECOND ACC                       
         MVC   GLPSUB,SPACES                                                    
         MVC   GLPACC1,INFGEN      GENERAL LEDGER ACC                           
         BAS   RE,ADDL                                                          
         B     XIT                                                              
                                                                                
GENP13   MVI   ERRNUM,ERRNGENL     ERROR                                        
         BAS   RE,ERROR                                                         
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ANALYSIS ACCOUNT                                                    *         
***********************************************************************         
                                                                                
ANAL     CLC   INFANL,SPACES      ANALYSIS ACCOUNT                              
         BER   RE                                                               
                                                                                
ANAL1    NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING SPAELD,R3                                                        
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATANAL                                                 
         MVC   SPAAANAL,INFANL                                                  
                                                                                
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKUNT(2),=C'11'                                                
         CLC   INFUL,=C'SE'                                                     
         BNE   *+10                                                             
         MVC   ACTKUNT(2),=C'13'   FOR SE ANALYSIS IS 13                        
         MVC   ACTKACT,SPAAANAL                                                 
         BAS   RE,DMHGH            TEST 11X                                     
         CLC   DKEY(L'ACTKEY),DIR                                               
         BNE   ANAL9                                                            
         CLC   INFUL,=C'SE'                                                     
         BE    ANAL3                                                            
         MVC   ACTKUNT(2),=C'12'                                                
         BAS   RE,DMHGH            TEST 12X                                     
         CLC   DKEY(L'ACTKEY),DIR                                               
         BNE   ANAL9                                                            
ANAL3    BAS   RE,ADDL                                                          
         B     XIT                                                              
                                                                                
ANAL9    MVI   ERRNUM,ERRNANL      SET ERROR NUMBER                             
         BAS   RE,ERROR            POST ERROR                                   
         XIT                                                                    
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INCOME ACCOUNT                                                      *         
***********************************************************************         
                                                                                
INCM     CLC   INFINC,SPACES      INCOME ACCOUNT                                
         BER   RE                                                               
         CLC   INFUL,=C'1R'                                                     
         BNER  RE                                                               
                                                                                
INCM1    NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING SPAELD,R3                                                        
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATINCO                                                 
         MVC   SPAAULA,INFINC                                                   
         CLC   SPAAULA(2),=C'SI'                                                
         BNE   INCM9                                                            
                                                                                
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKULA,SPAAULA                                                  
         BAS   RE,DMHGH            GET THE RECORD                               
         CLC   DKEY(L'ACTKEY),DIR  TEST RECORD FOUND                            
         BNE   INCM9                                                            
         BAS   RE,ADDL                                                          
         B     XIT                                                              
                                                                                
INCM9    MVI   ERRNUM,ERRNINC      SET ERROR NUMBER                             
         BAS   RE,ERROR            POST ERROR                                   
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* WRITEOFF ACCOUNT                                                    *         
***********************************************************************         
                                                                                
WOFF     CLC   INFWOF,SPACES      WRITEOFF ACCOUNT                              
         BER   RE                                                               
                                                                                
WOFF1    NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING SPAELD,R3                                                        
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATWOFF                                                 
         MVC   SPAAULA,INFWOF                                                   
         CLC   SPAAULA(2),=C'SI'                                                
         BNE   WOFF9                                                            
                                                                                
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKULA,SPAAULA                                                  
         BAS   RE,DMHGH            GET THE RECORD                               
         CLC   DKEY(L'ACTKEY),DIR  TEST RECORD FOUND                            
         BNE   WOFF9                                                            
         BAS   RE,ADDL                                                          
         B     XIT                                                              
                                                                                
WOFF9    MVI   ERRNUM,ERRNWOF      SET ERROR NUMBER                             
         BAS   RE,ERROR            POST ERROR                                   
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DISCOUNT                                                            *         
***********************************************************************         
                                                                                
DISC     OC    INFDISC,CZ                                                       
         CLC   INFDISC,CZ                                                       
         BER   RE                                                               
                                                                                
DISC1    NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING RATELD,R3           DISCOUNT ELEMENT                             
         MVI   RATEL,RATEDSCQ                                                   
         MVI   RATLN,RATLNQ                                                     
         PACK  DUB,INFDISC                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,RATRATE                                                     
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* NUMBER  AND  ID                                                     *         
***********************************************************************         
                                                                                
NUMB     NTR1  ,                                                                
         LA    R3,ELEMENT                                                       
         OC    INFID,SPACES                                                     
         CLC   INFID,SPACES                                                     
         BE    NUM3                                                             
         XC    ELEMENT,ELEMENT                                                  
         USING OTHELD,R3           OTHER NUMBER ELEMENT                         
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM,SPACES                                                    
         MVC   OTHNUM(L'INFID),INFID                                            
         MVC   OTHPROF,=C'I   '                                                 
         BAS   RE,ADDL                                                          
                                                                                
NUM3     OC    INFNUM,SPACES                                                    
         CLC   INFNUM,SPACES                                                    
         BE    XIT                                                              
         XC    ELEMENT,ELEMENT                                                  
         USING OTHELD,R3           OTHER NUMBER ELEMENT                         
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM,SPACES                                                    
         MVC   OTHNUM(L'INFNUM),INFNUM                                          
         MVC   OTHPROF,SPACES                                                   
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ON-LINE MEMO                                                        *         
***********************************************************************         
                                                                                
ONLM     OC    INFONLM,SPACES      TEST ANY MEMO                                
         CLC   INFONLM,SPACES                                                   
         BER   RE                                                               
         CLC   INFUL,=C'SJ'        NOT FOR SJ                                   
         BER   RE                                                               
         CLC   INFUL,=C'1R'        OR 1R                                        
         BER   RE                                                               
ONLM1    NTR1  ,                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING OMEELD,R3           ONLINE MEMO                                  
         MVI   OMEEL,OMEELQ                                                     
         MVI   OMELN,OMELN1Q+L'INFONLM                                          
         MVC   OMEMO(L'INFONLM),INFONLM                                         
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* HIRED DATE (1R LEDGER)                                              *         
***********************************************************************         
                                                                                
HIRD     OC    INFHIRD,SPACES      TEST ANY DATE                                
         CLC   INFHIRD,SPACES                                                   
         BER   RE                                                               
         CLC   INFUL,=C'1R'        ONLY 1R                                      
         BNER  RE                                                               
HIRD1    NTR1  ,                                                                
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING EMPELD,R3           EMPLOYEE HISTORY                             
         MVI   EMPEL,EMPELQ                                                     
         MVI   EMPLN,EMPLNQ                                                     
         MVC   WORK(2),INFHIRD+2   YYMMDD TO MM/DD/YY                           
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),INFHIRD+4                                              
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),INFHIRD                                                
         GOTO1 DATVAL,DMCB,(0,WORK),WORK+8                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    HIRD3                                                            
         GOTO1 DATCON,DMCB,(0,WORK+8),(1,EMPHIR)                                
         BAS   RE,ADDL                                                          
         B     XIT                                                              
                                                                                
HIRD3    MVI   ERRNUM,ERRNHRD                                                   
         BAS   RE,ERROR                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRODUCTION ELEMENTS                                                 *         
***********************************************************************         
                                                                                
PROD     CLC   INFUL,=C'SJ'                                                     
         BNER  RE                                                               
PROD1    NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING PPRELD,R3           PRODUCTION PROFILE                           
         MVI   PPREL,PPRELQ                                                     
         MVI   PPRLN,PPRLN1Q+50                                                 
         MVC   PPRGRUP,SPACES                                                   
         MVC   PPRUWRK(12),SPACES                                               
         MVC   PPRBILLP,SPACES                                                  
         MVC   PPRNARRP(50),SPACES   FOR CBS JOB #                              
         CLI   LEVEL,3             TEST CLIENT                                  
         BNE   PROD5                                                            
         MVI   ERRNUM,ERRNOFC      MISSING OFFICE                               
         CLI   INFOFC,C' '                                                      
         BNH   *+12                                                             
         CLI   INFOFC+1,C' '                                                    
         BH    *+8                                                              
         BAS   RE,ERROR                                                         
         MVI   ERRNUM,ERRNREC      MISSING RECEIVABLE ACCOUNT                   
         CLC   INFRECV,SPACES                                                   
         BH    *+8                                                              
         BAS   RE,ERROR                                                         
         MVI   ERRNUM,ERRNCST      MISSING COSTING ACCOUNT                      
         CLC   INFCOST,SPACES                                                   
         BH    *+8                                                              
         BAS   RE,ERROR                                                         
         B     PROD7                                                            
                                                                                
PROD5    CLI   LEVEL,12            LOWEST LEVEL ONLY                            
         BNE   PROD7                                                            
         MVI   ERRNUM,ERRNCBSJ     MISSING CBS JOB #                            
         CLC   INFCBSJB,SPACES                                                  
         BH    *+8                                                              
         BAS   RE,ERROR                                                         
                                                                                
PROD7    MVC   PPRGRUP,INFBGRP     BILL GROUP                                   
         OC    PPRGRUP,SPACES                                                   
         MVC   PPRGAOFF,INFOFC     OFFICE                                       
         OC    PPRGAOFF,SPACES                                                  
         USING ACTRECD,R2                                                       
         MVC   ACTRSOFF,PPRGAOFF                                                
         MVC   PPRNARRP(L'INFCBSJB),INFCBSJB                                    
                                                                                
         OC    INFRECV,SPACES      RECEIVABLE                                   
         CLC   INFRECV,SPACES                                                   
         BE    PROD9                                                            
         MVC   PPRRECVC,RCCOMPFL                                                
         MVC   PPRRECVU(2),=C'SR'                                               
         MVC   PPRRECVA,INFRECV                                                 
         MVI   ERRNUM,ERRNREC      MISSING RECEIVABLE ACCOUNT                   
         LA    R2,DKEY                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,PPRRECVC                                                
         BAS   RE,DMHGH            GET THE RECORD                               
         CLC   DKEY(L'ACTKEY),DIR  TEST RECORD FOUND                            
         BE    *+8                                                              
         BAS   RE,ERROR                                                         
                                                                                
PROD9    OC    INFCOST,SPACES      COSTING                                      
         CLC   INFCOST,SPACES                                                   
         BE    PROD11                                                           
         MVC   PPRCOSTC,RCCOMPFL                                                
         MVC   PPRCOSTU(2),=C'1C'                                               
         MVC   PPRCOSTA,INFCOST                                                 
         MVI   ERRNUM,ERRNCST      MISSING COST ACCOUNT                         
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,PPRCOSTC                                                
         BAS   RE,DMHGH            GET THE RECORD                               
         CLC   DKEY(L'ACTKEY),DIR  TEST RECORD FOUND                            
         BE    *+8                                                              
         BAS   RE,ERROR                                                         
                                                                                
PROD11   MVC   PPRBTYPE,INFBTYP                                                 
         CLI   INFBTYP,C' '                                                     
         BNE   *+8                                                              
         MVI   PPRBTYPE,C'C'       DEFAULT CLIENT                               
         LA    R1,BILLT                                                         
         CLC   PPRBTYPE,0(R1)                                                   
         BE    PROD13                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   *-18                                                             
         MVI   ERRNUM,ERRNBLT      MISSING BILL TYPE                            
         BAS   RE,ERROR                                                         
                                                                                
PROD13   CLI   PPRBTYPE,C'E'                                                    
         BE    *+12                                                             
         CLI   PPRBTYPE,C'S'                                                    
         BNE   PROD15                                                           
         OC    INFBAMT,CZ                                                       
         PACK  DUB,INFBAMT                                                      
         CVB   R1,DUB                                                           
         STCM  R1,15,PPRBLAMT                                                   
         OC    PPRBLAMT,PPRBLAMT                                                
         BNZ   PROD15                                                           
         MVI   ERRNUM,ERRNBLA                                                   
         BAS   RE,ERROR                                                         
         DROP  R2,R3                                                            
                                                                                
PROD15   BAS   RE,ADDL                                                          
         CLI   LEVEL,12            TEST JOB LEVEL                               
         BNE   PROD20                                                           
         XC    ELEMENT,ELEMENT                                                  
         USING JOBELD,R3           JOB ELEMENT                                  
         MVI   JOBEL,JOBELQ                                                     
         MVI   JOBLN,JOBLN3Q                                                    
         MVC   JOBADATE,TODAYP                                                  
         MVC   JOBCDATE,TODAYP                                                  
         MVC   WORK(2),INFCLSD+2   YYMMDD TO MM/DD/YY                           
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),INFCLSD+4                                              
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),INFCLSD                                                
         GOTO1 DATVAL,DMCB,(0,WORK),WORK+8                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+16                                                             
         MVI   ERRNUM,ERRNCLD                                                   
         BAS   RE,ERROR                                                         
         B     PROD19                                                           
         GOTO1 DATCON,DMCB,(0,WORK+8),(1,JOBCDATE)                              
PROD19   BAS   RE,ADDL                                                          
         DROP  R3                                                               
                                                                                
PROD20   CLI   LEVEL,3                                                          
         BNE   PROD21                                                           
         CLC   INFBIL,SPACES       PUT BIL INFO IN STANDARD COMMENT             
         BE    PROD21                                                           
         XC    ELEMENT,ELEMENT                                                  
         USING SCMELD,R3                                                        
         MVI   SCMEL,SCMELQ                                                     
         MVI   SCMLN,SCMLN1Q+L'INFBIL                                           
         MVI   SCMSEQ,1            COMMENT SEQUENCE NUMBER                      
         OI    SCMTYPE,SCMTPRBI    PRINT ON BILLS                               
         OI    SCMTYPE,SCMTPRAD    PRINT AFTER DATA                             
         MVC   SCMNARR(6),INFBIL                                                
         BAS   RE,ADDL                                                          
         DROP  R3,R5                                                            
                                                                                
PROD21   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD RECORD TO THE FILE                                              *         
***********************************************************************         
                                                                                
ADDR     NTR1  ,                                                                
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
         SR    R3,R3                                                            
         ICM   R3,3,ACTRLEN        SET LENGTH FOR TAPE                          
         LA    R3,4(R3)                                                         
         L     R5,AIO2                                                          
         SH    R5,=H'4'                                                         
         XC    0(4,R5),0(R5)                                                    
         STCM  R3,3,0(R5)                                                       
                                                                                
         TM    OPTN,OPTDUMP        DUMP OUTPUT                                  
         BNO   *+8                                                              
         BAS   RE,DUMP                                                          
         MVC   DKEY,ACTKEY                                                      
         BAS   RE,DMRD             READ FOR RECORD                              
         LA    R2,DIR                                                           
         SR    R0,R0               R0=0 RECORD NOT FOUND                        
         CLC   ACTKEY,DKEY         TEST RECORD EXISTS                           
         BNE   *+16                                                             
         LA    R0,1                                                             
         TM    ACTKSTAT,ACTSDELT   TEST DELETED                                 
         BNO   ADDR13              RECORD EXISTS                                
                                                                                
         TM    OPTN,OPTTAPE        TEST TAPE OUTPUT                             
         BNO   ADDR5                                                            
         L     R3,AOUTFIL                                                       
         PUT   (R3),(R5)                                                        
         B     ADDR9                                                            
                                                                                
ADDR5    LTR   R0,R0               TEST RECORD ON FILE                          
         BNZ   *+12                                                             
         BAS   RE,DMADDR           ADD TO FILE                                  
         B     ADDR9                                                            
         BAS   RE,DMGETR           GET OLD RECORD                               
         BAS   RE,DMPUT            REPLACE RECORD                               
                                                                                
ADDR9    L     R2,AIO2                                                          
         CLI   LEVEL,12            TEST POSTING ACCOUNT                         
         BE    *+10                                                             
         MVC   HKEY,ACTKEY         SAVE LAST HIGH LEVEL KEY                     
         SR    RE,RE               RETURN OK                                    
         B     XIT                                                              
                                                                                
ADDR13   MVI   ERRNUM,ERRNACCX     ACCOUNT EXISTS                               
         BAS   RE,ERROR                                                         
         LTR   RE,RE               RETURN ERROR                                 
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
                                                                                
REPT     NTR1  ,                                                                
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         ST    R6,AXPLN                                                         
         MVI   LNXPO,0                                                          
         MVI   OPTNUM,0            NUMBER OF OPTIONS FIELDS                     
         MVC   OPWK,SPACES                                                      
         L     R2,AIO2                                                          
         USING ACTRECD,R2                                                       
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
         EX    R1,*+4                                                           
         MVC   PLNME(0),NAMEREC                                                 
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT5    CLI   0(R3),ADRELQ        ADDRESS                                      
         BNE   REPT7                                                            
         USING ADRELD,R3                                                        
         LA    RF,PLADDR                                                        
         LA    RE,ADRADD1                                                       
         SR    R1,R1                                                            
         IC    R1,ADRNUM                                                        
         MVC   0(L'PLADDR,RF),0(RE)                                             
         LA    RF,L'XP(RF)                                                      
         LA    RE,L'ADRADD1(RE)                                                 
         BCT   R1,*-14                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT7    CLI   0(R3),ABLELQ        BALANCE                                      
         BNE   REPT9                                                            
         USING ABLELD,R3           BALANCE ELEMENT                              
         MVI   PLEVL,C'P'          POSTING LEVEL                                
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT9    CLI   0(R3),RSTELQ        STATUS                                       
         BNE   REPT11                                                           
         USING RSTELD,R3           ACCOUNT STATUS                               
         MVC   PLFLT1,RSTFILT1                                                  
         MVC   PLFLT2,RSTFILT2                                                  
         MVC   PLFLT3,RSTFILT3                                                  
         MVC   PLFLT4,RSTFILT4                                                  
         MVC   PLFLT5,RSTFILT5                                                  
         TM    RSTSTAT1,RSTSOFBR                                                
         BNO   *+14                                                             
         MVC   OPWK(9),=C'OUTFILE=Y'                                            
         BAS   RE,OPRT             ADD OPTIONS TO PRINT LINES                   
         TM    RSTSTAT1,RSTSEADD                                                
         BNO   *+14                                                             
         MVC   OPWK(6),=C'DEPT=Y'                                               
         BAS   RE,OPRT                                                          
         TM    RSTSTAT1,RSTSGPEI                                                
         BNO   *+14                                                             
         MVC   OPWK(7),=C'STAFF=Y'                                              
         BAS   RE,OPRT                                                          
         TM    RSTSTAT1,RSTSVB2C                                                
         BNO   *+14                                                             
         MVC   OPWK(8),=C'VEND2C=Y'                                             
         BAS   RE,OPRT                                                          
         TM    RSTSTAT5,RSTSBONO                                                
         BNO   *+14                                                             
         MVC   OPWK(9),=C'PROBONO=Y'                                            
         BAS   RE,OPRT                                                          
         TM    RSTSTAT5,RSTSNBIZ                                                
         BNO   *+14                                                             
         MVC   OPWK(8),=C'NEWBIZ=Y'                                             
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT11   CLI   0(R3),SPAELQ        SPECIAL ACCOUNTS                             
         BNE   REPT13                                                           
         USING SPAELD,R3                                                        
         MVC   OPWK,SPACES                                                      
         MVC   OPWK+4(14),SPAAULA                                               
         CLI   SPATYPE,SPATANAL                                                 
         BNE   *+10                                                             
         MVC   OPWK(4),=C'ANL='                                                 
         CLI   SPATYPE,SPATINCO                                                 
         BNE   *+10                                                             
         MVC   OPWK(4),=C'OIN='                                                 
         CLI   SPATYPE,SPATWOFF                                                 
         BNE   *+10                                                             
         MVC   OPWK(4),=C'OWO='                                                 
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT13   CLI   0(R3),RATEDSCQ      DISCOUNT                                     
         BNE   REPT15                                                           
         USING RATELD,R3                                                        
         MVC   OPWK(9),=C'DISCOUNT='                                            
         EDIT  (B2,RATRATE),(5,OPWK+9),2,ALIGN=LEFT                             
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
                                                                                
REPT15   CLI   0(R3),PPRELQ        PRODUCTION PROFILE                           
         BNE   REPT17                                                           
         USING PPRELD,R3                                                        
         MVC   PLOFC,PPRGAOFF      OFFICE                                       
         CLC   PPRGRUP,SPACES                                                   
         BNH   *+20                                                             
         MVC   OPWK(8),=C'BILLGRP='                                             
         MVC   OPWK+8(L'PPRGRUP),PPRGRUP                                        
         BAS   RE,OPRT                                                          
         CLC   PPRRECVU,SPACES                                                  
         BNH   *+20                                                             
         MVC   OPWK(5),=C'RECV='                                                
         MVC   OPWK+5(14),PPRRECVU                                              
         BAS   RE,OPRT                                                          
         CLC   PPRCOSTU,SPACES                                                  
         BNH   *+20                                                             
         MVC   OPWK(5),=C'COST='                                                
         MVC   OPWK+5(14),PPRCOSTU                                              
         BAS   RE,OPRT                                                          
         CLC   PPRBTYPE,SPACES                                                  
         BNH   *+20                                                             
         MVC   OPWK(9),=C'BILLTYPE='                                            
         MVC   OPWK+9(1),PPRBTYPE                                               
         BAS   RE,OPRT                                                          
         CLC   PPRNARRP(16),SPACES                                              
         BNH   *+20                                                             
         MVC   OPWK(6),=C'OTHER='                                               
         MVC   OPWK+6(16),PPRNARRP                                              
         BAS   RE,OPRT                                                          
         CLI   PPRBTYPE,C'E'                                                    
         BE    *+12                                                             
         CLI   PPRBTYPE,C'S'                                                    
         BNE   REPT31                                                           
         EDIT  (B4,PPRBLAMT),(8,OPWK),2,ALIGN=LEFT,ZERO=NOBLANK                 
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT17   CLI   0(R3),JOBELQ                                                     
         BNE   REPT19                                                           
         USING JOBELD,R3           JOB ELEMENT                                  
         MVC   OPWK(6),=C'CLOSE='                                               
         GOTO1 DATCON,DMCB,(1,JOBCDATE),(8,OPWK+6)                              
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT19   CLI   0(R3),PMDELQ        MEDIA(BILL NUMBER)                           
         BNE   REPT21                                                           
         USING PMDELD,R3                                                        
         MVC   OPWK(6),=C'FIRST='                                               
         MVC   OPWK+6(6),PMDFBILL                                               
         BAS   RE,OPRT                                                          
         MVC   OPWK(5),=C'LAST='                                                
         MVC   OPWK+5(6),PMDLBILL                                               
         BAS   RE,OPRT                                                          
         MVC   OPWK(6),=C'RESET='                                               
         MVC   OPWK+6(6),PMDRBILL                                               
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT21   CLI   0(R3),OTHELQ        NUMBER                                       
         BNE   REPT23                                                           
         USING OTHELD,R3           OTHER NUMBER ELEMENT                         
         MVC   OPWK(3),=C'ID='                                                  
         LA    RF,OPWK+3                                                        
         CLI   OTHPROF,C'I'                                                     
         BE    *+14                                                             
         MVC   OPWK(7),=C'NUMBER='                                              
         LA    RF,OPWK+7                                                        
         MVC   0(L'OTHNUM,RF),OTHNUM                                            
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT23   CLI   0(R3),GLPELQ        GENERAL LEDGER                               
         BNE   REPT24                                                           
         USING GLPELD,R3                                                        
         MVC   OPWK(8),=C'GENERAL='                                             
         MVC   OPWK+8(14),GLPACC1                                               
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
                                                                                
REPT24   CLI   0(R3),SCMELQ        STANDARD COMMENT                             
         BNE   REPT25                                                           
         USING SCMELD,R3                                                        
         MVC   OPWK(4),=C'BIL='                                                 
         MVC   OPWK+4(6),SCMNARR                                                
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT25   CLI   0(R3),OMEELQ        ONLINE MEMO                                  
         BNE   REPT26                                                           
         USING OMEELD,R3                                                        
         MVC   OPWK(5),=C'MEMO='                                                
         SR    R1,R1                                                            
         IC    R1,OMELN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   OPWK+5(0),OMEMO                                                  
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT26   CLI   0(R3),EMPELQ        EMPLOYEE HISTORY                             
         BNE   REPT27                                                           
         USING EMPELD,R3                                                        
         MVC   OPWK(5),=C'HIRE='                                                
         GOTO1 DATCON,DMCB,(1,EMPHIR),(8,OPWK+5)                                
         BAS   RE,OPRT                                                          
         B     REPT31                                                           
         DROP  R3                                                               
                                                                                
REPT27   DS    0H                                                               
REPT31   IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   REPT3                                                            
                                                                                
         CLI   ERRCNT,0                                                         
         BE    *+8                                                              
         BAS   RE,ERPRT            ERRORS TO PRINT                              
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
***********************************************************************         
* SET UP HEADLINES AND GO TO ACREPORT                                 *         
***********************************************************************         
                                                                                
ACRPT    NTR1  ,                                                                
         MVC   XHEAD1+75(L'CMPNAME),CMPNAME                                     
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD OPTIONS TO PRINT LINE                                           *         
***********************************************************************         
                                                                                
OPRT     NTR1  ,                                                                
         LA    R6,OPWK+L'OPWK-1    GET LENGTH OF THIS OPTION                    
         LA    R5,L'OPWK                                                        
OPRT3    CLI   0(R6),C' '                                                       
         BH    *+10                                                             
         BCTR  R6,0                                                             
         BCT   R5,OPRT3                                                         
                                                                                
OPRT5    L     R6,AXPLN            R6= ADDRESS OF THE PRINT LINE                
         LA    R6,PLOPT-PLD(R6)    PLUS DISPLACEMENT TO OPTIONS                 
         SR    R1,R1                                                            
         IC    R1,LNXPO            R1=LENGTH OF DATA ON THIS LINE               
         LA    R7,0(R1,R6)         R7=NEXT AVAILABLE SPACE                      
         LA    RF,1(R1,R5)         RF=LENGTH AFTER THIS IS ADDED                
         CH    RF,=Y(L'PLOPT)      TEST MAXIMUM LENGTH                          
         BNH   OPRT7                                                            
         L     R6,AXPLN                                                         
         LA    R6,L'XP(R6)         TO NEXT LINE                                 
         LA    RF,XPFOURTH                                                      
         CR    R6,RF                                                            
         BH    XIT                 MORE THAN 4 PRINT LINES                      
         ST    R6,AXPLN            SAVE ADDRESS ON NEW LINE                     
         MVI   LNXPO,0                                                          
         B     OPRT5                                                            
                                                                                
OPRT7    CLI   LNXPO,0             TEST FIRST ON THIS LINE                      
         BNE   *+10                                                             
         BCTR  RF,0                 REDUCE TOTAL LENGTH                         
         B     *+12                                                             
         MVI   0(R7),C','                                                       
         LA    R7,1(R7)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   0(0,R7),OPWK                                                     
         STC   RF,LNXPO                                                         
         MVC   OPWK,SPACES                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ERROR ITEM TO TABLE                                             *         
***********************************************************************         
                                                                                
ERROR    NTR1  ,                                                                
         SR    R1,R1                                                            
         IC    R1,ERRCNT           NUMBER SO FAR                                
         LA    RF,1(R1)                                                         
         STC   RF,ERRCNT           UPDATE COUNT                                 
         CH    RF,=Y(MXERR)                                                     
         BNH   *+6                                                              
         DC    H'0'                ERROR LIST IS FULL                           
         LA    R1,ERRLST(R1)                                                    
         MVC   0(1,R1),ERRNUM      SAVE ERROR NUMBER IN LIST                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ERROR MESSAGES TO PRINT LINE                                    *         
***********************************************************************         
                                                                                
ERPRT    NTR1  ,                                                                
         SR    R0,R0                                                            
         IC    R0,ERRCNT                                                        
         CH    R0,=H'4'            MAXIMUM 4 PER RECORD                         
         BNH   *+8                                                              
         LH    R0,=H'4'                                                         
         LA    R6,XP+(PLERR-PLD)                                                
         LA    R1,ERRLST                                                        
                                                                                
ERPRT3   SR    RE,RE                                                            
         IC    RE,0(R1)            ERROR NUMBER                                 
         BCTR  RE,0                                                             
         MH    RE,=Y(ERRLNQ)                                                    
         L     RF,AERRS                                                         
         AR    RF,RE                                                            
         MVC   0(L'PLERR,R6),0(RF)                                              
         LA    R6,L'XP(R6)                                                      
         LA    R1,1(R1)                                                         
         BCT   R0,ERPRT3                                                        
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
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
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
         GOTO1 DATAMGR,DMCB,DMWRTR,ACCDIR,DIR,DIR                               
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
* DUMP RECORDS                                                        *         
***********************************************************************         
                                                                                
DUMP     CP    DMPCNT,DMPMAX       TEST MAXIMUM DUMPS                           
         BHR   RE                                                               
         AP    DMPCNT,=P'1'                                                     
         NTR1  ,                                                                
         LA    R0,L'MSG1                                                        
         LA    R2,MSG1                                                          
         L     R3,AIO2                                                          
         SR    R4,R4                                                            
         ICM   R4,3,ACCRLEN-ACCRECD(R3)                                         
         TM    OPTN,OPTTAPE        OUTPUT TAPE                                  
         BNO   DUMP3                                                            
         SH    R3,=H'4'                                                         
         ICM   R4,3,0(R3)                                                       
                                                                                
DUMP3    LA    R5,=C'2D'                                                        
         GOTO1 ,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)            
         GOTO1 PRNTBL                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD AND ELEMENT TO THE RECORD                                       *         
***********************************************************************         
                                                                                
ADDL     LR    R0,RE                                                            
         L     R2,AIO2                                                          
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
                                                                                
MXRLNQ   EQU   2000                MAX RECORD SIZE                              
MXERR    EQU   50                  MAX ERROR TABLE ENTRIES                      
MXOPTN   EQU   20                  MAXIMUM OPTIONS                              
ERRLNQ   EQU   20                  SIZE OF ERROR ENTRY                          
                                                                                
DATVAL   DC    V(DATVAL)           DATVAL                                       
HELLO    DC    V(HELLO)            HELLO                                        
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
                                                                                
AIO      DC    A(IO)               IO AREA                                      
AIO2     DC    A(IO2)              IO AREA 2                                    
AERRS    DC    A(ERRS)             A(ERROR MESSAGES)                            
AINFIL   DC    A(INFIL)            A(INPUT FILE DCB)                            
AOUTFIL  DC    A(OUTFIL)           A(INPUT FILE DCB)                            
                                                                                
OPTN     DC    X'00'               RUN OPTIONS                                  
OPTTAPE  EQU   X'80'               TAPE IS OPEN                                 
OPTSKIP  EQU   X'40'               SKIP ACCOUNTS ALREADY ON FILE                
OPTDUMP  EQU   X'20'               DUMP OUTPUT RECORDS                          
ALL      EQU   X'FF'                                                            
                                                                                
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
                                                                                
RECCARD  DC    C'RECORD TYPE=F,LENGTH=999 '                                     
SORTCARD DC    C'SORT FIELDS=(999,999,A),FORMAT=BI,WORK=1 '                     
                                                                                
MSG1     DC    C'ACCOUNT RECORD'                                                
DMPCNT   DC    PL2'0'              DUMP COUNT - ERRORS                          
DMPMAX   DC    PL2'50'             MAX DUMP COUNT                               
                                                                                
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
         MVI   BOXCOLS+(PLC4-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC5-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC6-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC7-PLD),C'C'                                          
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
ERRNACC  EQU   1                                                                
         DC    CL(ERRLNQ)'NO ACCOUNT CODE'                                      
ERRNLVL  EQU   2                                                                
         DC    CL(ERRLNQ)'MISSING HIGH LEVEL'                                   
ERRNNME  EQU   3                                                                
         DC    CL(ERRLNQ)'MISSING NAME'                                         
ERRNANL  EQU   4                                                                
         DC    CL(ERRLNQ)'ANALYSIS ACCOUNT'                                     
ERRNINC  EQU   5                                                                
         DC    CL(ERRLNQ)'INCOME ACCOUNT'                                       
ERRNWOF  EQU   6                                                                
         DC    CL(ERRLNQ)'WRITEOFF ACCOUNT'                                     
ERRNOFC  EQU   7                                                                
         DC    CL(ERRLNQ)'OFFICE'                                               
ERRNREC  EQU   8                                                                
         DC    CL(ERRLNQ)'RECEIVABLE ACCOUNT'                                   
ERRNCST  EQU   9                                                                
         DC    CL(ERRLNQ)'COSTING ACCOUNT'                                      
ERRNBLT  EQU   10                                                               
         DC    CL(ERRLNQ)'BILL TYPE'                                            
ERRNBLA  EQU   11                                                               
         DC    CL(ERRLNQ)'BILL AMOUNT'                                          
ERRNCLD  EQU   12                                                               
         DC    CL(ERRLNQ)'CLOSE DATE'                                           
ERRNGENL EQU   13                                                               
         DC    CL(ERRLNQ)'NO GENERAL ACCOUNT'                                   
ERRNCBSJ EQU   14                                                               
         DC    CL(ERRLNQ)'NO CBS JOB NUMBER'                                    
ERRNACCX EQU   15                                                               
         DC    CL(ERRLNQ)'ACCOUNT EXISTS'                                       
ERRNHRD  EQU   16                                                               
         DC    CL(ERRLNQ)'HIRE DATE'                                            
         EJECT                                                                  
***********************************************************************         
* DCB'S                                                               *         
***********************************************************************         
                                                                                
INFIL    DCB   DDNAME=INFIL,DSORG=PS,MACRF=(GM),                       X        
               EODAD=REQF7                                                      
                                                                                
OUTFIL   DCB   DDNAME=OUTFIL,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
         EJECT                                                                  
***********************************************************************         
* DSECT FOR WORKING STORAGE                                           *         
***********************************************************************         
                                                                                
ACFUD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
SVRE     DS    F                   SAVE RE                                      
                                                                                
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
HKEY     DS    CL(L'ACCKEY)        LAST HIGH LEVEL ADDED                        
                                                                                
CMPNAME  DS    CL36                                                             
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
                                                                                
SVUL     DS    CL2                 CURRENT UNIT/LEDGER                          
LEVEL    DS    CL1                 LENGTH OF CURRENT LEVEL                      
LEVELA   DS    CL1                 LENGTH OF LEVEL A                            
LEVELB   DS    CL1                 LENGTH OF LEVEL B                            
LEVELC   DS    CL1                 LENGTH OF LEVEL C                            
LEVELD   DS    CL1                 LENGTH OF LEVEL D                            
                                                                                
ELEMENT  DS    XL255                                                            
ERRNUM   DS    X                   CURRENT ERROR NUMBER                         
ERRCNT   DS    X                   NUMBER OF ERRORS                             
ERRLST   DS    XL(MXERR)           LIST OF ERROR NUMBERS                        
                                                                                
OPTNUM   DS    X                   NUMNER OF OPTION FIELDS                      
AXPLN    DS    A                   A(OF THE CURRENT LINE FOR OPTIONS)           
LNXPO    DS    X                   LENGTH OF DATA ON THIS LINE                  
OPWK     DS    CL32                OPTIONS WORK AREA                            
                                                                                
INREC    DS    CL500               INPUT RECORD                                 
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER THE INPUT FILE RECORD                                *         
***********************************************************************         
                                                                                
INFD     DSECT                                                                  
INFKEY   DS    0C                                                               
INFUL    DS    CL2         UNIT/LEDGER          REQUIRED                        
INFACC   DS    CL12        ACCOUNT              REQUIRED                        
INFKLNQ  EQU   *-INFD                                                           
INFNAME  DS    CL36        ACCOUNT NAME         REQUIRED                        
INFADD1  DS    CL26        ADDRESS LINE 1       OPTIONAL                        
INFADD2  DS    CL26                     2       O                               
INFADD3  DS    CL26                     3       O                               
INFADD4  DS    CL26                     4       O                               
INFILT1  DS    CL1         FILTER 1             O                               
INFILT2  DS    CL1         FILTER 2             O                               
INFILT3  DS    CL1         FILTER 3             O                               
INFILT4  DS    CL1         FILTER 4             O                               
INFILT5  DS    CL1         FILTER 5             O                               
INFOFIL  DS    CL1         OUTFILE              Y OR BLANK                      
INFDEPT  DS    CL1         DEPT                 Y OR BLANK                      
INFSTAF  DS    CL1         STAFF                Y OR BLANK                      
INFVND2  DS    CL1         VEND2C               Y OR BLANK                      
INFANL   DS    CL12        ANALYSIS             O                               
INFDISC  DS    CL4         DISCOUNT(NNNN) 2DP   O                               
INFID    DS    CL9         ID                   O                               
         DS    CL2         N/D                                                  
INFBIL   DS    CL6         BILL NUMBER          O SJ ONLY                       
INFNBIZ  DS    CL1         NEW BIZ              Y OR BLANK                      
INFBONO  DS    CL1         PRO BONO             Y OR BLANK                      
INFINC   DS    0CL14       INCOME ACCOUNT       O                               
INFGEN   DS    CL14        GENERAL LEDGERS      O  NON-1R ACCOUNTS              
INFWOF   DS    CL14        WRITEOFF ACCOUNT     O                               
INFNUM   DS    CL9         NUMBER(SSN)          O                               
         DS    CL2         N/D                                                  
INFBGRP  DS    CL3         BILLING GROUP        R SJ ONLY                       
INFOFC   DS    CL2         OFFICE               R SJ ONLY                       
INFRECV  DS    CL12        RECEIVABLE ACCOUNT   R SJ ONLY                       
INFCOST  DS    CL12        COSTING ACCOUNT      R SJ ONLY)                      
INFCLSD  DS    CL6         CLOSE DATE - YYMMDD  R JOBS ONLY                     
INFBTYP  DS    CL1         BILLING TYPE         R SJ ONLY                       
INFBAMT  DS    CL8         BILL AMOUNT (S & E)                                  
         DS    CL3                                                              
INFCBSJB DS    CL16        CBS JOB#             R SJ ONLY, JOB LEVEL            
         ORG   INFCBSJB                                                         
INFONLM  DS    CL16        ON-LINE MEMO                                         
         ORG   INFONLM                                                          
INFHIRD  DS    CL6         HIRE DATE(FOR 1R LEDGER)                             
INFLNQ   EQU   *-INFD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER REPORT PRINT LINE                                    *         
***********************************************************************         
                                                                                
PLD      DSECT                                                                  
PLL      DS    X                                                                
PLACC    DS    CL14                UNIT/LEDGER ACCOUNT CODE                     
PLC1     DS    X                                                                
PLNME    DS    CL36                NAME                                         
PLC2     DS    X                                                                
PLADDR   DS    CL26                ADDRESS                                      
PLC3     DS    X                                                                
PLFLT1   DS    CL1                 FILTERS                                      
PLFLT2   DS    CL1                                                              
PLFLT3   DS    CL1                                                              
PLFLT4   DS    CL1                                                              
PLFLT5   DS    CL1                                                              
PLC4     DS    X                                                                
PLEVL    DS    CL1                 LEVEL                                        
PLC5     DS    X                                                                
PLOFC    DS    CL2                 OFFICE                                       
PLC6     DS    X                                                                
PLOPT    DS    CL50                OPTIONS                                      
PLC7     DS    X                                                                
PLERR    DS    CL20                ERROR MESSAGES                               
         ORG   PLD+163                                                          
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
**PAN#1  DC    CL21'012ACREPFU02S08/17/00'                                      
         END                                                                    
