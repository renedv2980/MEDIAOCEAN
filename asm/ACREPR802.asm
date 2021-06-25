*          DATA SET ACREPR802  AT LEVEL 022 AS OF 05/01/02                      
*PHASE ACR802A,+0                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACASOF                                                                 
         TITLE 'NEGATIVE ACCRUAL OF TIME INCOME'                                
ACR802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR8**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         USING ACR8D,RC                                                         
         LA    RC,SPACEND                                                       
         USING BIGPRNTD,R7                                                      
         L     R7,VBIGPRNT                                                      
         EJECT                                                                  
*-------------------------------------------------------------                  
*        F I R S T   F O R   R U N                                              
*-------------------------------------------------------------                  
*                                                                               
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         USING BOXD,R2                                                          
         MVC   BOXWIDTH,=F'198'                                                 
         L     RF,=A(BXHOOK)                                                    
         ST    RF,HEADHOOK                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFC                                       
*                                                                               
         BAS   RE,MAIN                       GETMAIN STORAGE                    
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        F I R S T  F O R  R E Q U E S T                                        
*-------------------------------------------------------------                  
*                                                                               
REQF     CLI   MODE,REQFRST                                                     
         BNE   LVA00                                                            
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
         MVC   DATADIS2,=Y(ACCRFST-ACCRECD)                                     
         XC    ALSORT,ALSORT                 CLEAR A(LAST SORT)                 
         LA    R1,SRTKLEN                    SORT KEY LENGTH                    
         CVD   R1,DUB                        CONVERT KEY LEN TO CHARS           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLEN                  SORT RECORD LENGTH                    
         CVD   R1,DUB                     CONVERT REC LEN TO CHARS              
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
         USING ACASOFD,R5                                                       
         L     R5,AASOFBLK                                                      
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   ACASAEND,ACMAEND    ACTIVITY END                                 
         MVI   ACAFROM,ACAFTRN     FROM TRANSACTIONS                            
         MVC   STRMOS,ACMFDTE      SAVE FISCAL START                            
         MVC   ACTSTART,ACMASTR    SAVE ACTIVITY START AND END                  
         MVC   ACTEND,ACMAEND                                                   
         MVC   WORK(2),ACMMEND      GET END MONTH                               
         LA    R3,ENDMOS                                                        
         LA    R2,ENDMON                                                        
         LA    R0,4                                                             
*                                                                               
REQF01   MVC   0(2,R3),WORK                       YYMM                          
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,0(R2))     MMM/YYY                       
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)    YYMMDD                        
         GOTO1 ADDAY,DMCB,WORK+3,WORK+9,F'-1'                                   
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK)    YYMM                          
         LA    R3,2(R3)                                                         
         LA    R2,6(R2)                                                         
         BCT   R0,REQF01                                                        
*                                                                               
         MVC   WORK(L'ENDMOS),ENDMOS            GET NEXT MONTH                  
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
         GOTO1 ADDAY,DMCB,WORK+3,WORK+9,F'34'                                   
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,NEXTMOS)                               
         GOTO1 DATCON,DMCB,(0,WORK+9),(6,NEXTMON)                               
         MVC   WORK+13(2),=C'01'                GET LAST DATE OF MONTH          
         GOTO1 ADDAY,DMCB,WORK+9,WORK,F'-1'     AS TRANSACTION DATE             
         GOTO1 DATCON,DMCB,(0,WORK),(1,TRANDATE)                                
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   ACMMEND,NEXTMOS                                                  
*                                                                               
         LA    R2,ENDMOS                                                        
         LA    R3,ENDCHR                                                        
         BAS   RE,SETCHR           SET CHARACTER FOR BATCH REF                  
*                                                                               
         LA    R2,NEXTMOS                                                       
         LA    R3,NEXTCHR                                                       
         BAS   RE,SETCHR           SET CHARACTER FOR NEXT MONTH                 
*                                                                               
         BAS   RE,CALENDAR         LOOKUP CALENDAR                              
         USING BIND,RF                                                          
         L     RF,APOSLST          CLEAR TABLES                                 
         XC    BININ,BININ                                                      
         L     RF,AOFFLST                                                       
         XC    BININ,BININ                                                      
         L     RF,ASILST                                                        
         XC    BININ,BININ                                                      
         DROP  RF                                                               
*                                                                               
         ZAP   PDEBITS,ZEROS                                                    
         ZAP   PCREDITS,ZEROS                                                   
         ZAP   POSTCASH,ZEROS                                                   
         ZAP   POSTREC,ZEROS                                                    
*                                                                               
         MVI   INCONLY,C'N'                                                     
         CLI   QOPT3,C'I'          INCOME REPT ONLY ?                           
         BE    *+12                                                             
         CLI   QOPT3,C'W'          INC ONLY(INCLUDING ACCRUAL)?                 
         BNE   REQF04                                                           
         MVI   QOPT1,C' '          MUST BE DRAFT                                
         MVI   INCONLY,C'Y'        SET INCOME ONLY SWITCH                       
*                                                                               
REQF04   MVC   R8MODE,=C'* * L  I  V  E  * *  '                                 
         CLI   QOPT1,C'L'                                                       
         BNE   REQF06                                                           
         XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'AR8'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
*****    OI    ID+13,X'01'         ALLOW DUPLICATE FILES                        
         BAS   RE,OPNPOST                                                       
         B     REQF08                                                           
*                                                                               
REQF06   MVC   R8MODE,=C'* * D  R  A  F  T * *'                                 
         MVI   RCPOSTNG,C'N'                                                    
*                                                                               
REQF08   DS    0H                                                               
         L     RE,AOFFTAB                                                       
         LA    RF,OFFTABLN         CLEAR OFFICE IN REQUEST TABLE                
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         L     RE,AOFFTAB                                                       
         LA    RF,ACMROFL                                                       
         ICM   R0,3,ACMOFCN        NUMBER OF OFFICES                            
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,RE),0(RF)                                                    
         LA    RE,2(RE)                                                         
         LA    RF,2(RF)                                                         
         BCT   R0,*-14                                                          
         MVI   0(RE),X'FF'         MARK THE END OF TABLE                        
*                                                                               
*                                  VERIFY SB ACCT FOR POSTINGS                  
         USING ACTRECD,R5                                                       
         LA    R5,DKEY                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKULA,=C'SB14400115    '                                       
         BAS   RE,DMRD                                                          
         CLC   ACTKCULA,DIR                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SBACCT,ACTKULA                                                   
*                                          BUILD LIST OF OFFICE NAMES           
         USING OGRRECD,R5                                                       
         LA    R5,DKEY                                                          
         USING OFFD,R3                                                          
         LA    R3,WORK                                                          
         MVC   OFFCKEY,SPACES                                                   
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,RCCOMPFL                                                 
         MVC   OGRKUNT(2),=C'SJ'                                                
         BAS   RE,DMHGH                                                         
         B     *+8                                                              
REQF14   BAS   RE,DMSEQ                                                         
         CLC   OGRKEY(OGRKOFC-OGRRECD),DIR PROD OFFICE RECORD?                  
         BNE   REQF20                                                           
         BAS   RE,DMGETR                   GET CURRENT FILE RECORD              
         L     R5,AIO                                                           
         MVC   OFFCKEY(L'OGRKOFC),OGRKOFC   SAVE OFFICE CODE                    
         L     R4,AIO                                                           
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                    GET THE NAME EL                      
         LA    R2,OFFNAME                                                       
         BAS   RE,GETNAME                  MOVE IT INTO OFFNAME                 
         GOTO1 BINADD,DMCB,(R3),AOFFLST    ADD TO TABLE                         
         B     REQF14                                                           
*                                                                               
REQF20   MVI   OFFCLEN,2                 ASSUME NEW OFFICE LENGTH 2             
         USING CPYELD,R4                                                        
         L     R4,ADCMPEL                ARE WE NEW OFFICE?                     
         TM    CPYSTAT4,CPYSOFF2                                                
         BO    *+8                       YES                                    
         MVI   OFFCLEN,1                 OLD OFFICES, LENGTH IS 1               
*                                                                               
REQFX    B     XIT                                                              
         DROP  R2,R3,R4                                                         
*                                                                               
SETCHR   LR    R0,RE                                                            
         MVC   WORK(2),0(R2)                                                    
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
         MVC   0(1,R3),WORK+4      YEAR                                         
         MVC   1(1,R3),WORK+6      MONTH                                        
         CLI   WORK+5,C'1'                                                      
         BNE   SETCHRX                                                          
         MVI   1(R3),C'A'                                                       
         CLI   WORK+6,C'0'                                                      
         BE    SETCHRX                                                          
         MVI   1(R3),C'B'                                                       
         CLI   WORK+6,C'1'                                                      
         BE    SETCHRX                                                          
         MVI   1(R3),C'C'                                                       
SETCHRX  LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        F I R S T   F O R   C L I E N T                                        
*-------------------------------------------------------------                  
*                                                                               
LVA00    CLI   MODE,PROCLEVA                                                    
         BNE   LVB00                                                            
         NI    ACTSW,X'FF'-ACTCLI TURN OFF CLIENT ACTIVITY                      
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------                  
*        F I R S T   F O R   P R O D U C T                                      
*-------------------------------------------------------------                  
*                                                                               
LVB00    CLI   MODE,PROCLEVB                                                    
         BNE   LVC00                                                            
         L     RE,ADGOBLOC                                                      
         USING GETOPTD,RE                                                       
         MVC   CLIENTOF,GOEFFOFC   CLIENT OFFICE FROM GETOPT                    
         L     RE,ADPROFIL                                                      
         USING PPRELD,RE                                                        
         MVC   COSTING,PPRCOSTA    COST ACCT FROM MONACC COMPOSITE PROF         
         DROP  RE                                                               
*                                                                               
         USING SRTD,R6                                                          
         L     R6,ASRTWRK                                                       
         MVC   SRTKEY(SRTLEN),XSPACES                                           
         MVI   SRTRPT,0                                                         
         MVC   SRTOFC,CLIENTOF         CLIENT OFFICE                            
         MVC   SRTCST,COSTING                                                   
         L     RF,ADHEIRB                                                       
         MVC   SRTCLTPD,3(RF)          CLT/PRD                                  
         LA    R1,SRTBUCKS             START OF COL BUCKETS                     
         LA    R0,SRTBKNUM             NUMBER OF BUCKETS INTO R0                
         ZAP   0(SRTBKLN,R1),ZEROS     CLEAR TO PACKED ZEROS                    
         LA    R1,SRTBKLN(R1)          BUMP TO NEXT BUCKET                      
         BCT   R0,*-10                                                          
*                                                                               
         NI    ACTSW,X'FF'-ACTPRD                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------                  
*        F I R S T   F O R   J O B                                              
*-------------------------------------------------------------                  
*                                                                               
LVC00    CLI   MODE,PROCACC                                                     
         BNE   TRN00                                                            
         CLI   QOPT2,C'J'                                                       
         BNE   LVC03                                                            
*                                                                               
         USING SRTD,R6                                                          
         L     R6,ASRTWRK                                                       
         L     RF,ADACC                                                         
         MVC   SRTACC,3(RF)            CLT/PRD/JOB                              
         LA    R1,SRTBUCKS             START OF COL BUCKETS                     
         LA    R0,SRTBKNUM             NUMBER OF BUCKETS INTO R0                
         ZAP   0(SRTBKLN,R1),ZEROS     CLEAR TO PACKED ZEROS                    
         LA    R1,SRTBKLN(R1)          BUMP TO NEXT BUCKET                      
         BCT   R0,*-10                                                          
*                                                                               
         USING ACASOFD,R5                                                       
LVC03    L     R5,AASOFBLK                                                      
         MVC   ACABILLS,ABILBUF    ADDR OF BILL BUFFER                          
         MVC   ACAAJOB,ADACC       JOB RECORD                                   
         MVC   ACACOMF,ADCOMFAC    COMFACS                                      
         L     RE,AMONACC                                                       
         L     RE,ACMAPRO2-ACMD(RE)                                             
         ST    RE,ACAPTABF         PTABLOCK FROM PRORATA                        
         MVI   ACAMODE,ACAMINIT                                                 
         MVC   ACASOFDT,ENDMOS     AS OF MONTH                                  
         GOTO1 ACASOF,(R5)                                                      
         NI    ACTSW,X'FF'-ACTJOB                                               
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        T R A N S A C T I O N S                                                
*-------------------------------------------------------------                  
*                                                                               
TRN00    CLI   MODE,PROCTRNS                                                    
         BNE   ACL00                                                            
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRNRECD,R6          R6 POINTS TO TRANSACTION RECORD              
         LR    R6,R4                                                            
         SH    R6,DATADISP                                                      
         CLC   TRNKCUNT(2),=C'1R'                                               
         BNE   XIT                                                              
*                                                                               
         USING ACASOFD,R5                                                       
         L     R5,AASOFBLK                                                      
         ST    R6,ACAATRN          TRANSACTION RECORD                           
         MVI   ACAMODE,ACAMTRN     SET MODE TO TRANSACTION                      
         GOTO1 ACASOF,(R5)                                                      
         CP    ACAUNBLD,ZEROS                                                   
         BE    XIT                 FULLY BILLED                                 
*                                                                               
         BAS   RE,GETMON                                                        
*                                                                               
         USING SRTD,R6                                                          
         L     R6,ASRTWRK                                                       
         LA    RF,SRTBUCKS                                                      
         LA    R0,3                                                             
         LA    R3,ENDMOS                                                        
         CLC   MNTH,ENDMOS         LOOK AHEAD MTH GOES IN CURRENT               
         BH    TRN08                                                            
TRN05    CLC   0(2,R3),MNTH        OTHERWISE AGE IN CORRECT COLUMN              
         BE    TRN08                                                            
         LA    RF,SRTBKLN(RF)                                                   
         LA    R3,2(R3)                                                         
         BCT   R0,TRN05                                                         
*                                                                               
TRN08    AP    0(SRTBKLN,RF),ACAUNBLD  SAVE UNBILLED                            
         OI    ACTSW,ACTCLI+ACTPRD+ACTJOB                                       
         B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        GET CALENDAR MONTH FOR AGEING                                          
*-------------------------------------------------------------                  
*                                                                               
         USING TRNELD,R4                                                        
GETMON   LR    R0,RE                                                            
         L     RE,ACALTAB                                                       
         USING CALD,RE                                                          
         LH    R3,CALNUM               NUMBER OF AGEING PERIODS                 
         XC    MNTH,MNTH                                                        
GETMON3  CLC   TRNDATE,CALPSTR                                                  
         BL    GETMON5                                                          
         CLC   TRNDATE,CALPEND                                                  
         BH    GETMON5                                                          
         MVC   MNTH,CALMOA                                                      
         B     GETMONX                                                          
GETMON5  LA    RE,CALLEN(RE)                                                    
         BCT   R3,GETMON3                                                       
GETMONX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4,RE                                                            
         EJECT                                                                  
*-------------------------------------------------------------                  
*        A C C L A S T   F O R  A  J O B                                        
*-------------------------------------------------------------                  
*                                                                               
ACL00    CLI   MODE,ACCLAST                                                     
         BNE   LVBL00                                                           
         CLI   QOPT2,C'J'                                                       
         BNE   XIT                                                              
         TM    ACTSW,ACTJOB                                                     
         BNO   XIT                                                              
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         L     R4,ADACCNAM         NAME TO RECORD                               
         LA    R2,SRTNME                                                        
         BAS   RE,GETNAME                                                       
         GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        L V B L A S T                                                          
*-------------------------------------------------------------                  
*                                                                               
LVBL00   CLI   MODE,LEVBLAST                                                    
         BNE   LVAL00                                                           
         TM    ACTSW,ACTPRD                                                     
         BNO   XIT                                                              
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         XC    SRTJOB,SRTJOB                                                    
         MVI   SRTJOB,X'FF'                                                     
         L     R4,ADLVBNAM         NAME TO RECORD                               
         LA    R2,SRTNME                                                        
         BAS   RE,GETNAME                                                       
         CLI   QOPT2,C'J'                                                       
         BE    LVBL03                                                           
         GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         MVI   ALSORT,1                ACTIVITY SWITCH                          
         B     XIT                                                              
*                                                                               
LVBL03   XC    SRTJOB,SRTJOB       SET PRODUCT HEADER                           
         GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         MVI   SRTJOB,X'FF'        SET PRODUCT TRAILER                          
         BASR  RE,RF                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*-------------------------------------------------------------                  
*        L V A L A S T                                                          
*-------------------------------------------------------------                  
*                                                                               
LVAL00   CLI   MODE,LEVALAST                                                    
         BNE   REQL00                                                           
         TM    ACTSW,ACTCLI                                                     
         BNO   XIT                                                              
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         XC    SRTJOB,SRTJOB       SET CLIENT  HEADER                           
         XC    SRTPRD,SRTPRD                                                    
         L     R4,ADLVANAM         NAME TO RECORD                               
         LA    R2,SRTNME                                                        
         BAS   RE,GETNAME                                                       
         GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         MVI   SRTPRD,X'FF'        SET CLIENT TRAILER                           
         BASR  RE,RF                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LAST REQUEST - GET RECORDS FROM SORTER                                 
*                       PRODUCE REPORT                                          
*------------------------------------------------------------------*            
REQL00   CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   CLIENTOF,SPACES                                                  
         MVC   ACCOUNT,SPACES                                                   
         BAS   RE,INCOME           READ SI                                      
*                                                                               
         LA    R1,TOTBKNUM         # TO CLEAR                                   
         LA    R2,TOTALS           STARTING TOTAL TO CLEAR                      
         BAS   RE,CLRTOTS          CLEAR ALL REPORT TOTALS                      
         LA    R3,XP                                                            
         USING PLD,R3                                                           
*                                                                               
REQL02   GOTO1 ADSORTER,DMCB,=C'GET',0                                          
         SR    R2,R2                                                            
         ICM   R2,15,4(R1)                                                      
         BZ    REQL20                  SORT IS DONE                             
*                                                                               
         USING SRTD,R5                                                          
         L     R5,ASRTWRK                                                       
         MVC   SRTD(SRTLEN),0(R2)  SAVE CURRENT SRT REC                         
         MVC   COSTING,SRTCST                                                   
         MVC   CLTPRDSV,SRTCLTPD                                                
         CLC   SRTOFC,CLIENTOF     SAME OFFICE                                  
         BE    REQL06                                                           
         CLC   CLIENTOF,SPACES     1ST TIME?                                    
         BE    REQL04                                                           
         BAS   RE,TOTOFF           OFFICE TOTALS                                
*                                                                               
REQL04   MVC   CLIENTOF,SRTOFC                                                  
         MVC   OFFICE,CLIENTOF                                                  
         BAS   RE,GETOFF           GET OFFICE NAME                              
         MVC   CLIOFFNM,WORK                                                    
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
REQL06   CLI   SRTPRD,X'00'        TEST CLIENT HEADER                           
         BNE   REQL08              YES- CONTINUE                                
         MVC   PLACCT(3),SRTACC    CLIENT                                       
         MVC   PLNME,SRTNME        NAME                                         
         BAS   RE,ACRPT                                                         
         B     REQL02                                                           
*                                                                               
REQL08   CLI   SRTPRD,X'FF'        TEST CLIENT TOTAL                            
         BNE   REQL10              YES- CONTINUE                                
         BAS   RE,TOTCLI                                                        
         B     REQL02                                                           
*                                                                               
REQL10   CLI   SRTJOB,X'00'        TEST PRODUCT HEADER                          
         BNE   REQL12                                                           
         MVC   PLACCT(6),SRTACC    PRODUCT                                      
         MVC   PLNME,SRTNME        NAME                                         
         BAS   RE,ACRPT                                                         
         B     REQL02                                                           
*                                                                               
REQL12   CLI   SRTJOB,X'FF'        TEST PRODUCT TOTAL                           
         BNE   REQL16                                                           
         CLI   QOPT2,C'J'                                                       
         BE    REQL14              ALREADY PRODUCT TOTAL(FROM JOBS)             
         LA    RE,SRTPER1          SAVE PRODUCT TOTALS                          
         LA    RF,TPRD1U                                                        
         LA    R0,4                                                             
         ZAP   0(6,RF),0(6,RE)                                                  
         LA    RE,6(RE)                                                         
         LA    RF,6(RF)                                                         
         BCT   R0,*-14                                                          
         MVC   PLACCT(6),SRTACC    PRODUCT                                      
         MVC   PLNME,SRTNME        NAME                                         
         BAS   RE,ACRPT                                                         
*                                                                               
REQL14   BAS   RE,ACCRUAL          FIGURE OUT THE ACCRUAL AND PRINT             
         BAS   RE,ADDTOTS          ADD TO ALL REPORT TOTALS                     
         BAS   RE,TOTPRD                                                        
         B     REQL02                                                           
*                                                                               
REQL16   MVC   PLACCT,SRTACC       JOB DETAIL                                   
         MVC   PLNME,SRTNME        NAME                                         
         LA    R4,PLCPER1                                                       
         LA    R6,SRTBUCKS                                                      
         LA    RE,TPRODUCT         UPDATE PRODUCT TOTALS                        
         LA    R2,4                                                             
         ZAP   TOTAL,ZEROS                                                      
REQL18   EDIT  (P6,0(R6)),(14,0(R4)),2,MINUS=YES                                
         AP    0(TOTBKLN,RE),0(SRTBKLN,R6)                                      
         AP    TOTAL,0(SRTBKLN,R6)                                              
         LA    RE,TOTBKLN(RE)                                                   
         LA    R4,L'PLCOL(R4)                                                   
         LA    R6,SRTBKLN(R6)                                                   
         BCT   R2,REQL18                                                        
         EDIT  (P8,TOTAL),(14,PLCTOT),2,MINUS=YES                               
         BAS   RE,ACRPT                                                         
         B     REQL02                                                           
*                                                                               
REQL20   GOTO1 ADSORTER,DMCB,=C'END'                                            
         BAS   RE,TOTOFF           OFFICE TOTALS                                
         LA    R1,TREQUEST         TOTAL REQUEST                                
         BAS   RE,PRNTTOT                                                       
*                                                                               
         BAS   RE,INCR             PRINT THE INCOME REPORT                      
         BAS   RE,POSTIT           DO THE POSTINGS                              
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFC,(X'80',1)                           
*                                                                               
         CLI   QOPT1,C'L'          LIVE RUN                                     
         BNE   XIT                                                              
         CP    POSTREC,ZEROS                                                    
         BE    XIT                                                              
         ZAP   DUB,PDEBITS                                                      
         SP    DUB,PCREDITS                                                     
         CP    DUB,=P'1000'                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         CP    DUB,=P'-1000'                                                    
         BNL   *+6                                                              
         DC    H'0'                ROUNDING MORE THAN 10 DOLLARS                
         LA    RE,T                RECEIVING FIELD                              
         LH    RF,=Y(L'T)          RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   T-4(2),=X'0021'                                                  
         MVC   T(2),=X'521D'                                                    
         MVC   T+2(15),=C'INCOME ACCRUAL '                                      
         ZAP   T+17(6),POSTREC                                                  
         ZAP   T+23(6),POSTCASH                                                 
         BAS   RE,ADDPOST                                                       
         BAS   RE,CLOSPOST                                                      
*                                                                               
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT THE INCOME REPORT                                             *         
***********************************************************************         
*                                                                               
INCR     NTR1  ,                                                                
         CLI   QOPT3,C'A'          AGEING REPORT ONLY?                          
         BE    XIT                                                              
         MVC   XP,XSPACES                                                       
         MVC   XPSECOND,XSPACES                                                 
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         XC    BUFKEY(BUFKLEN),BUFKEY                                           
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFC,BUFKEY,1                             
         CLI   8(R1),X'80'                                                      
         BE    XIT                 NOTHING TO PRINT                             
         LA    R3,XP                                                            
         USING PLD,R3                                                           
         XC    CLIENTOF,CLIENTOF                                                
         XC    CLTPRDSV,CLTPRDSV                                                
         MVI   MIDS,X'FF'                                                       
*                                                                               
INCR03   CLI   BUFOFFC,X'FF'                                                    
         BNE   INCR05                                                           
         MVC   PLNME(25),=CL25'REQUEST TOTAL'                                   
         BAS   RE,EDIT                                                          
         B     XIT                                                              
*                                                                               
INCR05   CLC   CLIENTOF,BUFOFFC                                                 
         BE    INCR06                                                           
         MVC   CLIENTOF,BUFOFFC                                                 
         MVC   OFFICE,CLIENTOF                                                  
         BAS   RE,GETOFF           GET OFFICE NAME                              
         MVC   CLIOFFNM,WORK                                                    
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
INCR06   CLI   BUFTYPE,2                                                        
         BNE   INCR07                                                           
         MVC   PLNME(25),=CL25'OFFICE TOTAL'                                    
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         B     INCR31                                                           
*                                                                               
INCR07   CLI   BUFTYPE,1           TEST OTHER OFFICES                           
         BNE   INCR15                                                           
         CLI   BUFPOFC,X'FF'                                                    
         BNE   INCR09                                                           
         MVC   PLNME(30),=CL30'TOTAL CLIENTS OTHER OFFICES'                     
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         B     INCR31                                                           
*                                                                               
INCR09   CLI   BUFCLI,X'FF'                                                     
         BNE   INCR11                                                           
         MVC   PLNME(30),=CL30'TOTAL CLIENTS    '                               
         MVC   PLNME+14(2),BUFPOFC   PERSON OFFICE                              
         MVC   OFFICE,BUFPOFC                                                   
         BAS   RE,GETOFF           GET OFFICE NAME(RETURNED IN WORK)            
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         GOTO1 CHOPPER,DMCB,(L'WORK,WORK),(19,PLNME+17),(L'XP,2)                
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         B     INCR31                                                           
*                                                                               
INCR11   CLI   BUFPRD,X'FF'                                                     
         BNE   INCR21                                                           
         MVC   PLNME(30),=CL30'TOTAL CLIENT'                                    
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         B     INCR31                                                           
*                                                                               
INCR15   CLI   BUFCLI,X'FF'        THIS OFFICE                                  
         BNE   INCR17                                                           
         MVC   PLNME(30),=CL30'TOTAL CLIENTS          '                         
         MVC   PLNME+14(2),BUFOFFC OFFICE                                       
         MVC   OFFICE,BUFOFFC                                                   
         BAS   RE,GETOFF           GET OFFICE NAME(RETURNED IN WORK)            
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         GOTO1 CHOPPER,DMCB,(L'WORK,WORK),(19,PLNME+17),(L'XP,2)                
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         B     INCR31                                                           
*                                                                               
INCR17   CLI   BUFPRD,X'FF'                                                     
         BNE   INCR21                                                           
         MVC   PLNME(30),=CL30'TOTAL CLIENT'                                    
         MVI   SPACING,2                                                        
         BAS   RE,EDIT                                                          
         B     INCR31                                                           
*                                                                               
INCR21   CLC   MIDS,BUFTYPE        DETAIL LINE                                  
         BE    INCR23                                                           
         MVC   MIDS,BUFTYPE                                                     
         MVC   PLNME(25),=CL25'CLIENTS IN THIS OFFICE'                          
         CLI   MIDS,0                                                           
         BE    *+10                                                             
         MVC   PLNME(25),=CL25'CLIENTS OF OTHER OFFICES'                        
         MVI   SPACING,2                                                        
         BAS   RE,ACRPT                                                         
*                                                                               
INCR23   CLC   CLTPRDSV(3),BUFCLI      TEST CLIENT                              
         BE    INCR25                                                           
         MVC   PLACCT(3),BUFCLI                                                 
         LA    R1,WORK                                                          
         MVC   WORK,SPACES         GET CLIENT  NAME                             
         MVC   WORK(2),=C'SJ'                                                   
         MVC   WORK+2(3),BUFCLI                                                 
         BAS   RE,XNME                                                          
         MVC   PLNME,WORK                                                       
         BAS   RE,ACRPT                                                         
*                                                                               
INCR25   MVC   PLACCT(6),BUFCLIPD                                               
         LA    R1,WORK                                                          
         MVC   WORK,SPACES         GET CLI/PRD NAME                             
         MVC   WORK(2),=C'SJ'                                                   
         MVC   WORK+2(6),BUFCLIPD                                               
         BAS   RE,XNME                                                          
         MVC   PLNME,WORK                                                       
         BAS   RE,EDIT                                                          
         MVC   CLTPRDSV(3),BUFCLI                                               
*                                                                               
INCR31   GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFC,BUFKEY,1                              
         CLI   8(R1),X'80'                                                      
         BNE   INCR03                                                           
         B     XIT                                                              
*                                                                               
EDIT     NTR1  ,                                                                
         LA    R4,PLCPER1                                                       
         LA    R5,BUFBK                                                         
         LA    R2,3                                                             
         ZAP   TOTAL,ZEROS                                                      
EDIT3    EDIT  (P8,0(R5)),(14,0(R4)),2,MINUS=YES                                
         AP    TOTAL,0(8,R5)                                                    
         LA    R4,L'PLCOL(R4)                                                   
         LA    R5,8(R5)                                                         
         BCT   R2,EDIT3                                                         
         EDIT  (P8,TOTAL),(14,PLCTOT),2,MINUS=YES                               
         ZAP   TOTAL,BUFMONTH                                                   
         AP    TOTAL,BUFACCRU                                                   
         EDIT  (P8,TOTAL),(14,PLCPER4),2,MINUS=YES                              
         BAS   RE,ACRPT            PRINT IT                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET UP HEADLINES AND GO TO ACREPORT                                 *         
***********************************************************************         
*                                                                               
ACRPT    NTR1  ,                                                                
         MVC   XHEAD4+12(L'CLIENTOF),CLIENTOF                                   
         MVC   XHEAD4+15(L'CLIOFFNM),CLIOFFNM                                   
         MVC   XHEAD4+100(L'R8MODE),R8MODE     LIVE OR DRAFT                    
         LA    R3,XHEAD6                                                        
         USING PLD,R3                                                           
         CLI   RCSUBPRG,0                                                       
         BNE   ACRPT5                                                           
         CLI   INCONLY,C'Y'                    INCOME REPT ONLY ?               
         BE    ACRPTX                                                           
         MVC   PLCPER1+5(4),=C'0%  '                                            
         MVC   PLCPER2+5(4),=C'0%  '                                            
         MVC   PLCPER3+5(4),=C'100%'                                            
         MVC   PLCPER4+5(4),=C'100%'                                            
*                                                                               
         LA    R3,XHEAD7                                                        
         USING PLD,R3                                                           
         MVC   PLCPER1+3(L'ENDMON),ENDMON                                       
         MVC   PLCPER2+3(L'LASTMON),LASTMON                                     
         MVC   PLCPER3+3(L'PRVMON),PRVMON                                       
         MVC   PLCPER4(L'PRIMON),PRIMON                                         
         MVC   PLCPER4+7(7),=C'+ PRIOR'                                         
         MVC   PLCTOT+3(5),=C'TOTAL'                                            
         B     ACRPT7                                                           
*                                                                               
ACRPT5   CLI   RCSUBPRG,1                                                       
         BNE   ACRPTX                                                           
         MVC   PLCPER1+6(L'LASTMON),LASTMON                                     
         MVC   PLCPER2+4(7),=C'INC/PRE'                                         
         MVC   PLCTOT+6(L'ENDMON),ENDMON                                        
*                                                                               
         LA    R3,XHEAD7                                                        
         USING PLD,R3                                                           
         MVC   PLCPER1+6(3),=C'YTD'                                             
         MVC   PLCPER2+4(7),=C'ACCRUAL'                                         
         MVC   PLCPER3+4(7),=C'ACCRUAL'                                         
         MVC   PLCPER4+6(6),=C'INCOME'                                          
         MVC   PLCTOT+6(3),=C'YTD'                                              
*                                                                               
ACRPT7   DS    0H                                                               
         GOTO1 ACREPORT                                                         
ACRPTX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              CLEAR TOTAL ACCUMS                                               
*              R1 CONTAINS THE NUMBER OF ACCUMS TO CLEAR                        
*              R2 ADDDR OF STARTING ACCUM TO CLEAR                              
***********************************************************************         
*                                                                               
*                                                                               
CLRTOTS  NTR1                                                                   
         ZAP   0(TOTBKLN,R2),ZEROS     CLEAR TO PACKED ZEROS                    
         LA    R2,TOTBKLN(R2)          BUMP TO NEXT BUCKET                      
         BCT   R1,*-10                                                          
         B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*              ADD PRODUCTS TO HIGHER LEVELS                                    
***********************************************************************         
*                                                                               
*                                                                               
ADDTOTS  NTR1                                                                   
         LA    R0,TOTBKHI              NUMBER OF HIGH LEVEL TOTALS              
         LA    R1,TOTHIGH              1ST HIGH LEVEL BUCKET                    
ADDT00   LA    RE,TPRD1U               1ST PRODUCT TOTAL                        
         LA    RF,TPRDBNUM                                                      
ADDT02   AP    0(TOTBKLN,R1),0(TOTBKLN,RE)                                      
         LA    R1,TOTBKLN(R1)          BUMP TO NEXT BUCKET                      
         LA    RE,TOTBKLN(RE)          BUMP TO NEXT BUCKET                      
         BCT   RF,ADDT02                                                        
         BCT   R0,ADDT00                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              CONTROL REPORT TOTALLING                                         
***********************************************************************         
*                                                                               
TOTOFF   LR    R0,RE               OFFICE TOTALS                                
         LA    R1,TOFFICE                                                       
         BAS   RE,PRNTTOT                                                       
         LA    R1,TOFFBCLR         # TO CLEAR                                   
         LA    R2,TOTALS           STARTING TOTAL TO CLEAR                      
         BAS   RE,CLRTOTS          CLEAR ALL REPORT TOTALS                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TOTPRD   LR    R0,RE                                                            
         LA    R1,TPRODUCT         PRODUCT TOTAL                                
         BAS   RE,PRNTTOT                                                       
         LA    R1,TPRDBCLR         # TO CLEAR                                   
         LA    R2,TOTALS           STARTING TOTAL TO CLEAR                      
         BAS   RE,CLRTOTS          CLEAR ALL REPORT TOTALS                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TOTCLI   LR    R0,RE                                                            
         LA    R1,TCLIENT          CLIENT TOTAL                                 
         BAS   RE,PRNTTOT                                                       
         LA    R1,TCLIBCLR         # TO CLEAR                                   
         LA    R2,TOTALS           STARTING TOTAL TO CLEAR                      
         BAS   RE,CLRTOTS          CLEAR ALL REPORT TOTALS                      
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              PRINT TOTALS                                                     
*              R1 HAS 1ST BUCKET OF GROUP TO PRINT                              
***********************************************************************         
*                                                                               
*                                                                               
PRNTTOT  NTR1                                                                   
         LA    R3,XP                                                            
         USING PLD,R3                                                           
         L     R2,ATOTDES          TOTAL DESCRIPTION TABLE                      
         LA    R0,TOTDNUM                                                       
         USING TOTDESD,R2                                                       
PRNTT00  SR    RE,RE                                                            
         ICM   RE,3,TOTOFFS        MATCH TOTAL ACCUM OFFSET                     
         AR    RE,RC                                                            
         CR    RE,R1                                                            
         BE    PRNTT02                                                          
         LA    R2,TOTDLNQ(R2)                                                   
         BCT   R0,PRNTT00                                                       
         DC    H'0'                DESCRIPTION MUST BE FOUND                    
*                                                                               
PRNTT02  DS    0H                                                               
         MVC   PLNME(L'TOTDESP),TOTDESP                                         
         LR    R2,R1                                                            
         USING TOTD,R2                                                          
         LA    R3,XP                                                            
         MVC   PLNME+L'TOTDESP+2(12),=C'UNBILLED INV'                           
         LA    R4,PLCPER1                                                       
         LA    R6,TOTPER1U                                                      
         LA    R5,4                                                             
         ZAP   TOTAL,ZEROS                                                      
PRNTT04  EDIT  (P6,0(R6)),(14,0(R4)),2,MINUS=YES                                
         AP    TOTAL,0(TOTBKLN,R6)                                              
         LA    R4,L'PLCOL(R4)                                                   
         LA    R6,TOTBKLN(R6)                                                   
         BCT   R5,PRNTT04                                                       
         EDIT  (P8,TOTAL),(14,PLCTOT),2,MINUS=YES                               
*                                                                               
         LA    R3,XPSECOND                                                      
         MVC   PLNME+L'TOTDESP+2(7),=C'ACCRUAL'                                 
         LA    R4,PLCPER1                                                       
         LA    R6,TOTPER1A                                                      
         LA    R5,4                                                             
         ZAP   TOTAL,ZEROS                                                      
PRNTT06  EDIT  (P6,0(R6)),(14,0(R4)),2,MINUS=YES                                
         AP    TOTAL,0(TOTBKLN,R6)                                              
         LA    R4,L'PLCOL(R4)                                                   
         LA    R6,TOTBKLN(R6)                                                   
         BCT   R5,PRNTT06                                                       
         EDIT  (P8,TOTAL),(14,PLCTOT),2,MINUS=YES                               
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,ACRPT                                                         
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        READ SI                                                      *         
***********************************************************************         
*                                                                               
*                                                                               
INCOME   NTR1                                                                   
*                                                                               
         L     R3,AOFFTAB                                                       
INC02    CLI   0(R3),X'FF'                                                      
         BE    INC20                                                            
         B     INC06                                                            
INC04    LA    R3,2(R3)                                                         
         B     INC02                                                            
         USING TRNRECD,R6                                                       
INC06    LA    R6,DKEY                                                          
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL    COMPANY                                      
         MVC   TRNKUNT(2),=C'SI'                                                
         MVC   TRNKACT(2),0(R3)    OFFICE                                       
         BAS   RE,DMHGH                                                         
INC08    BAS   RE,DMSEQ                                                         
         LA    R6,DIR                                                           
         CLC   TRNKEY(5),DKEY      MATCH ON SI OFFICE                           
         BNE   INC04                                                            
         CLC   =C'GEACACR',TRNKACT+2                                            
         BNE   INC10                                                            
         CLC   TRNKCULC,SPACES                                                  
         BH    INC10                                                            
         CLC   TRNKOFF,SPACES                                                   
         BH    INC10                                                            
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R6,AIO                                                           
         L     R5,AGENWRK                                                       
         USING SID,R5                                                           
         MVC   SIKEY(SILEN),SPACES                                              
         MVC   SIACCT,TRNKULA     SAVE POSTING SI ACCOUNT                       
         L     R4,AIO                                                           
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         LA    R2,SINAME                                                        
         BAS   RE,GETNAME                                                       
         L     R4,AIO                                                           
         MVI   ELCODE,SPAELQ       GET THE 12 ACCOUNT                           
         BAS   RE,GETEL                                                         
         USING SPAELD,R4                                                        
         CLI   SPATYPE,SPATANAL                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SI12UL,=C'12'                                                    
         MVC   SI12ACC,SPAAANAL                                                 
         GOTO1 BINADD,DMCB,(R5),ASILST    ADD TO TABLE                          
         B     INC08                                                            
         DROP  R5                                                               
INC10    CLC   TRNKCULC,SPACES     MUST HAVE A CONTRA                           
         BNH   INC08                                                            
         CLC   TRNKDATE,SPACES     MUST HAVE A TRANSACTION DATE                 
         BE    INC08                                                            
         CLC   TRNKSMOS,STRMOS     LOWER THAN START GET NEXT                    
         BL    INC08                                                            
         CLC   TRNKSMOS,ENDMOS     HIGHER THAN END GET NEXT                     
         BH    INC08                                                            
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R6,AIO                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TRSELQ                                                    
         BAS   RE,GETEL            TRANSACTION STATUS                           
         USING TRSELD,R4                                                        
         CLI   TRSEL,TRSELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRSDATE,ACTSTART    BEFORE ACTIVITY START                        
         BL    INC08                                                            
         CLC   TRSDATE,ACTEND      AFTER ACTIVITY END                           
         BH    INC08                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TRNELQ                                                    
         BAS   RE,GETEL            R4 POINTS AT TRANSACTION                     
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TRNTYPE,14          TEST WRITEOFF                                
         BE    INC08                                                            
         MVC   BUFOFFC,TRNKACT     SI OFFICE                                    
         MVC   BUFPOFC,TRNKOFF                                                  
         MVI   BUFTYPE,BUFIN                                                    
         CLC   BUFOFFC,BUFPOFC     MATCH VS TRANSACTION OFFICE                  
         BE    *+8                                                              
         MVI   BUFTYPE,BUFOUT      SET TO CLIENTS OUTSIDE OF OFFICE             
         MVC   BUFCLIPD,TRNKCACT   CLIENT PRODUCT                               
         ZAP   BUFYTD,ZEROS                                                     
         ZAP   BUFMONTH,ZEROS                                                   
         ZAP   BUFACCRU,ZEROS                                                   
         LA    R1,BUFYTD                                                        
         CLC   TRNRSMOS,ENDMOS     EQUAL TO END DO...                           
         BNE   INC12                                                            
         LA    R1,BUFACCRU                                                      
         CLI   TRNTYPE,55          55'S OR 56'S GO IN ACCRUAL COL               
         BE    INC12                                                            
         CLI   TRNTYPE,56                                                       
         BE    INC12                                                            
         LA    R1,BUFMONTH                                                      
INC12    ZAP   DUB,TRNAMNT                                                      
         TM    TRNSTAT,TRNSDR                                                   
         BNO   *+10                                                             
         MP    DUB,=P'-1'                                                       
         ZAP   0(L'BUFYTD,R1),DUB                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFKEY                                
         MVC   BUFPRD,FOXES                                                     
         BASR  RE,RF                CLIENT TOTAL                                
         MVC   BUFCLI,FOXES                                                     
         BASR  RE,RF                EMPLOYEE OFFICE TOTAL                       
         MVC   BUFPOFC,FOXES                                                    
         CLI   BUFTYPE,BUFIN        IF CLIENT IS IN SAME OFF SKIP TOTAL         
         BE    *+6                                                              
         BASR  RE,RF                CLIENT WITHIN SAME OFFICE TOTAL             
         MVI   BUFTYPE,BUFALL                                                   
         BASR  RE,RF                OVERALL OFFICE TOTAL                        
         MVC   BUFTYPE,FOXES                                                    
         MVC   BUFOFFC,FOXES                                                    
         BASR  RE,RF                REQUEST TOTAL                               
         B     INC08                                                            
*                                                                               
INC20    L     RF,ASILST                                                        
         USING BIND,RF                                                          
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R5,BINTABLE                                                      
*                                                                               
         USING SID,R5                                                           
         USING ACTRECD,R6                                                       
INC22    LA    R6,DKEY                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKULA,SI12ACT                                                  
         BAS   RE,DMRD                                                          
         LA    R6,DIR                                                           
         CLC   DKEY,ACTKEY         DID WE FIND IT?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R4,AIO                                                           
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         LA    R2,SI12NME                                                       
         BAS   RE,GETNAME                                                       
         LA    R5,SILEN(R5)        NEXT 12 ACCOUNT FOR LOOKUP                   
         BCT   R3,INC22                                                         
         B     XIT                                                              
         DROP  R4,R5,R6,RF                                                      
         EJECT                                                                  
***********************************************************************         
*              DO ACCRUAL CALCULATION PUT TO BUFFALO                  *         
***********************************************************************         
*                                                                               
*                                                                               
ACCRUAL  NTR1                                                                   
         USING ACCRUD,R6                                                        
         CLI   QOPT6,C'N'          SKIP ACCRUALS?                               
         BE    XIT                                                              
         CLI   QOPT3,C'I'          INCOME REPT ONLY SO SKIP ACCRUALS?           
         BE    XIT                                                              
         L     R6,AACCRU           ACCRUAL CALC TABLE                           
         LA    R0,ACCRUNUM         NUMBER OF ACCRUALS TO DO                     
         ZAP   POSTING,ZEROS                                                    
ACCR00   SR    R1,R1                                                            
         ICM   R1,3,ACCRFROM       R1 POINTS TO NUMBER TO ACCRUE                
         AR    R1,RC                                                            
         SR    R2,R2               R2 WILL POINT TO ACCRUAL AMOUNT              
         ICM   R2,3,ACCRTO                                                      
         AR    R2,RC                                                            
         ZAP   DIVWRK,ACCRRATE     ACCRUAL RATE                                 
         MP    DIVWRK,0(TOTBKLN,R1)                                             
         SRP   DIVWRK,64-5,5       SHIFT 6(4 FOR PCT + 2 FOR DOLLARS)           
         ZAP   0(TOTBKLN,R2),DIVWRK+(L'DIVWRK-TOTBKLN)(TOTBKLN)                 
         MP    0(TOTBKLN,R2),=P'-1'                                             
         AP    POSTING,0(TOTBKLN,R2)                                            
         LA    R6,ACCRULNQ(R6)                                                  
         BCT   R0,ACCR00                                                        
*                                                                               
*                                                                               
         L     R5,APOSTWRK                                                      
         USING POSTD,R5                                                         
         MVC   POSKEY(POSLEN),SPACES                                            
         ZAP   POSAMNT,ZEROS                                                    
         MVC   POSSBDR,SBACCT      SB ACCOUNT                                   
         MVC   POSSIUL,=C'SI'      SI ACCOUNT                                   
         MVC   POSSIOFC,CLIENTOF   OFFICE                                       
         MVC   POSSIRES,=CL10'GEACACR'                                          
         MVC   POSOFFIC,POSSIOFC                                                
         MVC   POSSICPD(2),=C'SJ'  SJ CLIENT PRODUCT                            
         MVC   POSSICPD+2(6),CLTPRDSV                                           
         MVC   POS1CDR(2),=C'1C'                                                
         MVC   POS1CDR+2(12),COSTING COSTING ACCOUNT                            
         L     R2,AGENWRK                                                       
         USING SID,R2                                                           
         MVC   SIKEY(SILEN),SPACES                                              
         MVC   SIACCT,POSSICR                                                   
         GOTO1 BINSRC,DMCB,(R2),ASILST,(R2)                                     
         MVC   POS12CR,SI12ACT                                                  
*                                                                               
         BAS   RE,PREPOST          CHECK FOR PREVIOUSLY POSTED                  
         CP    POSTING,ZEROS       IF NO ACCURAL LEAVE                          
         BE    XIT                                                              
         ZAP   POSAMNT,POSTING     POSTING AMOUNT                               
         GOTO1 BINADD,DMCB,(R5),APOSLST    ADD TO TABLE                         
*                                                                               
         MVC   BUFOFFC,CLIENTOF                                                 
         MVI   BUFTYPE,BUFIN                                                    
         MVC   BUFPOFC,CLIENTOF                                                 
         MVC   BUFCLIPD,CLTPRDSV                                                
         ZAP   BUFYTD,ZEROS                                                     
         ZAP   BUFMONTH,ZEROS                                                   
         ZAP   BUFACCRU,POSTING                                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFC,BUFKEY                                
         MVC   BUFPRD,FOXES                                                     
         BASR  RE,RF                CLIENT TOTAL                                
         MVC   BUFCLI,FOXES                                                     
         BASR  RE,RF                CLIENT WITHIN SAME OFFICE TOTAL             
         MVC   BUFPOFC,FOXES                                                    
         MVI   BUFTYPE,BUFALL                                                   
         BASR  RE,RF                OVERALL OFFICE TOTAL                        
         MVC   BUFTYPE,FOXES                                                    
         MVC   BUFOFFC,FOXES                                                    
         BASR  RE,RF                REPORT TOTAL                                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              CHECK PREVIOUSLY POSTED ACCURAL FOR THIS MONTH         *         
***********************************************************************         
*                                                                               
*                                                                               
PREPOST  NTR1                                                                   
         ZAP   PREPST,ZEROS                                                     
         L     R5,APOSTWRK                                                      
         USING POSTD,R5                                                         
         USING TRNRECD,R6                                                       
         LA    R6,DKEY                                                          
         USING TRNRECD,R6                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKULA,POSSICR     SI ACCOUNT                                   
         MVC   TRNKCCPY,TRNKCPY                                                 
         MVC   TRNKULC,POSSICPD    SJ CLT/PRD                                   
         MVC   TRNKOFF,POSOFFIC    OFFICE OF TRANSACTION                        
         BAS   RE,DMHGH                                                         
         B     *+8                                                              
PREP02   BAS   RE,DMSEQ                                                         
         LA    R6,DIR                                                           
         CLC   TRNKEY(TRNKDATE-TRNRECD),DKEY SAME SI  SJ AND OFFICE?            
         BNE   PREP10                                                           
         CLC   TRNKDATE,SPACES     MUST HAVE A TRANSACTION DATE                 
         BE    PREP02                                                           
         CLC   TRNKSMOS,ENDMOS     MUST BE FOR THIS MONTH                       
         BNE   PREP02                                                           
         CLI   TRNKSTYP,55         A TYPE 55                                    
         BNE   PREP02                                                           
         CLC   TRNKREF,REFER       WITH MY REFERENCE NUMBER                     
         BNE   PREP02                                                           
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R6,AIO                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TRNELQ                                                    
         BAS   RE,GETEL            R4 POINTS TO TRANS ELEMENT                   
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNMOS,ENDCHR                                                    
         BNE   PREP02                                                           
         CLC   TRNBREF,BREF        CONFIRM BATCH REFERENCE                      
         BNE   PREP02                                                           
         TM    TRNSTAT,TRNSDR      MUST BE A CREDIT                             
         BO    PREP02                                                           
         AP    PREPST,TRNAMNT      SAVE PREVIOUSLY POSTED                       
         B     PREP02                                                           
*                                                                               
PREP10   SP    POSTING,PREPST      INTENDED POSTING - PREVIOUS = ADJUST         
         B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
*              CALENDAR RECORD                                        *         
***********************************************************************         
*                                                                               
*                                                                               
CALENDAR NTR1                                                                   
         USING CALD,R2                                                          
         L     R2,ACALTAB                                                       
         XC    CALNUM,CALNUM                                                    
         LA    R3,ENDMOS           DO FOR END,LAST PREV AND PRIOR               
         LA    R5,4                                                             
         USING CASRECD,R6                                                       
CAL02    LA    R6,DKEY                                                          
         MVC   CASKEY,SPACES                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,RCCOMPFL                                                 
         MVC   CASKEMOA,0(R3)      PERIOD END                                   
         XC    CASKSMOA,CASKSMOA                                                
         MVC   CASKOFC,SPACES      OFFICE                                       
         BAS   RE,DMHGH                                                         
         CLC   CASKEY(CASKEMOA-CASRECD),DIR                                     
         BE    *+6                                                              
         DC    H'0'                COMPANY  CALENDAR NOT FOUND                  
         CLC   CASKEMOA,0(R3)                                                   
         BNL   *+6                                                              
         DC    H'0'                DEFAULT NOT FOUND                            
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R6,AIO                                                           
*                                                                               
         LA    R4,CASRFST          BUMP TO FIRST ELEMENT                        
         USING TMPELD,R4                                                        
CAL04    CLI   TMPEL,0                                                          
         BE    CAL12                                                            
         CLI   TMPEL,TMPELQ                                                     
         BNE   CAL06                                                            
         CLC   TMPMTH,0(R3)        MATCH ON MONTH                               
         BE    CAL10                                                            
CAL06    SR    R1,R1                                                            
         IC    R1,TMPLN                                                         
         AR    R4,R1                                                            
         B     CAL04                                                            
*                                                                               
CAL10    MVC   CALMOA,TMPMTH       START AT AGEING PERIOD START                 
         MVC   CALPSTR,TMPSTART                                                 
         MVC   CALPEND,TMPEND                                                   
         LH    R0,CALNUM                                                        
         AH    R0,=H'1'                                                         
         STH   R0,CALNUM                                                        
         LA    R2,CALLEN(R2)       BUMP TO NEXT TABLE ENTRY                     
         B     CAL06                                                            
*                                                                               
CAL12    LA    R3,L'ENDMOS(R3)                                                  
         BCT   R5,CAL02                                                         
*                                                                               
CALX     DS    0H                                                               
         B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*----------------------------------------------------                           
*        SEARCH FOR RECORDS FROM BIN TABLES                                     
*               PARM 1   ADDR OF KEY FOR LOOKUP                                 
*               PARM 2   ADDR OF BIND PARMS                                     
*               PARM 3   ADDR OF SAVE AREA FOR RECS                             
*----------------------------------------------------                           
*                                                                               
BINSRC   NTR1                                                                   
         USING BIND,R4                                                          
*                                                                               
         LM    R3,R5,0(R1)                                                      
*                                   R3 - A(ITEM)                                
*                                   R4 - ADDR OF BIND PARMS                     
*                                   R5 - ADDR OF WHERE TO PUT THE REC           
*                                   NUMBER,LENGTH,KEY MAX (3RD PARM)            
         MVC   DMCB+8(BINKLEN),BININ                                            
         LA    R2,BINTABLE          A(TABLE)                                    
         GOTO1 BINSRCH,DMCB,(X'00',(R3)),(R2)                                   
         CLI   DMCB,0                                                           
         BNE   XIT                  NOT FOUND                                   
***      DC    H'0'                                                             
         LTR   R5,R5                                                            
         BZ    XIT                  JUST PASS BACK ADDR OF ENTRY                
         L     R2,DMCB              ADDR OF RECORD FOUND                        
         L     R1,BINLEN            LENGTH OF THE RECORD                        
         BCTR  R1,0                 REDUCED FOR THE EX                          
         EXMVC R1,0(R5),0(R2)                                                   
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*              ROUTINE TO ADD TO A BINSRCH TABLE                                
*              PARAM1              A(RECORD TO BE ADDED)                        
*              PARAM2              A(BINSRCH PARAMS)                            
*---------------------------------------------------------                      
*                                                                               
*                                                                               
BINADD   NTR1                                                                   
         USING BIND,R5                                                          
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         TM    BINSTAT,BINNODUP                                                 
         BNO   *+6                                                              
         DC    H'0'                NO DUPLICATES ALLOWED IN THIS TABL           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         IC    R6,BINFRST          DISP. TO FIRST BUCKET                        
         LTR   R6,R6                                                            
         BZ    BINXIT              NO BUCKETS FOR THIS TABLE                    
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         SR    R0,R0                                                            
         IC    R0,BINNUMB          NUMBER OF BUCKETS                            
         LTR   R0,R0                                                            
         BZ    BINXIT              NO BUCKETS FOR THIS TABLE                    
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,*-14                                                          
BINXIT   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
* GET NAME TO OUTPUT AREA                                                       
*        R4 IS ADDRESS OF THE 20 ELEMENT                                        
*        R2 IS ADDRESS OF 36 BYTE AREA                                          
*----------------------------------------------------------------------         
*                                                                               
         USING ACTRECD,R2                                                       
XNME     NTR1  ,                  EXTRACT THE NAME FROM ACCOUNT RECORD          
         LA    R2,DKEY                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKULA,0(R1)                                                    
         BAS   RE,DMRD                                                          
         CLC   DKEY,DIR                    DID WE FIND IT?                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETR                   GET CURRENT FILE RECORD              
         L     R4,AIO                                                           
         MVI   ELCODE,NAMELQ                                                    
         BAS   RE,GETEL                                                         
         LA    R2,WORK                                                          
         BAS   RE,GETNAME                                                       
         B     XIT                                                              
*                                                                               
         USING NAMELD,R4                                                        
GETNAME  MVC   0(36,R2),SPACES                                                  
         CLI   0(R4),NAMELQ        MUST BE A NAME EL                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R2),NAMEREC    MOVE TO OUTPUT AREA                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD AND ELEMENT TO THE RECORD    R4 = ELEMENT    R2 = RECORD        *         
* ADDL SORTS THE ELEMENTS                                             *         
* ADDL2 ADDS ELEMENTS TO END OF RECORD                                *         
***********************************************************************         
*                                                                               
ADDL     LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R4)                               
         LR    RE,R0                                                            
         BR    RE                                                               
ADDL2    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R4),=C'ADD=END'                   
         LR    RE,R0                                                            
         BR    RE                                                               
         GETEL R4,DATADIS2,ELCODE                                               
*                                                                               
         EJECT                                                                  
*----------------------------------------------------------                     
*        PUT POSTINGS TO WORKER FILE                                            
*        NOTE:ACCRUAL POSTING AMOUNT IS SAVED AS A MINUS                        
*----------------------------------------------------------                     
*                                                                               
POSTIT   NTR1                                                                   
*                                                                               
         CLI   QOPT1,C'L'                  LIVE RUN                             
         BNE   XIT                                                              
         L     RF,APOSLST                                                       
         USING BIND,RF                                                          
         L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         BP    *+6                                                              
         DC    H'0'                                                             
         USING POSTD,R5                                                         
         LA    R5,BINTABLE                                                      
         DROP  RF                                                               
*                                                                               
*                                          T Y P E  55  (THIS MONTH)            
*                                          SB CREDIT                            
POS02    L     RE,AIO2                     RECEIVING FIELD                      
         LH    RF,=Y(MXRLNQ)               LENGTH                               
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         L     R6,AIO2                                                          
         USING TRNRECD,R6                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKULA,POSSBDR     SB ACCOUNT                                   
         MVC   TRNKCCPY,TRNKCPY                                                 
         MVC   TRNKULC(5),POSSICPD    SJ CLT                                    
         MVC   TRNKOFF,POSOFFIC    OFFICE OF TRANSACTION                        
         MVC   TRNKDATE,TRANDATE                                                
         MVC   TRNKREF,REFER                                                    
         XC    TRNKSBR,TRNKSBR                                                  
         MVC   TRNRSMOS,ENDMOS                                                  
         MVI   TRNRSTYP,55                                                      
         LH    R1,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  R1,3,TRNRLEN                                                     
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  44 ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING TRNELD,R4                                                        
         MVI   TRNEL,TRNELQ                X'44'                                
         LH    R1,=Y(TRNLN1Q+L'NARRATIV)   LENGTH                               
         STC   R1,TRNLN                                                         
         MVC   TRNDATE,TRNKDATE            DATE                                 
         MVC   TRNREF,TRNKREF              REFERENCE                            
         MVC   TRNSUB,TRNKSBR              SUB REF SAME AS KEY                  
         MVC   TRNMOS,ENDCHR               MOS AS PART OF BATCH REF             
         MVC   TRNANAL,TRNKWORK            OFFICE                               
         MVC   TRNBREF,BREF                BATCH REFERENCE                      
         MVC   TRNTYPE,TRNRSTYP            TRANSACTION TYPE                     
         XC    TRNSTAT,TRNSTAT             CREDIT                               
         ZAP   TRNAMNT,POSAMNT             AMOUNT                               
         MP    TRNAMNT,=P'-1'              MAKE IT A POSITVE CREDIT             
         MVC   TRNNARR(L'NARRATIV),NARRATIV  NARRATIVE                          
         LR    R2,R6                                                            
         BAS   RE,ADDL2                    ADD THE ELEMENT                      
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  1A ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING MDTELD,R4                                                        
         MVI   MDTEL,MDTELQ                X'1A'                                
         MVI   MDTLN,MDTLNQ                                                     
         MVC   MDTSYS(2),POSSICR+2         1ST 2 CHARS OF SI ACCOUNT            
         MVC   MDTCLI(6),POSSICPD+2        CLIENT PRODUCT                       
         MVC   MDTMOS,ENDMOS               MOS                                  
         MVC   MDTDSCP,SPACES                                                   
         ZAP   DUB,POSAMNT                                                      
         CVB   RF,DUB                                                           
         STCM  RF,15,MDTCOM                COMMISSION                           
         MP    DUB,=P'-1'                                                       
         CVB   RF,DUB                                                           
         STCM  RF,15,MDTNET                NET                                  
         LR    R2,R6                                                            
         BAS   RE,ADDL2                    ADD THE ELEMENT                      
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(5),POSSICPD            GET CLIENT NAME                      
         LA    R1,WORK                                                          
         BAS   RE,XNME                                                          
         MVC   CONTRANM,WORK                                                    
*                                                                               
         BAS   RE,WORKIT                   ADD RECORD TO WORKER FILE            
*                                                                               
*                                          T Y P E  55  (THIS MONTH)            
*                                          SI MINUS CREDIT                      
         L     RE,AIO2                     RECEIVING FIELD                      
         LH    RF,=Y(MXRLNQ)               LENGTH                               
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO2                                                          
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKULA,POSSICR     SI ACCOUNT                                   
         MVC   TRNKCCPY,TRNKCPY                                                 
         MVC   TRNKULC,POSSICPD    SJ CLT/PRD                                   
         MVC   TRNKOFF,POSOFFIC    OFFICE OF TRANSACTION                        
         MVC   TRNKDATE,TRANDATE                                                
         MVC   TRNKREF,REFER                                                    
         XC    TRNKSBR,TRNKSBR                                                  
         MVC   TRNRSMOS,ENDMOS                                                  
         MVI   TRNRSTYP,55                                                      
         LH    R1,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  R1,3,TRNRLEN                                                     
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  44 ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING TRNELD,R4                                                        
         MVI   TRNEL,TRNELQ                X'44'                                
         LH    R1,=Y(TRNLN1Q+L'NARRATIV)                                        
         STC   R1,TRNLN                                                         
         MVC   TRNDATE,TRNKDATE            DATE                                 
         MVC   TRNREF,TRNKREF              REFERENCE                            
         MVC   TRNSUB,TRNKSBR              SUB REF SAME AS KEY                  
         MVC   TRNMOS,ENDCHR               MOS AS PART OF BATCH REF             
         MVC   TRNANAL,TRNKWORK            OFFICE                               
         MVC   TRNBREF,BREF                BATCH REFERENCE                      
         MVC   TRNTYPE,TRNRSTYP            TRANSACTION TYPE                     
         XC    TRNSTAT,TRNSTAT             CREDIT                               
         ZAP   TRNAMNT,POSAMNT             AMOUNT                               
         MVC   TRNNARR(L'NARRATIV),NARRATIV  NARRATIVE                          
         LR    R2,R6                                                            
         BAS   RE,ADDL2                    ADD THE ELEMENT                      
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  50 ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING SCIELD,R4                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGRSS            GROSS MEMO                           
         ZAP   SCIGRS,ZEROS                ZERO                                 
         LR    R2,R6                                                            
         BAS   RE,ADDL2                                                         
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  1A ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING MDTELD,R4                                                        
         MVI   MDTEL,MDTELQ                X'1A'                                
         MVI   MDTLN,MDTLNQ                                                     
         MVC   MDTSYS(2),POSSICR+2         1ST 2 CHARS OF SI ACCOUNT            
         MVC   MDTCLI(6),POSSICPD+2        CLIENT PRODUCT                       
         MVC   MDTMOS,ENDMOS               MOS                                  
         MVC   MDTDSCP,SPACES                                                   
         ZAP   DUB,POSAMNT                                                      
         CVB   RF,DUB                                                           
         STCM  RF,15,MDTCOM                COMMISSION                           
         MP    DUB,=P'-1'                                                       
         CVB   RF,DUB                                                           
         STCM  RF,15,MDTNET                NET                                  
         LR    R2,R6                                                            
         BAS   RE,ADDL2                    ADD THE ELEMENT                      
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  C0 ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING APEELD,R4                                                        
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,51                                                         
         MVI   APENUM,3                                                         
         LA    R1,APENTRY                                                       
         USING APENTRY,R1                                                       
         MVI   APENLEN,16                                                       
         XC    APENSTAT,APENSTAT           CREDIT TO                            
         MVC   APENACT,POSSBDR             SB ACCOUNT                           
         LA    R1,16(R1)                                                        
         MVI   APENLEN,16                                                       
         MVI   APENSTAT,APENSDR            DEBIT TO                             
         MVC   APENACT,POS1CDR             1C ACCOUNT                           
         LA    R1,16(R1)                                                        
         MVI   APENLEN,16                                                       
         XC    APENSTAT,APENSTAT           CREDIT TO                            
         MVC   APENACT,POS12CR             12 ACCOUNT                           
         LR    R2,R6                                                            
         BAS   RE,ADDL2                                                         
*                                                                               
         LA    R1,POSSICPD                 GET PRODUCT NAME                     
         BAS   RE,XNME                                                          
         MVC   CONTRANM,WORK                                                    
*                                                                               
         BAS   RE,WORKIT                   ADD RECORD TO WORKER FILE            
*                                                                               
         L     RE,AIO2                     RECEIVING FIELD                      
         LH    RF,=Y(MXRLNQ)               LENGTH                               
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                          T Y P E  55  (THIS MONTH)            
*                                          1C MINUS DEBIT                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKULA,POS1CDR             1C ACCOUNT                           
         MVC   TRNKCCPY,TRNKCPY                                                 
         MVC   TRNKULC,POS12CR             12 ACCOUNT                           
         MVC   TRNKOFF,POSOFFIC            OFFICE OF TRANSACTION                
         MVC   TRNKDATE,TRANDATE                                                
         MVC   TRNKREF,REFER                                                    
         XC    TRNKSBR,TRNKSBR                                                  
         MVC   TRNRSMOS,ENDMOS                                                  
         MVI   TRNRSTYP,55                                                      
         LH    R1,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  R1,3,TRNRLEN                                                     
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  44 ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING TRNELD,R4                                                        
         MVI   TRNEL,TRNELQ                X'44'                                
         LH    R1,=Y(TRNLN1Q+L'NARRATIV)                                        
         STC   R1,TRNLN                                                         
         MVC   TRNDATE,TRNKDATE            DATE                                 
         MVC   TRNREF,TRNKREF              REFERENCE                            
         MVC   TRNSUB,TRNKSBR              SUB REF SAME AS KEY                  
         MVC   TRNMOS,ENDCHR               MOS AS PART OF BATCH REF             
         MVC   TRNANAL,TRNKWORK            OFFICE                               
         MVC   TRNBREF,BREF                BATCH REFERENCE                      
         MVC   TRNTYPE,TRNRSTYP            TRANSACTION TYPE                     
         MVI   TRNSTAT,TRNSDR              DEBIT                                
         ZAP   TRNAMNT,POSAMNT             AMOUNT                               
         MVC   TRNNARR(L'NARRATIV),NARRATIV  NARRATIVE                          
         LR    R2,R6                                                            
         BAS   RE,ADDL2                    ADD THE ELEMENT                      
*                                                                               
         L     R2,AGENWRK                                                       
         USING SID,R2                                                           
         MVC   SIKEY(SILEN),SPACES                                              
         MVC   SIACCT,POSSICR                                                   
         GOTO1 BINSRC,DMCB,(R2),ASILST,(R2)                                     
         CLC   SI12ACT,POS12CR                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CONTRANM,SI12NME            12 NAME                              
         BAS   RE,WORKIT                   ADD RECORD TO WORKER FILE            
*                                                                               
*                                                                               
         L     RE,AIO2                     RECEIVING FIELD                      
         LH    RF,=Y(MXRLNQ)               LENGTH                               
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                          T Y P E  55  (THIS MONTH)            
*                                          12 MINUS CREDIT                      
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKULA,POS12CR             12 ACCOUNT                           
         MVC   TRNKCCPY,TRNKCPY                                                 
         MVC   TRNKULC,POS1CDR             1C ACCOUNT                           
         MVC   TRNKOFF,POSOFFIC            OFFICE OF TRANSACTION                
         MVC   TRNKDATE,TRANDATE                                                
         MVC   TRNKREF,REFER                                                    
         XC    TRNKSBR,TRNKSBR                                                  
         MVC   TRNRSMOS,ENDMOS                                                  
         MVI   TRNRSTYP,55                                                      
         LH    R1,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  R1,3,TRNRLEN                                                     
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  44 ELEMENT                    
         LA    R4,ELEMENT                                                       
         USING TRNELD,R4                                                        
         MVI   TRNEL,TRNELQ                X'44'                                
         LH    R1,=Y(TRNLN1Q+L'NARRATIV)                                        
         STC   R1,TRNLN                                                         
         MVC   TRNDATE,TRNKDATE            DATE                                 
         MVC   TRNREF,TRNKREF              REFERENCE                            
         MVC   TRNSUB,TRNKSBR              SUB REF SAME AS KEY                  
         MVC   TRNMOS,ENDCHR               MOS AS PART OF BATCH REF             
         MVC   TRNANAL,TRNKWORK            OFFICE                               
         MVC   TRNBREF,BREF                BATCH REFERENCE                      
         MVC   TRNTYPE,TRNRSTYP            TRANSACTION TYPE                     
         XC    TRNSTAT,TRNSTAT             CREDIT                               
         ZAP   TRNAMNT,POSAMNT             AMOUNT                               
         MVC   TRNNARR(L'NARRATIV),NARRATIV  NARRATIVE                          
         LR    R2,R6                                                            
         BAS   RE,ADDL2                    ADD THE ELEMENT                      
*                                                                               
*                                                                               
*                                          GET 1C NAME                          
         LA    R1,POS1CDR                                                       
         BAS   RE,XNME                                                          
         MVC   CONTRANM,WORK                                                    
         BAS   RE,WORKIT                   ADD RECORD TO WORKER FILE            
*                                                                               
         LA    R5,POSLEN(R5)                                                    
         BCT   R3,POS02                                                         
         DROP  R2,R4,R6                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD RECORD TO WORKER FILE                                           *         
***********************************************************************         
*                                                                               
WORKIT   NTR1  ,                                                                
         USING TRNRECD,R6                                                       
         L     R6,AIO2                                                          
         LA    R3,TRNRFST                                                       
         CLI   0(R3),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNELD,R3                                                        
         LA    RE,PCREDITS                 KEEP TRACK OF TOTAL DR CR            
         TM    TRNSTAT,TRNSDR                                                   
         BNO   *+14                                                             
         AP    POSTCASH,TRNAMNT            UPDATE TOTAL FOR WORKFILE            
         LA    RE,PDEBITS                                                       
         AP    0(L'PDEBITS,RE),TRNAMNT                                          
*                                                                               
         USING PSHEADD,R4          BUILD POSTING ELEMENT                        
         LA    R4,T                                                             
         XC    T-4(4),T-4                                                       
         MVI   PSHDEL,PSHDELQ                                                   
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,TRNKCULA    ACCOUNT                                      
         MVC   PSHDANAL,TRNKWORK                                                
         MVC   PSHDSBAC,TRNKCULC   CONTRA                                       
         MVC   PSHDSBNM,CONTRANM   CONTRA NAME                                  
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
WORKIT2  SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         SH    R1,=H'1'                                                         
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)      MOVE ELEMENTS TO AREA ONE AT A TIME           
         LA    R4,1(R1,R4)        R4 TO NEXT OUTPUT AREA                        
         MVI   0(R4),0            END OF RECORD                                 
         LA    R3,1(R1,R3)        R3 TO NEXT ELEMENT                            
         CLI   0(R3),0                                                          
         BNE   WORKIT2                                                          
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
*----------------------------------------------------------                     
*        GET OFFICE NAME FROM TABLE                                             
*----------------------------------------------------------                     
*                                                                               
GETOFF   NTR1  ,                                                                
         MVC   WORK,SPACES                                                      
         L     R2,AGENWRK                                                       
         USING OFFD,R2                                                          
         MVC   OFFD(OFFLEN),SPACES                                              
         MVC   OFFCKEY(L'OFFICE),OFFICE                                         
         GOTO1 BINSRC,DMCB,(R2),AOFFLST,(R2)                                    
         MVC   WORK(L'OFFNAME),OFFNAME                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*----------------------------------------------------------                     
*        ACQUIRE CORE FOR BINARY TABLES                                         
*----------------------------------------------------------                     
*                                                                               
MAIN     NTR1                                                                   
         L     R0,=A(LENBUFF)            ACQUIRE AN ADDITIONAL BUFFER           
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF                  SAVE A(BUFFER)                         
         LR    R2,R1                     R2=BUFFER POINTER                      
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         STCM  R2,15,MCUSRDMP            PRINT THE BUFFER IN A DUMP             
         LR    RF,R2                                                            
         A     RF,=A(LENBUFF)                                                   
         STCM  RF,15,MCUSRDMP+4          END OF DUMP AREA                       
         DROP  RE                                                               
*                                        INIT BINARY TABLES                     
         USING MAIND,R3                                                         
         USING BIND,R2                                                          
         LA    R0,MAINNUM                NUMBER OF BUFFERS NEEDED               
         L     R3,AMAINTAB               TABLE OF BINARY PARMS                  
MAIN00   MVC   0(L'MAINEYE,R2),MAINEYE   SEED TABLE EYE CATCHER                 
         LA    R2,L'MAINEYE(R2)                                                 
         SR    RE,RE                                                            
         ICM   RE,3,MAINDSP              DISP TO TAB ADDR IN WORK STOR          
         AR    RE,RC                     ADD ADDR OF WORKING STORAGE            
         ST    R2,0(RE)                  SAVE ADDR OF TABLE                     
         XC    BININ,BININ               BUILD BIN TABLE PARMS                  
         MVC   BINLEN,MAINLEN                                                   
         MVC   BINDISPK,MAINDISK                                                
         MVC   BINMAX,MAINMAX                                                   
         MVC   BINNUMB,MAINNUMB                                                 
         MVC   BINFRST,MAINFRST                                                 
         MVC   BINSTATS,MAINSTAT                                                
         A     R2,MAINSIZE               BUMP BY LENGTH OF TABLE                
         LA    R3,MAINLNQ(R3)                                                   
         BCT   R0,MAIN00                                                        
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
*                                                                               
DUMP     CP    DMPCNT,DMPMAX       TEST MAXIMUM DUMPS                           
         BHR   RE                                                               
         AP    DMPCNT,=P'1'                                                     
DUMPER   DS    0H                                                               
         NTR1  ,                                                                
         LA    R3,T-4                                                           
         SR    R4,R4                                                            
         ICM   R4,3,T-4                                                         
*                                                                               
DUMP3    LA    R5,=C'2D'                                                        
         LA    R6,=C'PUT'                                                       
         GOTO1 ,DMCB,(3,(R6)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)               
         GOTO1 PRNTBL                                                           
         B     XIT                                                              
***********************************************************************         
*              PUT WORKER RECORD TO ACPOST                                      
***********************************************************************         
*                                                                               
PUTIT    NTR1  ,                                                                
         AP    POSTREC,=P'1'                                                    
         LA    R2,T                                                             
         SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
PUT2     AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    PUT4                                                             
         SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   PUT2                                                             
         MVI   0(R2),0                                                          
PUT4     LA    R2,1(R2)                                                         
         LA    R3,T-4                                                           
         SR    R2,R3                                                            
         STH   R2,T-4                                                           
         BAS   RE,ADDPOST                                                       
         CLI   QOPT7,C'D'          DUMPS                                        
         BNE   XIT                                                              
         BAS   RE,DUMP                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              WORKER INTERFACE                                                 
***********************************************************************         
*                                                                               
*                                                                               
OPNPOST  MVC   COMMAND,=CL6'OPEN'                                               
         B     FILE                                                             
*                                                                               
ADDPOST  MVC   COMMAND,=CL6'ADD'                                                
         B     FILE                                                             
*                                                                               
CLOSPOST MVC   COMMAND,=CL6'CLOSE'                                              
         B     FILE                                                             
*                                                                               
FILE     NTR1  ,                                                                
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         CLI   QOPT1,C'L'                                                       
         BNE   XIT                                                              
         LA    R3,T-4                                                           
         L     R4,POSTBUFF                                                      
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
*                                                                               
*                                                                               
DMRD     LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMWRTR,ACCDIR,DIR,DIR                               
         B     DMERR                                                            
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*------------------------------------------------------------*                  
*              CONSTANTS                                                        
*------------------------------------------------------------*                  
*                                                                               
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
FOXES    DC    30X'FF'                                                          
DMPCNT   DC    PL2'0'              DUMP COUNT - ERRORS                          
DMPMAX   DC    PL2'50'             MAX DUMP COUNT                               
ZEROS    DC    PL8'0'                                                           
REFER    DC    CL6'ACCRU '         TRANSACTION REFERENCE                        
BREF     DC    CL4'ACR8'           BATCH REFERENCE                              
NARRATIV DC    CL17'AUTOMATIC ACCRUAL'                                          
*                                                                               
ABUFC    DC    A(BUFFALOC)                                                      
AMAINTAB DC    A(MAINTAB)                                                       
AOFFTAB  DC    A(OFFTAB)                                                        
ACALTAB  DC    A(CALTAB)                                                        
ATOTDES  DC    A(TOTDES)                                                        
AACCRU   DC    A(ACCRU)                                                         
AIO      DC    A(IO)                                                            
AIO2     DC    A(IO2)                                                           
POSTBUFF DC    A(POSTBUF)                                                       
ABILBUF  DC    A(BILBUF)                                                        
AASOFBLK DC    A(ASOFBLK)                                                       
ASRTWRK  DC    A(SRTWRK)                                                        
AGENWRK  DC    A(GENWRK)                                                        
APOSTWRK DC    A(POSTWRK)                                                       
ACASOF   DC    V(ACASOF)                                                        
         DC    X'FF'                                                            
*                                                                               
HELLO    DC    V(HELLO)            HELLO                                        
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1 '                    
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,) '                            
         EJECT                                                                  
*--------------------------------------------------------------------*          
*                    LITERALS                                                   
*--------------------------------------------------------------------*          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
*        MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+(PLL-PLD),C'L'                                           
         MVI   BOXCOLS+(PLC1-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC2-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC3-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC4-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC5-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC6-PLD),C'C'                                          
         MVI   BOXCOLS+(PLR-PLD),C'R'                                           
BX200    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*        BUFFALO CSECT                                                          
*--------------------------------------------------------------------           
         BUFF  FLAVOR=PACKED,                                          X        
               KEYLIST=(11,A),                                         X        
               COMMENT=0,                                              X        
               LINES=100,                                              X        
               COLUMNS=3,                                              X        
               ROWS=1                                                           
*--------------------------------------------------------------------           
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*        COVERED BY MAIND                                                       
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
*                                                                               
         DC    CL8'*OFFLST*'                                                    
         DC    AL2(AOFFLST-ACR8D)  STORED ADDR OF TABLE                         
         DC    H'0'                ALIGNMENT                                    
         DC    A(OFFLEN)           LENGTH OF RECORD                             
         DC    A(OFFKLEN)          LENGTH OF KEY                                
         DC    A(OFFMAX)           MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL2(0)              TAB STATUS                                   
         DC    A(OFFSIZE)          SIZE OF TABLE                                
*                                                                               
         DC    CL8'*POSLST*'                                                    
         DC    AL2(APOSLST-ACR8D)  STORED ADDR OF TABLE                         
         DC    H'0'                ALIGNMENT                                    
         DC    A(POSLEN)           LENGTH OF RECORD                             
         DC    A(POSKLEN)          LENGTH OF KEY                                
         DC    A(POSMAX)           MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL1(BINNODUP)       TAB STATUS                                   
         DC    AL1(0)              TAB STATUS                                   
         DC    A(POSSIZE)          SIZE OF TABLE                                
*                                                                               
         DC    CL8'*SILST**'                                                    
         DC    AL2(ASILST-ACR8D)   STORED ADDR OF TABLE                         
         DC    H'0'                ALIGNMENT                                    
         DC    A(SILEN)            LENGTH OF RECORD                             
         DC    A(SIKLEN)           LENGTH OF KEY                                
         DC    A(SIMAX)            MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    AL2(0)              TAB STATUS                                   
         DC    A(SISIZE)           SIZE OF TABLE                                
*                                                                               
MAINNUM  EQU   (*-MAINTAB)/MAINLNQ                                              
*                                                                               
OFFMAX   EQU   300                                                              
OFFSIZE  EQU   L'MAINEYE+BINLENQ+(OFFMAX*OFFLEN)                                
POSMAX   EQU   5000                                                             
POSSIZE  EQU   L'MAINEYE+BINLENQ+(POSMAX*POSLEN)                                
SIMAX    EQU   255                                                              
SISIZE   EQU   L'MAINEYE+BINLENQ+(SIMAX*SILEN)                                  
*                                                                               
LENBUFF  EQU   OFFSIZE+POSSIZE+SISIZE                                           
         EJECT                                                                  
*        TABLE OF VALID OFICES FOR THIS REQUEST                                 
OFFTAB   DS    (OFFMAX*2)C                                                      
OFFTABLN EQU   *-OFFTAB                                                         
         DC    X'FF'                                                            
*                                                                               
*        TABLE OF CALENDAR START/END DATE                                       
CALTAB   DS    (CALLEN*300)C                                                    
CALTABLN EQU   *-CALTAB                                                         
         DC    X'FF'                                                            
*                                                                               
*        TABLE OF TOTAL DESCRIPTION                                             
TOTDES   DS    0F                                                               
         DC    AL2(TPRODUCT-ACR8D),CL17'                '                       
         DC    AL2(TCLIENT-ACR8D),CL17' *TOTAL CLIENT*  '                       
         DC    AL2(TOFFICE-ACR8D),CL17' *TOTAL OFFICE*  '                       
         DC    AL2(TREQUEST-ACR8D),CL17'*TOTAL REQUEST* '                       
TOTDNUM  EQU   (*-TOTDES)/(TOTDLNQ)                                             
*                                                                               
*                                                                               
*        TABLE OF ACCRUAL CALCS                                                 
ACCRU    DS    0F                                                               
         DC    AL2(TPRD1U-ACR8D),AL2(TPRD1A-ACR8D),PL6'000000'                  
         DC    AL2(TPRD2U-ACR8D),AL2(TPRD2A-ACR8D),PL6'000000'                  
         DC    AL2(TPRD3U-ACR8D),AL2(TPRD3A-ACR8D),PL6'100000'                  
         DC    AL2(TPRD4U-ACR8D),AL2(TPRD4A-ACR8D),PL6'100000'                  
ACCRUNUM EQU   (*-ACCRU)/(ACCRULNQ)                                             
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
*                                                                               
         DC    F'0'                      IOAREA #1                              
IO       DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    F'0'                      IOAREA #2                              
IO2      DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    F'0'                      IOAREA #2                              
POSTBUF  DC    4500X'00'                                                        
*                                                                               
*        BILL BUFFER AS OF BLOCK FOR ASOF                                       
BILBUF   DS    (ACABTBSZ)C                                                      
ASOFBLK  DS    (ACASOFLN)C                                                      
*                                                                               
*        SORT RECORD WORK AREAS                                                 
SRTWRK   DS    CL(SRTLEN)          SORT RECORD                                  
GENWRK   DS    CL200               GENERAL WORK RECORD                          
POSTWRK  DS    CL(POSLEN)          POSTING WORK RECORD                          
         EJECT                                                                  
*-----------------------------------------------                                
*        EQUATES                                                                
*-----------------------------------------------                                
*                                                                               
MAXLEN   EQU   12                  MAX LENGTH OF ACCOUNT                        
MXRLNQ   EQU   2000                MAX LENGTH OF RECORD                         
SPACE    EQU   X'40'                                                            
         EJECT                                                                  
*              DSECT FOR STORAGE AREA                                           
         SPACE 1                                                                
ACR8D    DSECT                                                                  
AOFFLST  DS    A                                                                
APOSLST  DS    A                                                                
ASILST   DS    A                                                                
ADBOX    DS    A                                                                
SAVERE   DS    A                                                                
ABUFF    DS    A                   ADDRESS OF GETMAINED BUFFER                  
COMMAND  DS    CL6                                                              
ID       DS    CL16                WORKER FILE ID AREA                          
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
DATADIS2 DS    H                                                                
OFFCLEN  DS    CL1                                                              
ELCODE   DS    CL1                                                              
R8MODE   DS    CL21                LIVE OR DRAFT FOR HEADLINES                  
INCONLY  DS    CL1                 INCOME REPORT ONLY SWITCH                    
*                                                                               
ACTSW    DS    XL1                 ACTIVITY SWITCH                              
ACTCLI   EQU   X'80'               CLIENT                                       
ACTPRD   EQU   X'40'               PRODUCT                                      
ACTJOB   EQU   X'20'               JOB                                          
*                                                                               
MNTH     DS    XL2                 CURRENT TRANSACTION MONTH                    
CALNUM   DS    H                   NUMBER OF CALENDAR ENTRIES                   
STRMOS   DS    XL2                 START MOS PACKED YM                          
*                                                                               
ENDMOS   DS    XL2                 END MOS PACKED YM                            
LASTMOS  DS    XL2                 LAST MONTH PACKED YM                         
PRVMOS   DS    XL2                 PREVIOUS MONTH                               
PRIMOS   DS    XL2                 AND PRIOR                                    
*                                                                               
ENDMON   DS    CL6                 END MONTH MMM/YY                             
LASTMON  DS    CL6                                                              
PRVMON   DS    CL6                                                              
PRIMON   DS    CL6                                                              
*                                                                               
NEXTMOS  DS    XL2                 NEXT MONTH PACKED YM                         
NEXTMON  DS    CL6                 NEXT MONTH MMM/YY                            
NEXTCHR  DS    CL2                                                              
ENDCHR   DS    CL2                                                              
*                                                                               
ACTSTART DS    XL2                 SAVED ACTIVITY START                         
ACTEND   DS    XL2                 SAVED ACTIVITY END                           
TRANDATE DS    XL3                 YYMMDD TRANSACTION DATE FOR POSTING          
DIVWRK   DS    PL16                FOR MULTIPLYING RATE                         
ALSORT   DS    A                   A(LAST SORT RECORD)                          
CLIENTOF DS    CL2                 CLIENT OFFICE FOR HEADLINES                  
CLIOFFNM DS    CL36                NAME OF OFFICE FOR HEADLINES                 
CONTRANM DS    CL36                CONTRA NAME                                  
OFFICE   DS    CL2                 OFFICE FOR GETOFF                            
COSTING  DS    CL12                COSTING ACCOUNT                              
SBACCT   DS    CL15                SB ACCOUNT FOR POSTING                       
ACCOUNT  DS    0CL12               CLI/PRD/JOB                                  
CLTPRDSV DS    0CL6                CLI/PRD                                      
CLTSV    DS    CL3                 CLI                                          
PRDSV    DS    CL3                 PRD                                          
JOBSV    DS    CL6                 JOB                                          
*                                                                               
MIDS     DS    XL1                 MIDLINE CONTROL                              
TOTAL    DS    PL8                                                              
POSTING  DS    PL8                 INTENDED POSTING AMOUNT                      
PREPST   DS    PL8                 PREVIOUSLY POSTED AMOUNT                     
*                                                                               
TOTALS   DS    PL6                                                              
TOTBKLN  EQU   *-TOTALS                                                         
         ORG   TOTALS                                                           
TPRODUCT DS    0X                   ***PRODUCT TOTALS***                        
TPRD1U   DS    PL(TOTBKLN)          TOTAL PROD UNBILLED PERIOD 1                
TPRD2U   DS    PL(TOTBKLN)                                                      
TPRD3U   DS    PL(TOTBKLN)                                                      
TPRD4U   DS    PL(TOTBKLN)                                                      
TPRD1A   DS    PL(TOTBKLN)          TOTAL PROD ACCRUAL PERIOD 1                 
TPRD2A   DS    PL(TOTBKLN)                                                      
TPRD3A   DS    PL(TOTBKLN)                                                      
TPRD4A   DS    PL(TOTBKLN)                                                      
TPRDBNUM EQU   (*-TPRD1U)/(TOTBKLN) NUMBER OF ACCUMS AT THIS LEVEL              
TPRDBCLR EQU   (*-TOTALS)/(TOTBKLN) NUMBER TO CLEAR IF LEVEL CHANGES            
*                                                                               
TCLIENT  DS    0X                   ***CLIENT TOTALS***                         
TOTHIGH  DS    0X                                                               
TCLI1U   DS    PL(TOTBKLN)          TOTAL CLIENT UNBILLED PERIOD 1              
TCLI2U   DS    PL(TOTBKLN)                                                      
TCLI3U   DS    PL(TOTBKLN)                                                      
TCLI4U   DS    PL(TOTBKLN)                                                      
TCLI1A   DS    PL(TOTBKLN)          TOTAL CLIENT ACCRUAL PERIOD 1               
TCLI2A   DS    PL(TOTBKLN)                                                      
TCLI3A   DS    PL(TOTBKLN)                                                      
TCLI4A   DS    PL(TOTBKLN)                                                      
TCLIBNUM EQU   (*-TCLI1U)/(TOTBKLN) NUMBER OF ACCUMS AT LEVEL                   
TCLIBCLR EQU   (*-TOTALS)/(TOTBKLN) NUMBER TO CLEAR IF LEVEL CHANGES            
*                                                                               
TOFFICE  DS    0X                   ***OFFICE TOTALS***                         
TOFF1U   DS    PL(TOTBKLN)          TOTAL OFFICE UNBILLED PERIOD 1              
TOFF2U   DS    PL(TOTBKLN)                                                      
TOFF3U   DS    PL(TOTBKLN)                                                      
TOFF4U   DS    PL(TOTBKLN)                                                      
TOFF1A   DS    PL(TOTBKLN)          TOTAL OFFICE ACCRUAL PERIOD 1               
TOFF2A   DS    PL(TOTBKLN)                                                      
TOFF3A   DS    PL(TOTBKLN)                                                      
TOFF4A   DS    PL(TOTBKLN)                                                      
TOFFBNUM EQU   (*-TOFF1U)/(TOTBKLN)                                             
TOFFBCLR EQU   (*-TOTALS)/(TOTBKLN) NUMBER TO CLEAR IF LEVEL CHANGES            
*                                                                               
TREQUEST DS    0X                   ***REQUEST TOTALS***                        
TREQ1U   DS    PL(TOTBKLN)          TOTAL REQUEST UNBILLED PERIOD 1             
TREQ2U   DS    PL(TOTBKLN)                                                      
TREQ3U   DS    PL(TOTBKLN)                                                      
TREQ4U   DS    PL(TOTBKLN)                                                      
TREQ1A   DS    PL(TOTBKLN)          TOTAL REQUEST ACCRUAL PERIOD 1              
TREQ2A   DS    PL(TOTBKLN)                                                      
TREQ3A   DS    PL(TOTBKLN)                                                      
TREQ4A   DS    PL(TOTBKLN)                                                      
TREQBNUM EQU   (*-TREQ1U)/(TOTBKLN)                                             
TOTBKNUM EQU   (*-TOTALS)/(TOTBKLN)                                             
TOTBKHI  EQU   ((*-TOTHIGH)/(TOTBKLN))/(TPRDBNUM)                               
*                                                                               
*              BUFFALO RECORDS                                                  
BUFKEY   DS    0C                                                               
BUFOFFC  DS    CL2                 CLIENT OFFICE                                
BUFTYPE  DS    CL1                                                              
BUFIN    EQU   0                   CLIENTS IN THIS OFFICE                       
BUFOUT   EQU   1                   CLIENTS OUT OF THIS OFFICE                   
BUFALL   EQU   2                   ALL CLIENTS                                  
BUFPOFC  DS    CL2                 EMPLOYEE OFFICE                              
BUFCLIPD DS    0CL6                CLIENT PRODUCT                               
BUFCLI   DS    CL3                 CLIENT                                       
BUFPRD   DS    CL3                 PRODUCT                                      
BUFKLEN  EQU   *-BUFKEY            KEY LENGTH                                   
BUFBK    DS    PL8                                                              
BUFBKLN  EQU   *-BUFBK             SIZE OF A BUCKET                             
         ORG   BUFBK                                                            
BUFYTD   DS    PL(BUFBKLN)         ALL SI THRU YTD-1 MOA                        
BUFMONTH DS    PL(BUFBKLN)         ALL SI EXCEPT MTH 55'S AND 56'S              
BUFACCRU DS    PL(BUFBKLN)         MONTH'S 55'S AND 56'S                        
BBKCNT   EQU   (*-BUFBK)/BUFBKLN   NUMBER OF BUCKETS                            
BLEN     EQU   *-BUFKEY                                                         
*                                                                               
LASTBUF  DS    CL(L'BUFKEY)        PREVIOUS BUFFALO KEY                         
PDEBITS  DS    PL6                 TOTAL DR BALANCE                             
PCREDITS DS    PL6                 TOTAL CR BALANCE                             
POSTCASH DS    PL6                 TOTAL DR POSTED FOR WORKER TOTALS            
POSTREC  DS    PL6                 NUMBER OF RECORDS ADDED TO WORKER            
         DS    F                                                                
T        DS    CL600               WORKER FILE BUILD AREA                       
ELEMENT  DS    CL256               ELEMENT BUILD AREA                           
         EJECT                                                                  
*              DSECT FOR SORT RECORD                                            
*                                                                               
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTRPT   DS    CL1                 REPORT NUMBER                                
SRTOFC   DS    CL2                 CLIENT OFFICE                                
SRTACC   DS    0CL12                                                            
SRTCLTPD DS    0CL6                                                             
SRTCLT   DS    CL3                 CLIENT                                       
SRTPRD   DS    CL3                 PRODUCT                                      
SRTJOB   DS    CL6                 JOB                                          
SRTKLEN  EQU   *-SRTKEY            SORT KEY LENGTH                              
SRTNME   DS    CL36                RECORD NAME                                  
SRTCST   DS    CL12                COSTING ACCOUNT                              
SRTBUCKS DS    PL6                 LOCATION OF BUCKETS                          
SRTBKLN  EQU   *-SRTBUCKS                                                       
         ORG   SRTBUCKS                                                         
SRTPER1  DS    PL(SRTBKLN)         UNBILLED PERIOD 1                            
SRTPER2  DS    PL(SRTBKLN)                                                      
SRTPER3  DS    PL(SRTBKLN)                                                      
SRTPER4  DS    PL(SRTBKLN)                                                      
SRTBKNUM EQU   (*-SRTBUCKS)/(SRTBKLN) NUMBER OF BUCKETS                         
SRTLEN   EQU   *-SRTD                                                           
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISPK DS    0F                                                               
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLEN  EQU   *-BIND                                                           
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTATS DS    0CL2                                                             
BINSTAT  DS    CL1                                                              
BINNODUP EQU   X'80'               NO DUPLICATE ENTRIES ALLOWED                 
         DS    CL1                 SPARE                                        
BINLENQ  EQU   *-BIND                                                           
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
*                                                                               
*              DSECT FOR MAINTAB FOR ACQUIRED STORAGE (GETMAIN)                 
MAIND    DSECT                                                                  
MAINEYE  DS    CL8                 TABLE EYE CATCHER IN DUMP                    
MAINDSP  DS    AL2                 ADDR TO STORE A(TABLE)                       
         DS    H                   ALIGNMENT                                    
MAINLEN  DS    A                   RECORD LENGTH                                
MAINDISK DS    A                   DISP/KEY LENGTH                              
MAINMAX  DS    A                   MAXIMUM NUMBER IN TABLE                      
MAINNUMB DS    AL1                 NUMBER OF BUCKETS                            
MAINFRST DS    AL1                 DISP TO FIRST BUCKET                         
MAINSTAT DS    AL2                 TABLE STATUS                                 
MAINSIZE DS    A                   TABLE SIZE                                   
MAINLNQ  EQU   *-MAIND                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER REPORT PRINT LINE                                    *         
***********************************************************************         
*                                                                               
PLD      DSECT                                                                  
PLL      DS    X                                                                
PLACCT   DS    CL12                CLT/PRD JOB                                  
PLC1     DS    X                                                                
PLNME    DS    CL36                NAME                                         
PLC2     DS    X                                                                
PLCOL    DS    0CL15                                                            
PLCPER1  DS    CL14                PERIOD 1                                     
PLC3     DS    X                                                                
PLCPER2  DS    CL14                PERIOD 2                                     
PLC4     DS    X                                                                
PLCPER3  DS    CL14                PERIOD 3                                     
PLC5     DS    X                                                                
PLCPER4  DS    CL14                PERIOD 4                                     
PLC6     DS    X                                                                
PLCTOT   DS    CL14                TOTAL                                        
PLR      DS    X                                                                
         ORG   PLD+163                                                          
         EJECT                                                                  
*-----------------------------                                                  
* DSECT FOR TOTAL DESCRIPTION                                                   
*-----------------------------                                                  
TOTDESD  DSECT                                                                  
TOTOFFS  DS    AL2                 OFFSET TO TOTALS                             
TOTDESP  DS    CL17                DESCRIPTION TO PRINT                         
TOTDLNQ  EQU   *-TOTDESD                                                        
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*-----------------------------                                                  
* DSECT FOR ACCRUAL CALC TABLE                                                  
*-----------------------------                                                  
ACCRUD   DSECT                                                                  
ACCRFROM DS    AL2                 OFFSET TO ACCUM TO DO ACCRUAL                
ACCRTO   DS    AL2                 OFFSET TO ACCUM TO PUT ACCRUAL               
ACCRRATE DS    PL6                 ACCRUAL RATE 3 DECIMALS                      
ACCRULNQ EQU   *-ACCRUD                                                         
*                                                                               
*                                                                               
*                                                                               
*-----------------------------                                                  
* DSECT FOR TOTAL ACCUMS                                                        
*-----------------------------                                                  
TOTD     DSECT                                                                  
TOTPER1U DS    PL(TOTBKLN)         PERIOD 1 UNBILLED                            
TOTPER2U DS    PL(TOTBKLN)         PERIOD 2 UNBILLED                            
TOTPER3U DS    PL(TOTBKLN)         PERIOD 3 UNBILLED                            
TOTPER4U DS    PL(TOTBKLN)         PERIOD 4 UNBILLED                            
TOTPER1A DS    PL(TOTBKLN)         PERIOD 1 ACCRUAL                             
TOTPER2A DS    PL(TOTBKLN)         PERIOD 2 ACCRUAL                             
TOTPER3A DS    PL(TOTBKLN)         PERIOD 3 ACCRUAL                             
TOTPER4A DS    PL(TOTBKLN)         PERIOD 4 ACCRUAL                             
TOTPLNQ  EQU   *-TOTD                                                           
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*------------------------*                                                      
* DSECT FOR OFFICE TABLE                                                        
*------------------------*                                                      
OFFD     DSECT                                                                  
OFFCKEY  DS    CL12                                                             
OFFKLEN  EQU   *-OFFCKEY                                                        
OFFNAME  DS    CL36                                                             
OFFLEN   EQU   *-OFFD                                                           
*                                                                               
*                                                                               
*------------------------*                                                      
* DSECT FOR POSTING TABLE                                                       
*------------------------*                                                      
POSTD    DSECT                                                                  
POSKEY   DS    0H                                                               
POSSBDR  DS    CL14                SB DEBIT ACCOUNT(SI CONTRA)                  
POSSICR  DS    0CL14               SI CREDIT ACCOUNT                            
POSSIUL  DS    CL2                 SI UL                                        
POSSIOFC DS    CL2                 SI OFFICE                                    
POSSIRES DS    CL10                SI REST OF ACCOUNT                           
POSSICPD DS    CL14                SI CONTRA (CLT/PRD)                          
POSOFFIC DS    CL2                 TRANSACTION OFFICE                           
POS1CDR  DS    CL14                1C DEBIT ACCOUNT(12 CONTRA)                  
POS12CR  DS    CL14                12 CREDIT(1C CONTRA)                         
POSKLEN  EQU   *-POSKEY                                                         
POSAMNT  DS    PL6                 AMOUNT                                       
POSLEN   EQU   *-POSTD                                                          
*                                                                               
*                                                                               
*                                                                               
*------------------------*                                                      
* DSECT FOR SI ACCOUNTS                                                         
*------------------------*                                                      
SID      DSECT                                                                  
SIKEY    DS    0H                                                               
SIACCT   DS    0CL14               SI POSTING ACCOUNT                           
SIUL     DS    CL2                 U/L                                          
SIOFFC   DS    CL2                 OFFICE                                       
SIREST   DS    CL10                REST OF THE SI ACCOUNT                       
SIKLEN   EQU   *-SIKEY                                                          
SINAME   DS    CL36                NAME                                         
SI12ACT  DS    0CL14               12 U/LACCOUNT                                
SI12UL   DS    CL2                 12 U/L                                       
SI12ACC  DS    CL12                12 ACCOUNT                                   
SI12NME  DS    CL36                NAME                                         
SILEN    EQU   *-SID                                                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*------------------------*                                                      
* DSECT FOR CALENDAR ENTRIES                                                    
*------------------------*                                                      
CALD     DSECT                                                                  
CALPSTR  DS    XL3                 PERIOD START                                 
CALPEND  DS    XL3                 PERIOD END                                   
CALMOA   DS    XL2                 MOA FOR AGEING                               
CALLEN   EQU   *-CALD                                                           
         EJECT                                                                  
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBOXEQUS                                                              
*        ACASOFD                                                                
GETOPTD  DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE ACASOFD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREPR802 05/01/02'                                      
         END                                                                    
