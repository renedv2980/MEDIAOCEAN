*          DATA SET ACREPZN02  AT LEVEL 080 AS OF 06/23/00                      
*PHASE ACZN02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'MATCH HISTORY FILE AGAINST LIVE FILE FOR YNRO'                  
ACZN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZN**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZND,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*********************************************************************           
* RUN FIRST                                                         *           
*********************************************************************           
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* REQUEST FIRST                                                     *           
*********************************************************************           
         SPACE 1                                                                
REQF     DS    0H                                                               
         USING BIND,R2                                                          
         L     R2,ATRNTTAB         TRANSACTION TABLE                            
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,APDTETAB         PEELED DATE TABLE                            
         XC    BININ,BININ         CLEAR BIN TABLE                              
         DROP  R2                                                               
*                                                                               
         XC    LASTREC,LASTREC                                                  
         XC    LASTDTE,LASTDTE                                                  
         ZAP   DMPCNT,=P'0'                                                     
*                                                                               
         XC    DMCB(24),DMCB       OPEN ACCHST                                  
         GOTO1 DATAMGR,DMCB,DMDTF,ACCHST                                        
         MVC   HISTDCB,12(R1)      SAVE ADDRESS OF HISTORY DCB                  
         MVI   TMPWRK,C'N'                                                      
         MVC   TMPWRK+1(7),ACCHST                                               
         MVI   TMPWRK+8,C'X'                                                    
         GOTO1 DATAMGR,DMCB,DMOPEN,ACCFIL,TMPWRK,ACFILEC                        
*                                                                               
         USING ACTRECD,R2                                                       
         LA    R2,SVKEY                                                         
         XC    SVKEY,SVKEY         GET ACCOUNT NAME                             
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKUNT(2),QUNIT    U/L                                          
         MVC   ACTKACT,QACCOUNT    ACCOUNT (IF ANY)                             
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCHST,SVKEY,IOKEY                   
         B     REQF20                                                           
*                                                                               
REQF10   GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),ACCHST,SVKEY,IOKEY                   
REQF20   LA    R1,1                                                             
         CLI   QUNIT,X'40'         ANYTHING IN QUNIT                            
         BNH   *+8                                                              
         AHI   R1,1                ADD IN LENGTH OF UNIT                        
         CLI   QLEDGER,X'40'       ANYTHING IN QLEDGER?                         
         BNH   *+8                                                              
         AHI   R1,1                ADD IN LENGTH OF LEDGER                      
*                                                                               
         CLC   QACCOUNT,SPACES     ANYTHING IN QACCOUNT                         
         BNH   REQF25                                                           
*                                                                               
         LA    RE,QACCOUNT+L'QACCOUNT-1                                         
         LA    RF,L'QACCOUNT                                                    
         CLI   0(RE),X'40'                                                      
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         AR    R1,RF                                                            
*                                                                               
REQF25   AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVKEY(0),IOKEY                                                   
         BNE   REQFX                                                            
*                                                                               
         USING TRNRECD,R4                                                       
         LA    R4,IO                                                            
*                                                                               
         USING CLITABD,RE                                                       
         L     RE,ACLITAB                                                       
         LA    R0,CLITNUM                                                       
         CLC   CLITCDE,TRNKACT     MATCH ON CLIENT CODES                        
         BE    *+16                                                             
         LA    RE,CLITLNQ(RE)                                                   
         BCT   R0,*-14                                                          
         B     REQF10              NOT FOUND - SKIP IT                          
*                                                                               
         MVC   SVOFF,CLITOFF       SAVE OFFICE FOR TABLE                        
         DROP  RE                                                               
*                                                                               
         MVC   MSG,=CL10'TRN REC IN'                                            
         GOTO1 ADUMP,DMCB,(RC),TRNKEY,56                                        
*                                                                               
         LR    RE,R4                                                            
         LA    RE,ACCOPEEL(RE)     BUMP TO PEEL DATE                            
         MVC   SVPDTE,0(RE)        SAVE PEELED DATE?                            
*                                                                               
         CLC   QSTART,SPACES       CHECK FOR START DATE?                        
         BH    *+14                                                             
         XC    WORK(2),WORK        START AT BEGINNING OF TIME                   
         B     REQF27                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(2,WORK)                                  
REQF27   CLC   QEND,SPACES         CHECK FOR END DATE?                          
         BH    *+14                                                             
         MVC   WORK+2(2),=X'FFFF'  END AT INFINATUM                             
         B     REQF28                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(2,WORK+2)                                  
*                                                                               
REQF28   CLC   SVPDTE,WORK         MAKE SURE PEELED DATE IS IN RANGE            
         BL    REQF10                                                           
         CLC   SVPDTE,WORK+2                                                    
         BH    REQF10                                                           
*                                                                               
         CLI   QOPT1,C'Y'          DO THEY WANT BOTH REPORTS?                   
         BE    *+12                                                             
         CLI   QOPT1,C'A'          DO THEY JUST WANT THE TRNSACTION RPT         
         BNE   REQF30                                                           
*                                                                               
*        CLC   LASTREC,0(R4)                                                    
*        BE    REQF10                                                           
*        MVC   LASTREC,0(R4)                                                    
*                                                                               
         USING TRNTD,R3                                                         
         LA    R3,TRNTWRK          TRANSACTION TABLE WORK AREA                  
         XC    TRNTWRK,TRNTWRK                                                  
*                                                                               
         MVC   TRNTKEY(TRNTKLNQ),TRNKULA   SAVE OFF KEY IN BINTABLE             
         MVC   TRNTPDTE,SVPDTE                                                  
         MVC   TRNTOFF,SVOFF               ADD OFFICE TO TABLE                  
*                                                                               
         MVC   MSG,=CL10'BIN IN #1'                                             
         GOTO1 ADUMP,DMCB,(RC),(R3),L'TRNTWRK                                   
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),TRNTWRK,ATRNTTAB ADD TABLE ENTRY               
*                                                                               
REQF30   CLI   QOPT1,C'Y'          DO THEY WANT BOTH REPORTS?                   
         BE    *+12                                                             
         CLI   QOPT1,C'B'          OR JUST WANT THE PEELED DATE RPT             
         BNE   REQF10                                                           
*                                                                               
         LR    RE,R4                                                            
         LA    RE,ACCOPEEL(RE)     BUMP TO PEEL DATE                            
         CLC   0(ACCOPLEN,RE),LASTDTE   SAME PEELED DATE?                       
         BE    REQF10                                                           
         MVC   LASTDTE,0(RE)                                                    
*                                                                               
         USING PDTED,R3                                                         
         LA    R3,PDTEWRK          PEELED DATE TABLE WORK AREA                  
         XC    PDTEWRK,PDTEWRK                                                  
*                                                                               
         MVC   PDTE,0(RE)                                                       
*                                                                               
         MVC   MSG,=CL10'BIN IN #2'                                             
*        SR    R6,R6                                                            
*        ICM   R6,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),L'PDTEWRK                                   
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),PDTEWRK,APDTETAB ADD TABLE ENTRY               
         B     REQF10                                                           
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*********************************************************************           
* PROCESS AN ACCOUNT                                                *           
*********************************************************************           
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
PACC     DS    0H                                                               
         L     R4,ADACC                                                         
*        MVC   SVULA,ACTKULA                                                    
*                                                                               
*        USING BIND,R5                                                          
*        L     R5,ATRNTTAB         R5=A(TRANSACTION TABLE)                      
*        MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
*        LA    R6,BINTAB           A(TABLE)                                     
*        GOTO1 BINSRCH,DMCB,(X'00',SVULA),(R6)                                  
*        CLI   DMCB,1              RECORD WAS NOT FOUND                         
*        BE    PACCX                                                            
*                                                                               
         USING BIND,R1                                                          
         L     R1,ATRNTTAB         R1=A(TRANSACTION TABLE)                      
         ICM   R0,15,BININ                                                      
         BZ    PACCX                                                            
         USING TRNTD,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
PACC10   CLC   ACTKULA,TRNTULA     MATCH ON U/L/A                               
         BNE   *+8                                                              
         OI    TRNTSTAT,TRNTAIOF   MARK ACCOUNT AS ACTIVE                       
*                                                                               
         LA    R2,TRNTLNQ(R2)                                                   
         BCT   R0,PACC10                                                        
*                                                                               
PACCX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*********************************************************************           
* REQUEST LAST                                                      *           
*********************************************************************           
         SPACE 1                                                                
REQL     DS    0H                                                               
         USING PLINED,R4                                                        
         LA    R4,P                                                             
*                                                                               
         USING BIND,R1                                                          
         L     R1,ATRNTTAB         R1=A(TRANSACTION TABLE)                      
         ICM   R0,15,BININ                                                      
         BZ    REQL40                                                           
         USING TRNTD,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
*                                                                               
         XC    LASTREC,LASTREC                                                  
         ZAP   PKCOUNT,=P'0'                                                    
REQL10   MVC   PRTLNE(PLNQ),SPACES                                              
         TM    TRNTSTAT,TRNTAIOF   IS ACCOUNT STILL ACTIVE?                     
         BO    REQL30                                                           
         CLC   LASTREC,TRNTULA                                                  
         BE    REQL20                                                           
         MVC   LASTREC,TRNTULA                                                  
         AP    PKCOUNT,=P'1'                                                    
         MVC   PULA,TRNTULA                                                     
REQL20   MVC   POFFICE,TRNTOFF                                                  
         GOTO1 DATCON,DMCB,(2,TRNTPDTE),(X'20',PRTPDTE)                         
         GOTO1 ACREPORT                                                         
*                                                                               
REQL30   LA    R2,TRNTLNQ(R2)                                                   
         BCT   R0,REQL10                                                        
*                                                                               
         CP    PKCOUNT,=P'0'       ANY RECORDS FOUND?                           
         BE    REQL40                                                           
         GOTO1 ACREPORT                                                         
         MVC   PULA(35),=CL35'TOTAL OF ACCOUNTS NOT ON FILE : '                 
         EDIT  PKCOUNT,(10,PULA+40)                                             
         GOTO1 ACREPORT                                                         
*                                                                               
         USING BIND,R1                                                          
REQL40   L     R1,APDTETAB         R1=A(PEELED DATE TABLE)                      
         ICM   R0,15,BININ                                                      
         BZ    REQLX                                                            
         USING PDTED,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
*                                                                               
         ZAP   PKCOUNT,=P'0'                                                    
*                                                                               
REQL50   MVC   PRTLNE(PLNQ),SPACES                                              
         GOTO1 DATCON,DMCB,(2,PDTE),(X'20',PRTDTE)                              
         AP    PKCOUNT,=P'1'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R2,PDTELNQ(R2)                                                   
         BCT   R0,REQL50                                                        
*                                                                               
         CP    PKCOUNT,=P'0'       ANY RECORDS FOUND?                           
         BE    REQLX                                                            
         GOTO1 ACREPORT                                                         
         MVC   PRTDTE(35),=CL35'TOTAL OF PEELS DONE TO THE FILE : '             
         EDIT  PKCOUNT,(10,PRTDTE+40)                                           
         GOTO1 ACREPORT                                                         
*                                                                               
REQLX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*********************************************************************           
* CONSTANTS                                                         *           
*********************************************************************           
         SPACE 1                                                                
ADCONS   DC    0F                                                               
         DC    A(TRNTTAB)          TRANSACTION TABLE                            
         DC    A(PDTETAB)          PEELED DATE TABLE                            
         DC    A(CLITAB)           CLIENT CODE TABLE                            
         DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
         DC    A(DUMP)             ROUTINE TO DUMP RECORDS                      
*                                                                               
         DC    V(PRNTBL)                                                        
         DC    V(HELLO)                                                         
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
HISTDCB  DC    A(0)                                                             
ACCHST   DC    CL8'ACCHST'                                                      
ACCFIL   DC    CL8'ACCOUNT'                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLOSE  DC    CL8'DMCLSE'                                                      
DMDTF    DC    CL8'DTFADD'                                                      
TMPWRK   DC    XL64'00'                                                         
         EJECT                                                                  
*********************************************************************           
* LITERALS                                                          *           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
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
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
*        SR    R6,R6                                                            
*        IC    R6,BINFRST          DISPLACEMENT TO FIRST BUCKET                 
*        AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
*        AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
*INA10   AP    0(TRNTBKLN,R4),0(TRNTBKLN,R3) ADD TO BUCKET                      
*        LA    R3,TRNTBKLN(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
*        LA    R4,TRNTBKLN(R4)     BUMP TO NEXT ENTRY IN TABLE                  
*        BCT   R0,BINA10                                                        
*                                                                               
BINXIT   XIT1                                                                   
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
         CP    DMPCNT,=P'100'                                                   
         BH    DUMPX                                                            
         AP    DMPCNT,=P'1'                                                     
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
* TABLES                                                             *          
**********************************************************************          
        SPACE 1                                                                 
*                                                                               
* BINTABLE 1 - TRANSACTION TABLE                                                
*                                                                               
         DC    C'***TRNT***'                                                    
TRNTTAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 1               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TRNTLNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TRNTKLNQ)           KEY LENGTH                               
         DC    AL4(TRNTMAX)            MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (TRNTMAX*TRNTLNQ)XL1    TABLE                                    
*                                                                               
         DC    C'***PDTE***'                                                    
PDTETAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 2               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(PDTELNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(PDTEKLNQ)           KEY LENGTH                               
         DC    AL4(PDTEMAX)            MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (PDTEMAX*PDTELNQ)XL1    TABLE                                    
*                                                                               
TRNTMAX  EQU   95000                                                            
PDTEMAX  EQU    4999                                                            
*                                                                               
         DC    C'**CLITAB**'                                                    
CLITAB   DS    0CL5                                                             
         DC    C'C1POA'                                                         
         DC    C'C1POB'                                                         
         DC    C'C1POC'                                                         
         DC    C'C1PSE'                                                         
         DC    C'C1PSF'                                                         
         DC    C'C1PSG'                                                         
         DC    C'C1PSV'                                                         
         DC    C'CVAD '                                                         
         DC    C'CVADM'                                                         
         DC    C'CVAD1'                                                         
         DC    C'CVAD2'                                                         
         DC    C'CVAL1'                                                         
         DC    C'CVAL2'                                                         
         DC    C'CVAM '                                                         
         DC    C'CVAMS'                                                         
         DC    C'CVAS '                                                         
         DC    C'CVASK'                                                         
         DC    C'CVCA1'                                                         
         DC    C'CVCA2'                                                         
         DC    C'CVCB '                                                         
         DC    C'CVCB1'                                                         
         DC    C'CVCC1'                                                         
         DC    C'CVCC2'                                                         
         DC    C'CVCE1'                                                         
         DC    C'CVCE2'                                                         
         DC    C'CVCF '                                                         
         DC    C'CVCF1'                                                         
         DC    C'CVCF2'                                                         
         DC    C'CVCLF'                                                         
         DC    C'CVCL1'                                                         
         DC    C'CVCL2'                                                         
         DC    C'CVCM1'                                                         
         DC    C'CVCM2'                                                         
         DC    C'CVCOA'                                                         
         DC    C'CVCO2'                                                         
         DC    C'CVCPS'                                                         
         DC    C'CVCR '                                                         
         DC    C'CVCRF'                                                         
         DC    C'CVCRL'                                                         
         DC    C'CVCS '                                                         
         DC    C'CVCS1'                                                         
         DC    C'CVCS2'                                                         
         DC    C'CVCT '                                                         
         DC    C'CVCT1'                                                         
         DC    C'CVCT2'                                                         
         DC    C'CVCU2'                                                         
         DC    C'CVDEL'                                                         
         DC    C'CVDIV'                                                         
         DC    C'CVDMK'                                                         
         DC    C'CVDN '                                                         
         DC    C'CVDNS'                                                         
         DC    C'CVDO '                                                         
         DC    C'CVDS1'                                                         
         DC    C'CVDS2'                                                         
         DC    C'CVECS'                                                         
         DC    C'CVEF1'                                                         
         DC    C'CVEF2'                                                         
         DC    C'CVEMK'                                                         
         DC    C'CVEM1'                                                         
         DC    C'CVEM2'                                                         
         DC    C'CVENG'                                                         
         DC    C'CVEPO'                                                         
         DC    C'CVEP1'                                                         
         DC    C'CVEP2'                                                         
         DC    C'CVEXM'                                                         
         DC    C'CVFC1'                                                         
         DC    C'CVFC2'                                                         
         DC    C'CVFST'                                                         
         DC    C'CVGD '                                                         
         DC    C'CVGDS'                                                         
         DC    C'CVGD1'                                                         
         DC    C'CVGD2'                                                         
         DC    C'CVGEP'                                                         
         DC    C'CVGK1'                                                         
         DC    C'CVGK2'                                                         
         DC    C'CVGLK'                                                         
         DC    C'CVGL1'                                                         
         DC    C'CVGL2'                                                         
         DC    C'CVGNL'                                                         
         DC    C'CVGPM'                                                         
         DC    C'CVGP1'                                                         
         DC    C'CVGP2'                                                         
         DC    C'CVGRL'                                                         
         DC    C'CVGSV'                                                         
         DC    C'CVHE2'                                                         
         DC    C'CVHO '                                                         
         DC    C'CVID '                                                         
         DC    C'CVIDT'                                                         
         DC    C'CVID1'                                                         
         DC    C'CVIE1'                                                         
         DC    C'CVIE2'                                                         
         DC    C'CVIMG'                                                         
         DC    C'CVIN '                                                         
         DC    C'CVIND'                                                         
         DC    C'CVINF'                                                         
         DC    C'CVIN1'                                                         
         DC    C'CVIN2'                                                         
         DC    C'CVIP1'                                                         
         DC    C'CVIP2'                                                         
         DC    C'CVIS1'                                                         
         DC    C'CVIS2'                                                         
         DC    C'CVLAD'                                                         
         DC    C'CVLC1'                                                         
         DC    C'CVLC2'                                                         
         DC    C'CVLI '                                                         
         DC    C'CVLIB'                                                         
         DC    C'CVLS '                                                         
         DC    C'CVMA1'                                                         
         DC    C'CVMA2'                                                         
         DC    C'CVMBW'                                                         
         DC    C'CVMDS'                                                         
         DC    C'CVMD1'                                                         
         DC    C'CVMD2'                                                         
         DC    C'CVMKS'                                                         
         DC    C'CVMM '                                                         
         DC    C'CVMMO'                                                         
         DC    C'CVMM1'                                                         
         DC    C'CVMM2'                                                         
         DC    C'CVMNO'                                                         
         DC    C'CVMS1'                                                         
         DC    C'CVNET'                                                         
         DC    C'CVNE1'                                                         
         DC    C'CVNE2'                                                         
         DC    C'CVNP2'                                                         
         DC    C'CVNW '                                                         
         DC    C'CVNY1'                                                         
         DC    C'CVNY2'                                                         
         DC    C'CVOR '                                                         
         DC    C'CVOR1'                                                         
         DC    C'CVOR2'                                                         
         DC    C'CVPA1'                                                         
         DC    C'CVPA2'                                                         
         DC    C'CVPA4'                                                         
         DC    C'CVPBI'                                                         
         DC    C'CVPB1'                                                         
         DC    C'CVPB2'                                                         
         DC    C'CVPB7'                                                         
         DC    C'CVPB9'                                                         
         DC    C'CVPC2'                                                         
         DC    C'CVPC3'                                                         
         DC    C'CVPC8'                                                         
         DC    C'CVPC9'                                                         
         DC    C'CVPD1'                                                         
         DC    C'CVPEC'                                                         
         DC    C'CVPE1'                                                         
         DC    C'CVPE2'                                                         
         DC    C'CVPE3'                                                         
         DC    C'CVPE4'                                                         
         DC    C'CVPE5'                                                         
         DC    C'CVPE6'                                                         
         DC    C'CVPE7'                                                         
         DC    C'CVPE8'                                                         
         DC    C'CVPE9'                                                         
         DC    C'CVPFF'                                                         
         DC    C'CVPF1'                                                         
         DC    C'CVPF2'                                                         
         DC    C'CVPF6'                                                         
         DC    C'CVPG1'                                                         
         DC    C'CVPG2'                                                         
         DC    C'CVPG5'                                                         
         DC    C'CVPH '                                                         
         DC    C'CVPHO'                                                         
         DC    C'CVPIW'                                                         
         DC    C'CVPK '                                                         
         DC    C'CVPK1'                                                         
         DC    C'CVPK2'                                                         
         DC    C'CVPM1'                                                         
         DC    C'CVPM2'                                                         
         DC    C'CVPN1'                                                         
         DC    C'CVPN2'                                                         
         DC    C'CVPO1'                                                         
         DC    C'CVPO2'                                                         
         DC    C'CVPPO'                                                         
         DC    C'CVPP1'                                                         
         DC    C'CVPP2'                                                         
         DC    C'CVPRM'                                                         
         DC    C'CVPR1'                                                         
         DC    C'CVPR2'                                                         
         DC    C'CVPSD'                                                         
         DC    C'CVPS1'                                                         
         DC    C'CVPS2'                                                         
         DC    C'CVPT1'                                                         
         DC    C'CVPT2'                                                         
         DC    C'CVPW1'                                                         
         DC    C'CVPW2'                                                         
         DC    C'CVRB1'                                                         
         DC    C'CVRB2'                                                         
         DC    C'CVRC1'                                                         
         DC    C'CVRC2'                                                         
         DC    C'CVREM'                                                         
         DC    C'CVRE1'                                                         
         DC    C'CVRE2'                                                         
         DC    C'CVRT '                                                         
         DC    C'CVRTL'                                                         
         DC    C'CVRT1'                                                         
         DC    C'CVRT2'                                                         
         DC    C'CVSB1'                                                         
         DC    C'CVSB2'                                                         
         DC    C'CVSF1'                                                         
         DC    C'CVSF2'                                                         
         DC    C'CVSH1'                                                         
         DC    C'CVSH2'                                                         
         DC    C'CVSIA'                                                         
         DC    C'CVSK '                                                         
         DC    C'CVSM1'                                                         
         DC    C'CVSM2'                                                         
         DC    C'CVSN '                                                         
         DC    C'CVSPR'                                                         
         DC    C'CVSP1'                                                         
         DC    C'CVSP2'                                                         
         DC    C'CVSS1'                                                         
         DC    C'CVSS2'                                                         
         DC    C'CVST '                                                         
         DC    C'CVSTM'                                                         
         DC    C'CVSU '                                                         
         DC    C'CVSU1'                                                         
         DC    C'CVSU2'                                                         
         DC    C'CVSV '                                                         
         DC    C'CVSV1'                                                         
         DC    C'CVSV2'                                                         
         DC    C'CVSW1'                                                         
         DC    C'CVSW2'                                                         
         DC    C'CVTRG'                                                         
         DC    C'CVUNV'                                                         
         DC    C'CVUPV'                                                         
         DC    C'CVWNG'                                                         
         DC    C'CVWS1'                                                         
         DC    C'CVWS2'                                                         
CLITNUM  EQU   (*-CLITAB)/CLITLNQ                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN TRANSACTION TABLE                                *         
***********************************************************************         
         SPACE 1                                                                
TRNTD    DSECT                                                                  
TRNTKEY  DS    0C                                                               
TRNTULA  DS    CL14                TRANSACTION ACCOUNT                          
TRNTPDTE DS    XL2                 PEELED DATE                                  
TRNTKLNQ EQU   *-TRNTD             LENGTH OF KEY                                
TRNTOFF  DS    CL2                 TRANSACTION OFFICE                           
TRNTSTAT DS    XL1                 STATUS BYTE                                  
TRNTAIOF EQU   X'80'               ACCOUNT IS ON FILE                           
TRNTLNQ  EQU   *-TRNTD             LENGTH OF ENTRY                              
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN PEELED DATE TABLE                                *         
***********************************************************************         
         SPACE 1                                                                
PDTED    DSECT                                                                  
PDTEKEY  DS    0C                                                               
PDTE     DS    CL(ACCOPLEN)          PEELED DATE                                
PDTEKLNQ EQU   *-PDTED               LENGTH OF KEY                              
PDTELNQ  EQU   *-PDTED               LENGTH OF ENTRY                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN CLIENT TABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
CLITABD  DSECT                                                                  
CLITKEY  DS    0C                                                               
CLITOFF  DS    CL2                   CLIENT OFFICE                              
CLITCDE  DS    CL3                   CLIENT CODE                                
CLITLNQ  EQU   *-CLITABD             LENGTH OF ENTRY                            
         EJECT                                                                  
*********************************************************************           
* STORAGE                                                           *           
*********************************************************************           
         SPACE 1                                                                
ACZND    DSECT                                                                  
VTYPES   DS    0A                                                               
ATRNTTAB DS    A                   TRANSACTION TABLE                            
APDTETAB DS    A                   PEELED DATE TABLE                            
ACLITAB  DS    A                   CLIENT CODE TABLE                            
ABINADD  DS    A                   ROUTINE TO ADD TO BINSEARCH TABLE            
ADUMP    DS    A                   ROUTINE TO DUMP RECORDS                      
*                                                                               
PRNTBL   DS    V                                                                
HELLO    DS    V                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
PARM     DS    6F                                                               
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
ACTSW    DS    CL1                                                              
ELCODE   DS    CL1                                                              
TRNTWRK  DS    CL(TRNTLNQ)         BINSEARCH WORK AREA - TRNS TABLE             
PDTEWRK  DS    CL(PDTELNQ)         BINSEARCH WORK AREA - PDTE TABLE             
SVKEY    DS    CL49                                                             
SVOFF    DS    CL2                 CLIENT OFFICE                                
LASTREC  DS    CL15                LAST KEY READ                                
LASTDTE  DS    XL(ACCOPLEN)        LAST DATE READ                               
*                                                                               
SVPDTE   DS    XL(ACCOPLEN)        PEELED DATE                                  
SVULA    DS    CL14                                                             
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
DMPCNT   DS    PL4                                                              
PKCOUNT  DS    PL8                                                              
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
*********************************************************************           
* PRINT LINE DSECT                                                  *           
*********************************************************************           
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    XL2                                                              
PULA     DS    CL14                ACCOUNT                                      
         DS    CL6                                                              
POFFICE  DS    CL2                 OFFICE                                       
         DS    CL6                                                              
PRTPDTE  DS    CL6                 PEELED DATE REPORT 1                         
         ORG   PULA                                                             
PRTDTE   DS    CL6                 PEELED DATE REPORT 2                         
         DS    CL16                                                             
PLNQ     EQU   *-PRTLNE                                                         
         EJECT                                                                  
*********************************************************************           
* DSECT FOR THE BINSRCH LIST                                        *           
*********************************************************************           
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTAB   DS    0CL1                                                             
         EJECT                                                                  
*********************************************************************           
* ++INCLUDES                                                        *           
*********************************************************************           
         SPACE 1                                                                
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080ACREPZN02 06/23/00'                                      
         END                                                                    
