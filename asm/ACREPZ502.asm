*          DATA SET ACREPZ502  AT LEVEL 056 AS OF 08/16/00                      
***********************************************************************         
* $CTA FIX                                                            *         
*   CHANGE BAD CONTRACT # IN THE KEYS OF S2 AND S1/S2 RECORDS         *         
***********************************************************************         
*PHASE ACZ502A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REMOVE WC FROM KEY'                                             
ACZ502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZ5**,R7,R9    BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACZ5D,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,REQFRST        RUN FIRST                                    
         BE    REQF                                                             
         CLI   MODE,REQLAST        REQUEST LAST                                 
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         USING CPYELD,R5                                                        
REQF     DS    0H                                                               
         MVI   STAT,0                                                           
         LA    R1,0                                                             
         ST    R1,COUNT                                                         
         ZAP   TOTAMT,=P'0'                                                     
*                                                                               
         MVI   FRSTTIME,C'Y'                                                    
         MVI   FCRDACC,C'Y'                                                     
         MVI   FCRDTRNS,C'Y'                                                    
         L     R5,ADCMPEL          POINT TO COMPANY ELEMENT                     
         TM    CPYSTAT4,CPYSOFF2   TWO BYTE OFFICE?                             
         BZ    EXIT                                                             
         OI    STAT,STSKIP         SKIP ALL                                     
         MVI   FCRDACC,C'N'        DON'T READ ACCOUNTS                          
         MVI   FCRDTRNS,C'N'       DON'T READ TRANSACTIONS                      
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
         USING LDGRECD,R5                                                       
LDGF     DS    0H                                                               
         TM    STAT,STSKIP         SKIP ALL?                                    
         BO    EXIT                                                             
         MVI   FCRDACC,C'Y'                                                     
         MVI   FCRDTRNS,C'Y'                                                    
         L     R5,ADLEDGER                                                      
         CLC   LDGKUNT(2),=C'SJ'   DON'T PROCESS SJ                             
         BNE   EXIT                                                             
         MVI   FCRDACC,C'N'        DON'T READ ACCOUNTS                          
         MVI   FCRDTRNS,C'N'       DON'T READ TRANSACTIONS                      
         B     EXIT                                                             
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* TRANSACTION FIRST                                                   *         
***********************************************************************         
         USING TRNELD,R4                                                        
         USING TRNRECD,R5                                                       
NEW      USING TRNRECD,R6                                                       
         USING PLINED,R2                                                        
PTRN     DS    0H                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP         GET ADDRESS OF KEY                           
         CLC   TRNKOFF,SPACES      OFFICE HAS TO BE SPACES                      
         BE    EXIT                                                             
*                                                                               
         CLC   TRNKULA(2),=C'SJ'   MAKE SURE SJ IS SKIPPED                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R0,R5                                                            
         L     RE,AIO1                                                          
         SR    R1,R1                                                            
         ICM   R1,3,TRNRLEN        LENGTH OF RECORD                             
         LR    RF,R1                                                            
         MVCL  RE,R0               COPY OLD RECORD TO IO1                       
*                                                                               
         OI    TRNRSTAT,TRNSDELT   MARK OLD RECORD DELETED                      
         L     R6,AIO1                                                          
         MVC   NEW.TRNKOFF,SPACES  NEW RECORD HAS NO OFFICE IN KEY              
*                                                                               
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         CLI   FRSTTIME,C'Y'                                                    
         BNE   PTRN100                                                          
         MVI   FORCEHED,C'Y'                                                    
PTRN100  MVI   FRSTTIME,C'N'                                                    
         LA    R2,P                                                             
         MVC   PACCOUNT,TRNKULA                                                 
         MVC   PCONTRA,TRNKULC                                                  
         MVC   PWC,TRNKOFF                                                      
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(13,PDATE)                              
         MVC   PREF,TRNKREF                                                     
         L     R4,ADTRANS                                                       
         EDIT  TRNTYPE,PTTYPE,ZERO=NOBLANK                                      
         MVC   PDRCR,=C'CR'                                                     
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+10                                                             
         MVC   PDRCR,=C'DR'                                                     
         EDIT  TRNAMNT,PAMOUNT,2,ZERO=NOBLANK,MINUS=YES                         
         AP    TOTAMT,TRNAMNT                                                   
*                                                                               
         LA    R5,DKEY                                                          
         MVC   TRNKEY,NEW.TRNKEY                                                
         MVI   TRNKSBR,0                                                        
PTRN200  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(0,DMREAD),ACCDIR,DKEY,DIR,0                        
         CLI   DMCB+8,X'10'        RECORD NOT FOUND                             
         BE    PTRN300                                                          
         CLI   DMCB+8,0            FOUND IT AND NO ERRORS                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,TRNKSBR          BUMP SUB REFERENCE NUMBER                    
         LA    R1,1(R1)                                                         
         STC   R1,TRNKSBR                                                       
         B     PTRN200                                                          
*                                                                               
PTRN300  MVC   NEW.TRNKSBR,TRNKSBR                 NEW SUB-REF                  
         EDIT  NEW.TRNKSBR,PNEWSBR,ZERO=NOBLANK                                 
         GOTO1 ACREPORT                                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,(0,DMADD),ACCFIL,AIO1,AIO1,0                        
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                YES, DIE                                     
*                                                                               
         MVI   FCRESET,C'Y'                                                     
         MVI   MODE,WRITRANS       WRITE OUT MODIFIED RECORD                    
         B     EXIT                                                             
*                                                                               
         DROP  R5,NEW                                                           
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
         SPACE 1                                                                
REQL     DS    0H                                                               
         OC    COUNT,COUNT                                                      
         BZ    EXIT                                                             
*                                                                               
         GOTO1 ACREPORT                                                         
         LA    R2,P                                                             
         MVI   P,C'='                                                           
         MVC   P+1(79),P                                                        
         GOTO1 ACREPORT                                                         
         MVC   P+2(20),=CL20'RECORDS CHANGED: '                                 
         EDIT  COUNT,(5,P+21),ZERO=NOBLANK                                      
         EDIT  TOTAMT,PAMOUNT,2,ZERO=NOBLANK,MINUS=YES                          
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R2                                                       
DMHIADIR NTR1                      DMRDHI FOR ACCT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEADIR NTR1                      DMRSEQ FOR ACCT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'ACCDIR',DKEY,DIR,0                
         LA    R2,DIR                                                           
         MVC   DA,ACCKDA           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETACC NTR1                      GETREC FOR ACCT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'ACCMST',DA,AIO1,DMWORK            
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHISDIR NTR1                      DMRDHI FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSESDIR NTR1                      DMRSEQ FOR SPOT DIR                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'SPTDIR',DKEY,DIR,0                
         MVC   DA,DIR+14           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETSPT NTR1                      GETREC FOR SPOT FILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMHIPDIR NTR1                      DMRDHI FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMSEPDIR NTR1                      DMRSEQ FOR PRINT DIR                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PRTDIR',DKEY,DIR,0                
         MVC   DA,DIR+27           MOVE IN DISK ADDRESS                         
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMGETPRT NTR1                      GETREC FOR PRINT FILE                        
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PRTFILE',DA,AIO1,DMWORK           
         BAS   RE,DMCHK                                                         
         B     EXIT                                                             
*                                                                               
DMCHK    NTR1                      CHECK RETURNS ON DATAMGR CALLS               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
UTL      DS    0D                                                               
         DC    F'0',X'02'                                                       
         DC    XL3'00'                                                          
*                                                                               
MXRLNQ   EQU   2000                MAX RECORD SIZE                              
MSG1     DC    C'ACCOUNT RECORD'                                                
*                                                                               
OPTN     DC    X'00'               RUN OPTIONS                                  
OPTTAPE  EQU   X'80'               TAPE IS OPEN                                 
OPTCLT   EQU   X'40'               RUN ON CLIENT LEVEL ONLY                     
OPTDUMP  EQU   X'20'               DUMP OUTPUT RECORDS                          
OPTAGY   EQU   X'10'               PRINT SELECTED AGENCY ONLY                   
OPTSYS   EQU   X'08'               RUN ON SELECTED FILES ONLY (N,P,S)           
*                                                                               
OPTNPRT  DC    X'00'               PRINT OPTIONS                                
OPTPRT1  EQU   X'80'               PRINT ROUTINE 1 - TOTALS BY AGENCY           
OPTPRT2  EQU   X'40'               PRINT ROUTINE 2 - GROUPS BY AGENCY           
OPTPRT3  EQU   X'20'               PRINT ROUTINE 3 - REGULAR PRINTOUT           
OPTPRT4  EQU   X'10'               PRINT ROUTINE 3 - REGULAR PRINTOUT           
TURNOFF  EQU   X'FF'                                                            
*                                                                               
DMPTOT   DC    PL4'0'              DUMP COUNT                                   
MAXDMP   DC    PL4'15'             MAXIMUM DUMPS                                
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
AIO1     DC    A(IO1)                                                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
         SPACE 1                                                                
OUTFIL   DCB   DDNAME=OUTFIL,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=2048,BLKSIZE=8032,BUFNO=2                         
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DC    C'**IO1***'                                                      
IO1      DC    (MXRLNQ)X'00'       IOAREA                                       
*                                                                               
CLIMAX   EQU   5000                                                             
AGNMAX   EQU   8000                                                             
SJMAX    EQU   10000                                                            
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
ACZ5D    DSECT                                                                  
AUTL     DS    A                                                                
STAT     DS    XL1                                                              
STSKIP   EQU   X'80'               SKIP ALL                                     
COUNT    DS    F                                                                
FRSTTIME DS    C                                                                
TOTAMT   DS    PL8                                                              
*                                                                               
SAVEKEY  DS    CL(L'ACCKEY)        SAVED AREA FOR KEY                           
PRTCKEY  DS    CL7                 STORAGE FOR PRINT KEY FOR CLIENT             
PRTPKEY  DS    CL10                STORAGE FOR PRINT KEY FOR PRODUCT            
SESAVE   DS    XL1                 SE NUMBER SAVE AREA                          
SVSYSID  DS    CL1                 SAVED AREA FOR SYSTEM ID                     
SVAGY    DS    CL1                 SAVED AREA FOR AGENCY ID                     
SVZEN    DS    CL3                 SAVED AREA FOR ZENITH CODE                   
*                                                                               
SVAGZN   DS    0CL6                SAVED AREA FOR AGENCY/ZENITH CODES           
SVAGCLT  DS    CL3                   AGENCY CLIENT CODE                         
SVZNCLT  DS    CL3                   ZENITH CLIENT CODE                         
*                                                                               
SYSSAVE  DS    CL1                 SAVED AREA FOR SYSTEM RUN REQUEST ID         
SYSNAME  DS    CL7                 SAVED AREA FOR SYSTEM RUN REQUEST            
HEADNM   DS    CL36                SAVED AREA FOR SYSTEM RUN REQUEST            
SVSENUM  DS    XL1                 SAVE AREA FOR SYSTEM SE NUMBER               
SAVEID   DS    CL2                 SAVE AREA FOR COMPANY ID                     
SVMEDLS  DS    CL2                 SAVED AREA FOR MEDIA LIST                    
SPTMED   DS    XL1                 SAVED AREA FOR SPOT/NET MEDIA                
SAVEAGN  DS    XL1                 SAVED AREA FOR AGENCY/MEDIA                  
*                                                                               
PKFLDS   DS    0PL8                START OF PACKED FIELDS                       
PKDPCNT  DS    PL8                 PACKED FIELD FOR DUPLICATE COUNT             
PKSJCNT  DS    PL8                 TOTAL SJ RECORDS CREATED                     
PKSRCNT  DS    PL8                 TOTAL SR RECORDS CREATED                     
PK1CCNT  DS    PL8                 TOTAL 1C RECORDS CREATED                     
PKCTCNT  DS    PL8                 TOTAL CT RECORDS CREATED                     
PKGDTOT  DS    PL8                 GRAND TOTAL OF ALL RECORDS CREATED           
PKNUMQ   EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
FLAG     DS    XL1                                                              
FLGCTF   EQU   X'80'               FLAG FOR CTFILE XSORT                        
*                                                                               
DUPFLG   DS    XL1                 FLAG FOR DUPLICATES                          
DUPFLGB  EQU   X'80'               DUP FLAG FOR BATES                           
DUPFLGT  EQU   X'40'               DUP FLAG FOR ZENITH                          
DUPFLGD  EQU   X'20'               DUP FLAG FOR SAATCHI                         
*                                                                               
SRTBUFF  DS    CL(SRTLNQ)          BUFFER FOR SORTER                            
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
*                                                                               
NAME     DS    CL36                                                             
*                                                                               
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
*                                                                               
ELEMENT  DS    XL255                                                            
         EJECT                                                                  
***********************************************************************         
* SYSTEM TABLE FILE DSECT                                             *         
***********************************************************************         
         SPACE 1                                                                
SYSTBLD  DSECT                                                                  
SYSTMED  DS    0XL1                COMPANY AGENCY/MEDIA                         
SYSTCPY  DS    XL1                 COMPANY ID (ACC SIDE ONLY)                   
SYSTSE   DS    XL1                 AGENCY SE NUMBER                             
SYSTCODE DS    CL2                 AGENCY CHARACTER CODE                        
SYSTID   DS    CL1                 ONE BYTE SYSTEM IDENTIFICATION               
SYSTAGY  DS    CL1                 ONE BYTE AGENCY IDENTIFICATION               
SYSTNAM  DS    CL8                 SYSTEM NAME (SPOT, NET, PRINT)               
ASYSFIL  DS    AL4                 A(SYSTEM FILES TO BE OPEN)                   
ASYSRD   DS    AL4                 A(SYSTEM READ ROUTINES)                      
SYSTLNQ  EQU   *-SYSTBLD           LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN CLIENT/PRODUCT TABLE 1                           *         
***********************************************************************         
         SPACE 1                                                                
CLID     DSECT                                                                  
CLICLI   DS    CL3             CLIENT CODE                                      
CLIPRD   DS    CL3             PRODUCT CODE                                     
CLIKLNQ  EQU   *-CLID          LENGTH OF KEY                                    
CLIAGY   DS    CL2             SOURCE AGENCY CODE(FOR PRINT ONLY)               
CLIMED   DS    CL1             SOURCE AGY/MED CODE(ONLY MEDIA - PRINT)          
CLISYS   DS    XL1             X'80' - SPOT/X'40' - ACC/X'00' - PRINT           
CLISE    DS    XL1             SE NUMBER FOR SYSTEM                             
CLIID    DS    CL1             ONE BYTE SYSTEM ID (N,P,S)                       
CLIAGNID DS    CL1             ONE BYTE AGENCY ID (B,C,D)                       
CLINAM   DS    CL36            CLIENT NAME(ONLY FOR ACC)                        
CLILNQ   EQU   *-CLID          LENGTH                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN CLIENT/PRODUCT TABLE 2                           *         
***********************************************************************         
         SPACE 1                                                                
AGND     DSECT                                                                  
AGNACLI  DS    CL3             AGENCY CLIENT CODE                               
AGNZCLI  DS    CL3             ZENITH CLIENT CODE                               
AGNAGY   DS    CL2             SOURCE AGENCY CODE                               
AGNKLNQ  EQU   *-AGND          LENGTH OF KEY                                    
AGNBKT   DS    PL8                 BUCKET                                       
AGNBKLN  EQU   *-AGNBKT            BUCKET LENGTH                                
AGNBKCT  EQU   (*-AGNBKT)/AGNBKLN  NUMBER OF BUCKETS                            
AGNXL1Q  EQU   *-AGND              DISPLACEMENT TO ID FOR XSORT                 
AGNSYSID DS    CL1                 AGENCY ID(B-BATES,D-SAATCHI)                 
AGNXL2Q  EQU   *-AGND              DISPLACEMENT TO SORT CLI FOR XSORT           
AGNSCLI  DS    CL3             SORT CLIENT CODE EITHER AGENCY/ZENITH            
AGNPCLI  DS    CL2             AGENCY CLIENT CODE PACKED                        
AGNCNAM  DS    CL20            CLIENT NAME                                      
AGNMED   DS    CL1             SOURCE AGY/MED CODE(ONLY MEDIA - PRINT)          
AGNSYS   DS    XL1             SYSTEM ID                                        
AGNSPT   EQU   X'80'           SPOT/NET   SYSTEM                                
AGNACC   EQU   X'40'           ACCOUNTING SYSTEM                                
AGNPRT   EQU   X'00'           PRINT      SYSTEM                                
AGNRAN   EQU   X'10'           ALREADY RUN                                      
AGNSPSE  DS    XL1             SE NUMBER FOR SPOT  SYSTEM                       
AGNNTSE  DS    XL1             SE NUMBER FOR NET   SYSTEM                       
AGNPTSE  DS    XL1             SE NUMBER FOR PRINT SYSTEM                       
AGNID    DS    CL1             ONE BYTE SYSTEM ID (N,P,S)                       
AGNAMLNQ EQU   *-AGND          DISPLACEMENT TO ACC   MEDIA LIST                 
AGNACMD  DS    CL10            ACC   MEDIA LIST                                 
AGNSMLNQ EQU   *-AGND          DISPLACEMENT TO SPOT  MEDIA LIST                 
AGNSPMD  DS    CL10            SPOT  MEDIA LIST                                 
AGNPMLNQ EQU   *-AGND          DISPLACEMENT TO PRINT MEDIA LIST                 
AGNPTMD  DS    CL10            PRINT MEDIA LIST                                 
AGNNMLNQ EQU   *-AGND          DISPLACEMENT TO NET   MEDIA LIST                 
AGNNTMD  DS    CL10            NET   MEDIA LIST                                 
AGNDUP   DS    CL9             DUPLICATE FIELD                                  
AGNLNQ   EQU   *-AGND          LENGTH                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN SJ RECORD TABLE - BINADD TABLE 3                 *         
***********************************************************************         
         SPACE 1                                                                
SJRECD   DSECT                                                                  
SJCDE    DS    XL1             SJ CLIENT CODE                                   
SJUL     DS    CL2             SJ UNIT/LEDGER(SR OR 1C)                         
SJTYPE   DS    CL1             TYPE(SR - A     1C - B,C OR S)                   
SJOFF    DS    CL2             OFFICE(SR - BS OR SA   1C - 01)                  
SJCLI    DS    CL3             CLIENT CODE(ONLY FOR 1C)                         
SJPRD    DS    CL3             PRODUCT CODE(ONLY FOR 1C)                        
SJKYLNQ  EQU   *-SJRECD        LENGTH OF KEY                                    
SJANAM   DS    CL36            AGENCY NAME(ONLY FOR SR RECORDS)                 
SJCPNAM  DS    CL20            CLIENT OR PRODUCT NAME(1C RECORDS)               
SJLNQ    EQU   *-SJRECD        LENGTH                                           
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
* PRINT DESCT                                                         *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE                                   
         DS    CL2                                                              
PACCOUNT DS    CL14                ACCOUNT                                      
         DS    CL2                                                              
PCONTRA  DS    CL14                CONTRA                                       
         DS    CL2                                                              
PWC      DS    CL2                 WORKCODE                                     
         DS    CL2                                                              
PDATE    DS    CL8                 DATE                                         
         DS    CL2                                                              
PREF     DS    CL6                 TRANSACTION REF                              
         DS    CL2                                                              
PTTYPE   DS    CL4                 TRANSACTION TYPE                             
         DS    CL2                                                              
PDRCR    DS    CL2                 DEBIT OR CREDIT                              
         DS    CL2                                                              
PAMOUNT  DS    CL15                AMOUNT                                       
         DS    CL5                                                              
PNEWSBR  DS    CL3                 NEW SUB-REFERENCE                            
         EJECT                                                                  
***********************************************************************         
* SORT DSECT 1                                                        *         
***********************************************************************         
         SPACE 1                                                                
SORTD    DSECT                                                                  
SRTCLT   DS    CL3                 CLIENT CODE                                  
SRTPRD   DS    CL3                 PRODUCT CODE                                 
SRTAGY   DS    CL2                 AGENCY CODE                                  
SRTACLT  DS    CL3                 ORIG CLIENT CODE(IF OVERRIDE EXISTS)         
SRTAGNID DS    CL1                 AGENCY ID(B-BATE,T-ZENITH,D-SAATCHI)         
SRTCNAM  DS    CL20                CLIENT NAME                                  
SRT1CPRD DS    CL3                 PRODUCT CODE FOR 1C ACCOUNT                  
SRTPNAM  DS    CL20                PRODUCT NAME                                 
SRTANAM  DS    CL36                COMPANY NAME                                 
SRTSYS   DS    CL1                 ONE BYTE SYSTEM ID (N,P,S)                   
SRTSRRC  DS    CL12                SJ RECORD                                    
SRT1CRC  DS    CL12                1C RECORD                                    
SRTLNQ   EQU   *-SORTD             LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* SPOT/NET FILE ++INCLUDES AND DSECTS                                 *         
***********************************************************************         
         SPACE 1                                                                
*        SPOT CLIENT DSECT                                                      
*                                                                               
SPCLID   DSECT                                                                  
*          DATA SET SPGENCLT   AT LEVEL 054 AS OF 12/09/94                      
CLTHDR   DS    0C                                                               
CKEY     DS    0CL13     V         KEY                                          
CKEYTYPE DS    CL1       B         RECORD TYPE X'00'                            
CKEYAM   DS    CL1       A/M       AGENCY/MEDIA                                 
CKEYCLT  DS    CL2       CLT       CLIENT CODE                                  
CKEYZRO  DS    CL9       B         BINARY ZEROS                                 
         SPACE 2                                                                
CLEN     DS    CL2       B         RECORD LENGTH (1000)                         
CCNTRL   DS    CL1       B         CONTROL BYTE                                 
CLINKS   DS    CL4       B         LINK FIELDS                                  
         DS    CL2       B         SPARE                                        
         DS    CL2       CLT       CLIENT INTERFACE                             
         SPACE 2                                                                
CNAME    DS    CL20      A         CLIENT NAME                                  
COFFICE  DS    CL1       N         OFFICE NUMBER                                
CPROF    DS    CL15      A/N       CLIENT PROFILE (SEE MANUAL)                  
CLIST    DS    880C      V         PRODUCT CODE LIST                            
*                        A         4 BYTE FIELDS  1-3=PRODUCT MNEMONIC          
*                        B                          4=PRODUCT NUMBER            
CCLTIFC  DS    CL8       A/N       NEW CLIENT INTERFACE CODE                    
CACCOFC  DS    CL2       A/N       2 CHAR ACC OFFICE CODE                       
*                                                                               
CGRP1    DS    CL3                 CLTGRP ASSGN                                 
CGRP2    DS    CL3                 CLTGRP ASSGN                                 
CGRP3    DS    CL3                 CLTGRP ASSGN                                 
CGRP4    DS    CL3                 CLTGRP ASSGN                                 
CGRP5    DS    CL3                 CLTGRP ASSGN                                 
*                                                                               
CLOCK    DS    CL1                 CLIENT LOCK                                  
CMCLTCOD DS    XL2                 MASTER TRAFFIC CLIENT CODE                   
CMCLTUNQ DS    XL1                 MASTER TRAFFIC CLIENT UNIQUE SEQNUM          
CMCLTPRD DS    XL1                 MASTER TRAFFIC CLIENT PRODUCT CODE           
*                                                                               
CACCAGY  DS    CL2                 ACC AGENCY OVERRIDE                          
CPOLONLY DS    CL1                 POL BUYING ONLY                              
*                                                                               
CCLTINTR DS    CL2       CLT       CLIENT INTERFACE                             
CEXTRA   DS    CL15      A/N       EXTRA PROFILE                                
CTITLE   DS    CL10                ID TITLE                                     
*                                                                               
CPU1     DS    CL20                PRODUCT USER FIELD DESC 1                    
CPU1TYPE DS    CL1                 PRODUCT USER TYPE (A/C/N)                    
CPU1LEN  DS    XL1                 PRODUCT USER LENGTH (MAX32)                  
CPU1FLG1 DS    XL1                                                              
CFLGREQQ EQU   X'80'               X'80' = REQUIRED                             
CFLGA2Q  EQU   X'40'               X'40' = SHOW ON A2                           
CFLGNIBQ EQU   X'20'               X'20' = (NET) INTEG BILLS                    
CFLGSPQ  EQU   X'10'               X'10' = (SPOT) SHOW ON BILLS                 
CFLGNTBQ EQU   X'10'               X'10' = (NET) TIME BILLS                     
CFLGMXQ  EQU   X'08'               X'08' = TRANSFER ON MX                       
CFLGNSBQ EQU   X'04'               X'04' = (NET) SPEC CHARGE BILLS              
*                                                                               
CPU1FLG2 DS    XL1                                                              
CUSERLNQ EQU   *-CPU1                                                           
*                                                                               
CPU2     DS    CL20                PRODUCT USER FIELD DESC 2                    
CPU2TYPE DS    CL1                 PRODUCT USER TYPE (A/C/N)                    
CPU2LEN  DS    XL1                 PRODUCT USER LENGTH (MAX16)                  
CPU2FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CPU2FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
*                                                                               
CEU1     DS    CL20                ESTIMATE USER FIELD DESC 1                   
CEU1TYPE DS    CL1                 ESTIMATE USER TYPE (A/C/N)                   
CEU1LEN  DS    XL1                 ESTIMATE USER LENGTH (MAX32)                 
CEU1FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CEU1FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
*                                                                               
CEU2     DS    CL20                ESTIMATE USER FIELD DESC 2                   
CEU2TYPE DS    CL1                 ESTIMATE USER TYPE (A/C/N)                   
CEU2LEN  DS    XL1                 ESTIMATE USER LENGTH (MAX16)                 
CEU2FLG1 DS    XL1                 SEE CPU1FLG1 FOR BIT EQUATES                 
CEU2FLG2 DS    XL1                 SEE CPU1FLG2 FOR BIT EQUATES                 
CULNQ    EQU   *-CPU1                                                           
*                                                                               
CMEDNAME DS    CL10                MEDIA NAME OVERRIDE                          
CNETID   DS    CL4                 NETWORK ID                                   
COPT1    DS    XL1                                                              
COP1COSQ EQU   X'80'               SECOND COST REQUIRED                         
COP1INFQ EQU   X'40'               INFOMERCIAL                                  
COP1DBLQ EQU   X'20'               DO NOT TEST DOUBLE-BOOKING                   
COP1MGRQ EQU   X'10'               REQUIRE MGREQ REC IF ID=MKTGRP               
COP1NMG  EQU   X'08'               CLIENT USES NEW MAKEGOODS                    
COP1CTAQ EQU   X'04'               CONTRACT ANALYSIS (CTA) CLIENT               
COPT2    DS    XL1                                                              
         DS    CL2                 SPARE                                        
*                                                                               
CPST     DS    CL10                PST CODES                                    
CDAILY   DS    CL1                 ESTIMATES WILL BE DAILY                      
CPWPCT   DS    XL3                 PROFIT WITHIN PERCENTAGE                     
CZENCLT  DS    CL3                 ZENITH CLIENT CODE                           
         DS    149C                ** NEW SPARE **                              
CLTHDRL  EQU   *-CLTHDR                                                         
         SPACE 1                                                                
* CONTENTS OF CPROF:                CONTENTS OF CEXTRA:                         
*   1 - BRAND/POL TRNDS               1 - CANADIAN DEMO OPTION                  
*   2 - LOCK BOX NUMBER               2 - CANADIAN NETWORK TAX                  
*   3 - MKT/STA TRNDS                 3 - BUY ID REQUIRED                       
*   4 - RATING SERVICE                4 - ESTIMATE FILTERS REQ                  
*   5 - BILL FORMULA CNTRL            5 - CAMPAIGNS                             
*   6 - BILL ESTIMATE CNTRL           6 - U.S. SPILL                            
*   7 - PRINT CLT CODE AS AAN         7 - 'EST=NO' EST NAME                     
*   8 - PRINT EST SERIES NM           8 - MKGDS IN MISSED MTH                   
*   9 - GOALS CPP OVERRIDE            9 - GOAL REQD FOR BUY                     
*   10- PROGRAM ADJ. CNTRL            10- COUNTRY                               
*   11- POL TIMESHEET DEMOS           11- OUT-OF-WEEK CLIENT                    
*   12- FORCE EST SERIES REQ          12- GST CODE                              
*   13- PRD REQ FOR TRUE POL          13- SPECIAL DEMO ADJ.                     
*   14- EXCLUSION GROUP CODE          14- PRD REQD FOR ADDS SEND                
*   15- CLIENT RATE CNTRL             15- RATE COVERAGE CONTROL (NET)           
*                                                                               
*        SPOT PRODUCT DSECT                                                     
*                                                                               
SPPRDD   DSECT                                                                  
*          DATA SET SPGENPRD   AT LEVEL 014 AS OF 05/11/93                      
*              PRODUCT HEADER RECORD                                            
         SPACE 1                                                                
PRDHDR   DS    0C                                                               
PKEY     DS    0CL13     V         KEY                                          
PKEYTYPE DS    CL1       B         RECORD TYPE X'00'                            
PKEYAM   DS    CL1       A/M       AGENCY/MEDIA                                 
PKEYCLT  DS    CL2       CLT       CLIENT CODE                                  
PKEYPRD  DS    CL3       A         PRODUCT CODE                                 
PKEYZRO  DS    CL6       B         BINARY ZEROS                                 
         SPACE 2                                                                
PLEN     DS    CL2       B         RECORD LENGTH (240)                          
PCNTRL   DS    CL1       B         CONTROL BYTE                                 
PLINKS   DS    CL4       B         LINK FIELDS                                  
         DS    CL4       B         SPARE                                        
         SPACE 2                                                                
PACCT    DS    CL4       A/N       ACCOUNT NUMBER                               
PNAME    DS    CL20      A         PRODUCT NAME                                 
PCODE    DS    CL2       B         PRODUCT CODE                                 
PADDR1   DS    CL30      A/N       BILL ADDRESS LINE 1                          
PADDR2   DS    CL30      A/N       BILL ADDRESS LINE 2                          
PADDR3   DS    CL30      A/N       BILL ADDRESS LINE 3                          
PADDR4   DS    CL30      A/N       BILL ADDRESS LINE 4                          
PDIV     DS    CL3       A/N       DIVISION CODE                                
PBILLDT  DS    CL2       B         EFFECTIVE Y/M OF SERVICE                     
PBILLBAS DS    CL1       B         2 4-BIT FIELDS - BILL BASE/COMM BASE         
*                                  B'0000' = GROSS, B'0001' = NET               
PBILLCOM DS    CL4       B         SIGNED COMMISSION (99.9999)                  
PAGYFEE  DS    CL2       P         OTHER AGENCY FEE (2 IMPLIED DEC)             
PPROF    DS    CL30      A/N       PROFILE                                      
PGRP1    DS    CL3                 PRDGRP ASSGN                                 
PGRP2    DS    CL3                 PRDGRP ASSGN                                 
PGRP3    DS    CL3                 PRDGRP ASSGN                                 
PCLASS   DS    CL1                 PRODUCT CLASS                                
PGRP4    DS    CL3                 PRDGRP ASSGN                                 
PGRP5    DS    CL3                 PRDGRP ASSGN                                 
PLOCK    DS    CL1                 PRD LOCKED                                   
PLKDAT   DS    CL2                 PRD LOCK ACTV DATE  (COMPRESSD)              
PGSTCODE DS    CL1                 GOODS AND SERVICE TAX                        
         DS    CL8                 SPARE                                        
PUSER1   DS    XL32                USER FIELD 1                                 
PUSER2   DS    XL16                USER FIELD 2                                 
PPST     DS    CL10                PST CODES                                    
PTALAGY  DS    CL6                 TALENT AGENCY                                
         DS    CL32                SPARE                                        
PRDHDRL  EQU   *-PRDHDR                                                         
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
* DDCNTRL                                                                       
*                                                                               
       ++INCLUDE DDCNTRL                                                        
*                                                                               
* DMWRKRK                                                                       
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056ACREPZ502 08/16/00'                                      
         END                                                                    
