*          DATA SET ACREPZC02  AT LEVEL 039 AS OF 12/22/99                      
*PHASE ACZC02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'CHECK PEELED TRANSACTIONS'                                      
ACZC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZC**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZCD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                      LOOKUP TRANSACTION INFO                
         CLI   MODE,SBACLAST             LAST CONTRA TRANS                      
         BE    PSBACL                                                           
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
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
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
         CLC   =C'SJ',TRNKCUNT             ONLY SJ CONTRA ACCS                  
         BNE   PTRNX                                                            
*                                                                               
         CLI   TRNTYPE,TRNTMABL    MANUAL BILLING TYPE 6                        
         BNE   PTRNX                                                            
*                                                                               
         MVI   FLAG,0                                                           
*        CLC   =C'TOTAL',TRNNARR                                                
*        BNE   *+12                                                             
*        OI    FLAG,FLAGTOT                                                     
*        B     PTRN10                                                           
         CLC   =C'100',TRNNARR                                                  
         BNE   PTRNX                                                            
         OI    FLAG,FLAGPCT                                                     
*                                                                               
PTRN10   MVC   SVREF,TRNREF        SAVE REFERENCE                               
         MVC   SVACT,TRNKCULC      SAVE SJ CONTRA                               
*                                                                               
*                                                                               
PTRNX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS SUB ACCOUNT LAST                                           *          
**********************************************************************          
         SPACE 1                                                                
PSBACL   DS    0H                                                               
         TM    FLAG,FLAGPCT                                                     
         BZ    PSBACLX                                                          
         USING TRNTD,R3                                                         
         LA    R3,TRNTWRK                                                       
         XC    TRNTWRK,TRNTWRK                                                  
*                                                                               
         MVC   TRNTCULA,SVACT      PUT ACCOUNT IN TABLE                         
         MVC   TRNTREF,SVREF       PUT REF IN TABLE                             
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),TRNTWRK,ATRNTTAB ADD TABLE ENTRY               
PSBACLX  B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R1                                                          
REQL     DS    0H                                                               
         L     R1,ATRNTTAB         R1=A(TRANSACTION TABLE)                      
         ICM   R0,15,BININ                                                      
         BZ    REQLX                                                            
         USING TRNTD,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         USING TRNRECD,R3                                                       
REQL10   LA    R3,SVKEY                                                         
         MVC   SVKEY,SPACES                                                     
*                                                                               
         MVC   TRNKCULA,TRNTCULA                                                
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     REQL12                                                           
*                                                                               
REQL11   GOTO1 =A(DMSEQDR),DMCB,(RC)     READ SEQUENTIAL                        
REQL12   CLC   SVKEY(TRNKWORK-TRNKEY),IOKEY         SAME KEY??                  
         BNE   REQL50                                                           
*                                                                               
         MVC   MSG,=CL10'KEY IN'                                                
         GOTO1 ADUMP,DMCB,(RC),(R3),L'TRNKEY                                    
*                                                                               
         LA    R3,IOKEY                                                         
         CLC   TRNKWORK,=C'99'     ONLY NEED 99                                 
         BNE   REQL11                                                           
         CLC   TRNKCUNT(2),=C'SR'  ONLY NEED SR CONTRA                          
         BNE   REQL11                                                           
         CLC   TRNKREF,SPACES      ANY REF                                      
         BNH   REQL11                                                           
*                                                                               
REQL15   GOTO1 =A(DMGETREC),DMCB,(RC)     GET RECORD                            
         LA    R3,IO                                                            
         LA    R4,TRNRFST                                                       
*                                                                               
         MVC   MSG,=CL10'REC IN'                                                
         SR    R6,R6                                                            
         ICM   R6,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R3),(R6)                                        
*                                                                               
         USING INCELD,R4                                                        
REQL20   CLI   0(R4),0             EOR, DON'T HAVE INCOME ELEMENT               
         BNE   REQL25              SHOULD HAVE ONE                              
         MVC   PDESC,=CL20'NO B7 ON RECORD'                                     
         B     REQL45                                                           
*                                                                               
REQL25   CLI   INCEL,INCELQ        INCOME ELEMENT 'B7'                          
         BE    REQL40                                                           
REQL30   SR    R1,R1                                                            
         IC    R1,INCLN            NO, BUMP UNTIL WE FIND IT                    
         AR    R4,R1                                                            
         B     REQL20                                                           
*                                                                               
REQL40   CP    INCAMNT,=P'0'       IS B7'S INCOME ZERO                          
         BNE   REQL11              NO-FIND NEXT ONE                             
         MVC   PDESC,=CL20'B7 WITH ZERO AMOUNT'                                 
*                                                                               
REQL45   MVC   PACC,TRNKULA                                                     
         MVC   PCULA,TRNKCUNT                                                   
         MVC   PREF,TRNKREF                                                     
         MVC   PWC,TRNKWORK                                                     
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(5,PDATE)                               
         GOTO1 ACREPORT                                                         
         B     REQL11                                                           
*                                                                               
REQL50   LA    R2,TRNTLNQ(R2)                                                   
         BCT   R0,REQL10                                                        
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ATRNTTAB DC    A(TRNTTAB)          TRANSACTION TABLE                            
ABINADD  DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
ADUMP    DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
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
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
*        CLI   DMCB,1              RECORD WAS ADDED                             
*        BE    BINXIT                                                           
*        DC    H'0'                                                             
*                                                                               
*        L     R4,DMCB             A(RECORD FOUND)                              
*        SR    R0,R0                                                            
*        ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
*        BZ    BINXIT                NO BUCKETS - EXIT                          
*        SR    R6,R6                                                            
*        IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
*        AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
*        AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
* BINA10   AP    0(TRNTBKLN,R4),0(TRNTBKLN,R3) ADD TO BUCKET                    
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
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',ACCKDA,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
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
         DC    C'***RANK***'                                                    
TRNTTAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TRNTLNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TRNTLNQ)            KEY LENGTH                               
         DC    AL4(TRNTMAX)            MAX IN TABLE                             
         DS    (TRNTMAX*TRNTLNQ)XL1    TABLE                                    
*                                                                               
TRNTMAX  EQU   1000                                                             
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZCD    DSECT                                                                  
VTYPES   DS    0A                                                               
PRNTBL   DS    V                   PRINT DATA                                   
VHELLO   DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO KEY                          
ELCODE   DS    CL1                 ELEMENT CODE                                 
PKCOUNT  DS    PL4                                                              
FLAG     DS    XL1                 UPDATE FLAG                                  
FLAGTOT  EQU   X'80'               TRNNARR IS TOTAL                             
FLAGPCT  EQU   X'40'               TRNNARR IS PERCENT                           
FLAGB7   EQU   X'20'               LOOKING FOR B7 ELEMENT                       
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
*                                                                               
SVKEY    DS    CL49                SAVED AREA FOR KEY                           
SVACT    DS    CL15                SAVED AREA FOR ACCOUNT                       
SVREF    DS    CL6                 SAVED AREA FOR REFERENCE                     
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
PACC     DS    CL14                                                             
         DS    CL2                                                              
PWC      DS    CL2                       WORK CODE/OFFICE CODE                  
         DS    CL2                                                              
PCULA    DS    CL14                      CONTRA UNIT,LEDGER,ACCOUNT             
         DS    CL2                                                              
PDATE    DS    CL8                       DATE                                   
         DS    CL2                                                              
PREF     DS    CL6                       REFERENCE NUM.                         
         DS    CL2                                                              
PDESC    DS    CL20                      DESCRIPTION                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN TRANSACTION TABLE                                *         
***********************************************************************         
         SPACE 1                                                                
TRNTD    DSECT                                                                  
TRNTCULA DS    CL15                  TRANSACTION CONTRA                         
TRNTREF  DS    CL6                   TRANSACTION REFERENCE                      
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
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
* ACREPWORKD                                                                    
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* DDLOGOD                                                                       
* ACMASTD                                                                       
* DDMASTD                                                                       
* DDBIGBOX                                                                      
* CTGENFILE                                                                     
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
**PAN#1  DC    CL21'039ACREPZC02 12/22/99'                                      
         END                                                                    
