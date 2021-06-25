*          DATA SET ACREP7702  AT LEVEL 015 AS OF 05/13/09                      
*PHASE AC7702A                                                                  
*INCLUDE ACCDIV                                                                 
*INCLUDE ACLABEL2                                                               
*INCLUDE ACLABEL3                                                               
*INCLUDE SORTER                                                                 
*INCLUDE GETLOGO                                                                
         TITLE 'STICKY LABELS/DIRECT CAPABILITY/ALPHA SORT OPTION'              
         USING ACWORKD,RA                                                       
         USING LINESD,R7                                                        
AC7702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*AC7702*,RR=R4                                                 
         L     RA,0(,R1)                                                        
         L     R7,ADIO                                                          
         L     R8,ADOUT                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                INITIALIZATIONS FOR THE RUN.                 
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST             LEDGER FIRST                           
         BE    LDGF                                                             
         CLI   MODE,PROCLEVA             PROCESS LEVEL A                        
         BE    PLEVA                                                            
         CLI   MODE,PROCLEVB             PROCESS LEVEL B                        
         BE    PLEVB                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
                                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
*                                                                               
         USING MASTD,R6                                                         
         USING REMOTED,R2                                                       
         L     R6,ADMASTC                                                       
         MVC   UPSI,MCUPSI                                                      
         MVI   OPENSW,NO                                                        
         MVI   DIRECT,NO                                                        
         L     R3,=V(ACLABEL3)                                                  
*&&US                                                                           
         TM    UPSI,X'80'                                                       
         BO    RUNF04              OPTION TO TEST AS IF DIRECT                  
         ICM   R2,15,REMOTEC                                                    
         BZ    RUNF05              NOT GOING TO QUEUE                           
         OC    REMOTKEY,REMOTKEY                                                
         BZ    RUNF05                                                           
         CLC   REMOTFRM,=C'3LBL'  FORCE DIRECT JOB TO 3 LABEL                   
         BE    RUNF05              YES                                          
                                                                                
RUNF04   L     R3,=V(ACLABEL2)                                                  
         MVI   DIRECT,YES                                                       
*&&                                                                             
                                                                                
RUNF05   AR    R3,R4                                                            
         ST    R3,ALABELS                                                       
         DROP  R2                                                               
                                                                                
         L     R3,=V(ACCDIV)                                                    
         AR    R3,R4                                                            
         ST    R3,ACCDIV                                                        
         L     R3,=V(SORTER)                                                    
         AR    R3,R4                                                            
         ST    R3,SORTER                                                        
         L     R3,=A(SORTWK)                                                    
         AR    R3,R4                                                            
         ST    R3,ASORTWK                                                       
*                                                                               
*&&US*&& GOTO1 PRINT,DMCB,SPACES,=C'BC01'      SKIP TO CHANNEL 1                
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     CLI   QSORT,C'A'                                                       
         BNE   REQF10                                                           
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
                                                                                
REQF10   OC    QOPT2(3),=C'000'    SET LABEL COUNT FROM QOPT2/3/4               
         PACK  DUB,QOPT2(3)                                                     
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                                                             
         ST    R0,COUNT                                                         
         L     R3,PRINT                                                         
*                                                                               
         SR    R4,R4                                                            
*&&UK*&& LA    R4,LABLK                                                         
         CLI   DIRECT,YES                                                       
         BNE   *+8                                                              
         LA    R4,LABLK                                                         
                                                                                
         CLI   OPENSW,NO                                                        
         BNE   REQFXIT                                                          
         BAS   RE,BLDBLK                                                        
         GOTO1 ALABELS,DMCB,=C'$OPEN',(R3),1,1,8,(R4)                           
         MVI   OPENSW,YES                                                       
                                                                                
REQFXIT  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R2                                                        
LDGF     CLC   QUNIT(2),=C'1R'                                                  
         BNE   LDGFXIT                                                          
         L     R2,ADLDGHIR                                                      
         MVI   RLEVEL,3                                                         
         CLI   ACLVLEN,0           IS THERE A 4TH LEVEL ?                       
         BE    LDGFXIT             NO                                           
         MVI   RLEVEL,4            YES                                          
LDGFXIT  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS LEVEL A                                                    *          
**********************************************************************          
         SPACE 1                                                                
PLEVA    CLC   QUNIT(2),=C'SJ'     LABEL FOR CLIENT ON SJ                       
         BNE   PLEVAX                                                           
         L     R2,ADLVANAM                                                      
         LTR   R2,R2                                                            
         BZ    PLEVAX                                                           
         BRAS  RE,CLRLABEL                                                      
         BAS   RE,NAMOUT                                                        
         L     R2,ADLVAADD                                                      
         BAS   RE,PROCUL                                                        
*                                                                               
PLEVAX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS LEVEL B                                                    *          
**********************************************************************          
         SPACE 1                                                                
PLEVB    DS    0H                                                               
                                                                                
*&&UK*&& CLC   QUNIT(2),=C'ST'     LABELS FOR AGENTS ON UK ARTISTS              
*&&UK*&& BE    PLEVB10                                                          
         CLC   QUNIT(2),=C'SJ'   LABEL FOR PRODUCT ON SJ                        
         BNE   PLEVBX                                                           
                                                                                
PLEVB10  L     R2,ADLVBNAM                                                      
         LTR   R2,R2                                                            
         BZ    PLEVBX            NO NAME NO LABEL                               
         BRAS  RE,CLRLABEL                                                      
         BAS   RE,NAMOUT                                                        
         L     R2,ADLVBADD                                                      
         LTR   R2,R2                                                            
         BZ    PLEVBX              NO ADDR NO LABEL                             
*                                                                               
         BAS   RE,PROCUL                                                        
*                                                                               
PLEVBX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
*              ROUTINE TO WRITE OUT STICKY LABELS                    *          
**********************************************************************          
         SPACE 3                                                                
PACC     L     R2,ADACC                                                         
         TM    ACTKSTAT-ACTRECD(R2),ACTSDRFT                                    
         BO    PACCXIT                                                          
*                                                                               
         USING RSTELD,R2                                                        
         CLC   QUNIT(2),=C'SJ'     NONE FOR SJ JOBS                             
         BE    PACCXIT                                                          
*&&UK*&& CLC   QUNIT(2),=C'ST'     NO LABELS FOR ARTISTS                        
*&&UK*&& BE    PACCXIT                                                          
         L     R2,ADACCSTA                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STRDTE)                                
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDDTE)                                  
*                                                                               
         CLC   QSTART,SPACES       REQ START DATE?                              
         BH    *+10                                                             
         MVC   STRDTE,=X'000000'                                                
         CLC   QEND,SPACES         REQ END DATE?                                
         BH    *+10                                                             
         MVC   ENDDTE,=X'FFFFFF'                                                
*                                                                               
         CLC   RSTTDATE,STRDTE                                                  
         BL    PACCXIT                                                          
         CLC   RSTTDATE,ENDDTE                                                  
         BH    PACCXIT                                                          
*                                                                               
         CLI   QOPT1,C' '                                                       
         BE    PACC20                                                           
         CLI   QOPT1,C'S'          SUPPRESS LOCKED                              
         BNE   PACC10                                                           
         TM    RSTSTAT,RSTSACIL    LOCKED ?                                     
         BO    PACCXIT                                                          
         B     PACC20                                                           
                                                                                
PACC10   TM    RSTSTAT,RSTSACIL    LOCKED ?                                     
         BZ    PACCXIT             NO, WANT LOCKED ONLY                         
                                                                                
PACC20   BRAS  RE,CLRLABEL                                                      
         L     R2,ADACCNAM                                                      
         BAS   RE,NAMOUT                                                        
         BAS   RE,PROCUL                                                        
*                                                                               
PACCXIT  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
REQL     DS   0H                                                                
                                                                                
         CLI   QSORT,C'A'          ANY SORTED LABELS OUT THERE?                 
         BNE   *+8                                                              
         BAS   RE,SRTLBS           GO TO SORTED GET/PRINT LOOP RTN.             
         CLI   DIRECT,YES                                                       
         BE    REQLX                                                            
                                                                                
*&&US*&& GOTO1 ALABELS,DMCB,=C'$CLOSE'                                          
*&&US*&& MVI   OPENSW,NO                                                        
                                                                                
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         CLI   DIRECT,YES                                                       
         BNE   RUNLX                                                            
                                                                                
         GOTO1 ALABELS,DMCB,=C'$CLOSE'                                          
         MVI   OPENSW,NO                                                        
RUNLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CLEAR LABEL AREA                                                              
**********************************************************************          
CLRLABEL MVC   LINE1,SPACES                                                     
         MVC   LINE2,SPACES                                                     
         MVC   LINE3,SPACES                                                     
         MVC   LINE4,SPACES                                                     
         MVC   LINE5,SPACES                                                     
         MVC   LINE6,SPACES                                                     
         MVC   LINE7,SPACES                                                     
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* PROCUL - PROCESSES U/L                                             *          
**********************************************************************          
PROCUL   NTR1                                                                   
         LA    R4,LINE2                                                         
         CLC   QUNIT(2),=C'1R'                                                  
         BE    PROCUL20                                                         
                                                                                
         USING ADRELD,R2                                                        
         L     R2,ADACCADD                                                      
         LTR   R2,R2                                                            
         BZ    PROCUL50                                                         
         IC    R3,ADRNUM           NUMBER OF LINES                              
         LTR   R3,R3                                                            
         BZ    PROCUL50                                                         
                                                                                
         LA    R5,ADRADD1          START ON LINE ONE                            
PROCUL10 MVC   0(L'ADRADD1,R4),0(R5)                                            
         AHI   R4,L'LINE1                                                       
         AHI   R5,L'ADRADD1                                                     
         BCT   R3,PROCUL10                                                      
         DROP  R2                                                               
                                                                                
         USING ACTRECD,R5                                                       
         CLI   PROGPROF,C'Y'       PRINT ACCOUNT NO.                            
         BNE   PROCUL60                                                         
         L     R5,ADACC                                                         
         MVC   LINE7(14),ACTKULA                                                
         B     PROCUL60                                                         
         DROP  R5                                                               
*                                                                               
*  PROCESS 1R LABLES                                                            
*                                                                               
PROCUL20 DS    0H                  SPECIAL FOR COSTING/STAFF                    
         LA    R4,LINE3                                                         
         L     R2,ADLDGHIR                                                      
         L     R5,ADACC                                                         
         GOTO1 ACCDIV,DMCB,(R2),(R5),WORK                                       
*                                                                               
         USING LEVELD,R5                                                        
         LA    R5,WORK                                                          
         CLI   RLEVEL,4                                                         
         BE    PROCUL30                                                         
         ZIC   R1,LEV1LEN                                                       
         EXMVC R1,LINE3,LEV1CODE   DEPT OR OFFICE/DEPT                          
         ZIC   R1,LEV2LEN                                                       
         EXMVC R1,LINE4,LEV2CODE   SUB-DEPT                                     
         ZIC   R1,LEV3LEN                                                       
         EXMVC R1,LINE5,LEV3CODE   EMPLOYEE                                     
         B     PROCUL40                                                         
*                                                                               
PROCUL30 ZIC   R1,LEV1LEN                                                       
         EXMVC R1,LINE3,LEV1CODE   OFFICE                                       
         LA    RF,LINE3                                                         
         AHI   R1,1                LENGTH IS -1 SO ADD BACK1                    
         AR    RF,R1               BUMP PAST OFFICE                             
         ZIC   R1,LEV2LEN                                                       
         EXMVC R1,0(RF),LEV2CODE   DEPT                                         
         ZIC   R1,LEV3LEN                                                       
         EXMVC R1,LINE4,LEV3CODE   SUB-DEPT                                     
         ZIC   R1,LEV4LEN                                                       
         EXMVC R1,LINE5,LEV4CODE   EMPLOYEE                                     
*                                                                               
         USING NAMELD,R2                                                        
PROCUL40 L     R2,ADLVBNAM         DEPT NAME                                    
         CLI   RLEVEL,4                                                         
         BE    *+8                                                              
         L     R2,ADLVANAM                                                      
                                                                                
         ZIC   R3,NAMLN                                                         
         SHI   R3,NAMLN1Q                                                       
         GOTO1 CHOPPER,DMCB,((R3),NAMEREC),(32,LINE3+5),1                       
         CLC   QEND,SPACES                                                      
         BE    PROCUL50                                                         
         GOTO1 DATCON,DMCB,(0,QEND),(8,LINE6)                                   
*                                                                               
         USING ACTRECD,R5                                                       
PROCUL50 CLI   PROGPROF,C'Y'       PRINT ACCOUNT NO.                            
         BNE   PROCUL60                                                         
         L     R5,ADACC                                                         
         MVC   LINE7(14),ACTKULA                                                
         DROP  R5                                                               
*                                                                               
         USING SORTD,R8                                                         
PROCUL60 CLI   QSORT,C'A'          WANT SORTED LABELS?                          
         BNE   PROCUL70                                                         
         L     R8,ASORTWK                                                       
         XC    SRLEN(4),SRLEN                                                   
         MVC   SRLEN,=H'313'                                                    
         MVC   SRREC(256),0(R7)                                                 
         MVC   SRREC+256(17),256(R7)                                            
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     PROCULX                                                          
         DROP  R8                                                               
*                                                                               
PROCUL70 L     R0,COUNT                                                         
PROCUL80 GOTO1 ALABELS,DMCB,(R7)                                                
         BCT   R0,PROCUL80                                                      
*                                                                               
PROCULX  XIT1                                                                   
         DROP R2                                                                
         EJECT                                                                  
**********************************************************************          
* NAMOUT - PULLS OUT A NAME                                          *          
**********************************************************************          
         USING NAMELD,R2                                                        
NAMOUT   NTR1                                                                   
*                                                                               
         LTR   R2,R2                                                            
         BZ    NAMEXIT                                                          
         ZIC   R3,NAMLN                                                         
         SHI   R3,3                                                             
         EXMVC R3,LINE1,NAMEREC                                                 
                                                                                
         USING SORTD,R8                                                         
         CLI   QSORT,C'A'                                                       
         BNE   NAMEXIT                                                          
         L     R8,ASORTWK                                                       
         MVC   SRNAME,SPACES                                                    
         EXMVC R3,SRNAME,NAMEREC                                                
         DROP  R8                                                               
                                                                                
NAMEXIT  XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* SRTLBS - ROUTINE TO WRITE SORTED LABELS                            *          
**********************************************************************          
         USING SORTD,R8                                                         
SRTLBS   NTR1                                                                   
*                                                                               
SRTLBL10 GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R8,15,4(R1)                                                      
         BZ    SRTLBLX                                                          
                                                                                
         LA    R8,SRREC                                                         
         L     R0,COUNT                                                         
SRTLBL20 GOTO1 ALABELS,DMCB,(R8)                                                
         BCT   R0,SRTLBL20                                                      
         B     SRTLBL10                                                         
*                                                                               
SRTLBLX  XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
**********************************************************************          
* BLDBLK - BUILD LABEL LINEUP BLOCK                                  *          
**********************************************************************          
         USING LOGOD,R4                                                         
         USING LBBLKD,R3                                                        
BLDBLK   NTR1                                                                   
         LA    R3,LABLK                                                         
         L     R4,LOGOC                                                         
         LA    R5,DMCB                                                          
         EXTRACT (5),'S',FIELDS=TIOT                                            
         L     R5,DMCB                                                          
         MVC   LBJNM,0(R5)         JOBNAME                                      
         MVC   LBSTPNM,8(R5)       STEPNAME                                     
         GOTO1 =V(GETLOGO),DMCB,ORIGINUM,(R4),DATAMGR,RR=RB                     
                                                                                
*&&UK*&& MVC   LBLOG2,LOGO2                                                     
         MVC   LBLOGNM,LOGONAME                                                 
         MVC   LBLOGAD,LOGOADD                                                  
*&&US                                                                           
         USING CTIREC,R2                                                        
         L     R2,=A(IO)                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,ORIGINUM                                                 
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=CL8'CTFILE',(R2),(R2),0               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'               MUST HAVE ORIGIN RECORD                      
         LA    R1,CTIDATA                                                       
BLDBLK10 CLI   0(R1),EOR                                                        
         BNE   *+6                                                              
         DC    H'00'               MUST HAVE ORIGIN ID                          
         CLI   0(R1),CTDSCELQ      X'02'                                        
         BE    BLDBLK20                                                         
         ZIC   RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     BLDBLK10                                                         
                                                                                
         USING CTDSCD,R1                                                        
BLDBLK20 ZIC   RF,CTDSCLEN                                                      
         SHI   RF,(CTDSC-CTDSCD)                                                
         BP    *+6                                                              
         DC    H'00'               MUST HAVE ID                                 
         CHI   RF,L'LBORGNID                                                    
         BNH   *+8                                                              
         LA    RF,L'LBORGNID                                                    
         EXMVC RF,LBORGNID,CTDSC                                                
         DROP  R1,R2                                                            
*&&                                                                             
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT ,                                                                
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
**********************************************************************          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   2024                                                             
EOR      EQU   0                                                                
                                                                                
ALABELS  DS    A                                                                
ACCDIV   DS    A                                                                
ASORTWK  DS    F                                                                
SORTER   DS    F                                                                
COUNT    DS    F                                                                
STRDTE   DS    PL3                                                              
ENDDTE   DS    PL3                                                              
LABLK    DS    CL(LBBLKLNQ)                                                     
OPENSW   DS    CL1                                                              
RLEVEL   DS    CL1                                                              
DIRECT   DS    CL1                                                              
UPSI     DS    X                                                                
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,36,A),FORMAT=CH,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=313'                                   
         EJECT                                                                  
***********************************************************************         
*  WORK AREAS                                                         *         
***********************************************************************         
IO       DS    0D                                                               
         DC    C'***IO***'                                                      
         DS    XL(2*K)                                                          
                                                                                
         DS    0D                                                               
SORTWK   DC    C'**SRTW**'                                                      
         DS    7000C                                                            
         EJECT 1                                                                
***********************************************************************         
* DSECT TO COVER SORT WORK RECORD                                     *         
***********************************************************************         
SORTD    DSECT                                                                  
SORTREC  DS    0CL313                                                           
SRLEN    DS    H                                                                
         DS    H                                                                
SRNAME   DS    CL36                                                             
SRREC    DS    273C                                                             
                                                                                
***********************************************************************         
* DSECT FOR LEVELS OF LEDGER                                          *         
***********************************************************************         
LEVELD   DSECT                                                                  
LEV1LEN  DS    CL1                 LENGTH -1                                    
LEV1CODE DS    CL12                CODE                                         
LEV2LEN  DS    CL1                                                              
LEV2CODE DS    CL12                                                             
LEV3LEN  DS    CL1                                                              
LEV3CODE DS    CL12                                                             
LEV4LEN  DS    CL1                                                              
LEV4CODE DS    CL12                                                             
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
* ACLABELD                                                                      
       ++INCLUDE ACLABELD                                                       
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACREP7702 05/13/09'                                      
         END                                                                    
