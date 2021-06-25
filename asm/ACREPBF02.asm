*          DATA SET ACREPBF02  AT LEVEL 018 AS OF 08/18/98                      
*PHASE ACBF02A                                                                  
*                                                                               
         TITLE 'BILLING PRINT FLAT FILE'                                        
ACBF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBF**,R9       BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACBFD,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEVAFRST       FIRST CLIENT?                                
         BE    LEVA                 Y, PROCESS                                  
         CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BE    PRAC                                                             
         CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BE    PRTR                                                             
         CLI   MODE,RUNLAST        RUN LAST                                     
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         DROP  R2,R4                                                            
*                                                                               
         XC    PRTFLG,PRTFLG                                                    
         MVC   SVPLINE,SPACES                                                   
*                                                                               
         LA    R2,DELITAB          DELITAB HAS POSITIONS OF DELIMITERS          
         LA    R3,SVPLINE                                                       
RUNF10   CLI   0(R2),0           SET UP DELIMITERS ON DUMMY PRINT LINE          
         BE    RUNF20                                                           
         ZIC   R1,0(R2)                                                         
         AR    R1,R3                                                            
         MVI   0(R1),DELIMITR                                                   
         LA    R2,1(R2)                                                         
         B     RUNF10                                                           
*                                                                               
         LA    R2,AGYTAB                                                        
RUNF20   CLI   0(R2),X'FF'         MAKE SURE VALID AGENCY TO OUTPUT             
         BNE   *+12                                                             
         MVI   FCRDTRNS,C'N'       CAN'T HAVE EDI REPORT                        
         B     RUNFX                                                            
         CLC   0(2,R2),ALPHAID                                                  
         BE    RUNFX                                                            
         LA    R2,L'AGYTAB(R2)                                                  
         B     RUNF20                                                           
*                                                                               
RUNFX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
*                                                                               
REQF     DS    0H                                                               
*                                                                               
         CLI   QOPT1,C'Y'             DO WE WANT OUTPUT TAPE?                   
         BNE   REQF10                  NO                                       
         TM    PRTFLG,OPENFIL         FILE ALREADY OPEN?                        
         BO    REQF10                  NO                                       
         MVC   DSPARM+13(2),ALPHAID   FILL IN TAPE DATASET NAME                 
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),DSPARM                                  
         OPEN  (BFFOUT,OUTPUT)        NO, OPEN IT                               
         OI    PRTFLG,OPENFIL                                                   
*                                                                               
REQF10   XC    SVSTDT,SVSTDT                                                    
         CLC   QSTART,SPACES                                                    
         BE    REQF20                                                           
         GOTO1 DATCON,DMCB,(0,QSTART),(1,SVSTDT)                                
*                                                                               
REQF20   MVC   SVENDDT,=X'FFFFFF'                                               
         CLC   QEND,SPACES                                                      
         BE    REQFX                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,SVENDDT)                                 
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PROCESS LEVEL A                                                     *         
***********************************************************************         
*                                                                               
LEVA     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'                                                    
*                                                                               
         USING ACTRECD,R4                                                       
         L     R4,ADHEIRA          GET THE CLIENT CODE                          
         MVC   SVCLTCD,ACTKACT                                                  
*                                                                               
         LA    R2,INFOTAB                                                       
LEVA10   CLI   0(R2),X'FF'         MAKE SURE VALID CLIENT FOR OUTPUT            
         BNE   *+12                                                             
         MVI   FCRDTRNS,C'N'                                                    
         B     LEVAX                                                            
         CLC   SVCLTCD,0(R2)                                                    
         BE    LEVA20                                                           
         LA    R2,ITABQ(R2)                                                     
         B     LEVA10                                                           
*                                                                               
         USING ITABD,R2                                                         
LEVA20   MVC   SVVENNO,ITVENNO         PULL HARD CODED VALUES FROM TAB          
         MVC   SVADDSQ,ITADDSQ                                                  
         MVC   SVSACSRC,ITSACSRC                                                
         MVC   SVREFTYP,ITREFTYP                                                
*                                                                               
LEVAX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*******************************************************************             
* PROCESS ACCOUNT                                                 *             
*******************************************************************             
*                                                                               
PRAC     DS    0H                                                               
*                                                                               
         USING ACTRECD,R4                                                       
         L     R4,ADACC            GET JOB CODE AND NAME                        
         XC    SVJOBCD,SVJOBCD                                                  
         MVC   SVJOBCD,ACTKACT+6                                                
         OC    SVJOBCD,SPACES                                                   
*                                                                               
         MVC   SVBUDCD,SPACES                                                   
         MVC   SVFISCYR,SPACES                                                  
         MVC   SVPROJNO,SPACES                                                  
*                                                                               
* ----------------------------------     USER FIELDS                            
         USING UFSELD,R4                                                        
         L     R4,ADACC                                                         
*                                                                               
         MVI   ELCODE,UFSELQ       X'A2'                                        
         BAS   RE,GETEL            FIND THE USER ELEMENT                        
         B     *+8                                                              
PRAC10   BAS   RE,NEXTEL                                                        
         BNE   PRACX                                                            
*                                                                               
         USING USERTABD,R2                                                      
         LA    R2,USERTAB                                                       
*                                                                               
PRAC15   CLI   UTUSRCD,EOT         END OF USER TABLE?                           
         BE    PRAC10                                                           
*                                                                               
         CLC   UTUSRCD,UFSCODE     A DESIRED USER CODE?                         
         BNE   PRAC20                NO                                         
*                                                                               
         ZIC   RF,UFSLN            ELEMENT LENGTH                               
         LA    R0,UFSLN1Q          OVERHEAD TO DATA                             
         SR    RF,R0               IS THERE ANY DATA?                           
         BNP   PRAC20              NO                                           
*                                                                               
         ZIC   R1,UTOUTLN          OUTPUT LENGTH                                
         CR    RF,R1               IS DATA LEN > OUTPUT LEN                     
         BNH   *+8                                                              
         LR    RF,R1               IF SO USE OUTPUT LEN                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,UTOUTAD        LOAD DISPLACEMENT TO OUTPUT FIELD            
         LA    R1,ACBFD(R1)                                                     
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R1),UFSDATA                                                  
*                                                                               
PRAC20   LA    R2,USERTABQ(R2)     BUMP TO NEXT USER TAB ENTRY                  
         B     PRAC15                                                           
* ----------------------------------                                            
*                                                                               
PRACX    B     EXIT                                                             
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS TRANSACTION                                             *             
*******************************************************************             
*                                                                               
PRTR     DS    0H                                                               
*                                                                               
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
         CLI   TRNEL,X'44'                                                      
         BNE   PRTRX                                                            
         CLC   TRNANAL,=C'99'                                                   
         BNE   PRTRX                                                            
*                                                                               
*                                           DATE CHECK                          
PRTR15   CLC   TRNDATE,SVSTDT      RUN DATE < USER SPEC. START DATE?            
         BL    PRTRX               YES                                          
         CLC   TRNDATE,SVENDDT    RUN DATE > USER SPEC. END DATE?               
         BH    PRTRX               YES                                          
*                                                                               
         MVC   SVBLDTE,TRNDATE                                                  
         MVC   SVBLNO,TRNREF                                                    
         ZAP   SVBLAMT,TRNAMNT                                                  
         BAS   RE,GETDUEDT         DUE DATE IN SVDUEDTE                         
         BAS   RE,PRNT                                                          
*                                                                               
PRTRX    B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*******************************************************************             
*        PROGRAM IS FINISHED                                                    
*------------------------------------------------------------------             
*                                                                               
RUNL     DS    0H                                                               
         TM    PRTFLG,OPENFIL          WAS OUPUT FILE OPENED?                   
         BZ    RUNLX                     NO                                     
         CLOSE (BFFOUT)                 YES, CLOSE IT                           
RUNLX    B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
* PRINT INFORMATION FROM TABLE                                  *               
*****************************************************************               
*                                                                               
         USING PLINED,R4                                                        
PRNT     NTR1                                                                   
         LA    R4,XP                                                            
         MVC   0(PRLNQ,R4),SVPLINE                                              
*                                                                               
         MVC   PVENNO,SVVENNO                                                   
*                                                                               
         MVC   PREFNO(1),SVJOBCD                                                
         MVC   PREFNO+1(L'SVBLNO),SVBLNO                                        
*                                                                               
         MVC   PADDSEQ,SVADDSQ                                                  
         MVC   PSACSRC,SVSACSRC                                                 
         MVC   PREFTYP,SVREFTYP                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(1,SVBLDTE),(20,WORK)                                
         MVC   PREFDTE(4),WORK+4                                                
         MVC   PREFDTE+4(4),WORK           NEED MMDDCCYY FORM                   
*                                                                               
*  PAY DATE IS DUE DATE -2 DAYS (CANNOT FALL ON A WEEKEND)                      
         OC    SVDUEDTE,SVDUEDTE                                                
         BZ    PRNT10                                                           
         GOTO1 DATCON,DMCB,(2,SVDUEDTE),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         LA    R2,2                START WITH 2                                 
         CLI   0(R1),1        IF MONDAY, BACK UP 3 DAYS                         
         BNE   *+8                                                              
         LA    R2,3                                                             
         CLI   0(R1),2        TUESDAY, BACK UP 4 DAYS                           
         BNE   *+8                                                              
         LA    R2,4                                                             
         LNR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,WORK,DUB,(R2)                                         
         GOTO1 DATCON,DMCB,(0,DUB),(20,WORK)    CCYYMMDD                        
         MVC   PPAYDTE(4),WORK+4                NEED MMDDCCYY                   
         MVC   PPAYDTE+4(4),WORK                                                
PRNT10   DS    0H                                                               
*                                                                               
         MVC   PGLACC(3),SVBUDCD+5    DON'T WANT THE '-' IN BUD CODE            
         MVC   PGLACC+3(4),SVBUDCD+9                                            
*                                                                               
         MVC   PCSTCNTR,SVBUDCD                                                 
*                                                                               
         MVC   PSUBSCD,=C'01'         SUBSIDIARY CODE                           
         CLC   SVBUDCD,=C'0110'                                                 
         BNE   *+10                                                             
         MVC   PSUBSCD,=C'07'                                                   
         CLC   SVBUDCD,=C'0877'                                                 
         BNE   *+10                                                             
         MVC   PSUBSCD,=C'10'                                                   
*                                                                               
         MVC   PESTNO(L'SVJOBCD),SVJOBCD                                        
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,WORK)    CCYYMMDD                          
         MVC   PCALYR,WORK                                                      
*                                                                               
         MVC   PPROJNO,SVPROJNO                                                 
         EDIT  SVBLAMT,PDISTAMT,2,FLOAT=-                                       
         MVC   PREMARKS,SPACES                                                  
*                                                                               
         MVC   PFISCYR,SVFISCYR       FISCAL YEAR                               
*                                                                               
         CLI   QOPT1,C'Y'             DO WE WANT OUTPUT TAPE?                   
         BNE   PRNT30                  NO                                       
         TM    PRTFLG,OPENFIL          WAS OUPUT FILE OPENED?                   
         BZ    PRNT30                    NO                                     
         PUT   BFFOUT,XP                                                        
*                                                                               
PRNT30   GOTO1 ACREPORT                                                         
*                                                                               
PRNTX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*****************************************************************               
*        GET THE DUE DATE                                                       
*****************************************************************               
*                                                                               
GETDUEDT NTR1                                                                   
         XC    SVDUEDTE,SVDUEDTE                                                
         MVC   BYTE,ELCODE         NEED TO SAVE THE ELCODE                      
         MVI   ELCODE,X'61'                                                     
         L     R4,ADTRANS                                                       
         BAS   RE,NEXTEL           FIND DUE DATE ELEMENT                        
         BNE   GDDX                NONE                                         
         USING DUEELD,R4                                                        
         MVC   SVDUEDTE,DUEDATE                                                 
         MVC   ELCODE,BYTE                                                      
GDDX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
* GETEL                                                        *                
****************************************************************                
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
****************************************************************                
* TABLES                                                       *                
****************************************************************                
*                                                                               
* TABLE FOR VALID AGENCIES REQUESTING THE BF                                    
*                                                                               
AGYTAB   DS    0X                                                               
*                                                                               
         DC    C'DW'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
* TABLE FOR THE POSITIONS OF THE DELIMETER                                      
*                                                                               
DELITAB  DS    0X                                                               
*                                                                               
         DC    AL1(PREFNO-1-PLINED)                                             
         DC    AL1(PADDSEQ-1-PLINED)                                            
         DC    AL1(PSACSRC-1-PLINED)                                            
         DC    AL1(PREFTYP-1-PLINED)                                            
         DC    AL1(PREFDTE-1-PLINED)                                            
         DC    AL1(PPAYDTE-1-PLINED)                                            
         DC    AL1(PGLACC-1-PLINED)                                             
         DC    AL1(PCSTCNTR-1-PLINED)                                           
         DC    AL1(PESTNO-1-PLINED)                                             
         DC    AL1(PCALYR-1-PLINED)                                             
         DC    AL1(PPROJNO-1-PLINED)                                            
         DC    AL1(PREMARKS-1-PLINED)                                           
         DC    AL1(PDISTAMT-1-PLINED)                                           
         DC    AL1(PSUBSCD-1-PLINED)                                            
         DC    AL1(PFISCYR-1-PLINED)                                            
         DC    AL1(0)                                                           
*                                                                               
*                                                                               
* HARD CODE INFORMATION TABLE                                                   
*                                                                               
INFOTAB  DS    0F                                                               
*                                                                               
         DC    CL3'TM ',CL8'01001539',CL3'001',CL6'100139',CL2'61'              
         DC    CL3'TMS',CL8'01001539',CL3'001',CL6'100139',CL2'61'              
*                                                                               
         DC    CL3'LEX',CL8'01029620',CL3'002',CL6'100611',CL2'71'              
         DC    CL3'LDA',CL8'01029620',CL3'002',CL6'100611',CL2'71'              
         DC    CL3'LDL',CL8'01029620',CL3'002',CL6'100611',CL2'71'              
*                                                                               
         DC    CL3'TOC',CL8'01021083',CL3'001',CL6'100139',CL2'61'              
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
* USER FIELD INFO TABLE (ALIGN)                                                 
*                                                                               
USERTAB  DS    0H                                                               
*                                                                               
         DC    C'BC',AL1(L'SVBUDCD),AL2(SVBUDCD-ACBFD)    BUDGET #              
         DC    C'FY',AL1(L'SVFISCYR),AL2(SVFISCYR-ACBFD)  FISCAL YEAR           
         DC    C'PJ',AL1(L'SVPROJNO),AL2(SVPROJNO-ACBFD)  PROJECT #             
*                                                                               
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
EOT      EQU   X'FF'            END OF TABLE                                    
         EJECT                                                                  
******************************************************************              
* LITERALS                                                       *              
******************************************************************              
*                                                                               
DDPARM   DC    CL8'BFFOUT'                                                      
DSPARM   DC    CL20'ACCTAPE.AC0BF**1'                                           
*                                                                               
BFFOUT   DCB   DDNAME=BFFOUT,DSORG=PS,MACRF=(PM),                      +        
               RECFM=FB,LRECL=PRLNQ,BLKSIZE=PRLNQ*10                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
* WORKING STORAGE                                                *              
******************************************************************              
*                                                                               
ACBFD    DSECT                                                                  
ELCODE   DS    X                                                                
FUSERCD  DS    CL2                                                              
*                                                                               
PRTFLG   DS    X                                                                
OPENFIL  EQU   X'80'                                                            
*                                                                               
SVSTDT   DS    PL3        QSTART (YMD)                                          
SVENDDT  DS    PL3        QEND  (YMD)                                           
SVUFLD   DS    CL200      SAVE THE USER FIELD                                   
*                                                                               
SVCLTCD  DS    CL3        CLIENT CODE                                           
SVJOBCD  DS    CL6        JOB CODE                                              
SVBLNO   DS    CL6        BILL NUMBER                                           
SVBLDTE  DS    PL3        BILL DATE                                             
SVDUEDTE DS    XL2        BILL DUE DATE                                         
SVBLAMT  DS    PL8        BILL AMOUNT DUE                                       
SVBUDCD  DS    CL13       BUGDET CODE                                           
SVVENNO  DS    CL8        HARD CODED VENDOR NUMBER                              
SVADDSQ  DS    CL3        HARD CODED ADDRESS SEQUENCE NO.                       
SVSACSRC DS    CL6        HARD CODED SAC-SRC NO.                                
SVREFTYP DS    CL2        HARD CODED REFERENCE TYPE NO.                         
SVPROJNO DS    CL7        USER FIELD OFF JOB SCR - CODE 'PJ'                    
SVREMARK DS    CL40       SPACES OR REMARKS FROM REQUEST                        
SVFISCYR DS    CL4        USER FIELD FISCAL YEAR - CODE 'FY'                    
*                                                                               
SVPLINE  DS    CL(PRLNQ)  SAVE THE USER FIELD                                   
DELIMITR EQU   C','                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
* USER TABLE DSECT                                               *              
******************************************************************              
*                                                                               
USERTABD DSECT                                                                  
*                                                                               
UTUSRCD  DS    CL2        USER CODE                                             
UTOUTLN  DS    X          OUTPUT FIELD LENGTH                                   
UTOUTAD  DS    AL2        OUTPUT FIELD ADDRESS                                  
*                                                                               
USERTABQ EQU   *-USERTABD                                                       
         EJECT                                                                  
*                                                                               
******************************************************************              
* INFO TABLE DSECT                                               *              
******************************************************************              
*                                                                               
ITABD    DSECT                                                                  
*                                                                               
ITCLICD  DS    CL3        CLIENT CODE                                           
ITVENNO  DS    CL8        VENDOR NUMBER                                         
ITADDSQ  DS    CL3        ADDRESS SEQUENCE NUMBER                               
ITSACSRC DS    CL6        SAC-SRC NUMBER                                        
ITREFTYP DS    CL2        REFERENCE TYPE NUMBER                                 
*                                                                               
ITABQ    EQU   *-ITABD                                                          
         EJECT                                                                  
***********************************************************************         
* PRINT DESCT                                                         *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
*                                                                               
PVENNO   DS    CL8        HARD CODED VENDOR NUMBER                              
         DS    C           DELIMETER                                            
PREFNO   DS    CL8        INVOICE NUMBER                                        
         DS    C                                                                
PADDSEQ  DS    CL3        HARD CODED ADDRESS SEQUENCE NO.                       
         DS    C                                                                
PSACSRC  DS    CL6        HARD CODED SAC-SRC NO.                                
         DS    C                                                                
PREFTYP  DS    CL2        HARD CODED REFERENCE TYPE NO.                         
         DS    C                                                                
PREFDTE  DS    CL8        MMDDCCYY - INVOICE DATE                               
         DS    C                                                                
PPAYDTE  DS    CL8        MMDDCCYY - DUE DATE LESS 2 DAYS (NO WKENDS)           
         DS    C                                                                
PGLACC   DS    CL7        BUGDET USER FIELD - CODE 'BC'                         
         DS    C                                                                
PCSTCNTR DS    CL4        FIRST 4 DIGITS OF BUDGET CODE                         
         DS    C                                                                
PESTNO   DS    CL12       JOB NO.                                               
         DS    C                                                                
PCALYR   DS    CL4        CURRENT YEAR                                          
         DS    C                                                                
PPROJNO  DS    CL7        USER FIELD OFF JOB SCR - CODE 'PJ'                    
         DS    C                                                                
PREMARKS DS    CL40       SPACES OR REMARKS FROM REQUEST                        
         DS    C                                                                
PDISTAMT DS    CL12       BILL AMOUNT DUE - EX.(22222.22 OR -12.11)             
         DS    C                                                                
PSUBSCD  DS    CL2        SUBSIDIARY CODE - DEPENDANT ON PCSTCNTR               
         DS    C                                                                
PFISCYR  DS    CL4        FISCAL YEAR - USER FIELD                              
*                                                                               
PRLNQ    EQU   *-PLINED                                                         
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
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
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACREPBF02 08/18/98'                                      
         END                                                                    
