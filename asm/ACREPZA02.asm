*          DATA SET ACREPZA02  AT LEVEL 006 AS OF 01/06/00                      
*PHASE ACZA02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
         TITLE 'FIX TYPE 48 TRANSACTION'                                        
ACZZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZA**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZA02D,RC                                                       
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
         SPACE 5                                                                
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,LEDGFRST             LEDGER FIRST                           
         BE    LDGF                                                             
         CLI   MODE,PROCLEVA             PROCESS LEVEL A                        
         BE    PLEVA                                                            
         CLI   MODE,PROCLEVB             PROCESS LEVEL B                        
         BE    PLEVB                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                      LOOKUP ACCOUNT INFO                    
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
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
         ZAP   PKDUMP,=P'0'        INIT DUMP COUNT                              
         ZAP   MAXDUMP,=P'100'     INIT MAX DUMP                                
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
         MVI   FLAG,FLGAGY                                                      
         ZAP   PKAMNT,=P'0'              INITIALIZE AMOUNT TOTAL                
         ZAP   PKCOUNT,=P'0'             INITIALIZE COUNT TOTAL                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                     *            
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         BAS   RE,GETLEVS                GET HOW MANY LEVELS                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS LEVEL A                                                    *          
**********************************************************************          
         SPACE 1                                                                
PLEVA    DS    0H                                                               
         OI    FLAG,FLGLEVA              INDICATES LEVL A CHANGE                
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS LEVEL B                                                    *          
**********************************************************************          
         SPACE 1                                                                
PLEVB    DS    0H                                                               
         OI    FLAG,FLGLEVB              INDICATES LEV B CHANGE                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         OI    FLAG,FLGLEVC              LEV C CHANGE                           
*                                                                               
         USING ACTRECD,R2                                                       
         L     R2,ADACC                  GET ACCOUNT INFO                       
*                                                                               
         USING PLINED,R7                 REPORT PRINT DSECT                     
         LA    R7,XP                                                            
*                                                                               
         USING NAMELD,R3                                                        
         L     R3,ADACCNAM               GET ACCOUNT NAME                       
*                                                                               
         ZIC   R1,NAMLN                  GET LENGTH OF NAME ELEM                
         SHI   R1,NAMLN1Q                GET LENGTH OF RECD                     
*                                                                               
         MVC   CPYNM,SPACES                                                     
         CHI   R1,L'CPYNM                IS NAMEREC BIGGER THAN PRTFLD          
         BNH   *+14                      GET EXACT NAME                         
         MVC   CPYNM,NAMEREC             YES DO REGULAR MOVE                    
         B     PACCX                                                            
*                                                                               
         BCTR  R1,0                      SUB ONE FOR EX                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CPYNM(0),NAMEREC                                                 
*                                                                               
         DROP  R2,R3,R7                                                         
PACCX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS TRANSACTION                                                *          
**********************************************************************          
         SPACE 1                                                                
PTRN     DS    0H                                                               
*                                                                               
         USING TRNELD,R4                                                        
         L     R4,ADTRANS                                                       
*                                                                               
         LR    R2,R4                                                            
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                GET ADDRESABILTY TO TRAN RECD          
*                                                                               
         CLI   TRNTYPE,TRNTEPRV          ESTIMATE PRODUCTION REVRSAL            
         BNE   PTRNX                                                            
*                                                                               
         CLC   =C'**AUTO REVERSE E.P.',TRNNARR DSRPTN MUST NOT BE THIS          
         BE    PTRNX                                                            
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         CLC   =C'**ESTIMATED PRODUCTION',TRNNARR                               
         BE    *+10                                                             
         MVC   PRTDSCEX,=C'**'           NOT ESTMTD PRDCTN PRINT **             
*                                                                               
         MVC   MSG,=CL10'TRNS RECIN'                                            
         SR    R6,R6                                                            
         ICM   R6,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         BAS   RE,GTACTVDT         GET ACTIVITY DATE AND FLAG IT                
*                                                                               
         TM    FLAG,FLGAGY               IS THEIR A CHANGE IN AGY               
         BNO   *+16                                                             
         MVC   PRTCMPCD,ALPHAID          PRINT COMPANY CODE                     
         MVC   PRTUNIT(L'PRTUNIT+L'PRTLDG),TRNKUNT   UNIT CODE                  
*                                                                               
         BAS   RE,PRNTLVLS               PRINT ACCOUNT LEVELS CHANGE            
*                                                                               
         EDIT  (1,TRNTYPE),PRTTYPE                                              
*                                                                               
         ZIC   R1,TRNLN                  GET LENGTH OF RECORD                   
         SHI   R1,TRNLN1Q                GET LENGTH OF DESCRIPTION              
         CHI   R1,L'PRTDISC              IS DESCRPTN BIGGER THAN PRTFLD         
         BNH   *+14                      GET EXACT DESCRIPTION                  
         MVC   PRTDISC,TRNNARR           DO REGULAR MOVE                        
         B     PTRN50                                                           
*                                                                               
         BCTR  R1,0                      SUB ONE FOR EXECUTED MOVE              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTDISC(0),TRNNARR        MOVE VAR LENGHT TEXT IN PRNT           
*                                                                               
PTRN50   DS    0H                                                               
*                                                                               
         MVC   PRTWC,TRNKOFF             OFFICE CODE                            
         MVC   PRTULC,TRNKULC            CONTRA UNIT,LEDGER,ACCOUNT             
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(8,PRTDATE)                             
         MVC   PRTREF,TRNKREF            REFERENCE NUMBER                       
*                                                                               
         EDIT  (P6,TRNAMNT),PRTAMNT,2,ZERO=NOBLANK,MINUS=YES                    
*                                                                               
         GOTO1 ACREPORT                  PRINT LINE OF REPORT                   
*                                                                               
         AP    PKCOUNT,=P'1'             INC RECD TOTAL                         
         AP    PKAMNT,TRNAMNT            ADD TO AMOUNT TOTAL.                   
*                                                                               
         BAS   RE,UPDTRNEL               UPDATE TRANSACTION ELEMENT             
         BAS   RE,UPDTRECD               UPDATE TRANSACTION RECORD              
*                                                                               
         CLI   QOPT1,C'Y'          DO WE WANT TO UPDATE                         
         BNE   PTRNX                                                            
         TM    FLAG,FLGNOCH        CHK TO SEE IF DATE IS OLD                    
         BO    PTRNX                                                            
*                                                                               
         MVC   MSG,=CL10'TRAN OUT'                                              
         SR    R6,R6                                                            
         ICM   R6,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    PTRNX                                                            
         MVI   MODE,WRITRANS       WRITE IT BACK                                
*                                                                               
PTRNX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R4,R7                                                         
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         CP    PKCOUNT,=P'0'                                                    
         BE    REQLX                                                            
         GOTO1 ACREPORT                  PRINT BLANK LINE                       
*                                                                               
         MVC   PRTTMSG,=CL10'TOTALS FOR'                                        
         MVC   PRTCPCD,ALPHAID                                                  
         MVI   PRTCOL,C':'                                                      
         MVC   PRTCMSG,=CL15'NUM OF RECDS = '                                   
         EDIT  (P4,PKCOUNT),PRTCOUNT     RECD/COMPANY TOTAL                     
         MVC   PRTAMSG,=CL15'TOTAL AMOUNT = '                                   
         EDIT  (P8,PKAMNT),PRTTOTL,2,MINUS=YES,ZERO=NOBLANK                     
         GOTO1 ACREPORT                                                         
*                                                                               
         DROP  R7                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
* SUBROTINE PRINTS ACTIVITY DATE AND FLAGS PRINTS OLD IF DATE IS *              
* BEFORE OCT20/99.                                               *              
*        R2 - TRANSACTION RECORD                                 *              
******************************************************************              
         SPACE 5                                                                
GTACTVDT NTR1                                                                   
         NI    FLAG,X'FF'-FLGNOCH        INIT FLAG TO OFF                       
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         LR    R6,R2                     GET ADDRESS OF TRANS ELMNT             
         MVI   ELCODE,TRSELQ             X'60'                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRSELD,R6                                                        
         GOTO1 DATCON,DMCB,(2,TRSDATE),(8,PRTADATE)                             
*                                                                               
         CLC   TRSDATE,ADATE             IS DATE OLDER THAN OCT20/99            
         BNL   GTACTX                                                           
         OI    FLAG,FLGNOCH              DON'T CHANG OLD DATES                  
         MVC   PRTODATE,=CL3'OLD'                                               
GTACTX   B     EXIT                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
*******************************************************************             
* BUILD NEW ELEMENT IN ELEM THAT NEEDS TO BE ADDED TO TRANSACTION *             
* RECD. R4-TRANSACTION ELEMENT                                    *             
*******************************************************************             
UPDTRNEL NTR1                                                                   
         XC    ELEM,ELEM                                                        
         USING TRNELD,R4                                                        
         ZIC   R1,TRNLN                  GET ELEMENTS LENGTH                    
         SHI   R1,1                      SUB 1 FOR EX                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),TRNELD            COPY INTO ELEM                         
*                                                                               
         LA    R4,ELEM                   POINT TO NEW ELEMENT                   
         ZIC   R1,TRNLN                                                         
         SHI   R1,TRNLN1Q                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    TRNNARR(0),TRNNARR        CLEAR ELEM'S DESCRIPTION AREA          
*                                                                               
         SR    R1,R1                                                            
         AHI   R1,TRNLN1Q                BUILD NEW LENGTH OF RECD               
         MVC   TRNNARR(L'NEWNARTV),NEWNARTV                                     
         AHI   R1,L'NEWNARTV             ADD LNGTH OF NEW NARATION              
         STC   R1,TRNLN                                                         
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*****************************************************************               
* UPDATE TRANSACTION RECORD DELETE X'44' WITH OLD NARRATIVE AND *               
* ADD NEW 44 WITH NEW NARRATIVE. R2-TRANSACTION RECORD          *               
* R2-TRANSACTION RECORD, R4-TRANSACTION ELEMENT                 *               
*****************************************************************               
UPDTRECD NTR1                                                                   
         USING TRNRECD,R2                                                       
         USING TRNELD,R4                                                        
*                                                                               
         MVI   TRNEL,DELELQ              INIT ELEMENT TO BE DELETED             
         GOTO1 HELLO,DMCB,(C'D',ACCFIL),('DELELQ',(R2)),0,0                     
*                                                                               
         MVI   ELEM,ONE            TO PUT ELEM AT BEGINING OF THE RECD          
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),TRNRECD,ELEM,0                          
*                                                                               
         CLI   12(R1),0                  CHECK IF ADD ELEM IS FINE              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TRNEL,TRNELQ              STICK BACK X'44'                       
         DROP  R2,R4                                                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GETLEVS SUBROUTINE MAKES LEVELS SOFT, RETURNS INDIVIDUAL LEVEL     *          
* LENGTHS AND GIVES NUMBER OF NESTED LEVELS IN LEDGER RECDS          *          
**********************************************************************          
GETLEVS  NTR1                                                                   
         USING ACLELD,R5                                                        
         L     R5,ADLDGHIR                                                      
         MVC   LEVELS(L'LEVLNQ),SPACES   CLEAR LEVELS LENGTH/DISC               
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LHI   R0,LEVELQ                 R0 = MAXIMUM NUMBER OF LEVELS          
         STC   R0,LEVNUM                 ASSUME 4 LEVEL STRUCTURE               
         LA    R1,ACLVALS                R1 = FIRST LEVEL LENGTH                
         LA    RE,LEVELS                 STORE ACCUMULATIVE LNTH HERE           
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
*                                                                               
GLEV10   ICM   R4,1,0(R1)                CURRENT LEVEL LENGTH                   
         BZ    GLEV20                    NO MORE LEVELS - ADJUST LEVNUM         
         STC   R4,0(RE)                                                         
         SR    R4,R3                     SUBTRACT CURRENT FROM PREVIOUS         
*                                                                               
         STC   R4,0(R2)                  CURRENT INDIVIDUAL LENGTH              
         IC    R3,0(R1)                  UPDATE R3                              
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,GLEV10                                                        
*                                                                               
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ                 R1 = MAXIMUM NUMBER OF LEVELS          
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* SUBROUTINE PRINTS CHANGE IN ACCOUNT AT ANY LEVEL AS SOON AS ANY   *           
* LEVEL CHANGES SUBROUTINE PRINTS THE CHANGED INFORMATION IN HIGH   *           
* TO LOW HIERARCHIAL FASHION, SUBROUTINE ALSO PRINTS ANY OTHER INFO *           
* THAT NEEDS TO BE CHANGED ALONG WITH THE LEVEL CHANGE              *           
*********************************************************************           
         SPACE 5                                                                
PRNTLVLS NTR1                                                                   
         USING TRNELD,R4                                                        
         USING TRNRECD,R2                GET ADDRESABILTY TO TRAN RECD          
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         MVC   PRTACT,SPACES             CLEAR ACCOUNT FIELD                    
         LA    R5,TRNKACT                                                       
         LA    R3,PRTACT                                                        
*                                                                               
         ZIC   R0,LEVNUM                 HOW MANY LEVELS DEEP                   
         LA    RE,LEVLNQS                FROM GETLEVS ACTUAL LENGTH             
         LA    RF,LEVELS                 ACCUMULATIVE LENGTHS                   
*                                                                               
         TM    FLAG,FLGLEVA              DID LEV A CHANGE                       
         BNO   *+14                                                             
         MVC   PRTNAME,CPYNM             PRINT NAME OF THE ACCOUNT              
         B     PRNTL40                                                          
*                                                                               
         TM    FLAG,FLGLEVB              DID LEV B CHANGE                       
         BNO   PRNTL10                                                          
         LA    RE,1(RE)                  GET LEV B'S ACTUAL LNGTH               
         ZIC   R1,0(RF)                  GET ACCUMULATIVE LNGTH                 
         AR    R5,R1                                                            
         AHI   R1,1                      TAKE INTO ACCOUNT 1 / FOR LEVB         
         SHI   R0,1                      CURRENT LEVNUM                         
         B     PRNTL30                                                          
*                                                                               
PRNTL10  TM    FLAG,FLGLEVC                                                     
         BNO   PRNTL20                                                          
         MVC   PRTNAME,CPYNM             DONT FORGET THIS IN LEVC CHNG          
         LA    RE,2(RE)                  GET LEV C'S ACTUAL LNGTH               
         ZIC   R1,1(RF)                                                         
         AR    R5,R1                                                            
         AHI   R1,2                      TAKE INTO ACCOUNT 2 / FOR LEVC         
         SHI   R0,2                      # OF LEVL'S,INFO TO BE PRNTD           
         B     PRNTL30                                                          
*                                                                               
PRNTL20  TM    FLAG,FLGLEVD                                                     
         BNO   PRNTLX                                                           
         LA    RE,3(RE)                                                         
         ZIC   R1,2(RF)                                                         
         AR    R5,R1                                                            
         AHI   R1,3                      TAKE INTO ACCOUNT 3 / FOR LEVD         
         SHI   R0,3                                                             
*                                                                               
PRNTL30  AR    R3,R1                                                            
PRNTL40  B     *+12                                                             
*                                                                               
PRNTL50  MVI   0(R3),C'/'                PRNT / AFTER EVERY LEVEL CHNG          
         LA    R3,1(R3)                  POINT AFTER SLASH                      
         ZIC   R1,0(RE)                  GET ACTUAL LENGTH OF NXT LEVL          
         BCTR  R1,0                      SUB 1 FOR EX                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R5)                                                    
*                                                                               
         LA    R5,1(R1,R5)               GET NXT LVL INFO FROM TRNKACT          
         LA    R3,1(R1,R3)               POINT WHERE TO PRINT                   
         LA    RE,1(RE)                                                         
         BCT   R0,PRNTL50                                                       
*                                                                               
         NI    FLAG,X'FF'-FLGACT         TURN OFF ACC LEVEL FLAGS               
         DROP  R2,R4,R7                                                         
PRNTLX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* LOCAL STORAGE                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0A                                                               
         DC    A(DUMP)                                                          
         DC    V(PRNTBL)                                                        
*                                                                               
ADATE    DC    XL2'C754'                 DATE OCT20/99                          
NEWNARTV DC    CL19'**AUTO REVERSE E.P.'                                        
HELLO    DC    V(HELLO)                                                         
ACCFIL   DC    CL8'ACCFIL'               CHANGE RECORD IN THIS FILE             
DELELQ   EQU   X'FF'                     DELETE ELEMENT FF                      
ONE      EQU   X'01'                     PUT ELEMENT AT STRT OF RECD            
         EJECT                                                                  
***********************************************************************         
*  DUMP   RECORDS                                                     *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    PKDUMP,MAXDUMP      ONLY PRINT HUNDRED DUMPS                     
         BH    DUMPX                                                            
         AP    PKDUMP,=P'1'        INC DUMP COUNT                               
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
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZA02D  DSECT                                                                  
VTYPES   DS    0A                                                               
ADUMP    DS    A                                                                
PRNTBL   DS    V                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
ELCODE   DS    CL1                       ELEMENT CODE                           
MSG      DS    CL10                      DUMP MESSAGE                           
ELEM     DS    CL255                     USED FOR UPDATING ELEMENT              
*                                                                               
PKCOUNT  DS    PL4                       RECORD/COMPANY TOTAL                   
PKDUMP   DS    PL4                       DUMP COUNTER                           
MAXDUMP  DS    PL4                                                              
PKAMNT   DS    PL8                       AMOUNT TOTAL                           
*                                                                               
FLAG     DS    XL1                                                              
FLGAGY   EQU   X'80'                     AGENCY CHANGED                         
FLGLEVA  EQU   X'40'                     CLIENT  CODE CHANGED                   
FLGLEVB  EQU   X'20'                     PRODUCT CODE CHANGED                   
FLGLEVC  EQU   X'10'                     JOB     CODE CHANGED                   
FLGLEVD  EQU   X'08'                     JOB     CODE CHANGED                   
FLGNOCH  EQU   X'04'                     JOB DON'T NEED TO BE CHNGD             
FLGACT   EQU   FLGAGY+FLGLEVA+FLGLEVB+FLGLEVC+FLGLEVD                           
*                                                                               
CPYNM    DS    CL36                      COMPANY NAME                           
*                                                                               
LEVELS   DS    0XL1                                                             
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LEVSLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
OFFSET   DS    XL1                 OFFSET TO LOWEST LEVEL                       
LOWLVLN  DS    XL1                 LOWEST LEVEL LENGTH                          
*                                                                               
         EJECT                                                                  
PLINED   DSECT                                                                  
         DS    CL1                                                              
PRTCMPCD DS    CL2                       COMPANY ALPHA CODE                     
         DS    CL2                                                              
PRTUNIT  DS    CL1                       UNIT                                   
PRTLDG   DS    CL1                       LEDGER                                 
         DS    CL2                                                              
PRTACT   DS    CL20                      LEVLA/LEVLB/LEVLC/LEVLD                
         DS    CL2                                                              
PRTNAME  DS    CL20                      ACCOUNT NAME                           
         DS    CL1                                                              
PRTWC    DS    CL2                       WORK CODE/OFFICE CODE                  
         DS    CL2                                                              
PRTTYPE  DS    CL2                       TRANSACTION TYPE 48                    
         DS    CL2                                                              
PRTULC   DS    CL14                      CONTRA UNIT,LEDGER,ACCOUNT             
         DS    CL2                                                              
PRTDATE  DS    CL8                       DATE                                   
         DS    CL2                                                              
PRTADATE DS    CL8                       ACTIVITY DATE                          
         DS    CL2                                                              
PRTODATE DS    CL3                       PRT OLD ACTIVITY DATE IS OLD           
         DS    CL2                                                              
PRTREF   DS    CL6                       REFERENCE NUM.                         
         DS    CL2                                                              
PRTDISC  DS    CL23                      DESCRIPTION                            
         DS    CL2                                                              
PRTDSCEX DS    CL2                       ** IF NOT ESTIMATED PRDCTN             
         DS    CL7                                                              
PRTAMNT  DS    CL16                                                             
*                                                                               
         ORG   PLINED                                                           
PRTTMSG  DS    CL10'TOTALS FOR'          PRINT TOTALS MESSAGE                   
         DS    CL1                                                              
PRTCPCD  DS    CL2                       PRINT COMPANY CODE                     
         DS    CL2                                                              
PRTCOL   DS    CL1':'                                                           
         DS    CL5                                                              
PRTCMSG  DS    CL15'NUM OF RECDS = '                                            
PRTCOUNT DS    CL6                       NUM OF RECDS                           
         DS    CL60                                                             
PRTAMSG  DS    CL15'TOTAL AMOUNT = '                                            
         ORG                                                                    
*                                                                               
         ORG   PRTAMNT                                                          
PRTTOTL  DS    CL16                      PRINT TOTAL AMOUNT                     
         ORG                                                                    
*                                                                               
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
**PAN#1  DC    CL21'006ACREPZA02 01/06/00'                                      
         END                                                                    
