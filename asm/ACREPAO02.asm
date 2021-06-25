*          DATA SET ACREPAO02  AT LEVEL 001 AS OF 02/09/21                      
*PHASE ACAO02A                                                                  
*INCLUDE ACRECTYP                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
                                                                                
ACAO02   TITLE 'ORPHANED AUDIT RECORDS FOR JOB AND ESTIMATE'                    
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 001 24JUL20 <SPEC-47804> ORPHANED AUDIT RECORDS FOR JOBS AND   *         
*                               ESTIMATES.                            *         
***********************************************************************         
***********************************************************************         
* THIS CONVERSION WILL READ AUDIT RECORD FOR JOB/ESTIMATE/ORDER & SEE *         
* IF CORRESPONDING RECORDS ARE AVAILABLE OR NOT. IF NOT AVAILABLE,    *         
* IT WILL CONSIDER IT AS ORPHANED AUDIT RECORD AND OPTIONALLY DELETE  *         
* IT.                                                                 *         
* RECORD                                                              *         
* QOPT2 = RUN FOR ENTIRE FILE                                                   
* QOPT3 = READ AUDIT RECORD FOR ACCOUNT OR ESTIMATE  (A/E/O)                    
* QOPT4 = OPTIONALLY DELETE THE ORPHANED AUDIT RECORD (Y/N)                     
***********************************************************************         
                                                                                
ACAO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAO**,R8,CLEAR=Y                                            
         L     RC,0(,R1)                                                        
         USING ACWORKD,RC                                                       
         LA    RA,SPACEND                                                       
         USING WORKD,RA            RA=A(WORK AREA SAVED AREA)                   
*                                                                               
         CLI   MODE,RUNLAST                                                     
         JE    RUNL                                                             
         J     EXIT                                                             
                                                                                
EXITL    CLI   *,FF                SET CC=LOW                                   
         J     EXIT                                                             
EXITE    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LAST  FOR REQUEST                                                   *         
* READ ALL AUDIT RECORDS FOR ACCOUNT AND ESTIMATE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING AUDRECD,R2                                                       
RUNL     XR    RF,RF                                                            
         LH    RF,=Y(IOAREA-WORKD)                                              
         LA    RE,WORKD                                                         
         AR    RF,RE                                                            
         ST    RF,AIOAREA                                                       
         LH    RF,=Y(TSAREA-WORKD)                                              
         LA    RE,WORKD                                                         
         AR    RF,RE                                                            
         ST    RF,ATSAREA                                                       
         L     RF,=A(30000*ZTRLENQ)                                             
         ST    RF,ATSABUFS                                                      
*                                                                               
         L     R3,ADMASTC                                                       
         USING MASTD,R3                                                         
         L     R0,ATSABUFS         ACQUIRE TSAR BUFFER (ABOVE THE LINE)         
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ATSABUF                                                       
         ST    R1,MCUSRDMP         PRINT THE BUFFER IN A DUMP                   
         A     R1,ATSABUFS                                                      
         ST    R1,MCUSRDMP+4                                                    
         DROP  R3                                                               
*                                                                               
         XC    SVAUDULA,SVAUDULA                                                
         XC    SVAUDORD,SVAUDORD                                                
         MVI   BYTE,0                                                           
*                                                                               
RUNL04   LA    R2,IOKEY                                                         
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,QCOMPANY                                                 
         CLI   QOPT2,YESQ                                                       
         BNE   *+8                                                              
         MVI   AUDKCPY,X'41'       COMP CODES RANGE FROM X'41' TO X'FD'         
         MVI   AUDKAUDT,AUDKACC                                                 
         MVC   COCODE,AUDKCPY                                                   
         CLI   QOPT3,ESTIMATE                                                   
         BNE   RUNL05                                                           
         MVI   AUDKAUDT,AUDKEST                                                 
         B     RUNL06                                                           
RUNL05   CLI   QOPT3,ORDER                                                      
         BNE   *+8                                                              
         MVI   AUDKAUDT,AUDKORD                                                 
*                                                                               
RUNL06   GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,AUDKEY,AUDKEY,0                       
         BE    RUNL12                                                           
         DC    H'0'                                                             
*                                                                               
RUNL08   MVC   IOKEY,SAVEKEY1      RESTORE KEY                                  
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,AUDKEY,AUDKEY,0                       
         BE    RUNL10                                                           
         DC    H'0'                                                             
*                                                                               
RUNL10   LA    R2,IOKEY                                                         
         GOTOR DATAMGR,DMCB,DMRSEQ,ACCDIR,AUDKEY,AUDKEY,0                       
         BE    RUNL12                                                           
         DC    H'0'                                                             
*                                                                               
RUNL12   CLI   QOPT2,YESQ                                                       
         BE    *+14                                                             
         CLC   COCODE,AUDKCPY                                                   
         BNE   RUNLX                                                            
         CLI   AUDKTYP,AUDKTYPQ    STILL READING AUDIT RECORD?                  
         BNE   RUNLX                                                            
         CLI   AUDKSUB,AUDKSUBQ                                                 
         BNE   RUNLX                                                            
         TM    AUDKSTAT,AUDSDELT   DELETED AUDIT RECORD?                        
         BO    RUNL10                                                           
         CLI   AUDKAUDT,AUDKEST    FOR ESTIMATES?                               
         BE    RUNL22                                                           
         CLI   AUDKAUDT,AUDKORD    FOR ORDER?                                   
         BE    RUNL28                                                           
         CLI   AUDKAUDT,AUDKACC    FOR ACCOUNT?                                 
         BNE   RUNL10                                                           
*                               ** COME HERE FOR EACH NEW ESTIMATE **           
RUNL14   CLI   QOPT3,ESTIMATE     DONT PROCESS ESTIMATE RECORD                  
         BE    RUNL10                                                           
         CLI   QOPT3,ORDER        DONT PROCESS ORDER RECORDS                    
         BE    RUNL10                                                           
         MVC   SAVEKEY1,IOKEY                                                   
         AP    NUMAUDR,=P'1'                                                    
         CLC   AUDKUNT(2),=C'SJ'   NO JOB RECORD?                               
         BNE   RUNL08                                                           
         AP    NUMJOBR,=P'1'                                                    
         XC    SVAUDULA,SVAUDULA                                                
         CLC   SVCPYCD,AUDKCPY                                                  
         BE    RUNL18                                                           
         MVC   SVCPYCD,AUDKCPY                                                  
         GOTOR GETCPY              GET USER ID                                  
RUNL18   MVC   SVCPYCD,AUDKCPY                                                  
         MVC   SVAUDULA,AUDKULA                                                 
         GOTOR CHKACC              CHECK WHETHER ACCOUNT IS AVAILABLE           
         BE    RUNL08                                                           
         CLI   QOPT4,YESQ          DELETE THE AUDIT RECORD?                     
         BNE   RUNL20                                                           
         GOTOR UPDREC              UPDATE THE RECORD                            
RUNL20   GOTOR PRNTREC                                                          
         TM    BYTE,X'80'                                                       
         BZ    RUNL08                                                           
         B     RUNL10                                                           
*                                                                               
RUNL22   CLI   QOPT3,ACCOUNT       DONT PROCESS ACCOUNT RECORD                  
         BE    RUNL10                                                           
         CLI   QOPT3,ORDER         DONT PROCESS ORDER RECORD                    
         BE    RUNL10                                                           
         MVC   SAVEKEY1,IOKEY                                                   
         AP    NUMAUDR,=P'1'                                                    
         AP    NUMESTR,=P'1'                                                    
         MVC   SVAUDCPJ,AUDKECPJ                                                
         MVC   SVAUDELN,AUDKELNO                                                
         MVC   SVAUDSEQ,AUDKSEQ                                                 
         CLC   SVCPYCD,AUDKCPY                                                  
         BE    RUNL24                                                           
         MVC   SVCPYCD,AUDKCPY                                                  
         GOTOR GETCPY              GET USER ID                                  
RUNL24   MVC   SVCPYCD,AUDKCPY                                                  
         GOTOR CHKEST              CHECK WHETHER ESTIMATE IS AVAILABLE          
         BE    RUNL08                                                           
         CLI   QOPT4,YESQ          DELETE THE AUDIT RECORD?                     
         BNE   RUNL26                                                           
         GOTOR UPDREC              UPDATE THE RECORD                            
RUNL26   GOTOR PRNTREC                                                          
         TM    BYTE,X'80'                                                       
         BZ    RUNL08                                                           
         B     RUNL10                                                           
*                                                                               
RUNL28   CLI   QOPT3,ACCOUNT       DONT PROCESS ACCOUNT RECORD                  
         BE    RUNL10                                                           
         CLI   QOPT3,ESTIMATE      DONT PROCESS ESTIMATE RECORD                 
         BE    RUNL10                                                           
         MVC   SAVEKEY1,IOKEY                                                   
         AP    NUMAUDR,=P'1'                                                    
         AP    NUMORDR,=P'1'                                                    
         MVC   SVAUDORD,AUDKORDN                                                
         CLC   SVCPYCD,AUDKCPY                                                  
         BE    RUNL30                                                           
         MVC   SVCPYCD,AUDKCPY                                                  
         GOTOR GETCPY              GET USER ID                                  
RUNL30   MVC   SVCPYCD,AUDKCPY                                                  
         GOTOR CHKORD              CHECK WHETHER ESTIMATE IS AVAILABLE          
         BE    RUNL08                                                           
         CLI   QOPT4,YESQ          DELETE THE AUDIT RECORD?                     
         BNE   RUNL32                                                           
         GOTOR UPDREC              UPDATE THE RECORD                            
RUNL32   GOTOR PRNTREC                                                          
         TM    BYTE,X'80'                                                       
         BZ    RUNL08                                                           
         B     RUNL10                                                           
*                                                                               
RUNLX    MVC   P+1(32),=C'NUMBER OF AUDIT RECORDS READ   ='                     
         CURED NUMAUDR,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT                                                         
         CLI   QOPT3,ACCOUNT                                                    
         BNE   RUNLX4                                                           
*                                                                               
         MVC   P+1(32),=C'NUMBER OF AUDIT RECORDS FOR JOB='                     
         CURED NUMJOBR,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT                                                         
         MVC   P+1(32),=C'NUMBER OF DELETED JOBS         ='                     
         CURED NUMJOBD,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT        1                                                
         B     RUNLX8                                                           
*                                                                               
RUNLX4   CLI   QOPT3,ESTIMATE                                                   
         BNE   RUNLX6                                                           
         MVC   P+1(32),=C'NUMBER OF AUDIT RECORDS FOR EST='                     
         CURED NUMESTR,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT                                                         
         MVC   P+1(32),=C'NUMBER OF DELETED ESTIMATES    ='                     
         CURED NUMESTD,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT        1                                                
         B     RUNLX8                                                           
*                                                                               
RUNLX6   CLI   QOPT3,ORDER                                                      
         BNE   RUNLX8                                                           
         MVC   P+1(32),=C'NUMBER OF AUDIT RECORDS FOR ORD='                     
         CURED NUMORDR,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT                                                         
         MVC   P+1(32),=C'NUMBER OF DELETED ORDERS       ='                     
         CURED NUMORDD,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT        1                                                
*                                                                               
RUNLX8   MVC   P+1(32),=C'NUMBER OF AUDIT RECORS UPDATED ='                     
         CURED NUMAUDU,(14,P+62),0,MINUS=YES                                    
         GOTOR ACREPORT        1                                                
         J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK WHETHER AN ACCOUNT RECORD IS AVAILABLE FOR GIVEN AUDIT RECORD *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
CHKACC   NTR1  ,                                                                
         J     *+12                                                             
         DC    C'*CHKACC*'                                                      
*                                                                               
CHKACC00 LA    R3,IOKEY                                                         
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,SVCPYCD                                                  
         MVC   ACTKULA,SVAUDULA                                                 
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,ACTKEY,ACTKEY,0                       
         BE    CHKACC02                                                         
         DC    H'0'                                                             
*                                                                               
CHKACC02 GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,ACTKEY,ACTKEY,0                       
         BE    CHKACC04                                                         
         DC    H'0'                                                             
*                                                                               
CHKACC04 CLC   ACTKULA,SVAUDULA                                                 
         BNE   CHKACC06                                                         
         TM    ACTKSTAT,ACTSDELT        JOB DELETED?                            
         BZ    EXITE                                                            
*                                                                               
CHKACC06 AP    NUMJOBD,=P'1'                                                    
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE THE ESTIMATE WITH ALL ELEMENTS                               *         
***********************************************************************         
         USING AUDRECD,R3                                                       
UPDREC   NTR1                                                                   
         LA    R3,IOKEY                                                         
         MVC   IOKEY,SAVEKEY1                                                   
         GOTOR DATAMGR,DMCB,(BYTE,DMREAD),ACCDIR,AUDKEY,AUDKEY,0                
         BE    UPDR04                                                           
         DC    H'0'                                                             
*                                                                               
UPDR04   MVC   SVDA,AUDKDA                                                      
*                                                                               
         L     R0,AIOAREA                                                       
         LHI   R1,L'IOAREA                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOR DATAMGR,DMCB,(BYTE,GETREC),ACCMST,SVDA,AIOAREA,DMWORK            
         BE    UPDR06                                                           
         DC    H'0'                                                             
*                                                                               
UPDR06   L     R3,AIOAREA                                                       
         MVI   BYTE,0                                                           
         CLI   RCWRITE,YESQ        WRITING TO FILE                              
         BNE   UPDRX                                                            
         OI    BYTE,X'80'                                                       
         OI    AUDRSTAT,AUDSDELT                                                
         GOTO1 DATAMGR,DMCB,(BYTE,PUTREC),ACCMST,SVDA,AIOAREA,DMWORK            
         BE    UPDR08                                                           
         DC    H'0'                                                             
*                                                                               
UPDR08   LA    R3,IOKEY                                                         
         MVC   IOKEY,SAVEKEY1                                                   
         OI    AUDKSTAT,AUDSDELT                                                
         GOTO1 DATAMGR,DMCB,(BYTE,DMWRT),ACCDIR,AUDKEY,AUDKEY,0                 
*                                                                               
UPDRX    AP    NUMAUDU,=P'1'                                                    
         B     EXITE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK WHETHER AN ESTIMATE RECORD IS AVAILABLE FOR GIVEN AUDIT REC   *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R3                                                       
CHKEST   NTR1  ,                                                                
         J     *+12                                                             
         DC    C'*CHKEST*'                                                      
*                                                                               
CHKEST00 LA    R3,IOKEY                                                         
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,SVCPYCD                                                  
         MVC   ESTKCLI,SVAUDCPJ                                                 
         MVC   ESTKPRO,SVAUDCPJ+3                                               
         MVC   ESTKJOB,SVAUDCPJ+6                                               
         MVC   ESTKLNO,SVAUDELN                                                 
         MVC   ESTKSEQ,SVAUDSEQ                                                 
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,ESTKEY,ESTKEY,0                       
         BE    CHKEST02                                                         
         DC    H'0'                                                             
*                                                                               
CHKEST02 GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,ESTKEY,ESTKEY,0                       
         BE    CHKEST04                                                         
         DC    H'0'                                                             
*                                                                               
CHKEST04 CLC   ESTKCLI,SVAUDCPJ                                                 
         BNE   CHKEST06                                                         
         CLC   ESTKPRO,SVAUDCPJ+3                                               
         BNE   CHKEST06                                                         
         CLC   ESTKJOB,SVAUDCPJ+6                                               
         BNE   CHKEST06                                                         
         CLC   ESTKLNO,SVAUDELN                                                 
         BNE   CHKEST06                                                         
         CLC   ESTKSEQ,SVAUDSEQ                                                 
         BNE   CHKEST06                                                         
         TM    ESTKSTA1,ESTKDELT        ESTIMATE DELETED?                       
         BZ    EXITE                                                            
*                                                                               
CHKEST06 AP    NUMESTD,=P'1'                                                    
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK WHETHER AN ORDER RECORD IS AVAILABLE FOR GIVEN AUDIT RECORD   *         
***********************************************************************         
         SPACE 1                                                                
         USING ORDRECD,R3                                                       
CHKORD   NTR1  ,                                                                
         J     *+12                                                             
         DC    C'*CHKORD*'                                                      
*                                                                               
CHKORD00 LA    R3,IOKEY                                                         
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,SVCPYCD                                                  
         MVC   ORDKORD,SVAUDORD                                                 
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,ORDKEY,ORDKEY,0                       
         BE    CHKORD02                                                         
         DC    H'0'                                                             
*                                                                               
CHKORD02 GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,ORDKEY,ORDKEY,0                       
         BE    CHKORD04                                                         
         DC    H'0'                                                             
*                                                                               
CHKORD04 CLC   ORDKORD,SVAUDORD                                                 
         BNE   CHKORD06                                                         
         TM    ORDKSTAT,ORDSDEL         ORDER RECORD DELETED?                   
         BZ    EXITE                                                            
*                                                                               
CHKORD06 AP    NUMORDD,=P'1'                                                    
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK WHETHER AN ACCOUNT RECORD IS AVAILABLE FOR GIVEN AUDIT RECORD *         
***********************************************************************         
         SPACE 1                                                                
         USING AUDRECD,R2                                                       
PRNTREC  NTR1  ,                                                                
         J     *+12                                                             
         DC    C'*PRNTRC*'                                                      
*                                                                               
         MVC   P+1(L'SVCPYLOG),SVCPYLOG                                         
         LA    R2,SAVEKEY1                                                      
*                                                                               
         CLI   AUDKAUDT,AUDKACC                                                 
         BNE   PRNTREC2                                                         
         MVC   P+10(L'AUDKULA),AUDKULA     PRINT ACCOUNT                        
         B     PRNTREC6                                                         
*                                                                               
PRNTREC2 CLI   AUDKAUDT,AUDKEST                                                 
         BNE   PRNTREC4                                                         
         MVC   P+10(L'AUDKECPJ),AUDKECPJ   PRINT CLIENT/PROD/JOB                
         B     PRNTREC6                                                         
*                                                                               
PRNTREC4 CLI   AUDKAUDT,AUDKORD                                                 
         BNE   EXITE                                                            
         MVC   P+10(L'AUDKORDN),AUDKORDN   PRINT ORDER NUMBER                   
*                                                                               
PRNTREC6 GOTO1 VHEXOUT,DMCB,AUDKDA,P+29,L'SVDA,=C'MIX'                          
         GOTO1 ACREPORT                                                         
         B     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
* GET USER ID DETAILS                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R3                                                       
GETCPY   NTR1  ,                                                                
         J     *+12                                                             
         DC    C'*GETCPY*'                                                      
*                                                                               
GETCPY00 MVC   SVCPYLOG,SPACES                                                  
         LA    R3,IOKEY                                                         
         XC    CPYKEY,CPYKEY                                                    
         MVC   CPYKCPY,SVCPYCD                                                  
         GOTOR DATAMGR,DMCB,DMRDHI,ACCDIR,CPYKEY,CPYKEY,0                       
         BE    GETCPY02                                                         
         DC    H'0'                                                             
*                                                                               
GETCPY02 GOTOR DATAMGR,DMCB,(BYTE,GETREC),ACCMST,CPYKDA,AIOAREA,DMWORK          
         BE    GETCPY04                                                         
         DC    H'0'                                                             
*                                                                               
GETCPY04 L     R3,AIOAREA                                                       
         LA    R3,CPYRFST                                                       
*                                                                               
         USING CPYELD,R3                                                        
GETCPY06 CLI   CPYEL,X'00'                                                      
         BE    GETCPY10                                                         
         CLI   CPYEL,CPYELQ                                                     
         BNE   GETCPY08                                                         
         MVC   SVCPYLOG,CPYLOGO                                                 
         B     GETCPY10                                                         
*                                                                               
GETCPY08 LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GETCPY06                                                         
*                                                                               
GETCPY10 MVC   IOKEY,SAVEKEY1                                                   
         B     EXITE                                                            
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
ACCOUNT  EQU   C'A'                                                             
ESTIMATE EQU   C'E'                                                             
ORDER    EQU   C'O'                                                             
K        EQU   1024                                                             
YESQ     EQU   C'Y'                                                             
NOQ      EQU   C'N'                                                             
FF       EQU   X'FF'                                                            
VRECTYP  DC    V(ACRECTYP)                                                      
VHEXOUT  DC    V(HEXOUT)                                                        
*                                                                               
GETREC   DC    C'GETREC  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
ADDEND   DC    C'ADD=END'                                                       
*                                                                               
NUMAUDR  DC    PL6'0'                                                           
NUMJOBR  DC    PL6'0'                                                           
NUMJOBD  DC    PL6'0'                                                           
NUMESTR  DC    PL6'0'                                                           
NUMESTD  DC    PL6'0'                                                           
NUMORDR  DC    PL6'0'                                                           
NUMORDD  DC    PL6'0'                                                           
NUMAUDU  DC    PL6'0'                                                           
PZERO    DC    P'0'                                                             
XFFS     DC    X'FFFFFF'                                                        
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=6024,BLKSIZE=32760                                
         DS    0F                                                               
*                                                                               
***********************************************************************         
WORKD    DSECT                                                                  
AIOAREA  DS    A            ADDRESS OF IO AREA 1                                
ATSAREA  DS    A            ADDRESS OF TSAR AREA                                
VTSAR    DS    A            ADDRESS OF TSAR                                     
ATSABUF  DS    A            A(TSAR BUFFER)                                      
ATSABUFS DS    A            SIZE OF TSAR BUFFER REQUIRED                        
SVDA     DS    XL4          SAVED DISK ADDRESS                                  
*                                                                               
COCODE   DS    XL1          SAVE COMP CODE                                      
SVAUDULA DS    CL14         SAVE AUDIT ACCOUNT                                  
SVCPYCD  DS    CL1          SAVE COMPANY CODE                                   
SVAUDCPJ DS    CL12         SAVE AUDIT CLIENT/PROD/JOB                          
SVAUDELN DS    XL1          SAVE ESTIMATE LOCAL NUMBER                          
SVAUDSEQ DS    XL1          SAVE ESTIMATE SEQUENCE NUMBER                       
SVCPYLOG DS    CL7          USER ID                                             
SVAUDORD DS    CL6          ORDER NO                                            
*                                                                               
ELEMENT  DS    XL256                                                            
IOKEY    DS    XL64                                                             
SAVEKEY1 DS    XL64                                                             
SAVEKEY2 DS    XL64                                                             
TSARBLK  DS    XL(TSPXTNL)                                                      
IOAREA   DS    XL(2*K)                                                          
TSAREA   DS    XL(ZTRLENQ)                                                      
         DS    XL2                                                              
*                                                                               
WORKX    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* TSAR RECORD DSECT                                                   *         
***********************************************************************         
ZTSARECD DSECT                                                                  
ZTRLEN   DS    XL2                 LENGTH OF TSAR RECORD                        
ZTRKEY   DS    0X                  KEY DATA                                     
ZTRKSRT  DS    XL1                 - SORT ORDER                                 
ZTRKSEQ  DS    XL4                 - SEQUENCE NUMBER                            
ZTRKEYL  EQU   *-ZTRKEY                                                         
ZTRDATA  DS    0X                  BUFFER DATA                                  
ZTRDELM  DS    XL256               - ELEMENT                                    
ZTRLENQ  EQU   *-ZTSARECD                                                       
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPAO02 02/09/21'                                      
         END                                                                    
