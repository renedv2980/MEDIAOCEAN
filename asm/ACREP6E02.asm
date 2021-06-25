*          DATA SET ACREP6E02  AT LEVEL 005 AS OF 09/21/07                      
*PHASE AC6E02A                                                                  
*INCLUDE DLFLD                                                                  
                                                                                
         TITLE 'MCS ESTIMATES - LIST AND DETAILS'                               
                                                                                
* LVL CHANGE DOCUMENTATION                                                      
* ------------------------                                                      
* TKLU 001 08JUN06 <DU01-4941> - NEW REPORT                                     
*                                                                               
* OPTIONS/FILTERS                                                               
* ---------------                                                               
* QOPT1    - ' ' FOR LIST MODE                                                  
*            'D' FOR DETAILS MODE                                               
* QOPT2    - ' ' LIST BY SJ ACCOUNT                                             
*            'N' LIST BY GLOBAL NUMBER                                          
* QOPT3    - ' ' LIST ALL STATUS                                                
*            'S' LIST SUBMITTED & APPROVED                                      
*            'A' LIST APPROVED                                                  
*            'R' LIST REJECTED                                                  
* QOPT7    - 'Y' FOR DOWNLOAD                                                   
* QACCOUNT - SJ FILTERING                                                       
* QOFFICE  - OFFICE CODE                                                        
* QSTART   - DATE RANGE FOR ADDED DATE                                          
* QEND     - SEE PREVIOUS                                                       
*                                                                               
* PROFILES                                                                      
* --------                                                                      
* (NO PROFILES USED YET)                                                        
*                                                                               
AC6E02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**6E02**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING PROGD,RC                                                         
         L     RF,=V(DLFLD)                                                     
         ST    RF,DOWNLOAD                                                      
         L     RF,ADCOMFAC                                                      
         MVC   VBLDCUR,CBLDCUR-COMFACSD(RF)                                     
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
         SPACE 1                                                                
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         MVI   RUNIND,0            RUN INDICATOR                                
         GOTO1 ADDICTAT,DMCB,C'LL  ',DDIN,DDOUT                                 
         MVI   RCFLAG1,RCFREPLC+RCFREQLC                                        
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         DROP  RF                                                               
         L     R1,MCBXAREA-MASTD(RF)                                            
         ST    R1,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R1,=A(HOOK)                                                      
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
RUNFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
         SPACE 1                                                                
REQF     CLI   MODE,REQFRST                                                     
         BNE   PROA                                                             
         SPACE 1                                                                
         ZAP   TOTEST,PZERO                                                     
         ZAP   TOTAMT,PZERO                                                     
         SPACE 1                                                                
         MVC   CSTART,QSTART                                                    
         CLC   QSTART,SPACES                                                    
         BH    REQF02                                                           
         XC    CSTART,CSTART                                                    
         SPACE 1                                                                
REQF02   MVC   CEND,QEND                                                        
         CLC   QEND,SPACES                                                      
         BH    REQF04                                                           
         MVC   CEND,=X'FFFFFF'                                                  
         SPACE 1                                                                
REQF04   MVI   CSTATUS,FFQ                                                      
         CLI   QOPT3,C' '                                                       
         BNH   REQF06                                                           
         MVI   CSTATUS,ESTKCAPP+ESTKSUBM                                        
         CLI   QOPT3,C'S'                                                       
         BE    REQF06                                                           
         MVI   CSTATUS,ESTKCAPP                                                 
         CLI   QOPT3,C'A'                                                       
         BE    REQF06                                                           
         MVI   CSTATUS,ESTKREJE                                                 
         CLI   QOPT3,C'R'                                                       
         BE    REQF06                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
REQF06   DS    0H                                                               
         SPACE 1                                                                
REQF08   MVI   RCSUBPRG,1          SUBPROGRAM DEFAULT                           
         MVI   REQIND,0                                                         
         L     RF,ADCMPEL                                                       
         MVC   AGYCURR,CPYCURR-CPYELD(RF)                                       
         CLI   QOPT7,C'Y'          DOWNLOAD?                                    
         BNE   REQF10                                                           
         OI    RUNIND,RUNIDWN                                                   
         MVI   RCSUBPRG,0                                                       
         B     REQF12                                                           
         SPACE 1                                                                
REQF10   MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT2,C' '          DEFAULT (BY JOB)                             
         BE    REQF12                                                           
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT2,C'N'          BY NUMBER                                    
         BE    REQF12                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
         USING LDGRECD,R3                                                       
REQF12   XC    CLILEN(3),CLILEN                                                 
         L     R3,=A(RECAREA)      READ SJ LEDGER RECORD                        
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,RCSVCOMP                                                 
         MVC   LDGKUNT(2),=C'SJ'                                                
         GOTO1 DATAMGR,DMCB,DMREAD,ACCFIL,LDGRECD,LDGRECD                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLELD,R3                                                        
         L     R3,=A(RECAREA)                                                   
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
REQF14   CLI   ACLEL,ACLELQ                                                     
         BE    REQF16                                                           
         CLI   ACLEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,ACLLN                                                         
         AR    R3,R0                                                            
         B     REQF14                                                           
         SPACE 1                                                                
REQF16   MVC   CLILEN,ACLELLVA     SAVE CLI/PRO/JOB LENGTH                      
         MVC   PROLEN,ACLELLVB                                                  
         MVC   JOBLEN,ACLELLVC                                                  
         DROP  R3                                                               
         SPACE 1                                                                
         BAS   RE,SETSJC                                                        
         SPACE 1                                                                
         CLI   QOPT2,C'N'          IF LIST BY NUMBER PROCESS HERE               
         BNE   REQFX                                                            
         USING EGNPASD,R2                                                       
         LA    R2,RECKEY           GET FIRST ESTIMATE                           
         XC    EGNPAS,EGNPAS                                                    
         MVI   EGNPTYP,EGNPTYPQ                                                 
         MVI   EGNPSUB,EGNPSUBQ                                                 
         MVC   EGNPCPY,RCSVCOMP                                                 
         MVC   SVKEY1,EGNPAS                                                    
         MVI   MYSTAT,1                                                         
         SPACE 1                                                                
REQF18   LA    R2,RECKEY                                                        
         CLI   MYSTAT,0                                                         
         BE    REQF20                                                           
         MVI   MYSTAT,0                                                         
         MVC   EGNPAS,SVKEY1                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,EGNPASD,EGNPASD,0                     
         B     REQF22                                                           
         SPACE 1                                                                
REQF20   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,EGNPASD,EGNPASD,0                     
         SPACE 1                                                                
REQF22   BNE   REQFX               EXIT ON ERROR                                
         CLC   EGNPAS(3),SVKEY1                                                 
         BNE   REQFX               EXIT ON DIFFERENT RECORD                     
         MVC   SVKEY1,EGNPAS                                                    
         BAS   RE,FLTNDIR                                                       
         BNE   REQF20              SKIP FOR NEXT RECORD                         
         SPACE 1                                                                
         L     RF,=A(RECAREA)                                                   
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,EGNPDA,(RF),RECWORK                   
         BE    *+6                                                              
         DC    H'0'                I/O ERROR                                    
         SPACE 1                                                                
         L     R2,=A(RECAREA)                                                   
         BAS   RE,FILTREC                                                       
         BNE   REQF20              SKIP RECORD                                  
         SPACE 1                                                                
         BAS   RE,PRINTREC         PRINT CURRENT RECORD                         
         B     REQF18              GET NEXT RECORD                              
         SPACE 1                                                                
REQFX    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNTS                                                    *         
***********************************************************************         
         SPACE 1                                                                
PROA     L     R2,ADHEIRA                                                       
         MVI   MYBYTE,1                                                         
         CLI   MODE,PROCLEVA                                                    
         BE    ACCP                                                             
         L     R2,ADHEIRB                                                       
         MVI   MYBYTE,2                                                         
         CLI   MODE,PROCLEVB                                                    
         BE    ACCP                                                             
         L     R2,ADACC                                                         
         MVI   MYBYTE,3                                                         
         CLI   MODE,PROCACC                                                     
         BE    ACCP                                                             
         L     R2,ADACC                                                         
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         B     REQL                                                             
         SPACE 1                                                                
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,R3                                                        
ACCP     CLI   QOPT2,C'N'                                                       
         BE    XIT                                                              
         LR    R3,R2               FIRST FOR ACCOUNT AT ANY LEVEL               
         AH    R3,DATADISP                                                      
         XR    R0,R0                                                            
         SPACE 1                                                                
ACCP02   CLI   RSTEL,RSTELQ                                                     
         BE    ACCP06                                                           
         CLI   RSTEL,0                                                          
         BE    ACCPX                                                            
         SPACE 1                                                                
ACCP04   IC    R0,RSTLN                                                         
         AR    R3,R0                                                            
         B     ACCP02                                                           
         SPACE 1                                                                
ACCP06   CLI   RSTLN,RSTLN3Q                                                    
         BL    ACCPX                                                            
         TM    RSTSTAT6,RSTSMCSE   ANY MCS ESTIMATES PRESENT?                   
         BZ    ACCPX                                                            
         DROP  R3                                                               
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         BAS   RE,SETEST           SET ESTIMATE KEY                             
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
         MVC   SVKEY2,KEY                                                       
         MVI   MYSTAT,1                                                         
         SPACE 1                                                                
ACCP08   LA    R2,RECKEY                                                        
         CLI   MYSTAT,0                                                         
         BE    ACCP10                                                           
         MVI   MYSTAT,0                                                         
         MVC   ESTKEY,SVKEY1                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,ESTRECD,ESTRECD,0                     
         B     ACCP12                                                           
         SPACE 1                                                                
ACCP10   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,ESTRECD,ESTRECD,0                     
         SPACE 1                                                                
ACCP12   BNE   ACCP20              EXIT ON ERROR                                
         CLC   ESTKEY(ESTKLNO-ESTRECD),SVKEY1                                   
         BNE   ACCP20              EXIT AT END OF ESTIMATE LIST                 
         BAS   RE,FLTEDIR                                                       
         BNE   ACCP10                                                           
         MVC   SVKEY1,ESTKEY                                                    
         SPACE 1                                                                
         L     RF,=A(RECAREA)                                                   
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,ESTKDA,(RF),RECWORK                   
         BE    *+6                                                              
         DC    H'0'                I/O ERROR                                    
         SPACE 1                                                                
         L     R2,=A(RECAREA)                                                   
         BAS   RE,FILTREC                                                       
         BNE   ACCP10              SKIP RECORD                                  
         SPACE 1                                                                
         BAS   RE,PRINTREC         PRINT CURRENT RECORD                         
         B     ACCP08              GET NEXT RECORD                              
         SPACE 1                                                                
ACCP20   MVC   KEY,SVKEY2                                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT ',KEY,RECAREA                 
         SPACE 1                                                                
ACCPX    B     XIT                                                              
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
         SPACE 1                                                                
ACCL     DS    0H                  LAST FOR ACCOUNT AT LOW LEVEL                
         SPACE 1                                                                
ACCLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
         SPACE 1                                                                
REQL     CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         TM    RUNIND,RUNIDWN      DOWNLOAD INITIALISED?                        
         BZ    REQL02                                                           
         TM    RUNIND,RUNIINI                                                   
         BZ    REQLX                                                            
         LA    RF,DLBUFF                                                        
         USING DLCBD,RF                                                         
         MVI   DLCBACT,DLCBEOR                                                  
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 DOWNLOAD,(RF)                                                    
         B     REQLX                                                            
         DROP  RF                                                               
         SPACE 1                                                                
REQL02   MVI   FORCEHED,C'Y'       PRINT OUT TOTALS                             
         MVI   SPACING,2                                                        
         MVI   RCSUBPRG,3                                                       
         MVC   P+02(20),AC@REQTS                                                
         MVC   PTHIRD+04(12),AC@NUMAZ                                           
         CURED (P8,TOTEST),(12,PTHIRD+17),0,MINUS=YES,ZERO=YES                  
         MVC   PFOURTH+04(12),AC@AMT                                            
         CURED (P8,TOTAMT),(12,PFOURTH+17),2,MINUS=YES,ZERO=YES                 
         GOTO1 ACREPORT                                                         
         SPACE 1                                                                
REQLX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET SJ CODES                                                        *         
***********************************************************************         
         SPACE 1                                                                
SETSJC   NTR1                                                                   
         SPACE 1                                                                
         MVC   SJCLIC,SPACES                                                    
         MVC   SJPROC,SPACES                                                    
         MVC   SJJOBC,SPACES                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
*                                                                               
         CLC   QACCOUNT,SPACES                                                  
         BE    SETSJCX                                                          
         LA    R3,QACCOUNT                                                      
*                                                                               
         IC    RE,CLILEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SJCLIC(0),0(R3)                                                  
         LA    R3,1(RE,R3)                                                      
*                                                                               
         IC    RE,PROLEN                                                        
         IC    RF,CLILEN                                                        
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SJPROC(0),0(R3)                                                  
         LA    R3,1(RE,R3)                                                      
*                                                                               
         IC    RE,JOBLEN                                                        
         IC    RF,PROLEN                                                        
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SJJOBC(0),0(R3)                                                  
*                                                                               
SETSJCX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FILTER DIRECTORY RECORD (NUMBER)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING EGNPASD,R2                                                       
FLTNDIR  NTR1                                                                   
         LA    R2,RECKEY           POINT TO READ RECORD                         
         CLC   SJCLIC,SPACES       CLIENT FILTER?                               
         BE    FLTND05                                                          
         CLC   EGNPCLI,SJCLIC                                                   
         BNE   FLTNDN                                                           
         CLC   SJPROC,SPACES       PRODUCT FILTER?                              
         BE    FLTND05                                                          
         CLC   EGNPPRO,SJPROC                                                   
         BNE   FLTNDN                                                           
         CLC   SJJOBC,SPACES       JOB FILTER?                                  
         BE    FLTND05                                                          
         CLC   EGNPJOB,SJJOBC                                                   
         BNE   FLTNDN                                                           
         SPACE 1                                                                
FLTND05  CLC   QOFFICE,SPACES      OFFICE FILTERING?                            
         BNH   FLTND10                                                          
         CLC   EGNPSOFF,QOFFICE                                                 
         BNE   FLTNDN                                                           
         SPACE 1                                                                
         USING OFFALD,R1                                                        
FLTND10  CLC   EGNPSOFF,SPACES     LIMIT ACCESS?                                
         BNH   FLTND15                                                          
         L     R1,ADOFFALD                                                      
         MVC   OFFAOFFC,EGNPSOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 ADOFFAL                                                          
         BNE   FLTNDN                                                           
         DROP  R1                                                               
         SPACE 1                                                                
FLTND15  MVC   BYTE,CSTATUS        STATUS FILTERING                             
         NC    BYTE,EGNPSTA1                                                    
         BZ    FLTNDN                                                           
         SPACE 1                                                                
FLTNDY   CR    RB,RB                                                            
         B     XIT                                                              
FLTNDN   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER DIRECTORY RECORD (JOB)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
FLTEDIR  NTR1                                                                   
         LA    R2,RECKEY           POINT TO READ RECORD                         
         CLC   QOFFICE,SPACES      OFFICE FILTERING?                            
         BNH   FLTDE05                                                          
         CLC   ESTKSOFF,QOFFICE                                                 
         BNE   FLTDEN                                                           
         SPACE 1                                                                
         USING OFFALD,R1                                                        
FLTDE05  CLC   ESTKSOFF,SPACES     LIMIT ACCESS?                                
         BNH   FLTDE10                                                          
         L     R1,ADOFFALD                                                      
         MVC   OFFAOFFC,ESTKSOFF                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 ADOFFAL                                                          
         BNE   FLTDEN                                                           
         DROP  R1                                                               
         SPACE 1                                                                
FLTDE10  MVC   BYTE,CSTATUS        STATUS FILTERING                             
         NC    BYTE,ESTKSTA1                                                    
         BZ    FLTDEN                                                           
         SPACE 1                                                                
FLTDEY   CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
FLTDEN   LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER MASTER RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
         USING EMDELD,R3                                                        
FILTREC  NTR1                                                                   
         L     R2,=A(RECAREA)                                                   
         LA    R3,ESTRFST                                                       
         XR    R0,R0                                                            
         CLI   EMDEL,EMDELQ        FIRST ELEMENT IS MAIN DATA ELEMENT           
         BNE   FILTRN                                                           
         SPACE 1                                                                
         CLC   EMDDAT,CEND         DATE FILTERING                               
         BH    FILTRN                                                           
         CLC   EMDDAT,CSTART                                                    
         BL    FILTRN                                                           
         SPACE 1                                                                
FILTRY   CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
FILTRN   LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT/DOWNLOAD A RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
         USING EMDELD,R3                                                        
         USING ENMELD,R4                                                        
PRINTREC NTR1  ,                                                                
         L     R2,=A(RECAREA)      *********************************            
         LA    R3,ESTRFST          *SET MYBYTE TO 1 IF ANY IOS DONE*            
         XR    R0,R0               *********************************            
         IC    R0,EMDLN                                                         
         LR    R4,R3                                                            
         AR    R4,R0                                                            
         SPACE 1                                                                
         TM    RUNIND,RUNIDWN                                                   
         BZ    PRINTREP                                                         
         BAS   RE,MYDOWN           DOWNLOAD                                     
         B     XIT                                                              
         SPACE 1                                                                
         USING PLINED,R5                                                        
PRINTREP LA    R5,P                                                             
         CLI   RCSUBPRG,2          PRINT DATA IN NUMBER MODE                    
         BNE   PRINTR10                                                         
         MVC   P2GLNO,EMDGNO                                                    
         GOTO1 DATCON,DMCB,(1,EMDDAT),(8,P2DATE)                                
         BAS   RE,SETSTA                                                        
         MVC   P2STAT(1),BYTE                                                   
         TM    ESTRSTA2,ESTKFCUR                                                
         BZ    *+8                                                              
         MVI   P2STAT+1,C'*'                                                    
         MVC   P2OFFC,ESTRSOFF                                                  
         MVC   P2CLIC,ESTKCLI                                                   
         MVC   P2PROC,ESTKPRO                                                   
         MVC   P2JOBC,ESTKJOB                                                   
         CLC   ESTKJOB,SPACES                                                   
         BH    *+10                                                             
         MVC   P2JOBC,EMDBMC                                                    
         EDIT  (B1,ESTKLNO),(3,P2LOCN),0                                        
         MVC   P2SCHC,EMDSCH                                                    
         CURED (P6,EMDAMT),(12,P2AMNT),2,MINUS=YES,ZERO=YES                     
         MVC   P2REPF,EMDFMT                                                    
         XR    RE,RE                                                            
         IC    RE,ENMLN                                                         
         SHI   RE,ENMLNQ+1                                                      
         MVC   MYWRK2,SPACES                                                    
         MVC   MYWRK2+L'SPACES(L'MYWRK2-L'SPACES),SPACES                        
         LA    RF,MYWRK2                                                        
         CLI   QOPT1,C'D'                                                       
         BE    PRINTR02                                                         
         LA    RF,P2NAME                                                        
         CHI   RE,L'P2NAME-1                                                    
         BL    *+8                                                              
         LA    RE,L'P2NAME-1                                                    
         SPACE 1                                                                
PRINTR02 EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ENMNAME                                                  
         CLI   QOPT1,C'D'                                                       
         BNE   PRINTR50                                                         
         LA    R5,MYWRK2                                                        
         AR    R5,RE                                                            
         AHI   R5,2                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
         MVC   0(L'AC@ESTAD,R5),AC@ESTAD                                        
         AHI   R5,L'AC@ESTAD+1                                                  
         GOTO1 DATCON,DMCB,(1,EMDADT),(8,0(R5))                                 
         AHI   R5,9                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
         MVC   0(L'AC@LSTAC,R5),AC@LSTAC                                        
         AHI   R5,L'AC@LSTAC+1                                                  
         GOTO1 DATCON,DMCB,(1,EMDLDT),(8,0(R5))                                 
         AHI   R5,9                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
*&&UK                                                                           
         CLC   EMDCUR,AGYCURR                                                   
         BE    PRINTR04                                                         
         MVC   0(L'AC@CURRY,R5),AC@CURRY                                        
         AHI   R5,L'AC@CURRY+1                                                  
         MVC   0(3,R5),EMDCUR                                                   
         AHI   R5,4                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
PRINTR04 MVC   0(L'AC@VATAM,R5),AC@VATAM                                        
         AHI   R5,L'AC@VATAM+1                                                  
         CURED (P6,EMDTVA),(12,0(R5)),2,MINUS=YES,ZERO=YES,ALIGN=LEFT           
         AR    R5,R0                                                            
         AHI   R5,1                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
*&&                                                                             
         MVC   0(L'AC@CMN,R5),AC@CMN                                            
         AHI   R5,L'AC@CMN+1                                                    
         CURED (P6,EMDTCA),(12,0(R5)),2,MINUS=YES,ZERO=YES,ALIGN=LEFT           
         AR    R5,R0                                                            
         AHI   R5,1                                                             
         LA    R5,P                                                             
         GOTO1 ADSQUASH,DMCB,MYWRK2,160                                         
         GOTO1 CHOPPER,DMCB,(160,MYWRK2),(40,P2NAME),(C'P',4)                   
         B     PRINTR50                                                         
         SPACE 1                                                                
PRINTR10 CLI   RCSUBPRG,1          PRINT DATA IN JOB MODE                       
         BNE   PRINTR20                                                         
         BAS   RE,DOHEAD                                                        
         EDIT  (B1,ESTKLNO),(3,P1LOCN),0                                        
         MVC   P1GLNO,EMDGNO                                                    
         GOTO1 DATCON,DMCB,(1,EMDDAT),(8,P1DATE)                                
         BAS   RE,SETSTA                                                        
         MVC   P1STAT(1),BYTE                                                   
         TM    ESTRSTA2,ESTKFCUR                                                
         BZ    *+8                                                              
         MVI   P1STAT+1,C'*'                                                    
         MVC   P1SCHC,EMDSCH                                                    
         MVC   P1MEDC,EMDBMC                                                    
         CLI   EMDBMC,C' '                                                      
         BH    *+10                                                             
         MVC   P1MEDC,ESTKJOB                                                   
         CURED (P6,EMDAMT),(12,P1AMNT),2,MINUS=YES,ZERO=YES                     
         MVC   P1REPF,EMDFMT                                                    
         MVC   MYWRK1,SPACES                                                    
         MVC   MYWRK1+L'SPACES(L'MYWRK1-L'SPACES),SPACES                        
         LA    RF,MYWRK1                                                        
         CLI   QOPT1,C'D'                                                       
         BE    *+8                                                              
         LA    RF,P1NAME                                                        
         XR    RE,RE                                                            
         IC    RE,ENMLN                                                         
         SHI   RE,ENMLNQ+1                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),ENMNAME                                                  
         CLI   QOPT1,C'D'                                                       
         BNE   PRINTR50                                                         
         LA    R5,MYWRK1                                                        
         AR    R5,RE                                                            
         AHI   R5,2                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
         MVC   0(L'AC@ESTAD,R5),AC@ESTAD                                        
         AHI   R5,L'AC@ESTAD+1                                                  
         GOTO1 DATCON,DMCB,(1,EMDADT),(8,0(R5))                                 
         AHI   R5,9                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
         MVC   0(L'AC@LSTAC,R5),AC@LSTAC                                        
         AHI   R5,L'AC@LSTAC+1                                                  
         GOTO1 DATCON,DMCB,(1,EMDLDT),(8,0(R5))                                 
         AHI   R5,9                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
*&&UK                                                                           
         CLC   EMDCUR,AGYCURR                                                   
         BE    PRINTR12                                                         
         MVC   0(L'AC@CURRY,R5),AC@CURRY                                        
         AHI   R5,L'AC@CURRY+1                                                  
         MVC   0(3,R5),EMDCUR                                                   
         AHI   R5,4                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
PRINTR12 MVC   0(L'AC@VATAM,R5),AC@VATAM                                        
         AHI   R5,L'AC@VATAM+1                                                  
         CURED (P6,EMDTVA),(12,0(R5)),2,MINUS=YES,ZERO=YES,ALIGN=LEFT           
         AR    R5,R0                                                            
         AHI   R5,1                                                             
         MVI   0(R5),C','                                                       
         AHI   R5,2                                                             
*&&                                                                             
         MVC   0(L'AC@CMN,R5),AC@CMN                                            
         AHI   R5,L'AC@CMN+1                                                    
         CURED (P6,EMDTCA),(12,0(R5)),2,MINUS=YES,ZERO=YES,ALIGN=LEFT           
         AR    R5,R0                                                            
         AHI   R5,1                                                             
         LA    R5,P                                                             
         GOTO1 ADSQUASH,DMCB,MYWRK1,200                                         
         GOTO1 CHOPPER,DMCB,(200,MYWRK1),(50,P1NAME),(C'P',4)                   
         B     PRINTR50                                                         
         SPACE 1                                                                
PRINTR20 DC    H'0'                                                             
         SPACE 1                                                                
PRINTR50 AP    TOTEST,PONE                                                      
         AP    TOTAMT,EMDAMT                                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* SET STATUS CHARACTER FROM RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING ESTRECD,R2                                                       
SETSTA   NTR1                                                                   
         L     R2,=A(RECAREA)                                                   
         MVI   BYTE,C'D'                                                        
         TM    ESTRSTA1,ESTKLOGD                                                
         BNZ   SETSTAX                                                          
         MVI   BYTE,C'R'                                                        
         TM    ESTRSTA1,ESTKREJE                                                
         BNZ   SETSTAX                                                          
         MVI   BYTE,C'A'                                                        
         TM    ESTRSTA1,ESTKCAPP                                                
         BNZ   SETSTAX                                                          
         MVI   BYTE,C'S'                                                        
         TM    ESTRSTA1,ESTKSUBM                                                
         BNZ   SETSTAX                                                          
         MVI   BYTE,C'P'                                                        
         TM    ESTRSTA1,ESTKCREA                                                
         BNZ   SETSTAX                                                          
         MVI   BYTE,C'*'                                                        
         SPACE 1                                                                
SETSTAX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DO HEADINGS (FOR SJ MODE)                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
         USING NAMELD,R3                                                        
DOHEAD   NTR1                                                                   
         L     R2,ADHEIRA                                                       
         XR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+17(0),ACTKACT                                              
         XR    R0,R0                                                            
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         SPACE 1                                                                
DOHEAD05 CLI   NAMEL,NAMELQ                                                     
         BE    DOHEAD10                                                         
         CLI   NAMEL,0                                                          
         BE    DOHEAD15                                                         
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     DOHEAD05                                                         
         SPACE 1                                                                
DOHEAD10 XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD5+27(0),NAMEREC                                              
         SPACE 1                                                                
DOHEAD15 CLI   MYBYTE,1                                                         
         BE    DOHEADX                                                          
         L     R2,ADHEIRB                                                       
         XR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         XR    RE,RE                                                            
         IC    RE,PROLEN                                                        
         SR    RE,R1                                                            
         LA    R1,ACTKACT(R1)                                                   
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   HEAD6+17(0),0(R1)                                                
         XR    R0,R0                                                            
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         SPACE 1                                                                
DOHEAD20 CLI   NAMEL,NAMELQ                                                     
         BE    DOHEAD25                                                         
         CLI   NAMEL,0                                                          
         BE    DOHEAD30                                                         
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     DOHEAD20                                                         
         SPACE 1                                                                
DOHEAD25 XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD6+27(0),NAMEREC                                              
         SPACE 1                                                                
DOHEAD30 CLI   MYBYTE,2                                                         
         BE    DOHEADX                                                          
         L     R2,ADACC                                                         
         XR    R1,R1                                                            
         IC    R1,PROLEN                                                        
         XR    RE,RE                                                            
         IC    RE,JOBLEN                                                        
         SR    RE,R1                                                            
         LA    R1,ACTKACT(R1)                                                   
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   HEAD7+17(0),0(R1)                                                
         XR    R0,R0                                                            
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         SPACE 1                                                                
DOHEAD35 CLI   NAMEL,NAMELQ                                                     
         BE    DOHEAD40                                                         
         CLI   NAMEL,0                                                          
         BE    DOHEADX                                                          
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         B     DOHEAD35                                                         
         SPACE 1                                                                
DOHEAD40 XR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD7+27(0),NAMEREC                                              
         SPACE 1                                                                
DOHEADX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET ESTIMATES KEY FOR CURRENT SJ ACCOUNT                            *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
         USING ESTRECD,R3                                                       
SETEST   NTR1                                                                   
         SPACE 1                                                                
         LA    R3,RECKEY                                                        
         XC    ESTKEY,ESTKEY                                                    
         MVI   ESTKTYP,ESTKTYPQ                                                 
         MVI   ESTKSUB,ESTKSUBQ                                                 
         MVC   ESTKCPY,RCSVCOMP                                                 
         MVC   ESTKCLI,SPACES                                                   
         MVC   ESTKPRO,SPACES                                                   
         MVC   ESTKJOB,SPACES                                                   
         LA    R5,ACTKACT                                                       
         XR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         XR    RE,RE                                                            
         IC    RE,PROLEN                                                        
         XR    RF,RF                                                            
         IC    RF,JOBLEN                                                        
         LR    R4,R1                                                            
         SHI   R4,1                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ESTKCLI(0),0(R5)                                                 
         AR    R5,R1                                                            
         LR    R4,RE                                                            
         SR    R4,R1                                                            
         SHI   R4,1                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ESTKPRO(0),0(R5)                                                 
         LA    R5,ACTKACT                                                       
         AR    R5,RE                                                            
         LR    R4,RF                                                            
         SR    R4,RE                                                            
         CHI   R4,L'ESTKJOB                                                     
         BNH   *+8                                                              
         LA    R4,L'ESTKJOB                                                     
         SHI   R4,1                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ESTKJOB(0),0(R5)                                                 
         MVC   SVKEY1,ESTKEY                                                    
         SPACE 1                                                                
SETESTX  B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* MYDOWN - PUT DATA TO DOWNLOAD REPORT (INCLUDING INITIALISATION)    *          
**********************************************************************          
         SPACE 1                                                                
         USING DLCBD,R2                                                         
         USING EMDELD,R3                                                        
         USING ENMELD,R4                                                        
         USING ESTRECD,R5                                                       
MYDOWN   NTR1                                                                   
         SPACE 1                                                                
         LR    R5,R2                                                            
         TM    RUNIND,RUNIINI      INITIALISED?                                 
         BNZ   MYDOWN4                                                          
         OI    RUNIND,RUNIINI                                                   
         LA    R2,DLBUFF                                                        
         LA    RE,DLPLINE                                                       
         ST    RE,DLCBAPL                                                       
         LA    RE,DLPRINT                                                       
         ST    RE,DLCBAPR                                                       
         MVI   DLCBACT,DLCBSOR                                                  
         OI    DLCBFLG1,DLCBFXTN   USE EXTENDED CONTROL BLOCK                   
         GOTO1 DOWNLOAD,DLCBD                                                   
         MVC   DLPLINE,SPACES                                                   
         SPACE 1                                                                
         DS    0H                  DO HEADLINES FIRST                           
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@CLIC),AC@CLIC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@PROC),AC@PROC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@JOBC),AC@JOBC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@MEDC),AC@MEDC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@OFF),AC@OFF                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@NUM),AC@NUM                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@ESTNO),AC@ESTNO                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@ESTDT),AC@ESTDT                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@STT),AC@STT                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@SCM),AC@SCM                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@AMT),AC@AMT                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@DESC),AC@DESC                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         CLI   QOPT1,C'D'                                                       
         BNE   MYDOWN2                                                          
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@AMT),AC@VATAM                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'AC@AMT),AC@CMN                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
MYDOWN2  MVI   DLCBACT,DLCBEOL     AND END OF LINE                              
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
MYDOWN4  DS    0H                  DO DATA NOW                                  
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'ESTKCLI),ESTKCLI                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'ESTKPRO),ESTKPRO                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'ESTKJOB),ESTKJOB                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'EMDBMC),EMDBMC                                         
         CLI   EMDBMC,C' '                                                      
         BH    *+10                                                             
         MVC   DLCBFLD(L'EMDBMC),ESTKJOB                                        
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'ESTRSOFF),ESTRSOFF                                     
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         EDIT  (B1,ESTKLNO),(3,DLCBFLD),0                                       
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'EMDGNO),EMDGNO                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         GOTO1 DATCON,DMCB,(1,EMDDAT),(8,DLCBFLD)                               
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         BAS   RE,SETSTA                                                        
         MVC   DLCBFLD(1),BYTE                                                  
         TM    ESTRSTA2,ESTKFCUR                                                
         BZ    *+8                                                              
         MVI   DLCBFLD+1,C'*'                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD(L'EMDSCH),EMDSCH                                         
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         CURED (P6,EMDAMT),(12,DLCBFLD),2,MINUS=YES,ZERO=YES                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBNUM                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         MVC   DLCBFLD,SPACES                                                   
         XR    RE,RE                                                            
         IC    RE,ENMLN                                                         
         SHI   RE,ENMLNQ+1                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),ENMNAME                                               
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         CLI   QOPT1,C'D'                                                       
         BNE   MYDOWN6                                                          
         LA    R2,DLBUFF                                                        
         CURED (P6,EMDTVA),(12,DLCBFLD),2,MINUS=YES,ZERO=YES                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBNUM                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
         LA    R2,DLBUFF                                                        
         CURED (P6,EMDTCA),(12,DLCBFLD),2,MINUS=YES,ZERO=YES                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBNUM                                                  
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
MYDOWN6  MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         NI    DLCBFLG1,FFQ-DLCBFXFL                                            
         GOTO1 DOWNLOAD,DLCBD                                                   
         SPACE 1                                                                
MYDOWNX  B     XIT                                                              
         DROP  R2,R3,R4,R5                                                      
         SPACE 3                                                                
***********************************************************************         
* GENERAL DOWNLOAD PRINT                                              *         
***********************************************************************         
         SPACE 1                                                                
DLPRINT  NTR1                                                                   
         MVC   P,DLPLINE                                                        
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,1                                                        
         GOTO1 ACREPORT                                                         
         MVC   DLPLINE,SPACES                                                   
         MVI   LINE,1                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              BOX ROUTINES (HOOK)                                    *         
***********************************************************************         
         SPACE 1                                                                
         ENTRY HOOK                                                             
HOOK     NTR1  ,                                                                
         L     R7,ADBOX                                                         
         USING BOXD,R7                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         CLI   RCSUBPRG,0                                                       
         BE    HOOKNO                                                           
         CLI   RCSUBPRG,3                                                       
         BE    HOOKNO                                                           
         USING PLINED,RE                                                        
         LA    RE,BOXCOLS                                                       
         CLI   RCSUBPRG,1                                                       
         BNE   HOOK1                                                            
         MVI   BOXROWS+7,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   P1HOOK1,C'L'        SET LH MARGIN                                
         MVI   P1HOOK2,C'C'                                                     
         MVI   P1HOOK3,C'C'                                                     
         MVI   P1HOOK4,C'C'                                                     
         MVI   P1HOOK5,C'C'                                                     
         MVI   P1HOOK6,C'C'                                                     
         MVI   P1HOOK7,C'C'                                                     
         MVI   P1HOOK8,C'C'                                                     
         MVI   P1HOOK9,C'C'                                                     
         MVI   P1HOOK10,C'R'                                                    
         B     HOOK5                                                            
         SPACE 1                                                                
HOOK1    CLI   RCSUBPRG,2                                                       
         BNE   HOOK2                                                            
         MVI   BOXROWS+5,C'T'      SET ROWS                                     
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   P2HOOK1,C'L'        SET LH MARGIN                                
         MVI   P2HOOK2,C'C'                                                     
         MVI   P2HOOK3,C'C'                                                     
         MVI   P2HOOK4,C'C'                                                     
         MVI   P2HOOK5,C'C'                                                     
         MVI   P2HOOK6,C'C'                                                     
         MVI   P2HOOK7,C'C'                                                     
         MVI   P2HOOK8,C'C'                                                     
         MVI   P2HOOK9,C'C'                                                     
         MVI   P2HOOK10,C'C'                                                    
         MVI   P2HOOK11,C'C'                                                    
         MVI   P2HOOK12,C'C'                                                    
         MVI   P2HOOK13,C'R'                                                    
         B     HOOK5                                                            
         DROP  RE                                                               
         SPACE 1                                                                
HOOK2    DC    H'0'                                                             
         SPACE 1                                                                
HOOK5    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         SPACE 1                                                                
HOOKNO   MVI   BOXYORN,C'N'                                                     
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
         DROP  R7                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT, DEFINED CONSTANTS AND LTORG                           *         
***********************************************************************         
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
PZERO    DC    PL1'0'                                                           
PONE     DC    PL1'1'                                                           
UNDERS   DC    64C'-'                                                           
CTFILE   DC    CL8'CTFILE'                                                      
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
         SPACE 1                                                                
DDIN     DCDDL AC#REQTS,20                                                      
         DCDDL AC#NUMAZ,12                                                      
         DCDDL AC#AMT,12                                                        
         DCDDL AC#CLIC,8                                                        
         DCDDL AC#PROC,8                                                        
         DCDDL AC#JOBC,8                                                        
         DCDDL AC#SCM,8                                                         
         DCDDL AC#OFF,8                                                         
         DCDDL AC#NUM,8                                                         
         DCDDL AC#STT,8                                                         
         DCDDL AC#ESTNO,16                                                      
         DCDDL AC#ESTDT,16                                                      
         DCDDL AC#DESC,12                                                       
         DCDDL AC#MEDC,10                                                       
         DCDDL AC#ESTAD,15                                                      
         DCDDL AC#LSTAC,16                                                      
         DCDDL AC#CURRY,9                                                       
         DCDDL AC#VATAM,12                                                      
         DCDDL AC#CMN,11                                                        
         DC    X'00'                                                            
         SPACE 1                                                                
RECKEY   DC    XL42'00'                                                         
         DC    XL64'00'                                                         
RECWORK  DC    XL64'00'                                                         
RECAREA  DC    2000X'00'                                                        
         SPACE 1                                                                
FFQ      EQU   X'FF'               END OF TABLE                                 
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
P1HOOK1  DS    CL1                                                              
P1LOCN   DS    CL3                                                              
         DS    CL1                                                              
P1HOOK2  DS    CL1                                                              
P1GLNO   DS    CL6                                                              
         DS    CL1                                                              
P1HOOK3  DS    CL1                                                              
P1DATE   DS    CL8                                                              
         DS    CL1                                                              
P1HOOK4  DS    CL1                                                              
P1STAT   DS    CL2                                                              
         DS    CL1                                                              
P1HOOK5  DS    CL1                                                              
P1SCHC   DS    CL8                                                              
         DS    CL1                                                              
P1HOOK6  DS    CL1                                                              
P1MEDC   DS    CL1                                                              
         DS    CL1                                                              
         DS    CL1                                                              
P1HOOK7  DS    CL1                                                              
P1AMNT   DS    CL12                                                             
         DS    CL1                                                              
P1HOOK8  DS    CL1                                                              
P1REPF   DS    CL8                                                              
         DS    CL1                                                              
P1HOOK9  DS    CL1                                                              
P1NAME   DS    CL50                                                             
         DS    CL1                                                              
P1HOOK10 DS    CL1                                                              
         SPACE 1                                                                
         ORG   P1HOOK1                                                          
P2HOOK1  DS    CL1                                                              
P2GLNO   DS    CL6                                                              
         DS    CL1                                                              
P2HOOK2  DS    CL1                                                              
P2DATE   DS    CL8                                                              
         DS    CL1                                                              
P2HOOK3  DS    CL1                                                              
P2STAT   DS    CL2                                                              
         DS    CL1                                                              
P2HOOK4  DS    CL1                                                              
P2OFFC   DS    CL2                                                              
         DS    CL1                                                              
P2HOOK5  DS    CL1                                                              
P2CLIC   DS    CL5                                                              
         DS    CL1                                                              
P2HOOK6  DS    CL1                                                              
P2PROC   DS    CL2                                                              
         DS    CL1                                                              
P2HOOK7  DS    CL1                                                              
P2JOBC   DS    CL6                                                              
         DS    CL1                                                              
P2HOOK8  DS    CL1                                                              
P2LOCN   DS    CL3                                                              
         DS    CL1                                                              
P2HOOK9  DS    CL1                                                              
P2SCHC   DS    CL8                                                              
         DS    CL1                                                              
P2HOOK10 DS    CL1                                                              
P2AMNT   DS    CL12                                                             
         DS    CL1                                                              
P2HOOK11 DS    CL1                                                              
P2REPF   DS    CL8                                                              
         DS    CL1                                                              
P2HOOK12 DS    CL1                                                              
P2NAME   DS    CL40                                                             
         DS    CL1                                                              
P2HOOK13 DS    CL1                                                              
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
PROGD    DSECT                                                                  
DOWNLOAD DS    V                                                                
VBLDCUR  DS    V                                                                
ADBOX    DS    F                                                                
         SPACE 1                                                                
RUNIND   DS    XL1                 RUN AND DOWNLOAD FLAGS                       
RUNIINI  EQU   X'01'                INITIALISED FOR DOWNLOAD                    
RUNIEND  EQU   X'10'                END REPORT                                  
RUNIDWN  EQU   X'08'                REPORT IS DOWNLOAD                          
REQIND   DS    XL1                 REQUEST INDICATOR                            
         SPACE 1                                                                
MYDA     DS    XL4                                                              
MYBYTE   DS    XL1                                                              
MYSTAT   DS    XL1                                                              
SVKEY1   DS    XL42                                                             
SVKEY2   DS    XL42                                                             
AGYCURR  DS    CL3                                                              
SJCLIC   DS    CL3                                                              
SJPROC   DS    CL3                                                              
SJJOBC   DS    CL6                                                              
CLILEN   DS    XL1                                                              
PROLEN   DS    XL1                                                              
JOBLEN   DS    XL1                                                              
         SPACE 1                                                                
CSTART   DS    XL3                                                              
CEND     DS    XL3                                                              
CSTATUS  DS    XL1                                                              
TOTEST   DS    PL8                                                              
TOTAMT   DS    PL8                                                              
         SPACE 1                                                                
MYWRK1   DS    CL200                                                            
MYWRK2   DS    CL160                                                            
         SPACE 1                                                                
DDOUT    DS    0C                                                               
         DSDDL PRINT=YES                                                        
         SPACE 1                                                                
DLSTAT   DS    C                                                                
DLBUFF   DS    CL(DLCBXLX)         DOWNLOAD BUFFERS                             
         DS    CL120                                                            
DLPLINE  DS    CL(L'P)                                                          
         DS    CL(2*L'P)                                                        
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
* DDEBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACOFFALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREP6E02 09/21/07'                                      
         END                                                                    
