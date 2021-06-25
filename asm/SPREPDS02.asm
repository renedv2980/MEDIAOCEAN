*          DATA SET SPREPDS02  AT LEVEL 005 AS OF 09/29/08                      
*PHASE SPDS02A                                                                  
*INCLUDE DLFLD                                                                  
                                                                                
*===================================================================            
* QOPT1 OUTPUTS FIXED LENGTH FIELDS                                             
* QOPT3 =Y EXCLUDE CLIENT IF COPT2 IS SET TO EXCLUDE FROM J1 REPORT             
*===================================================================            
         TITLE 'SPDS02 - CREATE DB2 DATA FILES FOR SUPERDESK'                   
SPDS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPDS02                                                         
         L     RC,=A(SPDSWORK)                                                  
         USING SPDSWORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   DS2                                                              
         BRAS  RE,CLTF                                                          
         MVI   MODE,CLTLAST                                                     
         J     EXIT                                                             
*                                                                               
DS2      CLI   MODE,REQFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,REQF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         J     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=============================================================*                 
* OPEN A REPORT ON THE PRTQUE AND INITIALIZE FOR DOWNLOADING  *                 
*=============================================================*                 
         SPACE 1                                                                
RUNF     DS    0H                                                               
         XC    DLCB,DLCB                                                        
D        USING DLCBD,DLCB                                                       
*                                                                               
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A'   LOAD EDITOR                                  
*                                                                               
         MVI   BYTE,QEDITOR                                                     
         GOTO1 HEXOUT,DMCB,BYTE,DUB+4,1,0                                       
*                                                                               
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   D.DLCBAED,4(R1)     DLFLD REQUIRES A(EDITOR)                     
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* AT RUNLAST, PUT OUT EOR                                                       
*=================================================================              
         SPACE 1                                                                
RUNL     BRAS  RE,GENCODES         GENERATE ACTIVE CODE TABLES                  
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 =V(DLFLD),(R1)                                                   
         DROP  R1                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*=====================================================================*         
         SPACE 1                                                                
REQF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,=A(MYHDHK)                                                    
         ST    RE,HEADHOOK                                                      
         L     RE,=A(HDHKR9)                                                    
         STM   R9,RC,0(RE)                                                      
*                                                                               
         MVC   SVRUNTYP,QCODE      SAVE RUN TYPE                                
         BAS   RE,BLDCODES                                                      
         J     EXIT                                                             
         EJECT                                                                  
CLTF     NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
*                                                                               
         CLI   QOPT3,C'Y'          TEST FOLLOW J1 EXCLUSION OPT                 
         BNE   CLTF2               NO                                           
         TM    COPT2,COP2EXDB      TEST EXCLUDE FROM J1 REQUEST                 
         BZ    CLTF2                                                            
         MVI   MODE,CLTLAST                                                     
         J     EXIT                                                             
*                                                                               
CLTF2    XC    SVDSPROF,SVDSPROF    READ APPROPRIATE PROFILE                    
         MVC   WORK(12),=CL12'S0DS'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),CLIENT                                                 
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
         GOTO1 GETPROF,DMCB,WORK,SVDSPROF,DATAMGR                               
                                                                                
*=============================================================                  
* READ FOR AUTHORIZATIONS                                                       
*=============================================================                  
                                                                                
         XC    BIGKEY,BIGKEY                                                    
*                                                                               
K        USING AUTKEY,BIGKEY                                                    
         MVC   K.AUTKTYP(2),=X'0D39'                                            
         L     R6,ADCLT                                                         
         MVC   K.AUTKAM,1(R6)      A-M                                          
         MVC   K.AUTKCLT,2(R6)     CLT                                          
*                                                                               
         BRAS  RE,XSPHIGH                                                       
         CLC   BIGKEY(AUTKPRD-AUTKEY),BIGKEYSV   SAME TYP/A-M/CLT               
         JNE   EXIT                                                             
*                                                                               
CLTF4    MVC   AREC,ADBUY                                                       
         BRAS  RE,XSPGET                                                        
*                                                                               
         BRAS  RE,EXLATEST         EXTRACT DATA FROM LATEST AUTH                
                                                                                
*===============================================================                
* READ FOR ORIGINAL AUTH (99) UNLESS WE HAVE IT                                 
*===============================================================                
                                                                                
         MVC   BIGKEYSV,BIGKEY     SAVE CURRENT AUTH                            
*                                                                               
CLTF10   CLI   K.AUTKREV,X'99'                                                  
         BE    CLTF12                                                           
         BRAS  RE,XSPSEQ                                                        
         CLC   BIGKEY(AUTKMKT-AUTKEY),BIGKEYSV TY/A-M/C/PR/PR2/EST/VER          
         BE    CLTF10                                                           
         DC    H'0'                                                             
*                                                                               
CLTF12   BRAS  RE,XSPGET                                                        
*                                                                               
         BRAS  RE,EXORIG           EXTRACT DATA FROM ORIGINAL AUTH              
                                                                                
*=================================================================              
* NOW READ MARKET AND STATION LEVEL AUTHORIZATIONS                              
*=================================================================              
                                                                                
CLTF20   BRAS  RE,XSPSEQ                                                        
         CLC   BIGKEY(AUTKPRD-AUTKEY),BIGKEYSV   SAME TYP/A-M/CLT               
         JNE   EXIT                                                             
         CLC   BIGKEY(AUTKMKT-AUTKEY),BIGKEYSV  TY/A-M/C/PR/PR2/EST/VER         
         BNE   CLTF4                                                            
*                                                                               
         MVC   AREC,ADBUY                                                       
         BRAS  RE,XSPGET                                                        
*                                                                               
         OC    K.AUTKSTA,K.AUTKSTA  TEST STATION PRESENT                        
         BNZ   CLTF30                                                           
*                                                                               
         BRAS  RE,EXMKT             EXTRACT MARKET DATA                         
*                                                                               
**       CLI   SVDSPROF+0,C'S'      TEST STATION LEVEL EXTRACT                  
**       BE    CLTF20                                                           
*                                                                               
         CLC   SVRUNTYP,=C'DS'       TEST TURNAROUND MODE                       
         BE    CLTF22                NO                                         
         MVC   ERECSTA(8),ERECBLNK   SUPPRESS STATION IN TABLE                  
         LA    R4,ERECTAB                                                       
         BRAS  RE,OUTPUT                                                        
*                                                                               
CLTF22   LA    R4,DRECTAB                                                       
         BRAS  RE,OUTPUT                                                        
         B     CLTF20                                                           
*                                                                               
CLTF30   CLI   SVDSPROF+0,C'S'     TEST STATION LEVEL                           
         BNE   CLTF20                                                           
*                                                                               
         CLC   SVRUNTYP,=C'DS'     TEST TURNAROUND MODE                         
         BE    CLTF32                                                           
         LA    R4,ERECTAB                                                       
         BRAS  RE,OUTPUT                                                        
*                                                                               
CLTF32   BRAS  RE,EXSTA            EXTRACT STATION DATA                         
         LA    R4,DRECTAB                                                       
         BRAS  RE,OUTPUT                                                        
         B     CLTF20                                                           
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*=========================================================                      
* EXTRACT DATA FROM LATEST AUTH                                                 
*=========================================================                      
                                                                                
EXLATEST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING AUTRECD,R8                                                       
*                                                                               
         MVC   THISAGY,QAGY                                                     
         MVC   THISMED,QMED                                                     
         MVC   THISCLT,CLT                                                      
         LA    R1,AUTKPRD                                                       
         BAS   RE,FNDPRD                                                        
         MVC   THISPRD,0(RF)                                                    
*                                                                               
         MVC   THISPRD2,SPACES                                                  
         LA    R1,AUTKPRD2                                                      
         CLI   0(R1),0                                                          
         BE    *+14                                                             
         BAS   RE,FNDPRD                                                        
         MVC   THISPRD2,0(RF)                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,AUTKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         LA    R1,AUTKAUN          VERSION/FLIGHT                               
         BRAS  RE,UNCOMP                                                        
         UNPK  THISVER,DUB                                                      
*                                                                               
         LA    R1,AUTKREV          POINT TO DATA                                
         BRAS  RE,UNCOMP                                                        
         UNPK  THISREV,DUB                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,AUDISDT),(10,THISISDT)                            
*                                                                               
         GOTO1 (RF),(R1),(3,AUDFLST),(10,THISAUST)                              
         GOTO1 (RF),(R1),(3,AUDFLEN),(10,THISAUND)                              
*                                                                               
         MVI   ELCODE,AINFELQ                                                   
         LA    R6,42(R8)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AINFELD,R6                                                       
         MVI   THISBYBS,C'G'         SET GOAL BASIS                             
         CLI   AINFBYBS,C'0'                                                    
         BE    *+8                                                              
         MVI   THISBYBS,C'$'         SET DOLLARS BASIS                          
         DROP  R6                                                               
*                                                                               
         J     EXIT                                                             
*                                                                               
FNDPRD   L     RF,ADCLT                                                         
         AHI   RF,CLIST-CLTHDRD                                                 
*                                                                               
FNDPRD2  CLC   0(1,R1),3(RF)                                                    
         BER   RE                                                               
         AHI   RF,4                                                             
         CLI   0(RF),C'A'                                                       
         BNL   FNDPRD2                                                          
         DC    H'0'                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* EXTRACT DATA FROM ORIGINAL AUTHORIZATION                                      
*============================================================                   
                                                                                
EXORIG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING AUTRECD,R8                                                       
*                                                                               
         MVC   THISCRDT,SPACES                                                  
         OC    AUDRVDT,AUDRVDT                                                  
         BZ    EXOR2                                                            
         GOTO1 DATCON,DMCB,(3,AUDRVDT),(10,THISCRDT)                            
*                                                                               
EXOR2    LA    R6,42(R8)                                                        
         MVI   ELCODE,AINFELQ                                                   
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AINFELD,R6                                                       
         MVC   THISCRBY,AINFBYGR                                                
         DROP  R6                                                               
* LOOK UP SUPERVISOR IN TABLE AND FLAG ACTIVE                                   
         XC    DUB,DUB                                                          
         MVI   DUB,C'S'                                                         
         MVC   DUB+1(6),THISCRBY                                                
         BRAS  RE,SETACTV                                                       
         BE    *+10                                                             
         MVC   THISCRBY+1(5),=C'ZZZZZ'                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* EXTRACT MARKET LEVEL DATA                                                     
*============================================================                   
                                                                                
EXMKT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   THISORDT,SPACES                                                  
         MVC   THISORCF,SPACES                                                  
         MVC   THISSTA,SPACES                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING AUTRECD,R8                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,AUTKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISMKT,DUB                                                      
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MINFRVNO                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISRVSN,DUB                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,MINFDUDT),(10,THISRVDD)                           
*                                                                               
         MVC   THISRVDT,SPACES                                                  
         OC    MINFRVDT,MINFRVDT                                                
         BZ    EXMKT10                                                          
         GOTO1 (RF),(R1),(3,MINFRVDT),(10,THISRVDT)                             
*                                                                               
EXMKT10  MVC   THISGLDT,SPACES                                                  
         OC    MINFGIDT,MINFGIDT                                                
         BZ    EXMKT12                                                          
         GOTO1 (RF),(R1),(3,MINFGIDT),(10,THISGLDT)                             
*                                                                               
EXMKT12  MVC   THISGLRV,SPACES                                                  
         OC    MINFGCDT,MINFGCDT                                                
         BZ    EXMKT14                                                          
         GOTO1 (RF),(R1),(3,MINFGCDT),(10,THISGLRV)                             
*                                                                               
EXMKT14  MVC   THISWKDT,SPACES                                                  
         OC    MINFWKDT,MINFWKDT                                                
         BZ    EXMKT16                                                          
         GOTO1 (RF),(R1),(3,MINFWKDT),(10,THISWKDT)                             
*                                                                               
EXMKT16  MVC   THISNWDT,SPACES                                                  
         OC    MINFNXDT,MINFNXDT                                                
         BZ    EXMKT18                                                          
         GOTO1 (RF),(R1),(3,MINFNXDT),(10,THISNWDT)                             
*                                                                               
EXMKT18  MVC   THISMLDT,SPACES                                                  
         OC    MINFMLDT,MINFMLDT                                                
         BZ    EXMKT20                                                          
         GOTO1 (RF),(R1),(3,MINFMLDT),(10,THISMLDT)                             
*                                                                               
EXMKT20  MVC   THISBGDT,SPACES                                                  
         OC    MINFBGDT,MINFBGDT                                                
         BZ    EXMKT22                                                          
         GOTO1 (RF),(R1),(3,MINFBGDT),(10,THISBGDT)                             
*                                                                               
EXMKT22  MVC   THISPLDT,SPACES                                                  
         OC    MINFRDDT,MINFRDDT                                                
         BZ    EXMKT24                                                          
         GOTO1 (RF),(R1),(3,MINFRDDT),(10,THISPLDT)                             
*                                                                               
EXMKT24  MVC   THISFLDT,SPACES                                                  
         OC    MINFRSDT,MINFRSDT                                                
         BZ    EXMKT30                                                          
         GOTO1 (RF),(R1),(3,MINFRSDT),(10,THISFLDT)                             
                                                                                
*========================================================                       
* MARKET COMPLETED BY BUYER X'08' OR SUPV X'0A' ELEM                            
*========================================================                       
                                                                                
EXMKT30  MVC   THISMKDT,SPACES                                                  
         MVC   THISMKBY,SPACES                                                  
         MVI   THISMKBT,C' '                                                    
*                                                                               
         MVI   ELCODE,X'08'        LOOK FOR BUYER COMP ELEMENT                  
         LA    R6,42(R8)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   EXMKT32                                                          
         MVI   THISMKBT,C'B'       SET BUYER COMPLETED                          
         B     EXMKT34                                                          
*                                                                               
EXMKT32  MVI   ELCODE,X'0A'        LOOK FOR SUPV COMP ELEMENT                   
         LA    R6,42(R8)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   EXMKT40                                                          
         MVI   THISMKBT,C'S'       SET SUPV COMPLETED                           
*                                                                               
         USING MSTAELD,R6                                                       
*                                                                               
EXMKT34  MVC   THISMKBY,MSTABYGR   SET GROUP/CODE                               
         GOTO1 DATCON,DMCB,(3,MSTADATE),(10,THISMKDT)                           
*                                                                               
         XC    DUB,DUB             LOOK UP TO FLAG ACTIVE                       
         MVC   DUB(1),THISMKBT                                                  
         MVC   DUB+1(6),THISMKBY                                                
         BRAS  RE,SETACTV                                                       
         BE    *+10                                                             
         MVC   THISMKBY+1(5),=C'ZZZZZ'                                          
                                                                                
*===========================================================                    
* SUPV STATUS FROM 06=REJECTED, 07=APPROVED 09=CANCELLED                        
*===========================================================                    
                                                                                
EXMKT40  MVC   THISSTDT,SPACES                                                  
         MVC   THISSTBY,SPACES                                                  
*                                                                               
         MVI   THISSTST,C'R'       SET REJECTED                                 
         MVI   ELCODE,X'06'                                                     
         LA    R6,42(R8)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    EXMKT42                                                          
*                                                                               
         MVI   THISSTST,C'A'       SET APPROVED                                 
         MVI   ELCODE,X'07'                                                     
         LA    R6,42(R8)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    EXMKT42                                                          
*                                                                               
         MVI   THISSTST,C'C'       SET CANCELLED                                
         MVI   ELCODE,X'09'                                                     
         LA    R6,42(R8)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    EXMKT42                                                          
*                                                                               
         MVI   THISSTST,C'D'       SET DELETED                                  
         MVI   ELCODE,X'0B'                                                     
         LA    R6,42(R8)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    EXMKT42                                                          
*                                                                               
         MVI   THISSTST,C' '                                                    
         B     EXMKT50                                                          
*                                                                               
         USING MSTAELD,R6                                                       
EXMKT42  GOTO1 DATCON,DMCB,(3,MSTADATE),(10,THISSTDT)                           
         MVC   THISSTBY,MSTABYGR                                                
*                                                                               
EXMKT50  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* EXTRACT STATION LEVEL DATA                                                    
*===============================================================                
                                                                                
EXSTA    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ADBUY                                                         
         USING AUTRECD,R8                                                       
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',AUTKMKT),WORK,THISSTA                         
*                                                                               
         MVC   THISORDT,SPACES                                                  
         MVC   THISORCF,SPACES                                                  
*                                                                               
         OC    SDTLORSN,SDTLORSN   TEST ORDER SENT                              
         BZ    EXSTAX                                                           
         GOTO1 DATCON,DMCB,(3,SDTLORSN),(10,THISORDT)                           
*                                                                               
         OC    SDTLSCNF,SDTLSCNF                                                
         BZ    EXSTAX                                                           
         GOTO1 DATCON,DMCB,(3,SDTLSCNF),(10,THISORCF)                           
*                                                                               
EXSTAX   J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* LOOK UP CODE IN TABLE AND SET AS ACTIVE                                       
*===============================================================                
                                                                                
SETACTV  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 BINSRCH,CODEPAR1,(2,DUB)                                         
*                                                                               
         L     RE,0(R1)                                                         
         CLC   DUB(7),0(RE)        MAKE SURE FOUND IT                           
         JNE   NEQXIT              TEST CALLER NOT FOUND                        
*                                                                               
         USING CTABD,RE                                                         
         OI    CTABFLAG,X'01'      SET ACTIVE                                   
         DROP  RE                                                               
         J     EQXIT               SET CC =                                     
         LTORG                                                                  
                                                                                
UNCOMP   NTR1  BASE=*,LABEL=*                                                   
         ZAP   DUB,=P'0'                                                        
         SR    R0,R0                                                            
         ICM   R0,1,0(R1)          GET 1 BYTE VALUE                             
         SLL   R0,4                MOVE 4 BITS                                  
         STCM  R0,3,DUB+6          SAVE THE 2 BYTES                             
         OI    DUB+7,X'0F'         MAKE IT PACKED AGAIN                         
         SP    DUB,=P'99'                                                       
         OI    DUB+7,X'0F'                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* BUILD TABLE OF BUYER/SUPERVISOR CODES                                         
*==============================================================                 
         SPACE 1                                                                
BLDCODES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,=A(CODETAB)                                                   
         L     R1,=A(CODETABX-CODETAB)                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
K        USING SPVKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   K.SPVKTYP,=X'0D61'     SUPV KEY                                  
         PACK  K.SPVKAGY,BAGY                                                   
         GOTO1 HIGH                                                             
         B     BLDCOD4                                                          
         DROP  K                                                                
*                                                                               
BLDCOD2  GOTO1 SEQ                                                              
*                                                                               
BLDCOD4  CLC   KEY(3),KEYSAVE                                                   
         BNE   BLDCOD10                                                         
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         BAS   RE,ADDCODE                                                       
         B     BLDCOD2                                                          
*                                                                               
K        USING BYRKEY,KEY                                                       
*                                                                               
BLDCOD10 XC    KEY,KEY                                                          
         MVC   K.BYRKTYP,=X'0D62'     BUYER KEY                                 
         PACK  K.BYRKAGY,BAGY                                                   
         GOTO1 HIGH                                                             
         B     BLDCOD14                                                         
         DROP  K                                                                
*                                                                               
BLDCOD12 GOTO1 SEQ                                                              
*                                                                               
BLDCOD14 CLC   KEY(3),KEYSAVE                                                   
         BNE   BLDCOD20                                                         
*                                                                               
         GOTO1 GETBUY                                                           
         BAS   RE,ADDCODE                                                       
         B     BLDCOD12                                                         
*                                                                               
BLDCOD20 XC    WORK,WORK          ADD DUMMY BUYER/SUPV RECS                     
         MVC   WORK(6),=C'BZZZZZ'                                               
         GOTO1 BINSRCH,CODEPAR1,(1,WORK)                                        
         MVC   WORK(6),=C'SZZZZZ'                                               
         GOTO1 BINSRCH,CODEPAR1,(1,WORK)                                        
         J     EXIT                                                             
         EJECT                                                                  
*===================================================================            
* NOTE THAT SUPV/BUYER RECORDS SHARE SAME ELEMENT CODES/LENGTHS                 
* FOR THE FIELDS WE ARE INTERESTED IN, EXCEPT FOR PLANNER FLAG                  
* IN SUPV RECORD.                                                               
*===================================================================            
                                                                                
ADDCODE  NTR1                                                                   
         XC    WORK,WORK                                                        
C        USING CTABD,WORK                                                       
*                                                                               
         L     R6,ADBUY                                                         
         USING BYRRECD,R6                                                       
*                                                                               
         MVC   C.CTABOFFC,BYRKOFC                                               
         MVC   C.CTABBYR,BYRKBYR                                                
         DROP  R6                                                               
*                                                                               
         LA    R6,24(R6)                                                        
         CLI   0(R6),X'01'                                                      
         BE    ADDCOD1                                                          
         L     RE,ADBUY                                                         
         CLC   =C'0D61',0(RE)      SUPERVISOR RECORD?                           
         BNE   *+6                                                              
         DC    H'0'                DIE IF SUPERVISOR REC MISSING X'01'          
         OC    BYRKOFC-BYRKEY(6,RE),BYRKOFC-BYRKEY(RE)                          
         BZ    BLDCODX             IF BUYER/BILLER  0D62 REC, EXIT              
         DC    H'0'                                                             
         USING BYRNAMED,R6                                                      
*                                                                               
ADDCOD1  MVC   C.CTABFRST,BYRFNAME  MOVE FIRST/LAST NAMES                       
         MVC   C.CTABLAST,BYRLNAME                                              
*                                                                               
         LA    R1,C.CTABFRST        REMOVE SEMICOLONS FROM TEXT                 
         LHI   R0,24                                                            
*                                                                               
ADDCOD2  CLC   0(1,R1),EOLCHR      TEST SEMICOLON                               
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLC   0(1,R1),EORCHR      OR COLON                                     
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         AHI   R1,1                                                             
         BCT   R0,ADDCOD2                                                       
*                                                                               
         MVI   C.CTABTYPE,C'B'     SET BUYER                                    
         L     RE,ADBUY            POINT TO RECORD                              
         CLI   1(RE),X'62'         TEST BUYER REC                               
         BE    ADDCOD20            NO                                           
         MVI   C.CTABTYPE,C'S'     SET SUPERVISOR                               
*                                                                               
         MVI   ELCODE,X'22'        FIND ELEM TO SEE IF PLNR/SUPV                
         BRAS  RE,NEXTEL                                                        
         BNE   ADDCOD20                                                         
*                                                                               
         USING SPIDELD,R6                                                       
         TM    SPIDFLAG,SPIDFPLN   TEST PLANNER FLAG                            
         BZ    *+8                                                              
         MVI   C.CTABFLAG,X'80'    SET PLANNER FLAG                             
*                                                                               
ADDCOD20 GOTO1 BINSRCH,CODEPAR1,(1,WORK) INSERT IF NOT FOUND                    
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDCODX  XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* GENERATE CODE TABLES - ALL ENTRIES FOR DS/ACTIVE ENTRIES FOR US               
*====================================================================           
         SPACE 1                                                                
GENCODES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,=A(CODETAB)                                                   
         USING CTABD,R6                                                         
         ICM   R7,15,CODEPAR3       GET RECORD COUNT                            
         JNP   EXIT                                                             
*                                                                               
GENCOD2  MVC   THISCTYP,CTABTYPE                                                
         MVC   THISCBYR,CTABOFFC                                                
         MVC   THISCFRS,CTABFRST                                                
         MVC   THISCLAS,CTABLAST                                                
*                                                                               
         CLC   SVRUNTYP,=C'DS'     TEST TO SEND ALL                             
         BNE   GENCOD10                                                         
*                                                                               
         LA    R4,URECTAB                                                       
         BRAS  RE,OUTPUT                                                        
         B     GENCOD20                                                         
*                                                                               
GENCOD10 TM    CTABFLAG,X'01'      TEST ACTIVE                                  
         BZ    GENCOD20                                                         
*                                                                               
         LA    R4,VRECTAB          KILL PREVIOUS ????                           
         BRAS  RE,OUTPUT                                                        
*                                                                               
         LA    R4,URECTAB                                                       
         BRAS  RE,OUTPUT                                                        
*                                                                               
GENCOD20 AHI   R6,L'CODETAB                                                     
         BCT   R7,GENCOD2                                                       
*                                                                               
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=============================================================*                 
* OUTPUT  DATA TO PRTQUE REPORT                                                 
*=============================================================*                 
         SPACE 1                                                                
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         AP    RUNRECS,=P'1'                                                    
*                                                                               
OUTPUT4  L     RE,0(R4)            GET DATA ADDR                                
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
*                                                                               
         CLI   5(R4),C'T'          TEST TEXT                                    
         BNE   OUTPUT6                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    D.DLCBFLD(0),SPACES                                              
         CLI   QOPT1,C'Y'          TEST FIXED LENGTH OUTPUT                     
         BNE   *+10                                                             
         MVC   D.DLCBLEN,4(R4)     FIX OUTPUT LENGTH                            
         B     OUTPUT18                                                         
*                                                                               
OUTPUT6  CLI   5(R4),C'B'          TEST BINARY                                  
         BNE   OUTPUT14                                                         
         CLI   QOPT1,C'Y'          TEST FIXED LENGTH OUTPUT                     
         BE    OUTPUT8                                                          
         MVC   D.DLCBNDP,7(R4)     SET NUMBER OF DECIMAL PLACES                 
         MVC   D.DLCBLEN,4(R4)     SET DATE LENGTH                              
         B     OUTPUT20                                                         
*                                                                               
* FIXED LEN NUMERIC OUTPUT                                                      
*                                                                               
OUTPUT8  ICM   R0,15,0(RE)         GET VALUE IN R0                              
         MVI   D.DLCBTYP,C'N'      TYPE=NUMERIC                                 
*                                                                               
         CLI   7(R4),1             TEST 1 DECIMAL                               
         BNE   OUTPUT10                                                         
         MVC   WORK(11),=X'4021202020202020204B20'                              
         CVD   R0,DUB                                                           
         ED    WORK(11),DUB+3                                                   
         MVC   D.DLCBFLD(8),WORK+3                                              
         MVI   D.DLCBLEN,8         FIX OUTPUT LEN                               
         B     OUTPUT30                                                         
*                                                                               
OUTPUT10 CLI   7(R4),2             TEST 2 DECIMAL                               
         BNE   OUTPUT13                                                         
*                                                                               
         MVC   WORK(17),=X'40212020202020202020202020204B2020'                  
         LA    R1,DUB                                                           
         LTR   R0,R0                                                            
         BNM   OUTPUT11                                                         
         MVC   WORK(17),=X'404021202020202020202020204B202060'                  
         LA    R1,DUB+1                                                         
*                                                                               
OUTPUT11 CVD   R0,DUB                                                           
         ED    WORK(17),DUB                                                     
         MVC   D.DLCBFLD(13),WORK+4                                             
         MVI   D.DLCBLEN,13                                                     
         B     OUTPUT30                                                         
*                                                                               
OUTPUT13 DC    H'0'                                                             
*                                                                               
OUTPUT14 TM    7(R4),X'01'         TEST CVD REQUIRED                            
         BZ    OUTPUT16                                                         
         ICM   R0,15,0(RE)         GET DATA VALUE                               
         CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         SLL   RF,4                SET LEN TO UNPK TO                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  D.DLCBFLD(0),DUB                                                 
         B     OUTPUT20                                                         
*                                                                               
OUTPUT16 CLI   5(R4),C'X'          TEST HEX                                     
         BNE   OUTPUT18                                                         
         CLI   QOPT1,C'Y'          TEST FIXED LENGTH OUTPUT                     
         BNE   *+10                                                             
         MVC   D.DLCBLEN,4(R4)     FIX OUTPUT LENGTH                            
*                                                                               
OUTPUT18 CLI   6(R4),0             TEST FIELD CAN END RECORD                    
         BE    OUTPUT20            NO                                           
         CLC   D.DLCBFLD(1),6(R4)  ELSE COMPARE                                 
         BNH   OUTPUT32            AND POSSIBLY END                             
*                                                                               
OUTPUT20 MVC   D.DLCBTYP(1),5(R4)                                               
         CLI   5(R4),C'X'          TEST HEX OUTPUT                              
         BNE   *+8                                                              
         MVI   D.DLCBTYP,C'T'      TELL DLFLD IT'S TEXT                         
*                                                                               
OUTPUT30 MVI   D.DLCBACT,DLCBPUT                                                
*                                                                               
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
         MVI   D.DLCXDELC,C' '     ALWAYS RESTORE TERMINATOR                    
*                                                                               
         LA    R4,L'RECTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT4                                                          
*                                                                               
OUTPUT32 MVI   D.DLCBACT,DLCBEOL                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
BLPRINT  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
MYHDHK   NTR1  BASE=*,LABEL=*                                                   
         LM    R9,RA,HDHKR9                                                     
         USING SPWORKD,RA,R9                                                    
         L     RC,HDHKRC                                                        
         XIT1                                                                   
*                                                                               
HDHKR9   DS    A                                                                
HDHKRA   DS    A                                                                
HDHKRB   DS    A                                                                
HDHKRC   DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCODE,0(R6)                                                     
         JNE   NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
XSPHIGH  MVC   COMMAND,DMRDHI                                                   
         MVC   BIGKEYSV,BIGKEY                                                  
         J     XSPDIR                                                           
*                                                                               
XSPSEQ   MVC   COMMAND,DMRSEQ                                                   
*                                                                               
XSPDIR   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,=C'XSPDIR',BIGKEYSV,BIGKEY                  
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
XSPGET   NTR1  BASE=*,LABEL=*                                                   
         MVC   COMMAND,GETREC                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,=C'XSPFIL',BIGKEY+36,AREC                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SPDSWORK DS    0D                                                               
         DC    CL8'SPDSWORK'                                                    
BIGKEY   DS    XL40                                                             
BIGKEYSV DS    XL40                                                             
BUFFCNT  DS    F                                                                
BUFFERIN DC    V(BUFFERIN)                                                      
ACODETAB DC    A(CODETAB)                                                       
SVDSPROF DS    CL16                                                             
ELCODE   DS    X                                                                
SVRUNTYP DS    CL2                                                              
*                                                                               
CODEPAR1 DC    A(0)                                                             
CODEPAR2 DC    A(CODETAB)          A(TABLE)                                     
CODEPAR3 DC    F'0'                RECORD COUNT                                 
CODEPAR4 DC    A(32)               RECORD LENGTH                                
CODEPAR5 DC    A(7)                KEYDSPL=0/KEYLEN=7                           
CODEPAR6 DC    A((CODETABX-CODETAB)/L'CODETAB)                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'RUNTOTS'                                                     
RUNTOTS  DS    0D                                                               
RUNRECS  DC    PL8'0'                                                           
RUNSPOTS DC    PL8'0'                                                           
RUNDOLS  DC    PL8'0'                                                           
*                                                                               
         DS    0D                                                               
DLCB     DS    XL256                                                            
*                                                                               
* THESE FIELDS USED TO GET WIDE PRINT LINE OVERRIDES                            
* THEY GET MOVED INTO THE DLCB TO OVERRIDE MAXLINE                              
*                                                                               
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
         EJECT                                                                  
* ENTRIES ARE                                                                   
* AL4(DATA)                                                                     
* AL1(L'DATA)                                                                   
* CL1'TYPE'                                                                     
* C'  '            IF NOT X'00' EOR IF FIELD NOT > THIS VALUE                   
* X'01'            CONVERT THE FIELD TO DECIMAL BEFORE WRITE                    
* OR IF TYPE=B,    LAST BYTE IS NUMBER OF DECIMAL PLACES                        
         SPACE 1                                                                
*===============================================================                
* IN FIXED LENGTH MODE, BINARY FIELDS ARE OUTPUT AT FIXED LENGTHS               
* 1 DECIMAL VALUES GO OUT AS 8 BYTES                                            
* 2 DECIMAL VALUES GO OUT AS 13 BYTES                                           
*===============================================================                
         SPACE 1                                                                
         DS    0D                                                               
RECTAB   DS    0XL8                                                             
         DC    CL8'DRECTAB*'                                                    
DRECTAB  DC    AL4(THISBEAD),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISPRD),AL1(L'THISPRD),C'T',2X'00'                          
         DC    AL4(THISPRD2),AL1(L'THISPRD2),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISVER),AL1(L'THISVER),C'N',2X'00'                          
         DC    AL4(THISREV),AL1(L'THISREV),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
*                                                                               
         DC    AL4(THISCRDT),AL1(L'THISCRDT),C'T',2X'00'                        
         DC    AL4(THISCRBY),AL1(L'THISCRBY),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISISDT),AL1(L'THISISDT),C'T',2X'00'                        
         DC    AL4(THISAUST),AL1(L'THISAUST),C'T',2X'00'                        
         DC    AL4(THISAUND),AL1(L'THISAUND),C'T',2X'00'                        
         DC    AL4(THISBYBS),AL1(L'THISBYBS),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISRVSN),AL1(L'THISRVSN),C'N',2X'00'                        
         DC    AL4(THISRVDD),AL1(L'THISRVDD),C'T',2X'00'                        
         DC    AL4(THISRVDT),AL1(L'THISRVDT),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISGLDT),AL1(L'THISGLDT),C'T',2X'00'                        
         DC    AL4(THISGLRV),AL1(L'THISGLRV),C'T',2X'00'                        
         DC    AL4(THISWKDT),AL1(L'THISWKDT),C'T',2X'00'                        
         DC    AL4(THISNWDT),AL1(L'THISNWDT),C'T',2X'00'                        
         DC    AL4(THISMLDT),AL1(L'THISMLDT),C'T',2X'00'                        
         DC    AL4(THISBGDT),AL1(L'THISBGDT),C'T',2X'00'                        
         DC    AL4(THISPLDT),AL1(L'THISPLDT),C'T',2X'00'                        
         DC    AL4(THISFLDT),AL1(L'THISFLDT),C'T',2X'00'                        
         DC    AL4(THISMKDT),AL1(L'THISMKDT),C'T',2X'00'                        
         DC    AL4(THISMKBY),AL1(L'THISMKBY),C'T',2X'00'                        
         DC    AL4(THISMKBT),AL1(L'THISMKBT),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISSTST),AL1(L'THISSTST),C'T',2X'00'                        
         DC    AL4(THISSTDT),AL1(L'THISSTDT),C'T',2X'00'                        
         DC    AL4(THISSTBY),AL1(L'THISSTBY),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISORDT),AL1(L'THISORDT),C'T',2X'00'                        
         DC    AL4(THISORCF),AL1(L'THISORCF),C'T',2X'00'                        
         DC   X'FF'                                                             
         EJECT                                                                  
*====================================================================           
* THIS TABLE TO KILL RECORDS FOR TURNAROUND MODE                                
* TABLES START WITH RECORD TYPE BYTE, THEN ACTION BYTE K=KILL,                  
* THEN 1 ENTRY FOR EACH COLUMN IN THE KEY TABLE                                 
*====================================================================           
* NOTE TABLE CAN BE MODIFIED TO SUPPRESS STATION !!!!!                          
*====================================================================           
         SPACE 1                                                                
ERECTAB  DC    AL4(THISBEAE),AL1(1),C'T',2X'00'      RECORD TYPE                
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISPRD),AL1(L'THISPRD),C'T',2X'00'                          
         DC    AL4(THISPRD2),AL1(L'THISPRD2),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'  VER                   
ERECBLNK DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'  REV                   
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
ERECSTA  DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    X'FF'                                                            
*                                                                               
         DC    CL8'URECTAB*'                                                    
URECTAB  DC    AL4(THISBEAU),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISCTYP),AL1(L'THISCTYP),C'T',2X'00'                        
         DC    AL4(THISCBYR),AL1(L'THISCBYR),C'T',2X'00'                        
         DC    AL4(THISCFRS),AL1(L'THISCFRS),C'T',2X'00'                        
         DC    AL4(THISCLAS),AL1(L'THISCLAS),C'T',2X'00'                        
         DC    X'FF'                                                            
*                                                                               
         DC    CL8'VRECTAB*'                                                    
VRECTAB  DC    AL4(THISBEAV),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISCTYP),AL1(L'THISCTYP),C'T',2X'00'                        
         DC    AL4(THISCBYR),AL1(L'THISCBYR),C'T',2X'00'                        
         DC    AL4(THISCFRS),AL1(L'THISCFRS),C'T',2X'00'                        
         DC    AL4(THISCLAS),AL1(L'THISCLAS),C'T',2X'00'                        
         DC    X'FF'                                                            
*                                                                               
THISBEAD DC   C'D'                                                              
THISBEAE DC   C'E'                                                              
THISBEAK DC   C'K'                                                              
THISBEAU DC   C'U'                                                              
THISBEAV DC   C'V'                                                              
*                                                                               
THISBLNK DC   C' '                                                              
*                                                                               
THISID   DS    XL19                                                             
         ORG   THISID                                                           
TIDAGYA  DS    CL2                                                              
TIDQMED  DS    CL1                                                              
TIDBCLT  DS    XL2                                                              
TIDBPRD  DS    XL1                                                              
TIDBEST  DS    XL1                                                              
TIDBMKT  DS    XL2                 MKT  FOR GOAL RECORDS                        
TIDBPRD2 DS    XL1                 PRD2 FOR GOAL RECORDS                        
         ORG   *-3                                                              
TIDBSTA  DS    XL3                 STATION FOR BUY RECORDS                      
TIDQSPL  DS    CL1                                                              
TIDBDPT  DS    XL1                                                              
TIDBSLN  DS    XL1                                                              
TIDPURP  DS    CL6                 PURPOSE CODE                                 
*                                                                               
THISAGY  DS    CL2                                                              
THISMED  DS    CL1                                                              
THISCLT  DS    CL3                                                              
THISPRD  DS    CL3                                                              
THISPRD2 DS    CL3                                                              
THISEST  DS    CL3                                                              
THISVER  DS    CL2                 VERSION/FLIGHT                               
THISREV  DS    CL2                 REVISION                                     
THISMKT  DS    CL4                                                              
THISSTA  DS    CL8                                                              
*                                                                               
THISCRDT DS    CL10                YYYY-MM-DD                                   
THISCRBY DS    CL6                                                              
THISISDT DS    CL10                YYYY-MM-DD                                   
THISAUST DS    CL10                YYYY-MM-DD                                   
THISAUND DS    CL10                YYYY-MM-DD                                   
THISBYBS DS    CL1                                                              
*                                                                               
THISRVSN DS    CL2                 MARKET REVISION NUMBER                       
THISRVDD DS    CL10                MARKET BUY DUE DATE                          
THISRVDT DS    CL10                MARKET REVISION DATE                         
*                                                                               
THISGLDT DS    CL10                GOAL INPUT DATE                              
THISGLRV DS    CL10                GOAL REVISION DATE                           
THISWKDT DS    CL10                WORK RECORD ADDED DATE                       
THISNWDT DS    CL10                LAST NWS TRANSFER                            
THISMLDT DS    CL10                LAST MEDIA LOCKIN DATE                       
THISBGDT DS    CL10                LAST BUYING GUIDELINES RUN                   
THISPLDT DS    CL10                REPORTS DUE PLANNING                         
THISFLDT DS    CL10                REPORTS SENT TO FIELD DATE                   
*                                                                               
THISMKDT DS    CL10                MARKET COMPLETED DATE                        
THISMKBY DS    CL6                 MARKET COMPLETED BY                          
THISMKBT DS    CL1                 COMP BY TYPE B=BUYER,S=SUPV                  
*                                                                               
THISSTST DS    CL1                 SUPERVISOR STATUS                            
THISSTDT DS    CL10                STATUS DATE                                  
THISSTBY DS    CL6                 SUPV STATUS BY                               
*                                                                               
THISORDT DS    CL10                STATION ORDER SEND DATE                      
THISORCF DS    CL10                STATION CONFIRMED DATE                       
*                                                                               
THISCTYP DS    CL1                                                              
THISCBYR DS    CL6                                                              
THISCFRS DS    CL10                                                             
THISCLAS DS    CL14                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*CODETAB'                                                    
CODETAB  DS    2500CL32                                                         
CODETABX EQU   *                                                                
*                                                                               
CTABD    DSECT                                                                  
CTABTYPE DS    CL1                                                              
CTABOFFC DS    CL2                                                              
CTABBYR  DS    CL4                                                              
CTABFRST DS    CL10                                                             
CTABLAST DS    CL14                                                             
CTABFLAG DS    CL1                 X'80'=SUPV IS PLANNER, X'01'=USED            
CTABX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
*                                                                               
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTA     DS    CL5                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PCOST    DS    CL8                                                              
         DS    CL1                                                              
PDATE    DS    CL6                                                              
         DS    CL1                                                              
PTIME    DS    CL6                                                              
         DS    CL1                                                              
         ORG   PDATE                                                            
PSPOTS   DS    CL3                                                              
         ORG                                                                    
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPGENBYR                                                       
       ++INCLUDE SPGENSPV                                                       
       ++INCLUDE SPGENAUTH                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPDS02 09/29/08'                                      
         END                                                                    
