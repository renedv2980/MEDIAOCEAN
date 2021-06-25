*          DATA SET NESFM47    AT LEVEL 232 AS OF 10/31/05                      
*PHASE T31C47A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
***********************************************************************         
*                                                                               
*  TITLE: T31C47 - CLIENT COPY OVERNIGHT UPDATIVE REPORT                        
*                                                                               
***********************************************************************         
         TITLE 'T31C47 CLIENT COPY OV REPORT'                                   
T31C47   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CLCOPY,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    *+12                                                             
         CLI   T31CFFD+1,C'*'      DDS ONLY?                                    
         BNE   DDSERR                                                           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         LA    R2,CLCMEDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         MVI   BYTE,C'A'                                                        
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 VALIMED             AGY/MED IN BAGYMD                            
*                                                                               
         LA    R2,CLCCLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALICLT             2 BYTE CLT IN BCLT                           
         MVC   FROMCLT,BCLT                                                     
         GOTO1 CLUNPK,DMCB,(BCLIAAN,FROMCLT),FROMCLTA                           
*                                                                               
         XC    KEY,KEY             GET FROM CLIENT RECORD AND CHECK             
         LA    R6,KEY              IF IT HAS PRODUCTS                           
         USING CLTRECD,R6                                                       
*                                                                               
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,FROMCLT                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CLTDNE                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         OC    CLIST(4),CLIST      ANY PRODUCTS IN FROM CLIENT?                 
         BZ    CLTPNEX                                                          
*                                                                               
         LA    R2,CLCTCLTH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         GOTO1 VALICLT             2 BYTE CLT IN BCLT                           
         MVC   TOCLT,BCLT                                                       
         GOTO1 CLUNPK,DMCB,(BCLIAAN,TOCLT),TOCLTA                               
*                                                                               
         XC    KEY,KEY             GET TO CLIENT RECORD AND CHECK               
         LA    R6,KEY              IF IT HAS PRODUCTS                           
         USING CLTRECD,R6          OR IS ASSIGNED TO ANY CLT GROUP              
*                                                                               
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,TOCLT                                                    
*                                                                               
         CLC   FROMCLT,TOCLT       CAN'T BE SAME CLIENT                         
         BE    CLTDUP                                                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CLTDNE                                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         OC    CLIST(4),CLIST      ANY PRODUCTS IN DESTINATION CLT?             
         BNZ   CLTPEX                                                           
*                                                                               
         OC    CGRP1,CGRP1         ASSIGNED TO A CLIENT GROUP?                  
         BNZ   CLTCGEX                                                          
         OC    CGRP2,CGRP2                                                      
         BNZ   CLTCGEX                                                          
         OC    CGRP3,CGRP3                                                      
         BNZ   CLTCGEX                                                          
         OC    CGRP4,CGRP4                                                      
         BNZ   CLTCGEX                                                          
         OC    CGRP5,CGRP5                                                      
         BNZ   CLTCGEX                                                          
*                                                                               
         BAS   RE,CLEARSC                                                       
         BAS   RE,PROFILES                                                      
*                                                                               
         LA    R5,PROFTAB                                                       
         CLI   0(R5),X'FF'         ANY MISSING PROFILES?                        
         BE    VKX                                                              
*                                                                               
         BAS   RE,DPROMISS         DISPLAY MISSING PROFILES                     
         B     PROFERR                                                          
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT RECORDS                                                                 
***********************************************************************         
PR       DS    0H                                                               
         MVC   P(5),=C'AT PR'                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,PRNTAGY                                                       
*                                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         BAS   RE,PRDRECS          COPY PRODUCT RECORDS                         
         MVC   P(11),=C'PRODUCTS - '                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,PCODUDEF         COPY PRODUCT CODES AND UDEF FIELDS           
         MVC   P(11),=C'PCODUDEF - '                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,PGROUPS          COPY PRODUCT GROUPS                          
         MVC   P(10),=C'PGROUPS - '                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,DAYPARTS         COPY USER DEFINED DAYPARTS                   
         MVC   P(7),=C'DPTS - '                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLEAR BOTTOM PORTION OF SCREEN                                                
***********************************************************************         
CLEARSC  NTR1                                                                   
         LA    R2,CLCERH                                                        
         LA    R3,7                                                             
*                                                                               
CSC10    DS    0H                                                               
         XC    8(L'CLCER,R2),8(R2)                                              
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R3,CSC10                                                         
*                                                                               
CSCX     DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* CHECK PROFILES                                                                
***********************************************************************         
PROFILES NTR1                                                                   
         LA    R2,CLCTCLTH                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         XC    PROFTAB,PROFTAB                                                  
         LA    R5,PROFTAB                                                       
         MVI   0(R5),X'FF'                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING CTUREC,R6                                                        
*                                                                               
         MVI   CTUKTYP,C'U'                                                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO,MYDMWRK              
         B     PROF20                                                           
*                                                                               
PROFSEQ  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'CTFILE ',KEY,AIO,MYDMWRK              
*                                                                               
PROF20   DS    0H                                                               
         L     R6,AIO                                                           
         MVC   PROFKEY,0(R6)                                                    
*                                                                               
         CLI   CTUKTYP,C'U'                                                     
         BNE   PROF200                                                          
         CLI   CTUKSYS,C'S'                                                     
         BNE   PROFSEQ                                                          
         CLC   CTUKAGY,AGENCY                                                   
         BNE   PROFSEQ                                                          
*                                                                               
         CLC   CTUKCLT,FROMCLTA    FOUND FROM CLIENT PROFILE?                   
         BNE   PROFSEQ                                                          
*&&DO                                                                           
         MVC   P(9),=C'FR PROF: '                                               
         MVC   P+11(1),CTUKSYS                                                  
         MVC   P+13(3),CTUKPROG                                                 
         GOTO1 HEXOUT,DMCB,CTUKLANG,P+17,1                                      
         MVC   P+20(2),CTUKAGY                                                  
         MVC   P+23(1),CTUKMED                                                  
         MVC   P+25(3),CTUKCLT                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*&&                                                                             
         MVC   CTUKCLT,TOCLTA      CHECK IF TO CLIENT PROFILE EXISTS            
         XC    KEY,KEY                                                          
         MVC   KEY(25),0(R6)                                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO,MYDMWRK              
*                                                                               
         L     R6,AIO                                                           
         MVC   PROFPGM,CTUKPROG                                                 
         OC    PROFPGM,SPACES                                                   
*                                                                               
***      CLI   CTUKTYP,C'U'                                                     
***      BNE   PROF50                                                           
*                                                                               
***      CLC   CTUKAGY,AGENCY                                                   
***      BNE   PROF50                                                           
*                                                                               
***      CLC   CTUKCLT,TOCLTA      FOUND TO CLIENT PROFILE?                     
***      BE    PROF100                                                          
         CLC   KEY(25),0(R6)                                                    
         BE    PROF100                                                          
*                                                                               
PROF50   DS    0H                  ADD TO MISSING PROF TABLE                    
         XC    KEY,KEY                                                          
         MVC   KEY(25),PROFKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO,MYDMWRK              
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         MVC   0(1,R5),CTUKMED     MOVE IN MEDIA TO TABLE                       
         MVC   1(3,R5),CTUKPROG    MOVE IN PROGRAM TO TABLE                     
         MVI   4(R5),X'FF'                                                      
         LA    R5,4(R5)                                                         
*                                                                               
PROF100  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),PROFKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY,AIO,MYDMWRK              
         B     PROFSEQ                                                          
*                                                                               
PROF200  DS    0H                                                               
*                                                                               
PROFX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY MISSING PROFILES                                                      
***********************************************************************         
DPROMISS NTR1                                                                   
         MVC   CLCER(37),=C'PLEASE ADD THE FOLLOWING CLIENT LEVEL'              
         MVC   CLCER+38(12),=C'PROFILES FOR'                                    
         MVC   CLCER+51(3),TOCLTA                                               
         MVI   CLCER+54,C':'                                                    
         OI    CLCERH+6,X'80'                                                   
*                                                                               
         LA    R2,CLCER1H                                                       
         LA    R3,CLCER1                                                        
         LA    R4,8                MAX # DISPLAY PER LINE                       
*                                                                               
         LA    R5,PROFTAB                                                       
*                                                                               
DPM10    DS    0H                                                               
         CLI   0(R5),X'FF'                                                      
         BE    DPROMX                                                           
*                                                                               
         CLI   0(R5),0                                                          
         BNE   DPM15                                                            
         MVC   0(3,R3),=C'ALL'                                                  
         MVI   3(R3),C'/'                                                       
         LA    R3,4(R3)                                                         
         B     DPM20                                                            
*                                                                               
DPM15    MVC   0(1,R3),0(R5)       MEDIA                                        
         MVI   1(R3),C'/'                                                       
         LA    R3,2(R3)                                                         
*                                                                               
DPM20    DS    0H                                                               
         MVC   0(2,R3),2(R5)                                                    
         CLI   1(R5),0             2 CHAR PROGRAM?                              
         BE    *+10                                                             
         MVC   0(3,R3),1(R5)                                                    
*                                                                               
         LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         BCT   R4,DPM10                                                         
*                                                                               
         LA    R4,8                                                             
         OI    6(R2),X'80'                                                      
         ZIC   RF,0(R2)            FINISHED LINE, GO TO NEXT ONE                
         AR    R2,RF                                                            
         LR    R3,R2                                                            
         LA    R3,8(R3)                                                         
         B     DPM10                                                            
*                                                                               
DPROMX   DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* COPY PRODUCT RECORDS                                                          
***********************************************************************         
PRDRECS  NTR1                                                                   
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   PKEYAM,BAGYMD       GET FROM PRODUCT RECORD                      
         MVC   PKEYCLT,FROMCLT                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   PDRX                                                             
*                                                                               
         OC    PKEYPRD,PKEYPRD     MAKE SURE IT'S A PRODUCT RECORD              
         BNZ   PDR10                                                            
*                                                                               
PDRSEQ   GOTO1 SEQ                                                              
*                                                                               
PDR10    DS    0H                                                               
         LA    R6,KEY                                                           
         CLI   0(R6),X'00'                                                      
         BNE   PDRX                                                             
         CLC   PKEYAM,BAGYMD       SAME FROM AGENCY                             
         BNE   PDRX                                                             
         CLC   PKEYCLT,FROMCLT     SAME FROM CLIENT?                            
         BNE   PDRX                                                             
*                                                                               
         OC    PKEYPRD,PKEYPRD     MAKE SURE IT'S A PRD RECORD                  
         BZ    PDRSEQ                                                           
         OC    PKEYPRD+3(6),PKEYPRD+3     MUST BE BLANK                         
         BNZ   PDRSEQ                     SKIP ESTIMATE RECORDS                 
*                                                                               
         MVC   SVSPTKEY,KEY        SAVE FROM PRD KEY                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              FROM PRODUCT IN AIO1                         
*                                                                               
         L     R6,AIO                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'FROM PRD',0(R6),C'DUMP',15,=C'1D'             
*                                                                               
         XC    KEY,KEY             GET TO PRODUCT RECORD                        
*                                                                               
         LA    R6,KEY                                                           
         MVC   KEY(13),SVSPTKEY                                                 
         MVC   PKEYCLT,TOCLT       REPLACE FROM PRD WITH TO PRD                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TO PRODUCT RECORD SHOULD NOT EXIST           
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R6,AIO              ADD NEW TO PRODUCT RECORD                    
         MVC   PKEYCLT,TOCLT                                                    
*                                                                               
         MVI   ACTELOPT,C'N'       DON'T ADD 'F1' ELEMENT                       
         GOTO1 ADDREC                                                           
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'TO PRD',0(R6),C'DUMP',15,=C'1D'               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVSPTKEY    RESTORE FROM PRD SEQUENCE                    
         GOTO1 HIGH                                                             
*                                                                               
         B     PDRSEQ                                                           
*                                                                               
PDRX     DS    0H                                                               
         MVC   P+12(10),=C'SUCCESSFUL'                                          
         B     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
* COPY PRODUCT CODES AND UDEF FIELDS                                            
***********************************************************************         
PCODUDEF NTR1                                                                   
         XC    KEY,KEY             GET FROM CLIENT RECORD                       
         MVC   AIO,AIO1            IN AIO1                                      
*                                                                               
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,FROMCLT                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC              FROM CLIENT IN AIO1                          
         L     R6,AIO1                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'FROM CLT',0(R6),C'DUMP',1500,=C'1D'           
*                                                                               
         XC    KEY,KEY             GET TO CLIENT RECORD                         
         MVC   AIO,AIO2            IN AIO2                                      
*                                                                               
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,TOCLT                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   P(11),=C'TO CLT KEY:'                                            
         GOTO1 HEXOUT,DMCB,KEY,P+13,18                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         GOTO1 GETREC              TO CLIENT IN AIO2                            
         L     R6,AIO2                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'TO BEFORE',0(R6),C'DUMP',1500,=C'1D'          
*                                                                               
         L     R6,AIO1                                                          
         LA    R6,CLIST                                                         
         ST    R6,AFCCLIST         A(FROM CLIENT PRODUCT LIST)                  
         L     R6,AIO1                                                          
         LA    R6,CLIST2                                                        
         ST    R6,AFCCLST2                                                      
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,CLIST                                                         
         ST    R6,ATCCLIST         A(TO CLIENT PRODUCT LIST)                    
         L     R6,AIO2                                                          
         LA    R6,CLIST2                                                        
         ST    R6,ATCCLST2                                                      
*                                                                               
         L     R4,ATCCLIST                                                      
         L     R2,AFCCLIST                                                      
         SR    R3,R3                                                            
         LH    R3,=H'880'          L'CLIENT PRODUCT LIST                        
         LR    R5,R3                                                            
         MVCL  R4,R2               COPY FROM CLT PRODUCTS                       
*                                                                               
         L     R4,ATCCLST2                                                      
         L     R2,AFCCLST2                                                      
         SR    R3,R3                                                            
         LH    R3,=H'140'          L'CLIENT PRODUCT LIST                        
         LR    R5,R3                                                            
         MVCL  R4,R2               COPY FROM CLT PRODUCTS                       
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,CPU1                                                          
         ST    R6,ATCCPU1          A(TO CLIENT UDEF FIELDS)                     
*                                                                               
         L     R6,AIO1             FROM CLIENT RECORD                           
*                                                                               
         L     RF,ATCCPU1                                                       
         MVC   0(CULNQ,RF),CPU1    COPY UDEF FIELDS                             
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'TO UPD',0(R6),C'DUMP',1500,=C'1D'             
*                                                                               
         XC    KEY,KEY             GET TO CLIENT RECORD                         
         MVC   KEY(13),0(R6)       RESTORE GETREC/PUTREC                        
*                                                                               
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   P(11),=C'TO CLT KEY:'                                            
         GOTO1 HEXOUT,DMCB,KEY,P+13,18                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'BEF PUT',0(R6),C'DUMP',1500,=C'1D'            
*                                                                               
         MVC   AIO,AIO2            AIO2 = UPDATED TO CLIENT RECORD              
*                                                                               
         MVI   ACTELOPT,C'N'       DON'T ADD 'F1' ELEMENT                       
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,AIO2,MYDMWRK          
*                                                                               
*!!!!    GOTO1 PUTREC                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER PUT',0(R6),C'DUMP',1500,=C'1D'          
*                                                                               
PCUDX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* COPY PRODUCT GROUPS                                                           
***********************************************************************         
PGROUPS  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R6,KEY                                                           
         USING PRGRECD,R6                                                       
*                                                                               
         MVC   0(2,R6),=XL2'0D01'                                               
         MVC   PRGKAGMD,BAGYMD                                                  
         MVC   PRGKCLT,FROMCLT                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      ANY PRODUCT GROUPS FOR FROM CLIENT?          
         BE    PGRP20                                                           
         MVC   P+10(10),=C'NO PGROUPS'                                          
         B     PGRPX                                                            
*                                                                               
PGRP10   DS    0H                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(5),KEYSAVE      ANY MORE PGROUPS FOR "FROM" CLIENT?          
         BNE   PGRP100                                                          
*                                                                               
PGRP20   DS    0H                                                               
         MVC   SVSPTKEY,KEY                                                     
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,KEY                                                           
         MVC   PRGKCLT,TOCLT       PGROUP EXISTS FOR "TO" CLIENT?               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PGRP30              NO - ADD NEW                                 
*                                                                               
         MVC   AIO,AIO2            YES - JUST DO PUTREC                         
         GOTO1 GETREC              GETREC/PUTREC LOGIC                          
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO                                                           
         MVC   PRGKCLT,TOCLT                                                    
*                                                                               
         MVC   P(6),=C'TO:   '                                                  
         GOTO1 HEXOUT,DMCB,0(R6),P,18                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   P(6),=C'PUTREC'                                                  
         GOTO1 HEXOUT,DMCB,0(R6),P,18                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PGRP50                                                           
*                                                                               
PGRP30   DS    0H                  ADD NEW PGROUP FOR "TO" CLT                  
         L     R6,AIO                                                           
*                                                                               
         MVC   P(6),=C'FROM: '                                                  
         GOTO1 HEXOUT,DMCB,0(R6),P+7,18                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PRGKCLT,TOCLT                                                    
*                                                                               
         MVC   P(6),=C'TO:   '                                                  
         GOTO1 HEXOUT,DMCB,0(R6),P+7,18                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   P(6),=C'ADDREC'                                                  
         GOTO1 HEXOUT,DMCB,0(R6),P+7,18                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)       THIS NEW "TO" PGROUP REC JUST ADDED          
         GOTO1 HIGH                GET A(RECORD) FOR PASSIVE KEYS               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   SVAREC,KEY+14       A(NEW "TO" PGRP RECORD)                      
*                                                                               
         BAS   RE,PGRPPAS          GO ADD PGROUP PASSIVE KEY                    
*                                                                               
PGRP50   DS    0H                  GET NEXT PRODUCT GROUP                       
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVSPTKEY                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PGRP10                                                           
         DC    H'00'                                                            
*                                                                               
PGRP100  DS    0H                                                               
         MVC   P+10(6),=C'COPIED'                                               
*                                                                               
PGRPX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* COPY PRODUCT GROUPS PASSIVE KEYS                                              
***********************************************************************         
PGRPPAS  NTR1                                                                   
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING PRGRECD,R6                                                       
*                                                                               
         L     RF,AIO                                                           
*                                                                               
         MVC   PRGPTYP,=XL2'0D81'                                               
         MVC   PRGPAGMD,BAGYMD                                                  
         MVC   PRGPCLT,FROMCLT     FROM PASSIVE KEYS                            
         MVC   PRGPID,5(RF)        GROUP ID                                     
         MVC   PRGPGRP,6(RF)       GROUP NUMBER                                 
*                                                                               
         GOTO1 HIGH                                                             
         B     PGRPA20                                                          
*                                                                               
PGRPA10  DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PGRPA20  DS    0H                                                               
         CLC   KEY(8),KEYSAVE      SAME PRODUCT GROUP?                          
         BNE   PGRPAX                                                           
*                                                                               
         MVC   SVSPTPSV,KEY        SAVE PASSIVE KEY                             
*                                                                               
         MVC   P(6),=C'FROMP:'                                                  
         MVC   P+8(3),PRGPPRD                                                   
         GOTO1 HEXOUT,DMCB,KEY,P+12,18                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   PRGPCLT,TOCLT       ADD NEW "TO" PGRP PASSIVE KEYS               
         MVC   KEY+14(4),SVAREC    WITH NEW "TO" A(PGROUP REC)                  
*                                                                               
         BAS   RE,MYDIRADD                                                      
*                                                                               
         MVC   P(6),=C'TOP:  '                                                  
         MVC   P+8(3),PRGPPRD                                                   
         GOTO1 HEXOUT,DMCB,KEY,P+12,18                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVSPTPSV    RESTORE SEQUENCE OF PASSIVE KEYS             
         GOTO1 HIGH                                                             
*                                                                               
         B     PGRPA10                                                          
*                                                                               
PGRPAX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* COPY USER DEFINED DAYPARTS                                                    
***********************************************************************         
DAYPARTS NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DPTHDRD,R6                                                       
*                                                                               
         MVI   NDPTKTY,X'0D'                                                    
         MVI   NDPTKST,X'07'                                                    
         MVC   NDPTAGM,BAGYMD                                                   
         MVC   NDPTCLT,FROMCLT     GET FROM CLIENT DAYPARTS                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR ',KEY,KEY,MYDMWRK              
*                                                                               
         CLC   KEY(5),KEYSAVE                                                   
         BE    DPT20                                                            
         MVC   P+9(7),=C'NO DPTS'                                               
         B     DPTX                                                             
*                                                                               
DPT10    DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR ',KEY,KEY,MYDMWRK              
*                                                                               
DPT20    DS    0H                                                               
         CLC   KEY(5),KEYSAVE                                                   
         BNE   DPT200                                                           
*                                                                               
         MVC   SVUNTKEY,KEY                                                     
*                                                                               
         MVC   SVDPTA,NDPTDPTA     SAVE ALPHA DAYPART                           
*                                                                               
         BAS   RE,CHKDPTA          CHECK IF THIS ALPHA DPT EXISTS               
         TM    MYFLAG,FNDDPT                                                    
         BZ    DPT50               DPT DOESN'T EXIST, ADD NEW DPT REC           
*                                                                               
         LA    R6,KEY                                                           
         MVC   NDPTDES,SVUNTKEY+6  CHANGE TO NEW DESCRIPTION                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
         MVC   P(6),=C'CDPT: '                                                  
         MVC   P+8(2),NDPTDPTA                                                  
         MVC   P+12(14),NDPTDES                                                 
         GOTO1 HEXOUT,DMCB,KEY,P+30,25                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     DPT100                                                           
*                                                                               
DPT50    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),SVUNTKEY                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R6,KEY                                                           
         MVC   SVDPTA,NDPTDPTA                                                  
*                                                                               
         BAS   RE,GETDPTE          GET AVAILABLE DPT EQUATE                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),SVUNTKEY                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R6,KEY                                                           
         MVC   NDPTCLT,TOCLT       ADD NEW DPT FOR "TO" CLIENT                  
         MVC   NDPTDPTE,DPTEQ      NEW DPT EQUATE                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'UNTDIR  ',KEY,KEY                      
*                                                                               
         MVC   P(6),=C'ADPT: '                                                  
         MVC   P+8(2),NDPTDPTA                                                  
         MVC   P+12(14),NDPTDES                                                 
         GOTO1 HEXOUT,DMCB,KEY,P+30,25                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
DPT100   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),SVUNTKEY                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         B     DPT10                                                            
*                                                                               
DPT200   DS    0H                                                               
         MVC   P+9(10),=C'SUCCESSFUL'                                           
*                                                                               
DPTX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF DAYPART ALREADY EXISTS IN "TO" CLIENT                                
***********************************************************************         
CHKDPTA  NTR1                                                                   
         NI    MYFLAG,X'FF'-FNDDPT                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DPTHDRD,R6                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,TOCLT       TO CLIENT                                    
         MVI   NDPTCLT+2,X'01'     GET FIRST CLIENT                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR ',KEY,KEY,MYDMWRK              
         B     CDPTA20                                                          
*                                                                               
CDPTA10  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR ',KEY,KEY,MYDMWRK              
*                                                                               
CDPTA20  CLC   KEY(5),KEYSAVE                                                   
         BNE   CHKDPTAX            UNIQUE TO CLIENT, DPT OK                     
*                                                                               
         CLC   NDPTDPTA,SVDPTA     DOES THE DPT ALREADY EXIST?                  
         BNE   CDPTA10                                                          
*                                                                               
         OI    MYFLAG,FNDDPT       YES - JUST CHANGE DESCRIPTION                
*                                                                               
CHKDPTAX DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GET FIRST AVAILABLE EQUATE FOR NEW DPT RECORD FOR "TO" CLIENT                 
***********************************************************************         
GETDPTE  NTR1                                                                   
         MVI   DPTEQ,X'01'                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DPTHDRD,R6                                                       
*                                                                               
         MVC   NDPTKEY(2),=XL2'0D07'                                            
         MVC   NDPTAGM,BAGYMD      AGENCY/MEDIA                                 
         MVC   NDPTCLT,TOCLT       TO CLIENT                                    
*                                                                               
GDTPE10  DS    0H                                                               
         XC    KEY+6(14),KEY+6                                                  
         ZIC   RF,DPTEQ                                                         
         STC   RF,NDPTDPTE                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR ',KEY,KEY,MYDMWRK              
         CLC   KEY(6),KEYSAVE                                                   
         BNE   GDTPEX                                                           
*                                                                               
         ZIC   RF,DPTEQ                                                         
         LA    RF,1(RF)                                                         
         STC   RF,DPTEQ                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR ',KEY,KEY,MYDMWRK              
         B     GDTPE10                                                          
*                                                                               
GDTPEX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT AGENCY RECORD                                                           
***********************************************************************         
PRNTAGY  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
*                                                                               
         MVI   0(R6),X'06'                                                      
         MVC   1(2,R6),AGENCY                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'AGY',0(R6),C'DUMP',1000,=C'1D'                
         MVC   AIO,AIO1                                                         
*                                                                               
PRNTAGYX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H3,3,C'MEDIA N'                                                  
         SSPEC H1,46,C'CLIENT COPY REPORT'                                      
         SSPEC H2,46,C'------------------'                                      
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,3,C'FROM CLIENT - '                                           
         SSPEC H4,93,RUN                                                        
         SSPEC H5,3,C'TO CLIENT - '                                             
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         MVC   H4+19(3),FROMCLTA   PRINT FROM CLIENT                            
         MVC   H5+19(3),TOCLTA     PRINT TO CLIENT                              
*                                                                               
         MVC   H8(7),=C'RESULTS'                                                
         MVC   H8+132(7),=C'-------'                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
INVAGY   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'ERROR RECORD NOT VALID FOR THIS AGENCY'           
         B     MYERR                                                            
*                                                                               
CLTDNE   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'ERROR TO CLIENT RECORD NOT SET UP'                
         B     MYERR                                                            
*                                                                               
CLTDUP   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'ERROR MUST BE DIFFERENT CLIENT   '                
         B     MYERR                                                            
*                                                                               
CLTPEX   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ERROR CLIENT RECORD ALREADY AS PRODUCTS'          
         B     MYERR                                                            
*                                                                               
CLTPNEX  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ERROR CLIENT DOES NOT HAVE ANY PRODUCTS'          
         B     MYERR                                                            
*                                                                               
CLTCGEX  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'ERROR CLIENT ASSIGNED TO A CLIENT GROUP'          
         B     MYERR                                                            
*                                                                               
DDSERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'DDS ONLY REPORT - PLEASE CONTACT DDS   '          
         B     MYERR                                                            
*                                                                               
PROFERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'MUST ADD CLIENT SPECIFIC PROFILES FIRST'          
         B     MYERR                                                            
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMAED                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C47 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
*                                                                               
SVSPTKEY DS    XL13                                                             
SVSPTPSV DS    XL13                                                             
SVUNTKEY DS    XL20                                                             
SVAREC   DS    A                   A(PRODUCT GROUP RECORD)                      
*                                                                               
SVDPTA   DS    XL2                 ALPHA DAYPART                                
DPTEQ    DS    XL1                                                              
*                                                                               
FROMCLT  DS    XL2                 FROM CLIENT                                  
FROMCLTA DS    CL3                 FROM CLIENT ALPHA                            
TOCLT    DS    XL2                 TO CLIENT                                    
TOCLTA   DS    CL3                 TO CLIENT ALPHA                              
*                                                                               
AFCCLIST DS    A                   A(FROM CLIENT PRODUCT LIST)                  
AFCCLST2 DS    A                   A(FROM CLIENT PRODUCT LIST 2)                
ATCCLIST DS    A                   A(TO CLIENT PRODUCT LIST)                    
ATCCLST2 DS    A                   A(TO CLIENT PRODUCT LIST 2)                  
ATCCPU1  DS    A                   A(TO CLIENT UDEF FIELDS)                     
*                                                                               
MYFLAG   DS    XL1                                                              
FNDDPT   EQU   X'01'               DPT EXISTS IN "TO" CLIENT                    
*                                                                               
PROFPGM  DS    CL3                PROFILE NAME                                  
*                                                                               
PROFKEY  DS    XL25                                                             
*                                                                               
PROFTAB  DS    XL200               TABLE OF MISSING PROFILES                    
*                                                                               
WORKEND  EQU   *                                                                
         EJECT                                                                  
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
DPTHDRD  DSECT                                                                  
       ++INCLUDE NEGENDPT                                                       
       ++INCLUDE SPGENPRG                                                       
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'232NESFM47   10/31/05'                                      
         END                                                                    
