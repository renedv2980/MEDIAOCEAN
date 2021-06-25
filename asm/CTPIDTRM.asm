*          DATA SET CTPIDTRM   AT LEVEL 154 AS OF 01/17/19                      
*PHASE CPIDTRMA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE SQUASHER                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SMTP                                                                   
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*&&      SET   NOP=N                                                            
         TITLE 'CPIDTRM - TERMINATE OLD UNUSED PERSON IDS'                      
CPIDTRM  CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**PTRM**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(27),=C'TERMINATE UNUSED PERSON IDS'                        
*                                                                               
         L     R2,=A(PQBUFF-WORKD)                                              
         AR    R2,RC                                                            
         ST    R2,APQBUFF                                                       
*                                                                               
         MVC   LINE,MAXLINE                                                     
         LARL  R8,SSB                                                           
         USING SSOOFF,R8                                                        
*                                                                               
TERM040  GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         LA    R3,CARD                                                          
         CLC   =C'/*',0(R3)                                                     
         BE    TERM050                                                          
         MVC   P(80),0(R3)                                                      
         GOTO1 VPRINTER                                                         
         CLC   0(8,R3),=C'WRITE=N '                                             
         JNE   *+8                                                              
         MVI   WRITE,C'N'                                                       
*                                                                               
         CLC   0(6,R3),=C'EMAIL='                                               
         JNE   *+10                                                             
         MVC   EMAILFLG,6(R3)                                                   
*                                                                               
         CLC   0(7,R3),=C'DSPACE='                                              
         JNE   *+18                                                             
         MVC   SSODSPAC,7(R3)                                                   
         CLI   8(R3),C' '                                                       
         JNE   *+2                                                              
*                                                                               
         J     TERM040                                                          
*                                                                               
TERM050  MVC   MID1(79),HEDR                                                    
         MVC   MID2(79),HEDRL                                                   
*                                                                               
         GOTO1 VDATCON,PARM,(5,0),(0,TODAY)                                     
         GOTO1 VDATCON,PARM,(0,TODAY),(3,TODAY3)                                
         GOTO1 VDATCON,PARM,(0,TODAY),(2,TODAYC)                                
         GOTO1 VDATCON,PARM,(0,TODAY),(11,TODAYA)                               
         MVC   TODAY2,TODAYC                                                    
         XC    TODAYC,FFS                                                       
*                                                                               
         GOTO1 VADDAY,DMCB,TODAY,DUB,-1                                         
         GOTO1 VDATCON,DMCB,(0,DUB),(2,YESTERDC)                                
*                                                                               
         TIME  TU                  SET DATE TIME IN HEADER                      
         ST    R0,FULL                                                          
         BRAS  RE,TIMEOUT                                                       
         MVC   HEDR1+29(5),WORK1                                                
*                                                                               
         MVC   HEDR1+08(2),TODAYA+3                                             
         MVC   HEDR1+11(3),TODAYA+0                                             
         MVC   HEDR1+15(2),TODAYA+6                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,OPENLST,IO                          
*                                                                               
         BRAS  RE,INIT             BUILD AGENCY TABLE                           
*                                                                               
TERM100  LA    R3,AGYTAB                                                        
         CLI   0(R3),0             HANDLE ZERO TABLE CONDITION                  
         BE    TERM990                                                          
*                                                                               
TERM110  BRAS  RE,GETGRP           GET GROUP NUMBER FOR GROUP                   
*                                                                               
         BRAS  RE,CHKPERS          CHECK PERSONS IN AGENCY                      
*                                                                               
         AHI   R3,AGYTLEN          NEXT AGENCY                                  
         CLI   0(R3),0                                                          
         BNE   TERM110                                                          
*                                                                               
TERM990  GOTO1 VDATAMGR,DMCB,DMCLS,CONTROL,WORK,IO                              
*                                                                               
XBASE    XBASE                                                                  
*                                                                               
XIT1     XIT1                                                                   
*                                                                               
************************************************************                    
*        FILL AGYTAB WITH AG,DAYS,GROUP WHERE REQUIRED     *                    
************************************************************                    
INIT     NTR1                                                                   
*                                                                               
INIT000  LA    R3,AGYTAB                                                        
         USING AGYTABD,R3                                                       
*                                                                               
         LA    R2,IO                                                            
         USING CT5KEY,R2                                                        
         XC    0(L'CT5KEY,R2),0(R2)                                             
         MVI   CT5KTYP,CT5KTYPQ                                                 
         GOTO1 ,DMCB,DMRDHI,CTFILE,(R2),(R2)                                    
*                                                                               
INIT010  GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   INIT100                                                          
         DC    H'0'                                                             
*                                                                               
         CLI   CT5KTYP,CT5KTYPQ    STILL ON ACCESS RECORDS                      
         BNE   INIT100                                                          
         MVC   AGYTAG,CT5KALPH                                                  
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('CTSEAELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BE    INIT090             IF SECURITY AGENCY FOUND IGNORE THIS         
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('CTDSCELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   INIT090             NOT FOUND                                    
         L     R5,12(R1)                                                        
         MVC   AGYTPQID,2(R5)      SAVE PQ FOR REPORT                           
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('CTAADELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   INIT090             NOT FOUND                                    
         L     R5,12(R1)                                                        
         B     INIT050                                                          
*                                                                               
         USING CTAADD,R5                                                        
INIT050  CLI   CTAADLEN,X'0C'      OLD ELEMENT LEN HAS NO EXP DETAIL            
         BNH   INIT090                                                          
         BH    INIT060                                                          
*                                                                               
INIT055  MVI   AGYTDAYS,60         EXPIRY DAYS  DEFAULT TO USE IF REQD          
         MVC   AGYTGRP,SPACES      EXPIRY GROUP                                 
         B     INIT070                                                          
*                                                                               
INIT060  MVC   AGYTDAYS,CTAAEXPD   EXPIRY DAYS                                  
         MVC   AGYTGRP,CTAAEXPG    EXPIRY GROUP                                 
*                                                                               
         CLI   CTAAEXPD,0          IGNORE ZERO DAYS                             
         BE    INIT090                                                          
*                                                                               
INIT070  DS    0H                                                               
*&&UK*&& MVC   AGYTPWRN,CTAAPWRN   WARNING DAYS BEFORE EXPIRY                   
*&&US*&& MVI   AGYTPWRN,7          WARNING DAYS BEFORE EXPIRY                   
*                                                                               
INIT080  LA    R3,AGYTLEN(R3)      NEXT TAB                                     
         CLI   0(R3),X'FF'                                                      
         BNE   INIT010                                                          
         DC    H'0'                OVERFLOW                                     
*                                                                               
INIT090  XC    0(AGYTLEN,R3),0(R3) IGNORE AGY DON'T SAVE TO TAB                 
         B     INIT010                                                          
*                                                                               
INIT100  B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        CHECK PERSON RECORD FOR INACTIVE PERSONS          *                    
************************************************************                    
         SPACE 1                                                                
CHKPERS  NTR1                                                                   
         USING PLINED,P                                                         
*                                                                               
CHKP200  LA    R2,IO               READ PERSON RECORDS                          
         USING SAPEREC,R2                                                       
         XC    0(L'SAPEKEY,R2),0(R2)                                            
CHKP210  MVC   P,SPACES                                                         
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGYTAG                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,(R2),(R2)                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   AGYTAG,SAPEAGY      STILL ON SAME AGENCY                         
         BNE   CHKP300             NO - EXIT                                    
*                                                                               
         MVC   PLAGY,SAPEAGY       SET UP FOR PRINTING                          
         MVC   PLPERSON,SAPEPID                                                 
*                                                                               
         MVC   EFFDATEC,SAPEDEF    EFFECTIVE DATE                               
         XC    EFFDATEC,=F'-1'                                                  
         GOTO1 VDATCON,PARM,(2,EFFDATEC),(3,EFFDATE)                            
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAPERELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   CHKP220             CONTINUE IF NOT FOUND                        
         L     R5,12(R1)                                                        
         USING SAPERD,R5                                                        
         OC    SAPERDTE,SAPERDTE   OR NOT TERMINATED                            
         BZ    CHKP220                                                          
         CLC   SAPERDTE,TODAY2     OR IF TERMINATION DATE => TODAY              
         BNL   CHKP220                                                          
         J     CHKP260             ELSE IGNORE IT                               
*                                                                               
CHKP220  GOTO1 VHELLO,PARM,(C'G',CTFILE),('SANAMELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   CHKP230             NO NAME ELEMENT                              
         L     R5,12(R1)                                                        
         USING SANAMD,R5                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SANAMLN        WHOLE ELEMENT LEN                            
         AR    R1,R5                                                            
         ST    R1,FULL             SAVE A(END)                                  
         MVC   PLPNAME,SPACES                                                   
*                                                                               
         XR    R0,R0               ZERO NAME COUNTER                            
         LA    RF,SANAMES                                                       
         LA    RE,PLPNAME                                                       
CHKP215  AHI   R0,1                COUNT WHICH NAME WE ARE ON                   
         XR    R1,R1                                                            
         ICM   R1,1,0(RF)                                                       
         SHI   R1,1                                                             
*                                                                               
         CHI   R0,2                                                             
         BNE   CHKP216                                                          
         TM    SANAMIND,SANAMIMN   DON'T BOTHER WITH MIDDLE NAME                
         BNZ   CHKP217                                                          
*                                                                               
CHKP216  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),1(RF)       COPY NAME TO PRINT AREA                      
         LA    RE,2(RE,R1)                                                      
*                                                                               
CHKP217  LA    RF,2(RF,R1)                                                      
         C     RF,FULL                                                          
         BL    CHKP215                                                          
*                                                                               
*****************************************************************               
*        SR    R1,R1                                                            
*        ICM   R1,1,SANAMLN                                                     
*        SHI   R1,13                                                            
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   PLPNAME(0),SANAME                                                
*                                                                               
*HKP219  LHI   R0,L'PLPNAME                                                     
*        LA    R1,PLPNAME                                                       
*HKP220  CLI   0(R1),C'A'                                                       
*        BNL   *+8                                                              
*        MVI   0(R1),C' '                                                       
*        BCT   R0,CHKP220                                                       
*                                                                               
*        TM    SANAMIND,SANAMIMN   DON'T BOTHER WITH MIDDLE NAME                
*        BZ    CHKP230                                                          
*****************************************************************               
*                                                                               
CHKP230  MVC   PLEMAIL,SPACES                                                   
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAPEEELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   CHKP240             NO EMAIL ELEMENT                             
         L     R5,12(R1)                                                        
         USING SAPEED,R5                                                        
*                                                                               
         MVI   BYTE,0                                                           
         SR    R1,R1               DO SOME EMAIL VALIDATION                     
         ICM   R1,1,SAPEELN                                                     
         AHI   R1,-2                                                            
         LA    RE,SAPEEID                                                       
CHKP235  CLI   0(RE),C' '          NO SPACES ALLOWED                            
         JNE   *+12                                                             
         CLI   BYTE,C'@'           ONCE @ FOUND                                 
         JE    CHKP240                                                          
         CLI   0(RE),C'@'          MUST HAVE AN @ IN IT                         
         JNE   *+8                                                              
         MVI   BYTE,C'@'                                                        
         LA    RE,1(RE)                                                         
         JCT   R1,CHKP235                                                       
         CLI   BYTE,C'@'                                                        
         JNE   CHKP240                                                          
*                                                                               
CHKP239  SR    R1,R1                                                            
         ICM   R1,1,SAPEELN        WHOLE ELEMENT LEN                            
         AHI   R1,-3                                                            
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   PLEMAIL(0),SAPEEID                                               
         LA    R1,PLEMAIL(R1)                                                   
         MVI   1(R1),C':'                                                       
*                                                                               
CHKP240  GOTO1 VHELLO,PARM,(C'G',CTFILE),('SALLOELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   CHKP245             NO LAST LOGON ELEMENT                        
         L     R5,12(R1)                                                        
*                                                                               
         USING SALLOD,R5                                                        
*                                                                               
         MVC   FULL,SALLODT        SET FULL TO LAST LOGON DATE                  
*                                                                               
         CLC   SALLODT,=X'640101'  IF 01JAN00 CHECK EFFDATE                     
         BE    CHKP245                                                          
         CLC   FULL,EFFDATE        IF LAST LOGON > EFFDATE                      
         BH    CHKP250             CHECK LAST LOGON                             
*                                                                               
CHKP245  MVC   FULL,EFFDATE        OR SET FULL TO EFFECTIVE DATE                
*                                                                               
CHKP250  GOTO1 VDATCON,PARM,(3,FULL),(17,PLLASTUS)                              
         GOTO1 VDATCON,PARM,(3,FULL),(0,LASTUSE)                                
         GOTO1 VPERVERT,PARM,LASTUSE,TODAY,0  DAYS SINCE LASTLOGON              
         SR    RF,RF                                                            
         ICM   RF,3,PARM+8                                                      
         BZ    *+6                                                              
         BCTR  RF,0                DROP 1 DAY AS PERVERT IS INCLUSIVE           
         STH   RF,CUTOFF                                                        
         EDIT  (B2,CUTOFF),(5,PLDAYS)                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,AGYTPWRN                                                    
         JZ    CHKP251                                                          
         LH    R1,CUTOFF           ADD N DAYS FOR WARNING                       
         AR    R1,RF                                                            
         STH   R1,CUTOFFW                                                       
*                                                                               
         SR    RF,RF               ARE THERE N DAYS TO EXPIRY                   
         IC    RF,AGYTDAYS                                                      
         CH    RF,CUTOFFW                                                       
         JNE   CHKP251             NO - CONTINUE                                
*                                                                               
         EDIT  (B1,AGYTPWRN),(2,HALF)                                           
         BRAS  RE,EMAILWRN         EMAIL A WARNING                              
         MVC   PLWARN,=C'Warn'                                                  
         J     CHKP255A                                                         
*                                                                               
CHKP251  SR    RF,RF               HAVE WE EXCEEDED DAYS ALLOWED                
         IC    RF,AGYTDAYS                                                      
         CH    RF,CUTOFF                                                        
         BNL   CHKP260             NO - NEXT                                    
*                                                                               
CHKP252  BRAS  RE,TERMINAT         HASTA LA VISTA BABY                          
         BE    CHKP255                                                          
*                                                                               
*NOP     MVC   P+53(10),=C'IGNORED   '  OR MAYBE NOT                            
         B     CHKP260             DON'T PRINT (ONLY WHEN TESTING)              
*                                                                               
CHKP255  BRAS  RE,PQPUT            REPORT TO PQ                                 
CHKP255A GOTO1 VPRINTER            REPORT ON TERMINATED PERSONID                
*                                                                               
CHKP260  MVC   SAPEDEF,=X'FFFF'    SET TO FF TO SKIP TO NEXT PERSON             
         B     CHKP210                                                          
*                                                                               
CHKP300  BRAS  RE,PQCLOSE          CLOSE PQ REPORT IF ANY                       
         B     XIT1                                                             
*                                                                               
         EJECT                                                                  
************************************************************                    
* TERMINATE THIS PERSON AND SET TO TERMINATED GROUP        *                    
************************************************************                    
         SPACE 1                                                                
TERMINAT NTR1                                                                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),CTFILE,(R2),(R2)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAPEDEF,TODAYC      SET DATE TO TODAY                            
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAPERELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   TERM030             NOT FOUND                                    
         L     R5,12(R1)                                                        
         USING SAPERD,R5                                                        
         OC    SAPERDTE,SAPERDTE   IF NOT TERMINATED                            
         BZ    *+14                                                             
         CLC   SAPERDTE,TODAY2     OF IF TERMINATION DATE > TODAY               
         BNH   TERMINNO                                                         
         MVC   SAPERDTE,YESTERDC   TERMINATE THEM YESTERDAY                     
*                                                                               
         CLI   SAPERPCN,SAPERPNQ   EXPIRY=N SO IGNORE                           
         BE    TERMINNO                                                         
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAACVELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   TERM010             NOT FOUND                                    
         L     R5,12(R1)                                                        
         USING SAACVD,R5                                                        
         MVC   SAACVDT,TODAY3      SET ACTIVITY TO TODAY                        
         B     TERM019                                                          
*                                                                               
TERM010  LA    R5,WORK                                                          
         MVI   SAACVEL,SAACVELQ                                                 
         MVI   SAACVLEN,SAACVLNQ                                                
         MVC   SAACVDT,TODAY3      SET ACTIVITY TO TODAY                        
         XC    SAACVSEQ,SAACVSEQ                                                
         GOTO1 VHELLO,PARM,(C'P',CTFILE),(R2),(R4),=C'ADD=CODE'                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TERM019  EQU   *                                                                
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAAGCELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   TERM030             NOT FOUND                                    
         L     R5,12(R1)                                                        
*                                                                               
         USING SAAGCD,R5                                                        
         MVC   OLDGRPN,SAAGCNUM    SAVE OLD GROUP NUM AND CODE                  
         MVC   OLDGRPC,SAAGCCOD                                                 
*                                                                               
         MVC   NEWGRPN,TERMGRP     SET NEW GROUP NUM AND CODE                   
         MVC   NEWGRPC,AGYTGRP                                                  
*                                                                               
         MVC   PLOLDGRP,OLDGRPC    SHOW IN PRINT LIVE                           
         MVC   PLNEWGRP,NEWGRPC                                                 
*                                                                               
         OC    NEWGRPC,NEWGRPC     IF NEW GROUP IS ZERO                         
         BNZ   TERM020                                                          
         XC    OLDGRPC,OLDGRPC     DON'T DEC OLD GROUP COUNT                    
         B     TERM021             AND SKIP ELEMENT SETTING                     
*                                                                               
TERM020  MVC   SAAGCNUM,TERMGRP    SET NEW GROUP AND ADD                        
         MVC   SAAGCCOD,AGYTGRP                                                 
TERM021  GOTO1 VDATAMGR,DMCB,DMADD,CTFILE,(R2),(R2)                             
         ORG   *-2                                                              
         CLI   WRITE,C'N'                                                       
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,INCGRP                                                        
         BRAS  RE,DECGRP                                                        
         B     TERMINX                                                          
         DROP  R5                                                               
*                                                                               
TERM030  MVC   NEWGRPN,TERMGRP     SAVE NEW GROUP                               
         MVC   NEWGRPC,AGYTGRP                                                  
         MVC   PLNEWGRP,NEWGRPC                                                 
         LA    R4,WORK                                                          
         USING SAAGCD,R4           BUILD ELEMENT AT R4                          
         MVI   SAAGCEL,SAAGCELQ                                                 
         MVI   SAAGCLN,SAAGCLNQ                                                 
         MVC   SAAGCNUM,TERMGRP                                                 
         MVC   SAAGCCOD,AGYTGRP                                                 
         GOTO1 VHELLO,PARM,(C'P',CTFILE),(R2),(R4),=C'ADD=CODE'                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMADD,CTFILE,(R2),(R2)                             
         ORG   *-2                                                              
         CLI   WRITE,C'N'                                                       
         BE    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,INCGRP                                                        
*                                                                               
TERMINX  CR    RB,RB                                                            
         B     XIT1                                                             
*                                                                               
TERMINNO LTR   RB,RB                                                            
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
* UPDATE GROUP PERSON COUNTS                               *                    
************************************************************                    
         SPACE 1                                                                
INCGRP   NTR1                                                                   
         MVI   BYTE,C'I'           FLAG INC                                     
         MVC   DUB,NEWGRPC                                                      
         B     UPDATGRP                                                         
*                                                                               
DECGRP   NTR1                                                                   
         MVI   BYTE,C'D'           FLAG DEC                                     
         MVC   DUB,OLDGRPC                                                      
         B     UPDATGRP                                                         
*                                                                               
UPDATGRP OC    DUB,DUB             NO GROUP SO IGNORE                           
         BZ    XIT1                                                             
*                                                                               
         LA    R2,IO2                                                           
         USING SAAGREC,R2                                                       
         XC    0(L'SAAGKEY,R2),0(R2)                                            
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGYTAG                                                   
         MVC   SAAGAGR,DUB                                                      
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),CTFILE,(R2),(R2)                    
         CLI   8(R1),X'10'                                                      
         BE    XIT1                                                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAPCTELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                NOT FOUND                                    
         L     R5,12(R1)                                                        
*                                                                               
         USING SAPCTD,R5                                                        
         SR    R1,R1               UPDATE COUNT                                 
         ICM   R1,3,SAPCTVAL                                                    
*                                                                               
         CLI   BYTE,C'I'           INC                                          
         BNE   *+8                                                              
         AHI   R1,1                                                             
*                                                                               
         CLI   BYTE,C'D'           DEC                                          
         BNE   *+8                                                              
         SHI   R1,1                                                             
*                                                                               
         LTR   R1,R1               TEST NEG                                     
         BP    *+6                                                              
         SR    R1,R1                                                            
         STCM  R1,3,SAPCTVAL       SAVE NEW COUNT                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',DMWRITE),CTFILE,(R2),(R2)                   
         ORG   *-2                                                              
         CLI   WRITE,C'N'                                                       
         BE    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
* READ GROUP FROM TAB AT R3 AND SET NUM IN TERMGRP         *                    
************************************************************                    
         SPACE 1                                                                
GETGRP   NTR1                                                                   
*                                                                               
GETG110  XC    TERMGRP,TERMGRP                                                  
         CLC   AGYTGRP,SPACES                                                   
         BE    GETG200                                                          
         OC    AGYTGRP,AGYTGRP                                                  
         BZ    GETG200                                                          
*                                                                               
         LA    R2,IO                                                            
         USING SAAGREC,R2                                                       
         XC    0(L'SAAGKEY,R2),0(R2)                                            
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGYTAG                                                   
         MVC   SAAGAGR,AGYTGRP                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R2),(R2)                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,PARM,(C'G',CTFILE),('SAAGNELQ',(R2)),0,0                  
         CLI   12(R1),0                                                         
         BNE   GETG190             NOT FOUND                                    
         L     R5,12(R1)                                                        
         B     GETG150                                                          
*                                                                               
         USING SAAGND,R5                                                        
GETG150  MVC   TERMGRP,SAAGNNUM    SAVE NUMBER                                  
         B     GETG200                                                          
*                                                                               
GETG190  DC    H'0'                GROUP NOT FOUND                              
*                                                                               
GETG200  B     XIT1                                                             
*                                                                               
         EJECT                                                                  
************************************************************                    
* SENDF A WARNING EMAIL AT 7 DAYS TO EXPIRY                *                    
************************************************************                    
         SPACE 1                                                                
EMAILWRN NTR1                                                                   
         MVC   TO,PLEMAIL                                                       
         CLC   TO,SPACES                                                        
         JE    XIT1                                                             
         CLI   EMAILFLG,C'N'                                                    
         JE    XIT1                                                             
         CLI   EMAILFLG,C'R'                                                    
         JNE   *+10                                                             
         MVC   TO,=CL60'RMORSE@MEDIAOCEAN.COM:'                                 
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         GOTO1 VSMTP,DMCB,('SMTPAPRS',TO),(L'SUBC,SUBC)                         
*                                                                               
         MVC   PLIN,SPACES                                                      
         MVC   PLIN+0(25),PLPNAME                                               
         MVI   PLIN+25,C'<'                                                     
         MVC   PLIN+27(8),PLPERSON                                              
         MVI   PLIN+35,C'>'                                                     
         GOTO1 VSQUASH,DMCB,PLIN,80                                             
         GOTO1 VSMTP,DMCB,('SMTPAPTL',PLIN)                                     
         GOTO1 VSMTP,DMCB,('SMTPAPTL',SPACES)                                   
*                                                                               
         MVC   PLIN,LINE1                                                       
         MVC   PLIN+3(2),HALF                                                   
         EDIT  (B1,AGYTDAYS),(2,PLIN+63)                                        
         GOTO1 VSQUASH,DMCB,PLIN,80                                             
         GOTO1 VSMTP,DMCB,('SMTPAPTL',PLIN)                                     
*                                                                               
         MVC   PLIN,LINE2                                                       
         MVC   PLIN+53(2),HALF                                                  
         GOTO1 VSQUASH,DMCB,PLIN,80                                             
         GOTO1 VSMTP,DMCB,('SMTPAPTL',PLIN)                                     
         GOTO1 VSMTP,DMCB,('SMTPAPTL',LINE3)                                    
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPAPTL',SPACES)                                   
         GOTO1 VSMTP,DMCB,('SMTPAPTL',LINE4)                                    
*                                                                               
         GOTO1 VSMTP,DMCB,('SMTPASND',0)                                        
         GOTO1 VSMTP,DMCB,('SMTPAEND',0)                                        
         J     XIT1                                                             
*                                                                               
SUBC     DC    CL60'Mediaocean account expiry'                                  
LINE1    DC    CL50'In xx days your Mediaocean account will have been '         
LINE1A   DC    CL30'inactive for yy days.         '                             
LINE2    DC    CL50'Please log in to a Mediaocean system within the ne'         
LINE2A   DC    CL30'xt nn days                    '                             
LINE3    DC    CL80'to keep your account active '                               
LINE4    DC    CL80'Thank you. '                                                
*                                                                               
         EJECT                                                                  
************************************************************                    
* PUT REPORT LINE TO PQ REPORT (OPEN IF NOT ALREADY)       *                    
************************************************************                    
         SPACE 1                                                                
PQPUT    NTR                                                                    
         CLI   PQOP,C'Y'           OPEN YET                                     
         JE    PQPRINT                                                          
*                                                                               
         LA    R4,PLINE            INITIALISE OPEN PRINT LINE                   
         USING PQPLD,R4                                                         
         XC    PLINE,PLINE                                                      
         MVI   QLEXTRA,X'FF'       SET NEW STYLE CALL                           
*                                                                               
         MVC   QLSUBID,=C'ATR'     TER,NNNN                                     
         MVC   QLSRCID,AGYTPQID    PRINCIPLE ID                                 
         MVC   QLRETNL,=X'0018'    24 HRS                                       
         MVC   QLRETND,=X'000C'    12 HRS                                       
         MVI   QLCLASS,C'S'        CLASS S                                      
         MVC   QLDESC,=CL11'Terminated '                                        
         MVI   QLLINET,QLLTFL+QLLTCC                                            
         MVI   QLLINEW,80                                                       
         MVI   QLSTAT,QLSTAC                                                    
         MVC   QLPAGES,=X'0001'                                                 
*                                                                               
PQOPEN   GOTO1 VDATAMGR,DMCB,DMOPEN,PRTQUE,0,PLINE,APQBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PQOP,C'Y'                                                        
         ZAP   LINES,=P'1'                                                      
         BRAS  RE,PQPRINTT                                                      
*                                                                               
PQPRINT  MVC   PLIN(80),SPACES                                                  
         MVC   PLIN+1(79),P                                                     
         GOTO1 VDATAMGR,DMCB,DMPRINT,PRTQUE,0,PLIN,APQBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    LINES,=P'1'                                                      
         CP    LINES,=P'50'                                                     
         BL    XIT1                                                             
         BRAS  RE,PQPRINTT                                                      
         B     XIT1                                                             
*                                                                               
PQPRINTT NTR1                                                                   
         MVI   PLIN,C'i'                                                        
         GOTO1 VDATAMGR,DMCB,DMPRINT,PRTQUE,0,PLIN,APQBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   LINES,=P'3'                                                      
         MVC   PLIN,SPACES                                                      
*                                                                               
         MVC   PLIN+1(79),HEDR1                                                 
         GOTO1 VDATAMGR,DMCB,DMPRINT,PRTQUE,0,PLIN,APQBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PLIN+1(79),HEDR1L                                                
         GOTO1 VDATAMGR,DMCB,DMPRINT,PRTQUE,0,PLIN,APQBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PLIN+1(79),HEDR                                                  
         GOTO1 VDATAMGR,DMCB,DMPRINT,PRTQUE,0,PLIN,APQBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PLIN+1(79),HEDRL                                                 
         GOTO1 VDATAMGR,DMCB,DMPRINT,PRTQUE,0,PLIN,APQBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT1                                                             
*                                                                               
PQCLOSE  NTR1                                                                   
         CLI   PQOP,C'Y'                                                        
         BNE   XIT1                                                             
         GOTO1 VDATAMGR,DMCB,DMCLOSE,PRTQUE,0,PLINE,APQBUFF                     
         MVI   PQOP,C'N'                                                        
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
* TIME FROM TU IN FULL TO HH:MM IN WORK                    *                    
************************************************************                    
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(5),=C'00:00'                                               
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,TUHOUR                                                        
         EDIT  (RF),(2,WORK1),FILL=0       HRS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUMINUTE                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0     MINS                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
* LITERALS AND CONSTANTS                                   *                    
************************************************************                    
         SPACE 1                                                                
HEDR1    DC   C' Run on -- --- --       Time 00:00        '                     
         DC   C'  Terminate unused Person Ids         '                         
HEDR1L   DC   C'                                          '                     
         DC   C' -----------------------------        '                         
HEDR     DC   C' AG  PersonId  Full name                  '                     
         DC   C'Old Group  Days   Last used New Group '                         
HEDRL    DC   C' --  --------  -------------------------  '                     
         DC   C'---------  -----  --------- --------- '                         
*                                                                               
TUHOUR   DC    F'138240000'        60*60*38400                                  
TUMINUTE DC    F'2304000'          60*38400                                     
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
VCARDS   DC    V(CARDS)                                                         
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VHELLO   DC    V(HELLO)                                                         
VDATCON  DC    V(DATCON)                                                        
VADDAY   DC    V(ADDAY)                                                         
VPERVERT DC    V(PERVERT)                                                       
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VREMOTEC DC    V(REMOTEC)                                                       
VSMTP    DC    V(SMTP)                                                          
VSQUASH  DC    V(SQUASHER)                                                      
         SPACE 1                                                                
DMOPEN   DC    C'OPEN   '                                                       
DMCLS    DC    C'DMCLSE '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMWRITE  DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
DMPRINT  DC    C'DMPRINT'                                                       
DMCLOSE  DC    C'CLOSE  '                                                       
CLOSE    DC    C'CLOSE  '                                                       
PRTQUE   DC    C'PRTQUE '                                                       
JESMAIL  DC    CL8'JESMAIL'                                                     
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
CTRCVR   DC    C'CTRCVR '                                                       
*                                                                               
OPENLST  DC    C'UCTFILE '                                                      
         DC    C'UCTRCVR '                                                      
         DC    C'X'                                                             
*                                                                               
FFS      DC    X'FFFF'                                                          
         EJECT                                                                  
************************************************************                    
* AGENCY TABLE AND DSECTS                                  *                    
************************************************************                    
         SPACE 1                                                                
*                                                                               
*        DC    CL2'AG',XL1'00',CL8'GROUP   ',X'PQID'                            
*                                                                               
AGYTAB   DC    1500XL(AGYTLEN)'00'                                              
         DC    XL13'FF'                                                         
*                                                                               
         DS    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    1024X'00'                                                        
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'140008' FULL RECOVERY                            
         ORG                                                                    
*                                                                               
AGYTABX  EQU   *                                                                
*                                                                               
AGYTABD  DSECT                                                                  
AGYTAG   DS    CL2                                                              
AGYTDAYS DS    XL1                                                              
AGYTGRP  DS    CL8                                                              
AGYTPQID DS    XL2                                                              
AGYTPWRN DS    XL1                                                              
         DS    XL2                                                              
AGYTLEN  EQU   *-AGYTABD                                                        
*                                                                               
PLINED   DSECT                                                                  
         DS   CL1                                                               
PLAGY    DS   CL2                                                               
         DS   CL2                                                               
PLPERSON DS   CL8                                                               
         DS   CL2                                                               
PLPNAME  DS   CL25                                                              
         DS   CL2                                                               
PLOLDGRP DS   CL9                                                               
         DS   CL2                                                               
PLDAYS   DS   CL5                                                               
         DS   CL2                                                               
PLLASTUS DS   CL9                                                               
         DS   CL1                                                               
PLNEWGRP DS   CL8                                                               
         DS   CL1                                                               
PLWARN   DS   CL4                                                               
         DS   CL1                                                               
PLEMAIL  DS   CL60                                                              
*                                                                               
         EJECT                                                                  
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
WRITE    DS    X                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
SAVERE   DS    F                                                                
*                                                                               
CUTOFF   DS    H                                                                
CUTOFFW  DS    H                                                                
*                                                                               
EMAILFLG DS    X                                                                
         DS    X                                                                
*                                                                               
EFFDATEC DS    XL2                                                              
EFFDATE  DS    CL3                                                              
LASTUSE  DS    CL6                                                              
TODAY    DS    CL6                                                              
TODAYC   DS    XL2                                                              
TODAY2   DS    XL2                                                              
YESTERDC DS    XL2                                                              
TODAY3   DS    XL3                                                              
TODAYA   DS    CL8                 MMMDD/YY                                     
TERMGRP  DS    XL2                                                              
WORK     DS    XL256                                                            
WORK1    DS    XL32                                                             
CARD     DS    CL80                                                             
OLDGRPN  DS    XL2                                                              
OLDGRPC  DS    CL8                                                              
NEWGRPN  DS    XL2                                                              
NEWGRPC  DS    CL8                                                              
TO       DS    CL60                                                             
PLIN     DS    CL80                                                             
PLINE    DS    CL255                                                            
PQOP     DS    C                                                                
LINES    DS    PL3                                                              
         SPACE 1                                                                
APQBUFF  DS    A                                                                
IOL      DS    F                                                                
IO       DS    2000X                                                            
IOL2     DS    F                                                                
IO2      DS    2000X                                                            
PQBUFF   DS    14336C                                                           
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'154CTPIDTRM  01/17/19'                                      
         END                                                                    
