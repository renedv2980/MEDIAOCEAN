*          DATA SET BUFIL43    AT LEVEL 045 AS OF 08/14/02                      
*PHASE T50243A                                                                  
*INCLUDE MEDGET                                                                 
         TITLE 'T50243 - BUDGET EXTRACT - NETPAK'                               
T50243   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         NMOD1 0,T50243,RR=R8                                                   
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T50243+4096,RA                                                   
         ST    R8,RELO                                                          
         SPACE 1                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         SPACE 1                                                                
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         L     R7,=A(NETBAREA)                                                  
         USING NEBLOCKD,R7                                                      
         LR    RE,R7               CLEAR NETBLOCK AREA                          
         LA    RF,NBBLKEND-NETBLOCK+1                                           
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE 1                                                                
         L     R0,VADUMMY                                                       
         ST    R0,NEXTADDR         SET NEXT AVAILABLE CORE ADDRESS              
         SPACE 1                                                                
         L     R3,ATWA                                                          
         USING T502FFD,R3                                                       
         SPACE 1                                                                
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         MVC   CLPACK,TCLPACK                                                   
         DROP  RE                                                               
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000A27'     PICK UP A(NETIO)                   
         MVC   NETIO,DMCB                                                       
         SPACE 1                                                                
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 TWAVBUFF,DMCB,=C'SET',BUFFBUFF                                   
         SPACE 1                                                                
         XC    LASTKEY,LASTKEY                                                  
         LA    R1,SVDTYPES                                                      
         USING SVDTD,R1                                                         
         LA    R0,MAXDTYP                                                       
         XC    DATAINDS,DATAINDS   RESET EXTRACT SWITCH                         
         SR    R4,R4               R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(TABLE OF EXTRACT TYPES)                 
         SPACE 1                                                                
NE2      CLI   SVDTEX,0            TEST EOT                                     
         BE    NE6                                                              
         LA    RE,EXTTBL           RE=A(VALID DATA TYPE TABLE)                  
         LA    RF,EXTYPES          RF=N'VALID DATA TYPES                        
NE3      CLC   SVDTEX,0(RE)        TEST IF VALID FOR NET EXTRACT                
         BE    NE4                 YES                                          
         LA    RE,L'EXTTBL(RE)                                                  
         BCT   RF,NE3                                                           
         B     NE5                                                              
*                                                                               
NE4      OC    DATAINDS,1(RE)      UPDATE CUMULATIVE MASK                       
         MVC   0(1,R5),0(RE)       SAVE EXTRACT TYPE                            
         LA    R4,1(R4)            INCREMENT EXTRACT TYPE COUNT                 
         LA    R5,1(R5)            BUMP TABLE POINTER                           
*                                                                               
NE5      LA    R1,SVDTL(R1)        NEXT DATA TYPE                               
         BCT   R0,NE2                                                           
         DROP  R1                                                               
         EJECT                                                                  
*              DEAL WITH AGENCY/MEDIA/CLIENT                                    
         SPACE 3                                                                
NE6      LTR   R4,R4               TEST ANYTHING TO EXTRACT                     
         BZ    EXIT                                                             
         STC   R4,NEXTYPS                                                       
         L     R2,ARULDATA         GET ADDRESS OF FIRST RULE                    
         USING QRD,R2                                                           
         SPACE 1                                                                
         GOTO1 =V(MEDGET),DMCB,(QRMED,QRAGYC),DATAMGR,WORK,RR=RELO              
         CLI   DMCB+8,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   QBAGYMD,WORK        SAVE AGY/MEDIA CODE                          
         SPACE 1                                                                
         GOTO1 CLPACK,DMCB,QRCLT,QBCLT                                          
         MVC   SAVAMC,QBAGYMD      SAVE AM/CLT                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),QBAGYMD                                                 
         BAS   RE,SPHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADCLT                                                           
         MVC   AIO,AIO3                                                         
         BAS   RE,SPGET                                                         
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         L     R6,AIO3                                                          
         MVC   SVCLNAME,CNAME-CLTHDRD(R6)                                       
         SPACE 1                                                                
NE8      XC    KEY,KEY             CHECK THAT POL IS DEFINED                    
         MVC   KEY+1(3),QBAGYMD                                                 
         MVC   KEY+4(3),=C'POL'                                                 
         BAS   RE,SPHIGH                                                        
         CLC   KEY(7),KEYSAVE      FIND PRDHDR                                  
         BNE   BADPOL                                                           
         BAS   RE,PGSET            BUILD PRODUCT GROUP BUFFER                   
         BAS   RE,ESTSET           AND VALID ESTIMATES FOR POL                  
         B     NE10                                                             
         EJECT                                                                  
*              BUILD PRODUCT GROUP BUFFER                                       
         SPACE 3                                                                
PGSET    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PKEY,R4                                                          
         MVC   PKEYAM(3),QBAGYMD   AM/CLT                                       
         L     R6,AIO3                                                          
         LA    R6,CLIST-CLTHDRD(R6)                                             
         SPACE 1                                                                
PGSET2   LA    R4,KEY                                                           
         MVC   PKEYPRD,0(R6)       PICK UP PRODUCT CODE                         
         BAS   RE,SPHIGH                                                        
         BAS   RE,SPGET                                                         
         L     R4,AIO                                                           
         LH    R1,PCODE            PICK UP PRODUCT CODE                         
         BCTR  R1,0                                                             
         MH    R1,=H'12'                                                        
         A     R1,=A(PGBUFF)                                                    
         A     R1,RELO                                                          
         MVC   0(3,R1),PKEYPRD                                                  
         MVC   3(3,R1),PGRP1                                                    
         MVC   6(3,R1),PGRP2                                                    
         MVC   9(3,R1),PGRP3                                                    
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    PGSET2                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              BUILD LIST OF ESTIMATES FOR POL                                  
         SPACE 3                                                                
ESTSET   NTR1                                                                   
         LA    R0,12               CLEAR ESTTAB (3K)                            
         L     R1,AESTTAB                                                       
         XC    0(256,R1),0(R1)                                                  
         LA    R1,256(R1)                                                       
         BCT   R0,*-10                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),QBAGYMD                                                 
         MVC   KEY+4(3),=C'POL'                                                 
         SPACE 1                                                                
         BAS   RE,SPHIGH                                                        
         CLC   KEY(7),KEYSAVE      FIND PRDHDR                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
ESET2    MVC   KEY+8(5),XFF                                                     
         BAS   RE,SPHIGH                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   XIT                                                              
         CLI   KEY+7,0             TEST EST NUM PRESENT                         
         BE    ESET2                                                            
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   ESET2                                                            
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         BAS   RE,SPGET                                                         
         SPACE 1                                                                
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         SPACE 1                                                                
         TM    DATAIND1,EXBAADVB+EXBADATB+EXBAINVB  ACTUAL BILL                 
         BNZ   ESET3               YES-SKIP ESTIMATE DATE CHECK                 
*                                                                               
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BNE   *+12                                                             
         TM    DATAIND,MBILMASK    TEST BDATE + BINV REQUESTED                  
         BNZ   ESET3               YES-SKIP OVERLAP DATE TEST                   
*                                                                               
         CLC   ESTART,SVEXTEND     EST START AFTER PLAN END                     
         BH    ESET2                                                            
         CLC   EEND,SVEXTST        EST END BEFORE PLAN START                    
         BL    ESET2                                                            
         SPACE 1                                                                
*                                  ADD ESTIMATE TO TABLE                        
         SPACE 1                                                                
ESET3    ZIC   RE,KEY+7            GET EST NUM                                  
         MH    RE,=H'3'            X 3                                          
         A     RE,AESTTAB                                                       
         MVC   0(3,RE),EPROF       SAVE FILTER VALUES                           
         OC    0(3,RE),0(RE)                                                    
         BNZ   *+10                                                             
         MVC   0(3,RE),=C'   '                                                  
         B     ESET2                                                            
         EJECT                                                                  
*              PRODUCT GROUP AND PRODUCT VALIDATION                             
         SPACE 3                                                                
*                                  EACH RULE HAS AM/CLT SET                     
         SPACE 1                                                                
NE10     MVC   QBAGYMD(3),SAVAMC   RESTORE AM/CLT                               
         CLI   QRPGR,C' '          TEST PRDGRP REQUEST                          
         BNH   NE20                NO                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(3),QBAGYMD    A-M/CLT                                      
         MVC   KEY+5(1),QRPGR                                                   
         MVC   FULL(3),QRPRD                                                    
         MVI   FULL+3,C'0'                                                      
         PACK  DUB,FULL(5)                                                      
         MVC   KEY+6(2),DUB+5                                                   
         BAS   RE,SPHIGH                                                        
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BADPGR                                                           
         B     NE50                                                             
         SPACE 3                                                                
*              PRODUCT VALIDATION                                               
         SPACE 3                                                                
NE20     CLI   QRPRD,X'41'                                                      
         BL    NE50                                                             
         CLC   QRPRD,=C'ALL'       TEST FOR PRODUCT=ALL                         
         BE    NE50                                                             
         L     R6,AIO3                                                          
         LA    R6,CLIST-CLTHDRD(R6)                                             
         SPACE 1                                                                
NE22     CLC   QRPRD,0(R6)                                                      
         BE    NE50                                                             
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNL   NE22                                                             
         B     BADPRD                                                           
         EJECT                                                                  
*              NOW SET UP TO HANDLE THE UNITS                                   
         SPACE 3                                                                
NE50     ICM   R2,15,QRNEXT        GET NEXT RULE                                
         BNZ   NE10                AND GO BACK TO VALIDATE                      
         L     R2,ARULDATA                                                      
*                                  AS ABOVE CLOBBERS IO3....                    
         XC    KEY,KEY             REREAD CLIENT RECORD                         
         MVC   KEY+1(3),QBAGYMD                                                 
         BAS   RE,SPHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADCLT                                                           
         MVC   AIO,AIO3                                                         
         BAS   RE,SPGET                                                         
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         CLI   DATAIND,0           TEST FOR ANY UNIT RELATED EXTRACT            
         BE    NE80                NO-TRY GOALS                                 
         SPACE 1                                                                
NE60     MVC   NBSELAGY,QRAGYC                                                  
         L     R2,ARULDATA                                                      
         USING QRD,R2                                                           
         MVC   NBSELMED,QRMED                                                   
         MVC   NBSELCL2,QBCLT      CLIENT                                       
         MVC   NBSELSTR,SVEXTST    START                                        
         MVC   NBSELEND,SVEXTEND   AND END DATES                                
*                                                                               
         TM    DATAIND,MBILMASK    TEST MULTI-YEAR BILL TYPES                   
         BZ    NE61                NONE OF THEM                                 
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BNE   NE61                NO                                           
*                                                                               
NE60A    XC    NBSELSTR,NBSELSTR   YES-WHOLE FILE MUST BE READ                  
         XC    NBSELEND,NBSELEND                                                
         SPACE 1                                                                
NE61     MVI   NBDATA,C'U'         SELECT UNITS                                 
         MVI   NBSPLOPT,X'C0'      SPLIT OPTION                                 
         MVI   NBSEQ,C'D'          DATE SEQUENCE                                
         MVI   NBUSER+13,C'N'      FORCE BACK PRE-EMPTED UNITS                  
         L     R1,=A(UNITREC)                                                   
         ST    R1,NBAIO                                                         
         MVC   NBACOM,ACOMFACS                                                  
         SPACE 1                                                                
NE62     GOTO1 NETIO,DMCB,(R7)                                                  
         CLI   TRACOPT,C'Y'                                                     
         BNE   NE63                                                             
         AP    TRACOUNT,=P'1'                                                   
         CP    TRACOUNT,=P'400'                                                 
         BL    *+6                                                              
         DC    H'0'                                                             
         MVC   P(5),=C'MODE='                                                   
         EDIT  (1,NBMODE),(3,P+5),FILL=0                                        
         GOTO1 HEXOUT,DMCB,NBACTAM,P+10,20,=C'TOG'                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     NE63                                                             
         SPACE 1                                                                
TRACOPT  DC    C'N'                                                             
TRACOUNT DC    PL2'0'                                                           
         SPACE 1                                                                
NE63     CLI   NBMODE,NBPROCUN                                                  
         BE    NE64                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    NE80                                                             
         B     NE62                                                             
         SPACE 1                                                                
NE64     MVI   COSTSW,C'T'         BROWSE FOR TIME                              
         BAS   RE,NE70                                                          
         MVI   COSTSW,C'I'         AND THEN INTEGRATION                         
         BAS   RE,NE70                                                          
         MVI   COSTSW,C'S'         FINISH WITH SPECIAL CHARGES                  
         BAS   RE,NE70                                                          
         L     RE,NBAIO                                                         
         MVC   LASTKEY,0(RE)                                                    
         B     NE62                                                             
         SPACE 1                                                                
NE70     NTR1                                                                   
         L     R2,ARULDATA                                                      
         XC    BUFFRULE,BUFFRULE                                                
         SPACE 1                                                                
NE72     BAS   RE,PROCUNIT         EXTRACT UNIT DATA                            
         OC    BUFFRULE,BUFFRULE   DID RULE USE THE RECORD                      
         BZ    NE74                NO                                           
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES                                          
         ICM   R2,15,QRJUMP        PICK UP JUMP POINT                           
         BNZ   NE72                SEE IF DATA FITS ANOTHER RULE                
         B     XIT                 NOWHERE TO GO SO EXIT                        
*                                                                               
NE74     ICM   R2,15,QRNEXT        ELSE TRY ANOTHER RULE                        
         BNZ   NE72                IF ANY                                       
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS THE UNIT FOR THIS RULE                                   
         SPACE 3                                                                
*                                  IF IT PASSES, RETURN BUFFRULE                
PROCUNIT NTR1                                                                   
         MVC   FEST,NBACTEST       SET FILTER VALUE                             
         BAS   RE,FILTEST          ESTIMATE                                     
         BNE   XIT                                                              
         MVC   FDPT,NBACTDP        SET FILTER VALUE                             
         BAS   RE,FILTDPT          DAYPART                                      
         BNE   XIT                                                              
         MVC   FSLN,NBLEN          SET FILTER VALUE                             
         BAS   RE,FILTSL           'SPOT' LENGTH                                
         BNE   XIT                                                              
         BAS   RE,FILTNET          NETWORK                                      
         BNE   XIT                                                              
         BAS   RE,FILTSTY          STATION TYPE                                 
         BNE   XIT                                                              
         BAS   RE,FILTPRG          PROGRAM CODE                                 
         BNE   XIT                                                              
         BAS   RE,FILTPF           PACKAGE FILTERS                              
         BNE   XIT                                                              
         BAS   RE,FILTPACK         PACKAGES                                     
         BNE   XIT                                                              
         BAS   RE,FILTCOST         AND COST OPTION                              
         BNE   XIT                                                              
         ZIC   R1,NBSPLPRN                                                      
         BAS   RE,FILTPGR          FILTER ON PRODUCT GROUP                      
         BNE   PU3                                                              
         BAS   RE,FILTPROD         PRODUCT                                      
         BNE   PU3                                                              
         MVC   WORK(2),NBACTDAT                                                 
         BAS   RE,SETPERD          SET PERIOD (FILLS IN BUFFPER)                
         BNE   PU3                 UNIT NOT RUNNING IN PLAN PERIOD              
         ST    R2,BUFFRULE         PASSED THE TESTS SO RETURN A(RULE)           
         SR    R0,R0               CLEAR GROSS ORDERED                          
         SR    R1,R1               AND NET ORDERED TO ZERO                      
         TM    NBUNITST,X'42'      TEST MISSING OR PRE-EMPTED                   
         BNZ   PU2                 YES-ORDERED IS ZERO                          
*                                                                               
         CLI   COSTSW,C'T'         TEST TIME CALL                               
         BNE   PU1A                NO                                           
         TM    COSTBITS,X'80'      TEST ASSIGNED FIRST FILTER                   
         BNO   PU1                                                              
         ICM   R0,15,NBASSIGN      YES-PICK UP ASSIGNED                         
         BNZ   PU1                 IF PRESENT                                   
         TM    NBUNITST,X'08'      TEST ZERO ASSIGNED COST INPUT                
         BO    *+8                 YES                                          
         ICM   R0,15,NBACTUAL      USE THE ACTUAL IF NO ASSIGNED                
         SPACE 1                                                                
PU1      TM    COSTBITS,X'40'                                                   
         BNO   *+8                                                              
         L     R0,NBACTUAL         OR, OPTIONALLY, ACTUAL                       
*                                                                               
         LR    RF,R0               SET AMOUNT TO BE NETTED DOWN                 
         BAS   RE,GETNET                                                        
         LR    R1,RF               INITIALIZE NET ORDERED                       
         B     PU2                 POST ORDERED                                 
*                                                                               
PU1A     CLI   COSTSW,C'I'         TEST INTEGRATION CALL                        
         BNE   PU1B                                                             
         L     RF,NBINTEG                                                       
         AR    R0,RF               ADD IN INTEG. TO GROSS ORDERED               
         TM    NBPACKST,X'04'      TEST FOR NON-COMM INTEG                      
         BO    *+8                                                              
         BAS   RE,GETNET           NET DOWN THE INTEGRATION                     
         AR    R1,RF               UPDATE NET ORDERED                           
         B     PU2                                                              
*                                                                               
PU1B     CLI   COSTSW,C'S'         TEST SPECIAL CHARGES CALL                    
         BNE   PU3                                                              
         BAS   RE,SPEC             ADD UP THE SPECIAL CHARGE AMOUNTS            
         LM    R0,R1,DUB                                                        
         SPACE 1                                                                
PU2      CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         CVD   R1,DUB                                                           
         ZAP   BUFFNET,DUB                                                      
         LH    R0,NBACTUN          RETURN UNITS                                 
         CVD   R0,DUB                                                           
         ZAP   BUFFSPTS,DUB                                                     
         MVI   BUFFTYPE,EXORD                                                   
         TM    DATAIND,EXORDB      IF GROSS ORDERED WAS NEEDED,                 
         BNO   *+8                                                              
         BAS   RE,BUFFPUT          WRITE BUFFALO RECORD                         
         MVI   BUFFTYPE,EXORDN                                                  
         TM    DATAIND,EXORDNB                                                  
         BZ    *+14                                                             
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
         SPACE 2                                                                
*              CHECK FOR BILLING                                                
*                                                                               
PU3      TM    DATAIND,BILLEDB     TEST BILLING REQUIRED                        
         BZ    XIT                 NO-ALL DONE                                  
         L     R6,NBAIO            NEED TO BROWSE AROUND THE UNIT               
         USING NURECD,R6                                                        
*                                                                               
* 9/12/90 - MUST SEPARATE PASSES THROUGH BILLING CODE FOR EACH                  
*           PIGGYBACK PRODUCT - CODE BELOW IS NO-OPED                           
*                                                                               
*        CLC   NUKEY,LASTKEY       TEST UNIT HAS BEEN PASSED BEFORE             
*        BE    XIT                 YES-HAVE ALREADY POSTED BILLING              
         SPACE 1                                                                
         LA    R6,NUDATA                                                        
         USING NUBILD,R6                                                        
         SPACE 1                                                                
PU4      ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BE    XIT                                                              
         CLI   0(R6),X'10'         LOOKING FOR BILLING UNITS                    
         BNE   PU4                                                              
         TM    NUBILST,X'20'       TEST FOR UNBILLED ELEMENT                    
         BO    PU4                                                              
*                                                                               
         CLI   NBSPLPRN,0          TEST FOR SPLIT PRODUCT                       
         BE    *+14                NO                                           
         CLC   NBSPLPRN,NUBILPRD   TEST ITS THE SAME ONE AS BILLING             
         BNE   PU4                 NO                                           
*                                                                               
         ZIC   R1,NUBILPRD                                                      
         BAS   RE,FILTPGR          NOTE-PRODUCT ALLOCATION CAN                  
         BNE   PU4                 CHANGE AFTER BILLING                         
         BAS   RE,FILTPROD                                                      
         BNE   PU4                                                              
         CLI   NUBILTYP,C'T'       TIME BILLING ELEMENT                         
         BNE   PU6                                                              
         CLI   COSTSW,C'T'         TEST TIME BILLING CALL                       
         BE    PU10                                                             
         B     PU4                                                              
         SPACE 1                                                                
PU6      CLI   NUBILTYP,C'I'       TEST INTEGRATION BILLED                      
         BNE   PU8                 NO                                           
         CLI   COSTSW,C'I'         TEST INTEGRATION CALL                        
         BE    PU10                                                             
         B     PU4                 NO                                           
         SPACE 1                                                                
PU8      CLI   COSTSW,C'S'         TEST SPECIAL CHARGES CALL                    
         BNE   PU4                 NO                                           
         SPACE 1                                                                
PU10     TM    DATAIND,EXBADVB+EXBNADVB TEST BILLING BY ADV. MONTH              
         BZ    PU12                                                             
         MVC   WORK(2),NBACTDAT    SET DATE FROM UNIT                           
         BAS   RE,SETPERD                                                       
         BNE   PU12                NOT WITHIN PLAN PERIOD                       
         BAS   RE,SETBUFF                                                       
*                                                                               
PU11     TM    DATAIND,EXBADVB     TEST GROSS BILLING NEEDED                    
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBADV     (BILLING BY ADVERTISING MONTH)               
         BAS   RE,BUFFPUT          WRITE BUFFALO RECORD                         
*                                                                               
         TM    DATAIND,EXBNADVB    TEST NET BILLING REQUIRED                    
         BZ    PU12                                                             
         MVI   BUFFTYPE,EXBNADV                                                 
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
PU12     TM    DATAIND,EXBDATB+EXBNDATB TEST BILLING BY BILLING MONTH           
         BZ    PU14                                                             
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BE    *+18                YES                                          
         MVC   WORK(2),NBACTDAT    NO-FILTER ON ADV PERIOD                      
         BAS   RE,SETPERD                                                       
         BNE   PU14                                                             
         MVC   WORK(2),NUBILDAT    SET DATE FROM BILLING DATE                   
         BAS   RE,SETPERD                                                       
         BNE   PU14                BILLING NOT IN PLAN PERIOD                   
         BAS   RE,SETBUFF                                                       
*                                                                               
PU13     TM    DATAIND,EXBDATB     TEST GROSS BILLING                           
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBDATE    (BILLING BY BILLING MONTH)                   
         BAS   RE,BUFFPUT          WRITE BUFFALO RECORD                         
*                                                                               
         TM    DATAIND,EXBNDATB    TEST NET BILLING                             
         BZ    PU14                                                             
         MVI   BUFFTYPE,EXBNDATE                                                
         ZAP   BUFFGRS,BUFFNET                                                  
         BAS   RE,BUFFPUT                                                       
         SPACE 1                                                                
PU14     TM    DATAIND,EXBINVB+EXBNINVB TEST BINV REQUESTED                     
         BZ    PU15                                                             
*                                                                               
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BE    *+18                YES                                          
         MVC   WORK(2),NBACTDAT    CHECK UNIT DATE W/IN EXTRACT                 
         BAS   RE,SETPERD                                                       
         BNE   PU15                NO                                           
*                                                                               
         MVC   WORK(2),NUBILIDT    FIND MONTH OF INVOICE DATE                   
         BAS   RE,SETPERD                                                       
         BNE   PU15                                                             
         BAS   RE,SETBUFF                                                       
*                                                                               
         TM    DATAIND,EXBINVB     TEST GROSS BILLING REQUESTED                 
         BZ    *+12                                                             
         MVI   BUFFTYPE,EXBINV                                                  
         BAS   RE,BUFFPUT                                                       
*                                                                               
         TM    DATAIND,EXBNINVB    TEST NET BILLING REQUESTED                   
         BZ    PU15                                                             
         MVI   BUFFTYPE,EXBNINV                                                 
         ZAP   BUFFGRS,BUFFNET     SET GROSS=NET                                
         BAS   RE,BUFFPUT                                                       
*                                                                               
PU15     B     PU4                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE THE NET AMOUNT                                       
* AT ENTRY, RF=AMOUNT TO BE NETTED DOWN, ON EXIT, RF=NET AMOUNT                 
*                                                                               
GETNET   ST    RE,SAVERE                                                        
         M     RE,=F'8500'         X 85 PERCENT TO 2 DEC PLACES                 
         SLDA  RE,1                                                             
         D     RE,=F'10000'                                                     
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO BUILD BUFFALO RECORD                                           
*                                                                               
SETBUFF  ST    R2,BUFFRULE                                                      
         ICM   R0,15,NUBILGRS      PICK UP BILLING                              
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         ICM   R0,15,NUBILNET                                                   
         CVD   R0,DUB                                                           
         ZAP   BUFFNET,DUB                                                      
         LH    R0,NBACTUN          RETURN UNITS                                 
         CVD   R0,DUB                                                           
         ZAP   BUFFSPTS,DUB                                                     
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS THE SPECIAL CHARGES                                    
* ON EXIT, DUB+0(4)=ORDERED GROSS, DUB+4(4) = ORDERED NET                       
*                                                                               
SPEC     NTR1  ,                                                                
         XC    DUB,DUB                                                          
         L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         LA    R6,NUDATA           POINT AT FIRST ELEMENT                       
         USING NUSPRD,R6                                                        
         SR    R5,R5                                                            
*                                                                               
SPEC2    CLI   NUSPREL,0           TEST FOR EOR                                 
         BE    SPECX                                                            
         CLI   NUSPREL,X'03'       TEST FOR SPECIAL CHARGE ELEM                 
         BE    SPEC4                                                            
*                                                                               
SPEC3    IC    R5,NUSPRLEN                                                      
         AR    R6,R5                                                            
         B     SPEC2                                                            
*                                                                               
SPEC4    ICM   RF,15,NUSPRAMT      GET AMOUNT                                   
         CLI   NBPRD2,0            TEST FOR PIGGYBACK                           
         BE    SPEC6               NO                                           
         CLI   NBSPLPRN,0          TEST SPLIT PRODUCT SET                       
         BE    SPEC6               NO                                           
*                                                                               
         MVC   HALF,NBP1SHR        GET FIRST PRODUCT SHARE                      
         LH    R1,HALF                                                          
         CLI   NBSPLTYP,C'F'       TEST FIRST PRODUCT CALL                      
         BE    *+10                YES                                          
         LCR   R1,R1                                                            
         A     R1,=F'10000'        SUBTRACT FROM 100 PERCENT                    
*                                                                               
         MR    RE,R1               PERCENT*AMOUNT (6 DEC PLS.)                  
         SLDA  RE,1                                                             
         D     RE,=F'10000'        ROUNDED DIVIDE TO 2 DEC PLS.                 
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
*                                                                               
SPEC6    LR    R0,RF               SET ORDERED GROSS                            
         CLI   NUSPRCOM,C'C'       TEST COMMISSION APPLIED                      
         BNE   *+8                 NO                                           
         BAS   RE,GETNET           YES-NET DOWN THE GROSS                       
         LR    R1,RF                                                            
         A     R0,DUB+0                                                         
         A     R1,DUB+4                                                         
         STM   R0,R1,DUB           UPDATE TOTALS                                
         B     SPEC3                                                            
*                                                                               
SPECX    B     XIT                                                              
         EJECT                                                                  
* GOAL RECORD PROCESSING                                                        
*                                                                               
NE80     TM    DATAIND1,EXGOALB    TEST IF GOALS SB EXTRACTED                   
         BZ    NE100               NO                                           
         CLI   THISFIS,0           TEST FOR BROADCAST FISCAL YEAR               
         BNE   NE100               NO-SKIP THIS EXTRACT                         
         BAS   RE,BLDMON           BUILD MONTH TABLE FOR GOALS                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GOALRECD,R4                                                      
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM(3),SAVAMC    AGYMED/CLT                                   
*                                                                               
NE81     BAS   RE,SPHIGH                                                        
         B     NE84                                                             
*                                                                               
NE82     BAS   RE,SPSEQ                                                         
*                                                                               
NE84     CLC   GKEY(GKEYPRD-GKEY),KEYSAVE  TEST SAME A-M/CLT                    
         BNE   NE100               ALL DONE                                     
*                                                                               
         CLC   GKEYMKT,=H'7777'                                                 
         BE    NE85                                                             
         BL    *+14                                                             
         MVC   GKEYMKT(GLENGTH-GKEYMKT),XFF  FORCE NEXT PRODUCT                 
         B     NE81                                                             
         MVC   GKEYMKT,=H'7777'                                                 
         XC    GKEYEST(GLENGTH-GKEYEST),GKEYEST                                 
         B     NE81                                                             
*                                                                               
NE85     L     R2,ARULDATA                                                      
*                                                                               
NE86     ZIC   R1,GKEYPRD                                                       
         BAS   RE,FILTPGR                                                       
         BNE   NE87                NEXT RULE                                    
         BAS   RE,FILTPROD                                                      
         BNE   NE87                                                             
         MVC   FEST,GKEYEST                                                     
         BAS   RE,FILTEST                                                       
         BNE   NE87                                                             
         MVC   FDPT,GKEYDPT                                                     
         BAS   RE,FILTDPT                                                       
         BNE   NE87                                                             
         MVC   FSLN,GKEYSLN                                                     
         BAS   RE,FILTSL                                                        
         BE    NE88                                                             
*                                                                               
NE87     ICM   R2,15,QRNEXT        NEXT RULE                                    
         BNZ   NE86                                                             
         B     NE82                GET NEXT RECORD                              
*                                                                               
NE88     MVC   AIO,AIO1                                                         
         BAS   RE,SPGET                                                         
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
NE90     BAS   RE,PROCGOAL                                                      
         OC    BUFFRULE,BUFFRULE   TEST IF POSTED REC VS. RULE                  
         BZ    NE92                NO-TRY NEXT RULE                             
*                                                                               
         XC    BUFFRULE,BUFFRULE   YES                                          
         ICM   R2,15,QRJUMP        PICK UP JUMP POINT                           
         BNZ   NE86                                                             
         B     NE82                                                             
*                                                                               
NE92     ICM   R2,15,QRNEXT                                                     
         BNZ   NE86                                                             
         B     NE82                                                             
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS A GOAL RECORD                                          
*                                                                               
PROCGOAL NTR1                                                                   
         L     R6,AIO                                                           
         USING GOALRECD,R6                                                      
*                                                                               
PG4      LA    R6,GDELEM                                                        
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
*                                                                               
PG5      BAS   RE,NEXTEL                                                        
         BNE   EXIT                NO MORE GOAL ELEMENTS                        
         USING GLEMENT,R6                                                       
         CLC   GLWEEK,MONST        TEST W/IN PLAN                               
         BL    PG5                 NO                                           
         CLC   GLWEEK,MONEND                                                    
         BH    PG5                                                              
*                                                                               
PG6      LA    R5,MONTAB                                                        
         LA    R0,13                                                            
*                                                                               
PG7      CLI   0(R5),0             TEST FOR EOT                                 
         BE    PG5                 YES-NOT IN PLAN                              
         CLC   GLWEEK,2(R5)        TEST FOR FIT W/IN PERIOD                     
         BL    PG8                                                              
         CLC   GLWEEK,4(R5)                                                     
         BH    PG8                                                              
         B     PG10                FOUND A HIT                                  
*                                                                               
PG8      LA    R5,L'MONTAB(R5)                                                  
         BCT   R0,PG7                                                           
         B     PG5                                                              
*                                                                               
PG10     ST    R2,BUFFRULE                                                      
         MVI   BUFFTYPE,EXGOAL                                                  
         MVC   BUFFPER,0(R5)                                                    
         ZAP   BUFFSPTS,=P'1'                                                   
         ICM   R0,15,GLBUDGET                                                   
         CVD   R0,DUB                                                           
         ZAP   BUFFGRS,DUB                                                      
         ZAP   BUFFNET,BUFFGRS                                                  
         BAS   RE,BUFFPUT                                                       
         B     PG5                 LOOK AT NEXT ELEMENT                         
         DROP  R6                                                               
         SPACE 2                                                                
* ELEMENT SEARCH SUB-ROUTINE                                                    
*                                                                               
NEXTEL   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         BL    NEXTEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
         EJECT                                                                  
* PROCESS INVOICE RECORDS                                                       
*                                                                               
NE100    TM    DATAIND1,EXBAADVB+EXBADATB+EXBAINVB                              
         BZ    NE200                                                            
*                                                                               
         XC    LASTPRD,LASTPRD                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BILLRECD,R4                                                      
         MVC   BKEYAM(3),SAVAMC    AGENCY/MEDIA-CLIENT                          
*                                                                               
NE102    BAS   RE,SPHIGH                                                        
         B     NE104                                                            
*                                                                               
NE103    BAS   RE,SPSEQ                                                         
*                                                                               
NE104    CLC   BKEY(BKEYPRD-BKEY),KEYSAVE                                       
         BNE   NE200               DONE WITH CLIENT                             
         OC    BKEYYSRV(BLEN-BKEYYSRV),BKEYYSRV                                 
         BZ    NE103               NOT A BILL RECORD                            
*                                                                               
         CLC   BKEYPRD,LASTPRD     TEST FOR CHANGE IN PRODUCT                   
         BE    NE108               NO                                           
         MVC   LASTPRD,BKEYPRD                                                  
         L     R6,AIO3             FIND PRODUCT IN CLIST                        
         LA    R6,CLIST-CLTHDRD(R6)                                             
         LA    R0,220                                                           
*                                                                               
NE105    OC    0(4,R6),0(R6)       TEST FOR EOL                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   BKEYPRD,0(R6)                                                    
         BE    NE106                                                            
         LA    R6,4(R6)                                                         
         BCT   R0,NE105                                                         
         DC    H'0'                                                             
*                                                                               
NE106    MVC   FPRD,3(R6)                                                       
         ZIC   R1,FPRD             SEE IF PRODUCT PASSES ANY RULE               
         L     R2,ARULDATA                                                      
*                                                                               
NE107    BAS   RE,FILTPGR                                                       
         BNE   *+12                                                             
         BAS   RE,FILTPROD                                                      
         BE    NE108                                                            
         ICM   R2,15,QRNEXT                                                     
         BNZ   NE107               TRY ANOTHER RULE                             
*                                                                               
         MVC   BKEYEST(BLEN-BKEYEST),XFF                                        
         B     NE102               NEXT PRODUCT                                 
*                                                                               
NE108    L     R2,ARULDATA         SEE IF BILL PASSES RULES                     
*                                                                               
NE109    ZIC   R1,FPRD                                                          
         BAS   RE,FILTPGR                                                       
         BNE   NE110                                                            
         BAS   RE,FILTPROD                                                      
         BNE   NE110                                                            
         MVC   FEST,BKEYEST                                                     
         BAS   RE,FILTEST                                                       
         BE    NE112                                                            
*                                                                               
NE110    ICM   R2,15,QRNEXT                                                     
         BNZ   NE109                                                            
         B     NE103               NEXT RECORD                                  
*                                                                               
NE112    MVC   AIO,AIO1            GET BILL                                     
         BAS   RE,SPGET                                                         
         XC    BUFFRULE,BUFFRULE                                                
*                                                                               
         BAS   RE,PROCINV                                                       
         OC    BUFFRULE,BUFFRULE                                                
         BZ    NE110               REJECT BILL                                  
*                                                                               
         XC    BUFFRULE,BUFFRULE                                                
         ICM   R2,15,QRJUMP                                                     
         BNZ   NE109               TRY TO POST AGAIN                            
         B     NE103               NO-GET NEXT RECORD                           
         EJECT                                                                  
* SUB-ROUTINE TO PROCESS INVOICE RECORDS FOR ACTUAL BILL AMOUNTS                
*                                                                               
PROCINV  NTR1                                                                   
         L     R4,AIO                                                           
         USING BILLRECD,R4                                                      
*                                                                               
* BY ADVERTISING PERIOD                                                         
*                                                                               
PI1      TM    DATAIND1,EXBAADVB   TEST ACTUAL BILL BY ADV MONTH                
         BZ    PI4                 NO                                           
         CLC   BKEYYSRV(2),SVEXTSPE TEST WITHIN EXTRACT                         
         BL    PI4                                                              
         CLC   BKEYYSRV(2),SVEXTNPE                                             
         BH    PI4                                                              
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
PI2      CLC   BKEYYSRV(2),SVEXPER                                              
         BE    *+12                                                             
         LA    R6,SVEXL(R6)                                                     
         B     PI2                                                              
*                                                                               
         MVI   BUFFTYPE,EXBAADV                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
* BY INVOICE DATE                                                               
*                                                                               
PI4      TM    DATAIND1,EXBAINVB                                                
         BZ    PI6                                                              
*                                                                               
         GOTO1 INVPER,BQDATE                                                    
         BNE   PI6                 INVOICE DATE OUTSIDE EXTRACT                 
         MVI   BUFFTYPE,EXBAINV                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
* BY BILL DATE                                                                  
*                                                                               
PI6      TM    DATAIND1,EXBADATB   TEST ACTUAL BILL BY BILL DATE                
         BZ    PIX                 NO                                           
         GOTO1 INVPER,BDATE                                                     
         BNE   PIX                 BILLED OUTSIDE EXTRACT                       
         CLI   MULTIYR,C'Y'        TEST MULTI-YEAR PLAN                         
         BE    PI8                 YES                                          
         CLC   BKEYYSRV(2),SVEXTSPE NO-TEST ALSO IF ADV PERIOD                  
         BL    PIX                 IS WITHIN EXTRACT                            
         CLC   BKEYYSRV(2),SVEXTNPE                                             
         BH    PIX                                                              
*                                                                               
PI8      MVI   BUFFTYPE,EXBADAT                                                 
         BAS   RE,INVPUT                                                        
*                                                                               
PIX      B     EXIT                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO FIND THE PERIOD, AT ENTRY R1=A(DATE), ON EXIT                  
* CC=EQ IF WITHIN EXTRACT AND R6=A(DATE TABLE ENTRY), CC=NEQ IF                 
* OUTSIDE EXTRACT                                                               
*                                                                               
INVPER   ST    RE,SAVERE                                                        
         MVC   DUB(6),0(R1)                                                     
         CLC   DUB(6),SVEXTST      TEST IF WITHIN PLAN                          
         BL    INVPERN                                                          
         CLC   DUB(6),SVEXTEND                                                  
         BH    INVPERN                                                          
*                                                                               
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
         GOTO1 DATCON,PARAS,DUB,(3,FULL)                                        
*                                                                               
INVPER2  CLC   FULL(3),SVEXSTB     LOCATE THE PERIOD                            
         BL    INVPER4                                                          
         CLC   FULL(3),SVEXENDB                                                 
         BH    INVPER4                                                          
         B     INVPERY                                                          
*                                                                               
INVPER4  LA    R6,SVEXL(R6)        NEXT DATE ENTRY                              
         B     INVPER2                                                          
*                                                                               
INVPERY  CR    RB,RB               SET CC=EQ                                    
         B     INVPERX                                                          
*                                                                               
INVPERN  LTR   RB,RB                                                            
*                                                                               
INVPERX  L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
* SUB-ROUTINE TO PUT INVOICE DATA TO BUFFALO                                    
*                                                                               
INVPUT   NTR1                                                                   
         ST    R2,BUFFRULE                                                      
         MVC   BUFFPER,SVEXPER                                                  
*                                                                               
         ZAP   BUFFGRS,BACTP       GET ACTUAL BILLED FIGURE                     
         ZAP   BUFFNET,BUFFGRS                                                  
         ZAP   BUFFSPTS,=P'1'                                                   
         BAS   RE,BUFFPUT                                                       
         B     EXIT                                                             
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
* WRITE WORKER RECORDS *                                                        
         SPACE 1                                                                
NE200    L     R2,ARULDATA         R2=A(RULE ENTRY)                             
*                                                                               
NE202    ZIC   R4,NEXTYPS          R4=N'EXTRACT TYPES                           
         LA    R5,EXTYPS           R5=A(EXTRACT TYPE TABLE)                     
         XC    BUFFREC,BUFFREC                                                  
         ST    R2,BUFFRULE                                                      
         GOTO1 VPRTRULE,PARAS,(R2)                                              
         GOTO1 VDATAHD                                                          
*                                                                               
NE204    MVC   BUFFTYPE,0(R5)      SET EXTRACT TYPE                             
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
*                                                                               
NE206    MVC   BUFFPER,SVEXPER     SET PERIOD                                   
         BAS   RE,BUFFGET                                                       
         CLI   DMCB+8,0            TEST IF ITEM FOUND                           
         BE    NE208               YES                                          
         ZAP   BUFFGRS,=P'0'       NO-MANUFACTURE ZERO RECORD                   
         ZAP   BUFFNET,=P'0'                                                    
         ZAP   BUFFSPTS,=P'0'                                                   
*                                                                               
NE208    MVI   DMCB,BUPPUT                                                      
         TM    WHEN,X'18'          TEST OVERNIGHT                               
         BNZ   NE210               YES                                          
         MVI   DMCB,BUPADD                                                      
         TM    WHEN,X'20'          TEST SOON                                    
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NE210    GOTO1 VBUPPER                                                          
         LA    R6,SVEXL(R6)        NEXT PERIOD                                  
         OC    SVEXPER,SVEXPER     TEST FOR LAST PERIOD                         
         BNZ   NE206               NO                                           
*                                                                               
         LA    R5,1(R5)            NEXT EXTRACT TYPE                            
         BCT   R4,NE204                                                         
*                                                                               
         MVI   DMCB,BUPFINAL       FINAL CALL FOR OUTLINE                       
         GOTO1 VBUPPER                                                          
         ICM   R2,15,QRNEXT                                                     
         BNZ   NE202                                                            
         B     EXIT                NO MORE RULES                                
         DROP  R6                                                               
         EJECT                                                                  
*              FILTER ON PRODUCT OR PRODUCT GROUP                               
         SPACE 1                                                                
* AT ENTRY, R1 CONTAINS PRODUCT NUMBER TO FILTER ON                             
*                                                                               
FILTPROD NTR1                      PRODUCT FILTERING                            
         STC   R1,BYTE                                                          
         CLI   QRPGR,X'40'                                                      
         BH    YESXIT                                                           
         CLI   QRPRD,X'41'                                                      
         BL    YESXIT                                                           
         CLC   QRPRD,=C'POL'                                                    
         BE    YESXIT                                                           
         CLC   QRPRD,=C'ALL'                                                    
         BE    YESXIT                                                           
         L     R6,AIO3                                                          
         LA    R6,CLIST-CLTHDRD(R6)                                             
         SPACE 1                                                                
FILTPRD2 CLC   QRPRD,0(R6)         LOOK UP PRODUCT IN CLIENT HEADER             
         BE    FILTPRD4                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNL   FILTPRD2                                                         
         DC    H'0'                                                             
         SPACE 1                                                                
FILTPRD4 CLC   BYTE,3(R6)          CHECK FOR MATCH ON PRODUCT NO                
         BE    YESXIT                                                           
         B     NOXIT                                                            
         SPACE 1                                                                
* AT ENTRY, R1 CONTAINS PRODUCT NUMBER TO FILTER ON                             
*                                                                               
FILTPGR  NTR1                      PRODUCT GROUP FILTERING                      
         STC   R1,BYTE                                                          
         CLI   QRPGR,X'41'                                                      
         BL    YESXIT                                                           
         CLI   BYTE,0                                                           
         BE    NOXIT                                                            
         CLI   BYTE,X'FF'                                                       
         BE    NOXIT                                                            
         MVC   WORK(1),QRPGR       GET COMPRESSED PRODUCT GROUP                 
         MVC   FULL(3),QRPRD                                                    
         MVI   FULL+3,C'0'                                                      
         PACK  DUB,FULL(5)                                                      
         MVC   WORK+1(2),DUB+5     AND SAVE IN WORK                             
         BCTR  R1,0                DECREMENT PRODUCT NUMBER                     
         MH    R1,=H'12'                                                        
         A     R1,=A(PGBUFF)       DISPLACE INTO PRODUCT GROUP BUFFER           
         A     R1,RELO                                                          
         LA    R1,3(R1)                                                         
         LA    R0,3                                                             
         SPACE 1                                                                
FILTPGR2 CLC   0(3,R1),WORK        LOOK FOR A MATCH                             
         BE    YESXIT                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,FILTPGR2                                                      
         B     NOXIT                                                            
         EJECT                                                                  
*              FILTER ON ESTIMATE                                               
*  AT ENTRY, FEST CONTAINS ESTIMATE TO FILTER AGAINST                           
*                                                                               
FILTEST  NTR1                                                                   
         OC    QRESTDSP,QRESTDSP   TEST ESTIMATE LIST                           
         BZ    FLTEST6             NO - GO FILTER                               
         SR    R4,R4                                                            
         ICM   R4,3,QRESTDSP                                                    
         AR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
FLTEST2  CLC   FEST,1(R4)          TEST LESS THAN START                         
         BL    FLTEST4                                                          
         CLC   FEST,3(R4)          OR MORE THAN END                             
         BNH   FLTEST6                                                          
         SPACE 1                                                                
FLTEST4  LA    R4,4(R4)                                                         
         BCT   R0,FLTEST2                                                       
         B     NOXIT                                                            
         SPACE 1                                                                
FLTEST6  ZIC   RF,FEST             PICK UP ESTIMATE NUMBER                      
         MH    RF,=H'3'            AND ADDRESS INTO FILTER LIST                 
         A     RF,AESTTAB                                                       
         OC    0(3,RF),0(RF)       TEST ESTIMATE ACTIVE                         
         BZ    NOXIT               NO - DONE                                    
         SPACE 1                                                                
         LA    R0,3                                                             
         LA    RE,QRFLT                                                         
         SPACE 1                                                                
FLTEST8  CLI   0(RE),C'*'                                                       
         BE    FLTEST12                                                         
         CLI   0(RE),C' '                                                       
         BNH   FLTEST12                                                         
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    FLTEST10            YES                                          
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   NOXIT                                                            
         B     FLTEST12                                                         
         SPACE 1                                                                
FLTEST10 MVC   BYTE,0(RE)          NEGATIVE FILTER TEST                         
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          MUST NOT MATCH                               
         BE    NOXIT                                                            
         SPACE 1                                                                
FLTEST12 LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,FLTEST8                                                       
         B     YESXIT                                                           
         EJECT                                                                  
*              FILTER ON DAYPART AND SPOT LENGTH                                
         SPACE 3                                                                
FILTDPT  NTR1                                                                   
         OC    QRDPTDSP,QRDPTDSP   TEST DAYPART LIST                            
         BZ    YESXIT              NO - WE'RE THROUGH                           
         SR    R4,R4                                                            
         ICM   R4,3,QRDPTDSP                                                    
         AR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
FILTDP2  CLC   FDPT,0(R4)          TEST MATCH                                   
         BE    YESXIT                                                           
         LA    R4,1(R4)                                                         
         BCT   R0,FILTDP2                                                       
         B     NOXIT                                                            
         SPACE 1                                                                
FILTSL   NTR1                                                                   
         OC    QRSLNDSP,QRSLNDSP   TEST SPOT LENGTH LIST                        
         BZ    YESXIT              NO - WE'RE THROUGH                           
         SR    R4,R4                                                            
         ICM   R4,3,QRSLNDSP                                                    
         AR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
FILTSL2  CLC   FSLN,0(R4)        TEST MATCH                                     
         BE    YESXIT                                                           
         LA    R4,1(R4)                                                         
         BCT   R0,FILTSL2                                                       
         B     NOXIT                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              FILTER ON NETWORK AND PACKAGE                                    
         SPACE 3                                                                
FILTNET  NTR1                                                                   
         OC    QRNETDSP,QRNETDSP   TEST NETWORK LIST                            
         BZ    YESXIT              NO - WE'RE THROUGH                           
         SR    R4,R4                                                            
         ICM   R4,3,QRNETDSP                                                    
         AR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
FILTNET2 CLC   NBACTNET,0(R4)      TEST MATCH                                   
         BE    YESXIT                                                           
         LA    R4,4(R4)                                                         
         BCT   R0,FILTNET2                                                      
         B     NOXIT                                                            
         SPACE 1                                                                
FILTPRG  NTR1                                                                   
         OC    QRPCODSP,QRPCODSP   TEST PROGRAM CODE LIST                       
         BZ    YESXIT              NO - WE'RE THROUGH                           
         SR    R4,R4                                                            
         ICM   R4,3,QRPCODSP                                                    
         AR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
FILTPRG2 CLC   NBACTPRG,0(R4)      TEST MATCH                                   
         BE    YESXIT                                                           
         LA    R4,LRUPCO(R4)                                                    
         BCT   R0,FILTPRG2                                                      
         B     NOXIT                                                            
         SPACE 1                                                                
FILTPACK NTR1                                                                   
         OC    QRPKGDSP,QRPKGDSP   TEST PACKAGE LIST                            
         BZ    YESXIT              NO - WE'RE THROUGH                           
         SR    R4,R4                                                            
         ICM   R4,3,QRPKGDSP                                                    
         AR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         LA    R4,1(R4)                                                         
         SPACE 1                                                                
FILTPAK2 CLC   NBPACK,0(R4)      TEST MATCH                                     
         BE    YESXIT                                                           
         LA    R4,1(R4)                                                         
         BCT   R0,FILTPAK2                                                      
         B     NOXIT                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              FILTER ON COST SELECTION                                         
         SPACE 3                                                                
FILTCOST NTR1                                                                   
         MVI   COSTBITS,X'80'      DEFAULT IS ASSIGNED TIME ONLY                
         OC    QRCOSDSP,QRCOSDSP   CHECK COST INDICATORS                        
         BZ    FILTCST1                                                         
         SR    R4,R4                                                            
         ICM   R4,3,QRCOSDSP                                                    
         LA    R4,1(R2,R4)         DISP AND POINT PAST ENTRY COUNT              
         MVC   COSTBITS,0(R4)                                                   
         SPACE 1                                                                
FILTCST1 CLI   COSTSW,C'T'         ARE WE PROCESSING TIME                       
         BNE   FILTCST2                                                         
         TM    COSTBITS,X'80'+X'40'  TEST TIME ON THIS RULE                     
         BNZ   YESXIT                                                           
         B     NOXIT                                                            
         SPACE 1                                                                
FILTCST2 CLI   COSTSW,C'I'         TEST PROCESSING INTEGRATION                  
         BNE   FILTCST4                                                         
         TM    COSTBITS,X'20'      TEST INTEGRATION NEEDED FOR RULE             
         BO    YESXIT                                                           
         B     NOXIT                                                            
         SPACE 1                                                                
FILTCST4 TM    COSTBITS,X'10'      TEST SPECIAL CHARGES RULE                    
         BO    YESXIT                                                           
         B     NOXIT                                                            
         SPACE 2                                                                
*              FILTER ON PACKAGE FILTER VALUES                                  
         SPACE 1                                                                
FILTPF   NTR1                                                                   
         OC    QRPFIDSP,QRPFIDSP                                                
         BZ    YESXIT                                                           
         SR    R4,R4                                                            
         ICM   R4,3,QRPFIDSP                                                    
         AR    R4,R2                                                            
         ZIC   R0,0(R4)                                                         
         LA    R4,1(R4)            R4=A(PACKAGE FILTER RULE)                    
*                                                                               
FILTPF2  LA    R3,L'NBPKFILT       R3=L'PACKAGE FILTER                          
         LA    R5,NBPKFILT         R5=A(UNIT'S PACKAGE FILTER VALUES)           
         LR    R6,R4               R6=A(PACKAGE FILTER RULE)                    
*                                                                               
FILTPF4  CLI   0(R6),C' '          TEST FOR END-OF-RULE                         
         BE    YESXIT              YES-PASSED FILTER                            
         CLI   0(R6),C'*'          TEST FOR WILD CARD                           
         BE    FILTPF6             TAKE ANY VALUE IN THIS POSITION              
         TM    0(R6),X'40'         TEST FOR POSITIVE FILTER                     
         BO    FILTPF5             YES                                          
*                                                                               
         MVC   BYTE,0(R6)          GET VALUE                                    
         OI    BYTE,X'40'          RESTORE LOWER CASE BIT                       
         CLC   BYTE,0(R5)          TEST FOR MATCH                               
         BE    NOXIT               YES-SO BOUNCE IT                             
         B     FILTPF6                                                          
*                                                                               
FILTPF5  CLC   0(1,R5),0(R6)                                                    
         BNE   NOXIT                                                            
*                                                                               
FILTPF6  LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R3,FILTPF4                                                       
*                                                                               
         LA    R4,LRUPFI(R4)       ANOTHER RULE                                 
         BCT   R0,FILTPF2                                                       
         B     YESXIT                                                           
         SPACE 2                                                                
*              FILTER ON STATION TYPE                                           
         SPACE 1                                                                
FILTSTY  NTR1  ,                                                                
         OC    QRSTYDSP,QRSTYDSP                                                
         BZ    YESXIT                                                           
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,QRSTYDSP                                                    
         LA    R4,0(R2,R4)                                                      
         ZIC   R0,0(R4)            R0=LOOP COUNTER                              
         LA    R4,1(R4)            R1=A(VALUES)                                 
         TM    0(R4),X'40'         TEST FOR POSITIVE FILTERS                    
         BO    FILTSTY4                                                         
*                                                                               
FILTSTY2 MVC   BYTE,0(R4)          NEGATIVE FILTERS                             
         OI    BYTE,X'40'                                                       
         CLC   NBSTATYP,BYTE       TEST FOR MATCH                               
         BE    NOXIT                                                            
         LA    R4,1(R4)                                                         
         BCT   R0,FILTSTY2                                                      
         B     YESXIT                                                           
*                                                                               
FILTSTY4 CLC   NBSTATYP,0(R4)      POSITIVE FILTER                              
         BE    YESXIT                                                           
         LA    R4,1(R4)                                                         
         BCT   R0,FILTSTY4                                                      
         B     NOXIT                                                            
         EJECT                                                                  
*              SET PERIOD NUMBER - FILTER UNIT/BILL DATE AGAINST PLAN           
         SPACE 3                                                                
SETPERD  NTR1                                                                   
         CLC   WORK(2),SVEXTSTP    TEST AGAINST PLAN START                      
         BL    NOXIT                                                            
         CLC   WORK(2),SVEXTNDP    TEST AGAINST PLAN END                        
         BH    NOXIT                                                            
         SPACE 1                                                                
SETPERD1 LA    R4,SVEXTDTS                                                      
         USING SVEXD,R4                                                         
         LA    R0,14                                                            
         SPACE 1                                                                
SETPERD2 CLC   WORK(2),SVEXSTC     COMPARE AGAINST START                        
         BL    SETPERD4                                                         
         CLC   WORK(2),SVEXENDC    AND AGAINST END                              
         BH    SETPERD4                                                         
         MVC   BUFFPER,SVEXPER     MATCH - SO RETURN PERIOD                     
         B     YESXIT                                                           
         SPACE 1                                                                
SETPERD4 LA    R4,SVEXL(R4)                                                     
         BCT   R0,SETPERD2                                                      
         DC    H'0'                MUST HAVE A MATCH                            
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A TABLE OF MONDAYS OF START AND END WEEKS                
* IN EACH PERIOD                                                                
*                                                                               
BLDMON   NTR1                                                                   
         GOTO1 GETMON,DMCB,SVEXTST,MONST                                        
         GOTO1 GETMON,DMCB,SVEXTEND,MONEND                                      
         XC    MONTAB(13*L'MONTAB),MONTAB                                       
         LA    R6,SVEXTDTS                                                      
         USING SVEXD,R6                                                         
         LA    R5,MONTAB                                                        
*                                                                               
BLDMON2  OC    SVEXPER,SVEXPER                                                  
         BZ    BLDMONX                                                          
         GOTO1 DATCON,DMCB,(3,SVEXSTB),DUB                                      
         GOTO1 GETMON,DMCB,DUB,2(R5)                                            
         GOTO1 DATCON,DMCB,(3,SVEXENDB),DUB                                     
         GOTO1 GETMON,DMCB,DUB,4(R5)                                            
         MVC   0(2,R5),SVEXPER                                                  
         LA    R5,L'MONTAB(R5)                                                  
         LA    R6,SVEXL(R6)                                                     
         B     BLDMON2                                                          
*                                                                               
BLDMONX  B     XIT                                                              
         SPACE 2                                                                
GETMON   ST    RE,SAVERE                                                        
         L     R2,0(R1)            A(DATE)                                      
         L     R4,4(R1)            A(OUTPUT)                                    
         GOTO1 GETDAY,PARAS,0(R2),FULL                                          
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,PARAS,0(R2),WORK,(R0)                                      
         GOTO1 DATCON,PARAS,WORK,(2,0(R4))                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*              DATAMGR AIDS                                                     
         SPACE 3                                                                
SPHIGH   MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   DMFILE,=C'SPTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    SPDIR                                                            
         SPACE 1                                                                
         LA    R0,KEYSAVE          BUILD TRACE PARAMS                           
         ST    R0,TRIO1                                                         
         MVI   TRIO1,13                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,18                                                         
         B     SPDIR                                                            
         SPACE 1                                                                
SPSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         MVC   DMFILE,=C'SPTDIR'                                                
         TM    SVTRACE,X'80'                                                    
         BZ    SPDIR                                                            
         LA    R0,KEY                                                           
         ST    R0,TRIO1                                                         
         MVI   TRIO1,13                                                         
         LA    R0,KEY                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,18                                                         
         SPACE 1                                                                
SPDIR    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),DMFILE,KEY,KEY                    
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         B     SPTRACE                                                          
         SPACE 1                                                                
SPGET    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),=C'SPTFILE',          X        
               KEY+14,AIO,DMWORK                                                
         SPACE 1                                                                
         LR    RE,R0                                                            
         TM    SVTRACE,X'80'                                                    
         BZR   RE                                                               
         LA    R0,KEY+14                                                        
         ST    R0,TRIO1                                                         
         MVI   TRIO1,4                                                          
         L     R0,AIO                                                           
         ST    R0,TRIO2                                                         
         MVI   TRIO2,16                                                         
         B     SPTRACE                                                          
         EJECT                                                                  
*              TRACE FACILITY                                                   
         SPACE 3                                                                
SPTRACE  NTR1                                                                   
         SPACE 1                                                                
         SPACE 1                                                                
         L     RE,DMCB                                                          
         MVC   P(6),0(RE)                                                       
         L     RE,DMCB+4                                                        
         MVC   P+8(6),0(RE)                                                     
         SPACE 1                                                                
         LA    R4,P+16                                                          
         ZIC   R0,TRIO1                                                         
         GOTO1 HEXOUT,DMCB,TRIO1,(R4),(R0),=C'TOG'                              
         A     R4,DMCB+16                                                       
         LA    R4,2(R4)                                                         
         SPACE 1                                                                
         ZIC   R0,TRIO2            GET OUTPUT REC LEN                           
         GOTO1 HEXOUT,DMCB,TRIO2,(R4),(R0),=C'TOG'                              
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                RETURN TO CALLER                             
         SPACE 1                                                                
YESXIT   CR    RB,RB                                                            
         B     *+6                                                              
         SPACE 1                                                                
NOXIT    LTR   RB,RB                                                            
         SPACE 1                                                                
XIT      DS    0H                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
BUFFPUT  MVC   COMMAND(8),=CL8'BPUT'                                            
         B     GOBUFF                                                           
         SPACE 1                                                                
BUFFGET  MVC   COMMAND(8),=CL8'BGET'                                            
         SPACE 1                                                                
GOBUFF   LR    R0,RE               SAVE CALLING REG                             
         L     RF,TWAVBUFF                                                      
         GOTO1 (RF),DMCB,COMMAND+1,BUFFBUFF,BUFFREC,1                           
         LR    RE,R0               RESTORE CALLING REG                          
         SPACE 1                                                                
         TM    SVTRACE,X'40'       TEST TRACE BUFFALO                           
         BZR   RE                                                               
         SPACE 1                                                                
BUFFTRC  NTR1                                                                   
         MVC   WORK(16),DMCB       SAVE DMCB                                    
         SPACE 1                                                                
         MVC   P(5),COMMAND                                                     
         L     R4,BUFFRULE                                                      
         LA    R5,QRNODE-QRD(R4)                                                
         GOTO1 HEXOUT,DMCB,(R5),P+7,4,=C'TOG'                                   
         SPACE 1                                                                
         LA    R5,QRCODE-QRD(R4)                                                
         MVC   P+17(8),0(R5)                                                    
         SPACE 1                                                                
         GOTO1 HEXOUT,DMCB,BUFFREC,P+27,32,=C'TOG'                              
         SPACE 1                                                                
         GOTO1 HEXOUT,DMCB,WORK+8,P+92,1,=C'TOG'                                
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         MVC   DMCB(16),WORK       RESTORE DMCB                                 
         B     EXIT                                                             
         EJECT                                                                  
*              ERROR HANDLING                                                   
         SPACE 3                                                                
BADCLT   BAS   RE,BADRULE                                                       
         MVC   P(5),QRCLT                                                       
         MVC   P+10(19),=C'INVALID CLIENT CODE'                                 
         B     BADALL                                                           
         SPACE 1                                                                
BADPRD   BAS   RE,BADRULE                                                       
         MVC   P(3),QRPRD                                                       
         MVC   P+10(20),=C'INVALID PRODUCT CODE'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 1                                                                
BADPOL   BAS   RE,BADRULE                                                       
         MVC   P+10(32),=C'PRODUCT ''POL'' IS NOT DEFINED FOR'                  
         MVC   P+43(L'QRCLT),QRCLT                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE 1                                                                
BADEST   GOTO1 VPRTRULE,PARAS,(R2)                                              
         BAS   RE,BADRULE                                                       
         MVC   P+10(16),=C'INVALID ESTIMATE'                                    
         B     BADALL                                                           
         SPACE 1                                                                
BADPGR   BAS   RE,BADRULE                                                       
         MVC   P(4),QRPGR                                                       
         MVC   P+10(21),=C'INVALID PRODUCT GROUP'                               
         SPACE 1                                                                
BADALL   GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(3),=C'KEY'                                                     
         MVC   P+10(32),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY,P4,32,=C'SEP'                                    
         MVC   P2+10(32),P4                                                     
         MVC   P3+10(32),P4+32                                                  
         MVC   P4,SPACES                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(7),=C'KEYSAVE'                                                 
         MVC   P+10(32),KEYSAVE                                                 
         GOTO1 HEXOUT,DMCB,KEYSAVE,P4,32,=C'SEP'                                
         MVC   P2+10(32),P4                                                     
         MVC   P3+10(32),P4+32                                                  
         MVC   P4,SPACES                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE 1                                                                
BADRULE  LR    R0,RE                                                            
         GOTO1 VPRTRULE,PARAS,0                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*              STORAGE FOR NETWORK AND LTORG FOR PHASE                          
*                                                                               
* TABLE OF VALID EXTRACT TYPES FOR NETWORK EXTRACT                              
*                                                                               
EXTTBL   DS    0CL3                                                             
         DC    AL1(EXORD),AL1(EXORDB),AL1(0)                                    
         DC    AL1(EXBADV),AL1(EXBADVB),AL1(0)                                  
         DC    AL1(EXBDATE),AL1(EXBDATB),AL1(0)                                 
         DC    AL1(EXORDN),AL1(EXORDNB),AL1(0)                                  
         DC    AL1(EXBNADV),AL1(EXBNADVB),AL1(0)                                
         DC    AL1(EXBNDATE),AL1(EXBNDATB),AL1(0)                               
         DC    AL1(EXBINV),AL1(EXBINVB),AL1(0)                                  
         DC    AL1(EXBNINV),AL1(EXBNINVB),AL1(0)                                
         DC    AL1(EXGOAL),AL1(0),AL1(EXGOALB)                                  
         DC    AL1(EXBAADV),AL1(0),AL1(EXBAADVB)                                
         DC    AL1(EXBADAT),AL1(0),AL1(EXBADATB)                                
         DC    AL1(EXBAINV),AL1(0),AL1(EXBAINVB)                                
EXTYPES  EQU   (*-EXTTBL)/L'EXTTBL                                              
         SPACE 2                                                                
BUFFBUFF DS    A                                                                
ANYBUFF  DC    C'N'                                                             
XFF      DC    16X'FF'                                                          
RELO     DC    A(0)                                                             
NETIO    DC    A(0)                                                             
COSTSW   DS    CL1                                                              
COSTBITS DS    XL1                                                              
SAVAMC   DS    CL3                                                              
*                                                                               
DATAINDS DS    0XL2                                                             
DATAIND  DS    X                                                                
DATAIND1 DS    X                                                                
NEXTYPS  DS    X                                                                
EXTYPS   DS    CL(MAXDTYP)                                                      
*                                  PARAMETER VALUES FOR FILTER ROUTINES         
FPRD     DS    X                   PRODUCT NUMBER                               
FEST     DS    X                                                                
FDPT     DS    X                                                                
FSLN     DS    X                                                                
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
*                                                                               
LASTKEY  DS    CL(L'NUKEY)                                                      
LASTPRD  DS    CL3                                                              
*                                                                               
MONST    DS    XL2                                                              
MONEND   DS    XL2                                                              
MONTAB   DS    13CL6                                                            
         SPACE 1                                                                
* EQUATES                                                                       
*                                                                               
BILLEDB  EQU   EXBADVB+EXBNADVB+EXBDATB+EXBNDATB+EXBINVB+EXBNINVB               
MBILMASK EQU   EXBDATB+EXBNDATB+EXBINVB+EXBNINVB                                
ORDEREDB EQU   EXORDB+EXORDNB                                                   
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
         BUFF  LINES=250,ROWS=1,COLUMNS=3,FLAVOR=PACKED,               X        
               KEYLIST=(8,A)                                                    
         SPACE 1                                                                
NETBAREA DC    2000X'00'                                                        
         DS    0D                                                               
         DC    C'**UNIT**'                                                      
UNITREC  DS    3000X'00'                                                        
         DS    0D                                                               
         DC    C'*PGBUFF*'                                                      
PGBUFF   DC    2640X'00'           PRODUCT GROUP CODES HERE                     
         EJECT                                                                  
*              DSECT TO COVER NETBLOCK                                          
         SPACE 3                                                                
NEBLOCKD DSECT                                                                  
*              NETBLOCKD                                                        
*              BUFILWORKD                                                       
         PRINT OFF                                                              
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE BUFILWORKD                                                     
         PRINT ON                                                               
       ++INCLUDE BUEXTWORKD                                                     
         EJECT                                                                  
*              BUPPERD HERE                                                     
*              DDCOMFACS                                                        
*              SPGENCLT                                                         
*              SPGENPRD                                                         
*              SPGENEST                                                         
*              SPGENGOAL                                                        
*              NEGENUNIT                                                        
         PRINT OFF                                                              
       ++INCLUDE BUPPERD                                                        
       ++INCLUDE DDCOMFACS                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045BUFIL43   08/14/02'                                      
         END                                                                    
