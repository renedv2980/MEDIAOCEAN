*          DATA SET NENAV52    AT LEVEL 020 AS OF 02/26/20                      
*PHASE T31852B                                                                  
*                                                                               
*===============================================================*               
* SOME BASICS --                                                *               
* SVREASON IS 0 ON THE FIRST PASS, NON-ZERO SUBSEQUENTLY        *               
* SVRCVEL IS NON-ZERO ON FIRST PASS ONLY                        *               
* SVOLDRCV IS SAVE AREA FOR SVRCVEL BETWEEN PASSES              *               
*===============================================================*               
*                                                                               
T31852   TITLE 'NENAV52 - FRONTRUNNER - BUY UPLOAD OVERLAY'                     
T31852   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV52**,RA                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     R7,ATWA                                                          
         USING TWAD,R7                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
         MVC   TSACOM,ACOMFACS                                                  
         MVC   TSAREC,ANETBLK                                                   
*                                                                               
         CLC   SVRCVEL,=X'007C'     SEARCH PACKAGE?                             
         BE    SRCHPKGE                                                         
*                                                                               
         BAS   RE,TSARRES            RESTORE THE TSAR BLOCK                     
*                                                                               
* DID WE GO TO THE BUY ALREADY                                                  
*                                                                               
*SVGLOBSW MEANINGS  NULL=FIRST PASS GET FIRST TSAR RECORD CALL NBUY             
*                   C'B'=SEND DATA TO THE PC                                    
*                                                                               
         CLI   SVGLOBSW,C'B'                                                    
         BE    ROUT040                                                          
*                                                                               
*  SEND DATA TO THE BUY (FIRST TIME THROUGH)                                    
*                                                                               
         GOTO1 VALIMED                                                          
*                                                                               
         MVI   TSACTN,TSAGET        GET FIRST RECORD FROM TSAR                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'0'                 MUST BE ONE RECORD                          
*                                                                               
         BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
*  PROCESS RETURNED BUY INFORMATION                                             
*  SEND NEXT TSAR RECORD TO THE BUY                                             
*                                                                               
ROUT040  BAS   RE,SENDATA           SEND OUT DATA TO FALINK                     
         CLI   BUYERRSW,C'Y'        IS THERE AN ERROR                           
         BNE   ROUT060              NO, CONTINUE                                
*****    BAS   RE,TSARSAV           SAVE THE TSAR BLOCK                         
         B     EXIT                                                             
*                                                                               
ROUT060  L     RF,ANETBLK                                                       
         MVC   0(1,RF),SVTSRKEY                                                 
         MVI   TSACTN,TSARDH        READ HIGH                                   
         BAS   RE,CALLTSAR          REPOSITION POINTER                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSANXT        READ NEXT TSAR RECORD                       
         BAS   RE,CALLTSAR                                                      
         BE    ROUT080                                                          
*****    BAS   RE,TSARSAV           SAVE THE TSAR BLOCK                         
         B     EXIT                                                             
*                                                                               
ROUT080  BAS   RE,CALLBUY                                                       
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         SPACE 3                                                                
         EJECT                                                                  
*=================================================================*             
* PUT DATA IN AIO TO GLOBBER AND PASS CONTROL TO SPOT BUY         *             
*=================================================================*             
         SPACE 1                                                                
CALLBUY  NTR1                                                                   
         MVI   SVXFROV,X'52'       RETURN CONTROL TO THIS OVLY                  
         MVI   SVGLOBSW,C'B'       SET SWITCH TO UNITS BOUGHT                   
*                                                                               
         BAS   RE,TSARSAV          SAVE THE TSAR BLOCK                          
*                                                                               
*  TEST THE GLOBBER CALLS                                                       
*                                                                               
         L     R3,ANETBLK                                                       
         USING PKGUPLDD,R3                                                      
         MVC   SVTSRKEY,0(R3)                                                   
******   MVC   RPKCLI(33),SVFRCLI  SET THE KEY UP                               
         MVI   SVFRTYP,C'P'        SET AS PACKAGE UPLOAD                        
         CLC   RPKTYPE,=CL2'PB'                                                 
         BE    CALLB50                                                          
         CLC   RPKTYPE,=CL2'PD'                                                 
         BE    CALLB50                                                          
         CLC   RPKTYPE,=CL2'PC'                                                 
         BE    CALLB50                                                          
         MVI   SVFRTYP,C'B'        SET AS BUY UPLOAD                            
CALLB50  CLI   SVFRTYP,C'P'                                                     
         JNE   *+10                                                             
         MVC   RPKPRSID,SVPRSMID   PRISMA ID                                    
*                                                                               
         CLC   RPKPACK,SPACES      IF PACAKGE ALREADY IN RECORD                 
         BNE   *+10                BYPASS, SEED                                 
         MVC   RPKPACK,SVFRPACK    SEED THE PACKAGE NUMBER                      
*                                                                               
*****    XC    DMCB(24),DMCB                                                    
*****    L     RE,ATWA                                                          
*****    MVC   DMCB+10(2),2(RE)    TERMINAL NUMBER                              
*****    MVI   DMCB+8,1            PAGE NUMBER                                  
*****    MVI   DMCB+9,0                                                         
*****    MVC   DMCB+20(2),=C'L='                                                
*****    MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
*****    MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                       
*****    GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R3),0                      
*****    CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
*****    BE    *+6                                                              
*****    DC    H'0'                                                             
*                                                                               
* DO WSSVR CALL YTO PASS INFO TO THE BUY SYSTEM                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         MVC   FAWSTOKN,=CL4'NNAV'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,ANETBLK                                                  
         MVC   FAWSLEN,=H'1000'                                                 
         GOTOR VWSSVRS,FAWSSVRD                                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
* PASS TOKEN NAME FOR WSSVR THROUGH GLOBBER                                     
         MVC   WORK,=CL4'NNAV'                                                  
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,4,GLVBUY1                            
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING GLVXFRSY,R6                                                      
         MVC   GLVXFRSY,=C'NET'                                                 
         MVC   GLVXFRPR,=C'NNA'                                                 
         MVC   GLVXTOSY,=C'NET'                                                 
         MVC   GLVXTOPR,=C'NBU'                                                 
* SET DIALOGUE PARAMETERS                                                       
         OI    GLVXFLG1,GLV1SEPS+GLV1SEPD                                       
         OC    SVSESSNS,SVSESSNS    FIRST TIME                                  
         BZ    CALLB075                                                         
         MVC   GLVXSESR(2),SVSESSNS                                             
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE                                       
         B     CALLB100                                                         
* GET CURRENT SESSION NUMBER                                                    
CALLB075 OI    GLVXFLG1,GLV1SIDR                                                
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         ICM   RF,15,CSWITCH                                                    
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 (RF),DMCB                                                        
         L     R1,DMCB                                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING UTLD,R1                                                          
         MVC   SVSESSNS(1),TSESSION                                             
         MVC   GLVXSESR(2),SVSESSNS                                             
         DROP  R1,R3                                                            
*                                                                               
******   OI    GLVXFLG1,GLV1SEPS+GLV1SIDR                                       
CALLB100 GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   GLBERR                                                           
*                                                                               
CBX      B     EXIT                                                             
*                                                                               
GLBERR   DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* BUILD ESTIMATE TABLE                                            *             
*       BYTE 0   - ESTIMATE                                       *             
*       BYTE 1-2 - ESTIMATE START DATE                            *             
*       BYTE 3-4 - ESTIMATE END DATE                              *             
*=================================================================*             
BLDEST   NTR1                                                                   
         LA    R4,ESTTAB                                                        
         MVI   0(R4),X'FF'                                                      
*                                                                               
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
*                                                                               
         LA    R3,KEY                                                           
         USING ESTHDRD,R3                                                       
*                                                                               
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BEST                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+HIGH                                        
*                                                                               
BEST10   CLC   KEY(EKEYEST-EKEY),KEYSAVE                                        
         BNE   BLDESTX                                                          
         CLI   EKEYEST,0                                                        
         BE    BEST20                                                           
*                                                                               
         OC    BEST,BEST                                                        
         BZ    *+14                                                             
         CLC   EKEYEST,BEST                                                     
         BNE   BLDESTX                                                          
*                                                                               
         L     R3,AIO                                                           
         GOTO1 AIOCALL,DMCB,SPT+FIL+GET,AIO                                     
*                                                                               
         MVC   0(1,R4),EKEYEST                                                  
         GOTO1 VDATCON,DMCB,(0,ESTART),(2,HALF)                                 
         MVC   1(2,R4),HALF                                                     
         GOTO1 VDATCON,DMCB,(0,EEND),(2,HALF)                                   
         MVC   3(2,R4),HALF                                                     
         AHI   R4,5                                                             
         MVI   0(R4),X'FF'                                                      
*                                                                               
BEST20   LA    R3,KEY                                                           
*        CLI   EKEYEST,X'FF'                                                    
*        BE    BLDESTX                                                          
*                                                                               
         GOTO1 AIOCALL,DMCB,SPT+DIR+SEQ                                         
         B     BEST10                                                           
*                                                                               
BLDESTX  B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
*=================================================================*             
* SEARCH PACKAGE                                                  *             
*=================================================================*             
SRCHPKGE DS    0H                                                               
         BAS   RE,BLDEST            BUILD ESTIMATE TABLE                        
*                                                                               
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
*                                                                               
         LA    R3,KEY                                                           
         USING NPRECD,R3                                                        
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET,BNET                                                      
         MVC   NPKEST,BEST                                                      
         MVC   NPKPACK,BPKG                                                     
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+DIR+HIGH                                        
         B     SPKG10                                                           
*                                                                               
SPKGSEQ  LA    R3,KEY                                                           
         GOTO1 AIOCALL,DMCB,UNT+DIR+SEQ                                         
SPKG10   CLC   KEY(NPKNET-NPKEY),KEYSAVE                                        
         BNE   SPKGEX                                                           
*                                                                               
         OC    BNET,BNET           NETWORK FILTER?                              
         BZ    *+14                                                             
         CLC   NPKNET,BNET                                                      
         BNE   SPKGSEQ                                                          
*                                                                               
         OC    BEST,BEST           ESTIMATE FILTER?                             
         BZ    *+14                                                             
         CLC   NPKEST,BEST                                                      
         BNE   SPKGSEQ                                                          
*                                                                               
         OC    BPKG,BPKG           PACKAGE FILTER?                              
         BZ    *+14                                                             
         CLC   NPKPACK,BPKG                                                     
         BNE   SPKGSEQ                                                          
*                                                                               
         GOTO1 AIOCALL,DMCB,UNT+FIL+GET,AIO                                     
*                                                                               
         L     R3,AIO                                                           
*                                                                               
         OC    SPKDPT,SPKDPT       DAYPART FILTER?                              
         BZ    *+14                                                             
         CLC   NPAKDP,SPKDPT                                                    
         BNE   SPKGSEQ                                                          
*                                                                               
         CLI   SPKSTAT,0            RETURN PACKAGES W/O ANY STATUS?             
         BNE   SPKG18                                                           
         MVC   BYTE,NPAKSTAT                                                    
         NI    BYTE,X'FF'                                                       
         TM    BYTE,X'A0'                                                       
         BNZ   SPKGSEQ                                                          
         MVC   BYTE,NPAKCNTL                                                    
         NI    BYTE,X'FF'                                                       
         TM    BYTE,X'10'                                                       
         BNZ   SPKGSEQ                                                          
         B     SPKG20                                                           
*                                                                               
SPKG18   CLI   SPKSTAT,C'0'         STATUS FILTER?                              
         BE    SPKG20                                                           
*                                                                               
         CLI   SPKSTAT,SPKSTLOC     LOCKED?                                     
         BNE   *+16                                                             
         TM    NPAKSTAT,X'20'                                                   
         BZ    SPKGSEQ                                                          
         B     SPKG20                                                           
*                                                                               
         CLI   SPKSTAT,SPKSTULO     UNLOCKED?                                   
         BNE   *+16                                                             
         TM    NPAKSTAT,X'20'                                                   
         BNZ   SPKGSEQ                                                          
         B     SPKG20                                                           
*                                                                               
         CLI   SPKSTAT,SPKSTFRZ     FROZEN?                                     
         BNE   *+16                                                             
         TM    NPAKSTAT,X'80'                                                   
         BZ    SPKGSEQ                                                          
         B     SPKG20                                                           
*                                                                               
         CLI   SPKSTAT,SPKSTUFR     UNFROZEN?                                   
         BNE   *+16                                                             
         TM    NPAKSTAT,X'80'                                                   
         BNZ   SPKGSEQ                                                          
         B     SPKG20                                                           
*                                                                               
         CLI   SPKSTAT,SPKSTUPL     UPLSET?                                     
         BNE   *+16                                                             
         TM    NPAKCNTL,X'08'                                                   
         BZ    SPKGSEQ                                                          
         B     SPKG20                                                           
*                                                                               
         CLI   SPKSTAT,SPKSTUUP     UPLRESET?                                   
         BNE   *+16                                                             
         TM    NPAKCNTL,X'10'                                                   
         BNZ   SPKGSEQ                                                          
         B     SPKG20                                                           
*                                                                               
SPKG20   CLI   SPKESTD,0            ESTIMATE DATE FILTER?                       
         BE    SPKG30                                                           
*                                                                               
         LA    R2,ESTTAB                                                        
SPKG22   CLC   0(2,R2),=X'FF00'          FOUND ESTIMATE IN TABLE?               
         BE    SPKGSEQ                                                          
         CLC   NPKEST,0(R2)                                                     
         BE    *+12                                                             
         AHI   R2,5                                                             
         B     SPKG22                                                           
*                                                                               
         CLC   SPKESTD,3(R2)        > ESTIMATE END DATE?                        
         BH    SPKGSEQ                                                          
         CLC   SPKEEND,1(R2)        < ESTIMATE START DATE?                      
         BL    SPKGSEQ                                                          
*                                                                               
SPKG30   BAS   RE,SENDPKG                                                       
         B     SPKGSEQ                                                          
*                                                                               
SPKGEX   B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
*=================================================================*             
* SEND PACKAGE INFO TO THE PC                                     *             
*=================================================================*             
SENDPKG  NTR1                                                                   
         L     R3,AIO                                                           
         USING NPRECD,R3                                                        
*                                                                               
         MVC   NSIVT,=C'PL'        DEFAULT NSI VIEWING TYPE                     
         MVC   COMVT,=C'RL'        DEFAULT COM VIEWING TYPE                     
*                                                                               
         LHI   R1,X'7D'             RETURN PACKAGE                              
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R4,QCLT              CLIENT                                      
         LHI   R1,X'01'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NPKEST            ESTIMATE                                    
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NPKNET            NETWORK                                     
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NPKPACK           PACKAGE #                                   
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'01',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SENDPK18                                                         
         L     R6,12(R1)                                                        
         USING NPAKEL,R6                                                        
*                                                                               
         LA    R4,NPAKNAME          NAME                                        
         LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,NPAKDP            DAYPART                                     
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         MVI   BYTE,0                                                           
         CLI   SPKSTAT,C'0'         STATUS FILTER?                              
         BE    *+14                                                             
         MVC   BYTE,SPKSTAT                                                     
         B     SENDPK08                                                         
*                                                                               
         TM    NPAKSTAT,X'20'       LOCKED?                                     
         BZ    *+8                                                              
         MVI   BYTE,C'1'                                                        
         TM    NPAKSTAT,X'80'       FROZEN?                                     
         BZ    *+8                                                              
         MVI   BYTE,C'3'                                                        
         TM    NPAKCNTL,X'08'       UPLSET?                                     
         BZ    *+8                                                              
         MVI   BYTE,C'5'                                                        
*                                                                               
SENDPK08 CLI   BYTE,0                                                           
         BE    *+16                                                             
         LA    R4,BYTE              STATUS                                      
         LHI   R1,X'0B'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         XC    PAKOPT,PAKOPT                                                    
         LA    R4,PAKOPT                                                        
         TM    NPAKSTAT,X'10'                                                   
         BZ    *+14                                                             
         MVC   PAKOPT(8),=C'PRINT=NO'                                           
         AHI   R4,8                                                             
*                                                                               
         OC    NPAKZONE,NPAKZONE                                                
         BZ    SENDPK12                                                         
*                                                                               
         LA    RF,PAKOPT                                                        
         CR    R4,RF                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         AHI   R4,1                                                             
*                                                                               
         MVC   0(5,R4),=C'ZONE='                                                
         MVC   5(1,R4),NPAKZONE                                                 
*                                                                               
SENDPK12 ST    R4,APAKOPT                                                       
*                                                                               
         MVI   BYTE,C'N'                                                        
         TM    NPAKSTAT,X'02'       AUDIT?                                      
         BZ    *+8                                                              
         MVI   BYTE,C'Y'                                                        
*                                                                               
         LA    R4,BYTE                                                          
         LHI   R1,X'14'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R2,ESTTAB                                                        
SENDPK14 CLC   0(2,R2),=X'FF00'                                                 
         BE    SENDPK16                                                         
         CLC   NPKEST,0(R2)                                                     
         BE    *+12                                                             
         AHI   R2,5                                                             
         B     SENDPK14                                                         
*                                                                               
         LA    R4,1(R2)                                                         
         LHI   R1,X'0C'             ESTIMATE START DATE                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,3(R2)                                                         
         LHI   R1,X'0D'             ESTIMATE END DATE                           
         BAS   RE,SENDD                                                         
*                                                                               
SENDPK16 OC    NPAKCOST,NPAKCOST                                                
         BZ    *+16                                                             
         LA    R4,NPAKCOST          COST                                        
         LHI   R1,X'06'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKINT,NPAKINT                                                  
         BZ    SENDPK17                                                         
         LA    R4,NPAKINT           INTEGRATION COST                            
         LHI   R1,X'10'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         TM    NPAKSTAT,X'04'       INTEGRATION NON-COMMISSIONABLE              
         BZ    SENDPK17                                                         
         MVI   BYTE,C'Y'                                                        
         LA    R4,BYTE                                                          
         LHI   R1,X'20'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SENDPK17 OC    NPAKFEED,NPAKFEED                                                
         BZ    *+16                                                             
         LA    R4,NPAKFEED          FEED %                                      
         LHI   R1,X'1C'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKUNCD,NPAKUNCD                                                
         BZ    SENDP17A                                                         
         LA    R4,NPAKUNCD          UNIVERSE                                    
         XC    FULL,FULL                                                        
         MVO   FULL(3),0(2,R4)                                                  
         OI    FULL+2,X'0F'                                                     
         LA    R4,FULL                                                          
         LHI   R1,X'13'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SENDP17A OC    NPAKUNIV,NPAKUNIV                                                
         BZ    *+16                                                             
         LA    R4,NPAKUNIV          UNIV %                                      
         LHI   R1,X'1A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKIMP,NPAKIMP                                                  
         BZ    *+16                                                             
         LA    R4,NPAKIMP           IMPACT %                                    
         LHI   R1,X'1D'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKHUTA,NPAKHUTA                                                
         BZ    *+16                                                             
         LA    R4,NPAKHUTA          HUT AVERAGE                                 
         LHI   R1,X'17'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         MVI   BYTE,C'V'                                                        
         TM    NPAKCNTL,X'40'       IMP BASED?                                  
         BZ    *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                                                               
         LA    R4,BYTE                                                          
         LHI   R1,X'08'             DEMO BASE                                   
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKGCPM,NPAKGCPM                                                
         BZ    *+16                                                             
         LA    R4,NPAKGCPM          CPM                                         
         LHI   R1,X'0F'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKHUTF,NPAKHUTF                                                
         BZ    *+16                                                             
         LA    R4,NPAKHUTF          HUT TYPE                                    
         LHI   R1,X'19'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKHPCT,NPAKHPCT                                                
         BZ    *+16                                                             
         LA    R4,NPAKHPCT          HUT ADJUSTMENT                              
         LHI   R1,X'18'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKLENG,NPAKLENG                                                
         BZ    *+16                                                             
         LA    R4,NPAKLENG          LENGTH                                      
         LHI   R1,X'1E'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKSREP,NPAKSREP                                                
         BZ    SENDPK18                                                         
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,NPAKSREP                                                    
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
*                                                                               
         LA    R4,FULL              SPECIAL REP                                 
         LHI   R1,X'1B'                                                         
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
*                                                                               
SENDPK18 L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SENDPK55                                                         
         L     R6,12(R1)                                                        
         USING NPK2D,R6                                                         
*                                                                               
         L     R4,APAKOPT                                                       
         OC    NPK2TRAD,NPK2TRAD   TRADE PERCENT                                
         BZ    SENDPK20                                                         
*                                                                               
         LA    RF,PAKOPT                                                        
         CR    R4,RF                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
*                                                                               
         MVC   0(6,R4),=C'TRADE='                                               
         EDIT  (B2,NPK2TRAD),(3,6(R4)),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
SENDPK20 OC    NPK2CASH,NPK2CASH   CASH PERCENT                                 
         BZ    SENDPK25                                                         
*                                                                               
         LA    RF,PAKOPT                                                        
         CR    R4,RF                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
*                                                                               
         MVC   0(5,R4),=C'CASH='                                                
         CLC   NPK2CASH,=X'FFFF'    CHECK ZERO OVERRIDE                         
         BNE   *+12                                                             
         MVI   5(R4),C'0'                                                       
         B     SENDPK25                                                         
*                                                                               
         EDIT  (B2,NPK2CASH),(3,5(R4)),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
SENDPK25 CLI   NPKHPNAM,X'40'                                                   
         BNH   SENDPK40                                                         
*                                                                               
         LA    RE,60                                                            
         LA    R4,PAKOPT                                                        
SENDPK30 CLI   0(R4),X'40'                                                      
         BNH   *+12                                                             
         AHI   R4,1                                                             
         BCT   RE,SENDPK30                                                      
*                                                                               
         LA    RF,PAKOPT                                                        
         CR    R4,RF                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         AHI   R4,1                                                             
         MVC   0(3,R4),=CL3'HP='                                                
         AHI   R4,3                                                             
         MVC   0(3,R4),NPKHPNAM                                                 
*                                                                               
         LA    RE,60                                                            
         LA    R4,PAKOPT                                                        
SENDPK35 CLI   0(R4),X'40'                                                      
         BNH   *+12                                                             
         AHI   R4,1                                                             
         BCT   RE,SENDPK35                                                      
*                                                                               
         MVI   0(R4),C'*'                                                       
         AHI   R4,1                                                             
         EDIT  (B2,NPKHPPCT),(2,0(R4)),ALIGN=LEFT,ZERO=NOBLANK                  
         DROP  R3                                                               
*                                                                               
* COS2 OPTION                                                                   
*                                                                               
SENDPK40 TM    NPK2FLAG,NPK2FC2I+NPK2FC20                                       
         JNZ   *+14                                                             
         OC    NPK2COS2,NPK2COS2   COS2?                                        
         JZ    SENDPK45                                                         
*                                                                               
         LA    RE,60                                                            
         LA    R4,PAKOPT                                                        
SENDPK41 CLI   0(R4),X'40'                                                      
         BNH   *+12                                                             
         AHI   R4,1                                                             
         BCT   RE,SENDPK41                                                      
*                                                                               
         MVC   0(5,R4),=C'COS2='                                                
         LA    R2,5(R4)                                                         
         TM    NPK2FLAG,NPK2FC2I   ICOS2?                                       
         JZ    *+14                                                             
         MVC   0(6,R4),=C'ICOS2='                                               
         AHI   R2,1                                                             
*                                                                               
         TM    NPK2FLAG,NPK2FC20   ZERO COS2 INPUT?                             
         JZ    *+14                                                             
         MVC   0(3,R2),=C'0.0'                                                  
         J     SENDPK45                                                         
*                                                                               
         EDIT  NPK2COS2,(8,0(R2)),6,ALIGN=LEFT,FILL=0,DROP=5                    
*                                                                               
SENDPK45 OC    PAKOPT,PAKOPT                                                    
         BZ    *+16                                                             
         LA    R4,PAKOPT            OPTIONS                                     
         LHI   R1,X'1F'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPK2NME2,NPK2NME2                                                
         BZ    *+16                                                             
         LA    R4,NPK2NME2          NAME 2                                      
         LHI   R1,X'0A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPK2BTYP,NPK2BTYP                                                
         BZ    *+16                                                             
         LA    R4,NPK2BTYP          BUY TYPE                                    
         LHI   R1,X'12'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NPAKMPRD,NPAKMPRD                                                
         BZ    *+16                                                             
         LA    R4,NPAKMPRD          MASTER PRODUCT                              
         LHI   R1,X'0E'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         LA    R4,HALF                                                          
         MVC   HALF(3),=C'PL '                                                  
         OC    NPK2VTYP,NPK2VTYP                                                
         BZ    SENDPK49                                                         
         MVC   HALF(2),NPK2VTYP     VIEWING STREAM                              
         TM    NPK2MPOD,X'80'                                                   
         BZ    SENDPK49                                                         
         MVI   HALF+2,C'P'       SET POD INDICATOR                              
SENDPK49 LHI   R1,X'09'                                                         
         BAS   RE,SENDD                                                         
         MVC   NSIVT,HALF          SAVE NSI VIEWING TYPE                        
*                                                                               
         OC    NPKCVTYP,NPKCVTYP   COMSCORE VIEWING TYPE?                       
         JZ    SENDPK50                                                         
         CLI   NPKCVTYP,NPKVTRLQ                                                
         JNE   *+10                                                             
         MVC   HALF,=C'RL'                                                      
         CLI   NPKCVTYP,NPKVTRCQ                                                
         JNE   *+10                                                             
         MVC   HALF,=C'RC'                                                      
         CLI   NPKCVTYP,NPKVTR3Q                                                
         JNE   *+10                                                             
         MVC   HALF,=C'R3'                                                      
         CLI   NPKCVTYP,NPKVTR7Q                                                
         JNE   *+10                                                             
         MVC   HALF,=C'R7'                                                      
*                                                                               
         LA    R4,HALF                                                          
         LHI   R1,X'24'                                                         
         BAS   RE,SENDD                                                         
         MVC   COMVT,HALF          SAVE COMSCORE VIEWING TYPE                   
*                                                                               
SENDPK50 OC    NPAKGDEM,NPAKGDEM                                                
         BZ    SENDPK55                                                         
         MVC   THREE,NPAKGDEM                                                   
         LA    RE,BLOCK             GUARENTEE DEMO CATEGORY                     
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         DROP  RE                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMOCON-COMFACSD(RF)                                         
         XC    EDSAVE,EDSAVE                                                    
         GOTO1 (RF),DMCB,THREE,(9,EDSAVE),(C'S',BLOCK),0                        
         LA    R4,EDSAVE                                                        
         LHI   R1,X'21'                                                         
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
         SPACE                                                                  
*                                                                               
SENDPK55 L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SENDPK60                                                         
         L     R6,12(R1)                                                        
         USING NPFILD,R6                                                        
*                                                                               
         OC    NPFILTER,NPFILTER                                                
         BZ    *+16                                                             
         LA    R4,NPFILTER          FILTER                                      
         LHI   R1,X'11'                                                         
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
*                                                                               
SENDPK60 L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'09',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SENDPK70                                                         
         L     R6,12(R1)                                                        
         USING NAUDD,R6                                                         
*                                                                               
         OC    NAUDGRP,NAUDGRP                                                  
         BZ    *+16                                                             
         LA    R4,NAUDGRP           AUDIT GROUP                                 
         LHI   R1,X'16'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    NAUDCOM,NAUDCOM                                                  
         BZ    *+16                                                             
         LA    R4,NAUDCOM           AUDIT NAME                                  
         LHI   R1,X'15'                                                         
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
*                                                                               
SENDPK70 L     R3,AIO                                                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',(R3)),0                        
         CLI   12(R1),0                                                         
         BNE   SENDPK80                                                         
         L     R6,12(R1)                                                        
         USING NPPSELD,R6                                                       
*                                                                               
         LA    R4,NPPSID           PRISMA ID                                    
         LHI   R1,X'22'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(2,NPPSCDT),(8,DUB)                                 
         LA    R4,DUB               PRISMA LAST CHANGE DATE                     
         LHI   R1,X'23'                                                         
         BAS   RE,SENDD                                                         
         DROP  R6                                                               
*                                                                               
SENDPK80 DS    0H                  SEND VIEWING TYPES                           
         MVC   FULL,=C'NSI '                                                    
         LA    R4,FULL                                                          
         LHI   R1,X'25'            MAPCODE 37                                   
         BAS   RE,SENDD                                                         
         LA    R4,NSIVT                                                         
         LHI   R1,X'26'            MAPCODE 38                                   
         BAS   RE,SENDD                                                         
*                                                                               
         MVC   FULL,=C'COM '                                                    
         LA    R4,FULL                                                          
         LHI   R1,X'25'            MAPCODE 37                                   
         BAS   RE,SENDD                                                         
         LA    R4,COMVT                                                         
         LHI   R1,X'26'            MAPCODE 38                                   
         BAS   RE,SENDD                                                         
*                                                                               
SENDPKGX B     EXIT                                                             
*=================================================================*             
* SEND DRAFT BUY INFO TO THE PC                                   *             
*=================================================================*             
         SPACE 1                                                                
SENDATA  NTR1                                                                   
*                                                                               
         L     R3,AIO3                                                          
         USING PKGUPLDD,R3                                                      
*                                                                               
*                                                                               
*  CHECK IF ERROR RETURNED                                                      
*                                                                               
         CLI   BUYERRSW,C'Y'                                                    
         BNE   SND040                                                           
         LA    R6,WORK2                                                         
         USING ERRDATA,R6                                                       
*                                                                               
         LHI   R1,X'96'            UNIT ERROR ELEMENT                           
         CLI   SVFRTYP,C'B'        IS THIS A UNIT REPLY                         
         BE    *+8                 SEND THE UNIT INFORMATION                    
         LHI   R1,X'94'            PACKAGE ERROR ELEMENT                        
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R4,ERNUMBR                                                       
         LHI   R1,X'39'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         GOTO1 VGETMSG,DMCB+12,(ERNUMBR,WORK),(X'FF',DMCB),(7,0)                
         LA    R4,WORK+8                                                        
         LHI   R1,X'3A'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    ERESTMT,ERESTMT                                                  
         BZ    *+16                                                             
         LA    R4,ERESTMT                                                       
         LHI   R1,X'03'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    ERCLINT,ERCLINT                                                  
         BZ    *+16                                                             
         LA    R4,ERCLINT                                                       
         LHI   R1,X'04'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    ERNTWK,ERNTWK                                                    
         BZ    *+16                                                             
         LA    R4,ERNTWK                                                        
         LHI   R1,X'05'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    ERPKGE,ERPKGE                                                    
         BZ    SND025                                                           
         ZIC   R4,ERPKGE                                                        
         EDIT  (R4),(3,FULL),ALIGN=LEFT                                         
         LA    R4,FULL                                                          
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
SND025   CLI   SVFRTYP,C'B'         IS THIS A UNIT REPLY                        
         BNE   SND030               NO GO STRAIGHT TO EXTRA FIELD               
*                                                                               
         OC    ERPROG,ERPROG                                                    
         BZ    *+16                                                             
         LA    R4,ERPROG                                                        
         LHI   R1,X'07'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    ERLNGTH,ERLNGTH                                                  
         BZ    *+16                                                             
         LA    R4,ERLNGTH                                                       
         LHI   R1,X'08'                                                         
         BAS   RE,SENDD                                                         
*                                                                               
         OC    ERDATE,ERDATE                                                    
         BZ    *+16                                                             
         LA    R4,ERDATE                                                        
         LHI   R1,X'09'                                                         
         BAS   RE,SENDD                                                         
         BE    SNDEX                BUY UPLOAD DON'T SEND EXTRA FIELD           
*                                                                               
SND030   LA    R4,ERMXTRA                                                       
         LHI   R1,X'3B'                                                         
         BAS   RE,SENDD                                                         
         B     SNDEX                                                            
*                                                                               
SND040   CLI   SVFRTYP,C'B'         IS THIS A UNIT REPLY                        
         BE    SNDEX                UNLESS ERROR DONT PASS ANYTHING             
*                                                                               
         LHI   R1,X'94'                                                         
         BAS   RE,SENDH                                                         
*                                                                               
         CLI   SVFRTYP,C'B'         IS THIS A UNIT REPLY                        
         BE    SNDEX                DON'T SEND THE PACKAGE NUMBER               
         LA    R4,RPKPACK           PACKAGE NUMBER                              
         LHI   R1,X'02'                                                         
         BAS   RE,SENDD                                                         
         MVC   SVFRPACK,RPKPACK     SAVE PACKAGE NUMBER FOR UNITS               
*                                                                               
SNDEX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* SAVE TSAR BLOCK                                                 *             
*=================================================================*             
         SPACE 1                                                                
TSARSAV  NTR1                                                                   
         CLI   SVRSTSAV,C'R'        MUST BE PROCEEDED BY RESTORE                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSASAV        SAVE TSAR                                   
         BAS   RE,CALLTSAR                                                      
         MVI   SVRSTSAV,C'S'                                                    
         B     EXIT                                                             
*=================================================================*             
* SAVE TSAR BLOCK                                                 *             
*=================================================================*             
         SPACE 1                                                                
TSARRES  NTR1                                                                   
         CLI   SVRSTSAV,C'S'        MUST BE PROCEEDED BY SAVE                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSARES        RESTORE TSAR                                
         BAS   RE,CALLTSAR                                                      
         MVI   SVRSTSAV,C'R'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* COMMON CALL TO TSAR                                             *             
*=================================================================*             
         SPACE 1                                                                
CALLTSAR LR    R0,RE                                                            
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSERRS,0            SET CC ON EXIT                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
CALBUYEX XIT1                                                                   
         EJECT                                                                  
UNTFILE  DC    CL8'UNTFILE'                                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DENADCATS                                                      
         EJECT                                                                  
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
NDAYS    DS    F                                                                
BUYDSTR  DS    A                                                                
BUYDEND  DS    A                                                                
SPOTNUM  DS    H                                                                
INVSDATE DS    CL6                                                              
INVHDPRD DS    X                   INVOICE HEADER PRD                           
INVHDPR2 DS    X                                                                
INVHDEST DS    X                                                                
RELAFDAY DS    X                                                                
RELAFTIM DS    XL2                                                              
SVBDELEM DS    CL67                                                             
ALDATE   DS    XL2                 ALLOC DATE                                   
LADATE   DS    XL2                 LAST ALLOC DATE                              
V10301   DS    CL4                                                              
EDSAVE   DS    XL17                                                             
*                                                                               
NSIVT    DS    CL2                 NIELSEN VIEWING TYPE                         
COMVT    DS    CL2                 COMSCORE VIEWING TYPE                        
*                                                                               
OLDPRDS  DS    CL2                                                              
UNPAYSW  DS    CL1                                                              
UNBILLSW DS    CL1                                                              
PRDSCRN  DS    CL15                                                             
HOLDCLT  DS    CL2                                                              
N2PROFLE DS    CL16                                                             
THREE    DS    CL3                                                              
FREEZESW DS    CL1                                                              
UNITPRDS DS    CL2                                                              
SEQNUM   DS    CL1                                                              
*                                                                               
CLIOPTN2 DS    CL1                                                              
*                                                                               
PAKOPT   DS    CL60                                                             
APAKOPT  DS    F                                                                
*                                                                               
OUT20    DS    CL20                                                             
*                                                                               
SECAGY   DS    CL2                                                              
SVPASSWD DS    CL2                                                              
*                                                                               
WSVRBLK  DS    XL(FAWSSVRL)                                                     
*                                                                               
ESTTAB   DS    XL1500                                                           
*                                                                               
         DS    0D                                                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE SPGENPROG                                                      
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPGENREAS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
       PRINT ON                                                                 
       ++INCLUDE NAVDSECTS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020NENAV52   02/26/20'                                      
         END                                                                    
