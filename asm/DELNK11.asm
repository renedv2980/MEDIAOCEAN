*          DATA SET DELNK11    AT LEVEL 029 AS OF 06/08/17                      
*PHASE TF2F11A                                                                  
*INCLUDE DUMPOUT                                                                
*INCLUDE UNBOOK                                                                 
*INCLUDE GETKSRC                                                                
*INCLUDE NSIWEEK                                                                
DELNK11  TITLE '- REP DEMO SYSTEM SERVER'                                       
**************************************************************                  
*&&DO                                                                           
SVRDEF   CSECT                                                                  
         DC    (RSVRDEFL)X'00'                                                  
         ORG   SVRDEF                                                           
         DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
         DC    AL2(CODE-SVRDEF)    SERVER ENTRY POINT                           
         DC    AL2(FILESREP-SVRDEF)   SYSTEM/FILE LIST                          
         DC    AL2(FACS-SVRDEF)    FACILITIES LIST                              
         DC    AL2(REQUEST-SVRDEF) REQUEST MAP                                  
         ORG   SVRDEF+(RSVRSYS2-RSVRDEFD)                                       
         DC    AL1(2)              SPOT SYSTEM                                  
         DC    AL2(FILESPOT-SVRDEF)                                             
         ORG   SVRDEF+(RSVRTYPE-RSVRDEFD)                                       
         DC    C'D'                SERVER TYPE                                  
         DC    AL1(8)              REP SYSTEM                                   
         DC    C'DE'               SYSTEM                                       
         DC    C'LK'               PROGRAM                                      
         DC    AL1(WRKIFTWF)       WORKER FILE IN                               
         DC    AL1(WRKIAAPP)       OPEN APPEND                                  
         DC    X'0F2F'             SYSPHASE                                     
         DC    AL1(RSVRILNK+RSVRILCO+RSVRIMLT)                                  
         ORG                                                                    
         EJECT                                                                  
**************************************************************                  
*&&                                                                             
SVRDEF   CSECT                                                                  
         LKSVR IDF=Y,REQUEST=*,CODE=CODE,SYSTEM=REPSYSQ,FACS=FACS,     *        
               APPEND=Y,SERVERTYPE=TSTDEMO,AUTOCLEAR=Y,                *        
               WORKERKEY=DELK,BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),    *        
               FILES=FILESREP,SYSPHASE=SYSPHASE,TYPE=D,LOADFACSOFF=Y            
*                                                                               
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 IUNWRKL+FETCHWKL,**LK11*,CLEAR=YES,RR=RE                         
         USING LP_D,R1                                                          
***      L     R7,LP_ARUNP                                                      
***      USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         OC    RMASTC,RMASTC       TEST RUNNING ONLINE                          
         BNZ   INIT02                                                           
         ICM   R9,15,LP_ABLK1      ROOT SHOULD PASS A(WORKD)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
B#WORKD  EQU   1                   WORKD                                        
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         SR    R8,R8                                                            
         ICM   R8,3,=AL2(WORKL)                                                 
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
         USING SAVED,R8,R7         R8=A(SAVE W/S)                               
                                                                                
INIT04   LR    R7,R8                                                            
         AHI   R7,4096                                                          
         ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         ST    RC,AIUNWRK                                                       
         LA    RC,IUNWRKL(RC)                                                   
         USING FETCHWKD,RC                                                      
                                                                                
         MVC   VERSNUM,LP_VRSN1                                                 
         MVC   AGYALPH,LP_AGY                                                   
         MVC   OFFLFLAG,LP_FLAG    SAVE ONLINE/OFFLINE FLAG                     
         MVC   USERID,LP_USRID                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         BASR  RE,0                                                             
         AHI   RE,LVALUES-*                                                     
         LA    R0,SVVALUES                                                      
         LHI   R1,SVVALUESL                                                     
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RE,RELOLST2                                                      
         LHI   R0,RLOLSTN2                                                      
RUNSTR01 L     RF,0(RE)            RELOCATE ADCONS                              
         A     RF,SRVRRELO                                                      
         ST    RF,0(RE)                                                         
         AHI   RE,L'RELOLST2                                                    
         BCT   R0,RUNSTR01                                                      
                                                                                
*                                                                               
         LA    R0,REQVALS          CLEAR DOWN SAVE AREA                         
         LHI   R1,REQVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,RCOMFACS         YES - LOAD FACILITIES OVERLAYS               
         ST    RF,ACOMFACS                                                      
         L     RE,CGETPROF-COMFACSD(RF)                                         
         ST    RE,VGETPROF                                                      
*                                                                               
         L     RE,CGETDAY-COMFACSD(RF)                                          
         ST    RE,VGETDAY                                                       
         L     RE,CADDAY-COMFACSD(RF)                                           
         ST    RE,VADDAY                                                        
         L     RE,CDEFINE-COMFACSD(RF)                                          
         ST    RE,VDEFINE                                                       
                                                                                
         L     RE,CHELLO-COMFACSD(RF)                                           
         ST    RE,VHELLO                                                        
                                                                                
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
                                                                                
         GOTO1 (RF),DMCB,0,X'D9000A29'       REGETIUN                           
         MVC   VREGTIUN,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A24'       SPGETIUN                           
         MVC   VSPGTIUN,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A21'       SPGETDEMF                          
         MVC   VSPDEMLK,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A00'       BOOKVAL                            
         MVC   VBOOKVAL,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000AE0'       DEMOCON                            
         MVC   VDEMOCON,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A11'       UNTIME                             
         MVC   VUNTIME,0(R1)                                                    
         GOTO1 (RF),(R1),0,X'D9000AA4'       REFETCH                            
         MVC   VFETCH,0(R1)                                                     
         GOTO1 (RF),(R1),0,X'D9000A22'       SPDEMUP                            
         MVC   VSPDEMUP,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A08'       REDEMUP                            
         MVC   VREDEMUP,0(R1)                                                   
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
                                                                                
         L     R1,ALP                                                           
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR02 L     R1,ALP                                                           
         LA    R0,SAVED                                                         
B#SAVED  EQU   2                   SAVED                                        
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    REQVALS(REQVALSL),REQVALS                                        
***      GOTOR RVALPAR,=CL80'TRACE=B'                                           
                                                                                
         L     RE,=V(DUMPOUT)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,VDUMPOUT                                                      
         LA    R0,REQVALS          CLEAR DOWN SAVE AREA                         
         LHI   R1,REQVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
***      MVC   AGYALPH,LP_AGY                                                   
         J     EXITY                                                            
                                                                                
*=====================================================================*         
* RUN A DOWNLOAD REQUEST                                              |         
*=====================================================================*         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         L     RF,ALP                                                           
         L     RF,LP_ARUNP-LP_D(RF)                                             
         L     RF,RUNPMODE-RUNPARMD(RF)                                         
         L     RF,RMASTC-RUNFACSD(RF)                                           
         MVC   DEPRINT,MCVPRINT-MASTD(RF)      ADDRESS OF PRINT ROUT            
         TM    OFFLFLAG,LP_FOFFL              IF OFFLINE THEN SET OVSYS         
         BZ    *+10                           ELSE IT IS ALREADY SETTED         
         MVC   OVSYS,MCOVSYS-MASTD(RF)         ADDRESS OF SYSTEM NUMBER         
                                                                                
         CLC   LP_QMAPN,=AL2(M#PGDET)          PROCESS REPORT                   
         JE    PROGDET                                                          
         CLC   LP_QMAPN,=AL2(M#PAVHD)          PROCESS PROGRAM AVG              
         JE    PROGHDR                         HEADER                           
         CLC   LP_QMAPN,=AL2(M#INVHD)          PROCESS INVENTORY                
         JE    PINVHDR                         HEADER                           
         CLC   LP_QMAPN,=AL2(M#RECAL)          DETAILS RECALC                   
         JE    DRECALC                                                          
         CLC   LP_QMAPN,=AL2(M#CANLK)          CANADIAN LOOKUP                  
         JE    SDCANLK                         FOR SPOT DESKTOP                 
         J     EXITY                                                            
                                                                                
PROGDET  GOTOR RUNPGDET                                                         
         J     EXITY                                                            
PROGHDR  GOTOR PAVHDR                                                           
         J     EXITY                                                            
DRECALC  GOTOR RECALC                                                           
         J     EXITY                                                            
PINVHDR  GOTOR INVHDR                                                           
         J     EXITY                                                            
SDCANLK  GOTOR CANLOOK                                                          
         J     EXITY                                                            
         DROP  R6                                                               
**********************************************************************          
*                                                                    *          
*     PROCESS INVENTORY HEADER INFORMATION                           *          
*                                                                    *          
**********************************************************************          
INVHDR   NTR1                                                                   
                                                                                
         MVC   DUB(4),=C'SELW'                                                  
         XC    DMCB,DMCB                                                        
         LA    RF,DUB                                                           
         ST    RF,DMCB                                                          
         GOTOR (#GETPROF,AGETPROF),DMCB                                         
         MVC   SELWPROF,PROFSELW                                                
                                                                                
         LA    R5,FETCHBLK                                                      
         USING RFTBLKD,R5                                                       
         LA    R0,RFTBLKD          INITIALIZE FETCH BLOCK                       
         LHI   R1,RFTBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   RFTACOM,ACOMFACS                                                 
         MVC   RFTAIO1,AIO2                                                     
         MVC   RFTAIO2,AIO3                                                     
         MVC   RFTACOM,ACOMFACS                                                 
         LA    R0,FETCHWRK                                                      
         STCM  R0,15,RFTAWRK                                                    
         MVC   RFTCREP,AGYALPH                                                  
         MVI   RFTCSRC,C'N'                                                     
         LA    RE,FHOOK                                                         
         STCM  RE,15,RFTHOOKA                                                   
         MVI   RFTCDCTL,RFTCDC1Q            SET REPORT INVENTORY                
                                                                                
         SR    R4,R4                                                            
         ICM   R4,7,AEFFDT                                                      
         LA    R2,1                                                             
         TM    EFFDTIND,LQ_TSINQ                                                
         BNO   INVHDR06                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     INVHDR08                                                         
INVHDR06 TM    EFFDTIND,LQ_TLSTQ                                                
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
INVHDR08 MVC   TMPEFFDT,0(R4)                                                   
                                                                                
         SR    R4,R4                                                            
         ICM   R4,7,ASTAT                                                       
         LA    R2,1                                                             
         TM    STAIND,LQ_TSINQ                                                  
         BNO   INVHDR20                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
         J     INVHDR30                                                         
INVHDR20 TM    STAIND,LQ_TLSTQ                                                  
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
                                                                                
INVHDR30 STCM  R2,3,NUMSTAT                                                     
         ST    R4,MYSTAPTR                                                      
                                                                                
         L     R4,MYSTAPTR                                                      
         USING INVHDRD,R4                                                       
         MVC   RFTCSTAT,INVSTAT                                                 
         MVI   OVRFLAG,C'N'                      OVERRIDE STATION?              
         OC    INVOSTAT,INVOSTAT                                                
         BZ    *+14                                                             
         MVC   RFTCSTAT,INVOSTAT                                                
         MVI   OVRFLAG,C'Y'                                                     
         OC    RFTCSTAT(5),=C'     '                                            
         CLI   RFTCSTAT+L'RFTCSTAT-1,C' '                                       
         BNE   *+8                                                              
         MVI   RFTCSTAT+L'RFTCSTAT-1,C'T'                                       
         CLC   =C'B3',AGYALPH                                                   
         BNE   *+10                                                             
         CLC   =C'TELE',RFTCSTAT                 YES-TEST STAT=TELE             
         BNE   *+10                                                             
         MVC   RFTCSTAT,=C'TEL H'                YES -SET LOOKUP TO             
                                                                                
         CLI   RFTCSTAT+3,C'+'                                                  
         BNE   INVHDR34                                                         
         MVC   RFTCSTAT+3(2),=C' 1'                                             
         B     INVHDR36                                                         
INVHDR34 CLI   RFTCSTAT+4,C'+'                                                  
         BNE   INVHDR36                                                         
         MVC   RFTCSTAT+4(1),=C'1'                                              
                                                                                
                                                                                
                                                                                
INVHDR36 XC    DUMSTAT,DUMSTAT                                                  
         MVC   DUMSTAT(L'INVSTAT),INVSTAT            STORE NON OVERRIDE         
         MVC   RFTCEFST,TMPEFFDT                                                
         MVC   RFTCEFEN,TMPEFFDT+2                                              
         MVI   RFTAMODE,RFTAINVQ   SET FETCH BY DAYPART POINTERS                
         MVI   RFTCNTL,NINVINDQ    SET CONTROL FLAGS                            
                                                                                
         LA    RE,INVDYPT                                                       
         XC    TMPDYPT,TMPDYPT                                                  
         MVC   TMPDYPT,0(RE)                                                    
         LA    R0,L'INVDYPT                                                     
         L     R2,AIO1                                                          
                                                                                
         MVI   RFTCDTMS,X'FF'                                                   
                                                                                
         STCM  R2,15,RFTCDTMS+1                                                 
         USING RFTCDTMS,R2                                                      
INVHDR40 XC    RFTCDTMS(RFTCDTLQ),RFTCDTMS                                      
         MVC   RFTCDTDP,0(RE)                                                   
         AHI   R2,RFTCDTLQ                                                      
         AHI   RE,1                                                             
         CLI   0(RE),0                                                          
         BE    INVHDR42                                                         
         BCT   R0,INVHDR40                                                      
INVHDR42 XC    RFTCDTMS(RFTCDTLQ),RFTCDTMS                                      
         AHI   R2,RFTCDTLQ                                                      
         DROP  R2                                                               
         GOTOR VFETCH,PARM,RFTBLKD                                              
         L     R4,MYSTAPTR                                                      
         AHI   R4,INVHDRL                                                       
         ST    R4,MYSTAPTR                                                      
         ZICM  R2,NUMSTAT,(3)                                                   
         BCT   R2,INVHDR30                                                      
         J     EXITY                                                            
                                                                                
**************************************************************                  
* FETCH HOOK FOR INVENTORY HEADERS                                              
**************************************************************                  
FHOOK    NTR1                                                                   
         OC    RFTERR,RFTERR       DIE IF ANY ERRORS RETURNED                   
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   RFTMODE,RFTNHDRQ    TEST NEW INVENTORY HEADER                    
         BNE   FHOOK80                                                          
                                                                                
         OC    RFTFPGMS,RFTFPGMS   YES - TEST ANY PROGRAMS                      
         BNZ   *+12                                                             
         MVI   RFTRETRN,RFTRBADQ   NO - IGNORE THIS HEADER                      
         B     FHOOKX                                                           
                                                                                
         MVI   OHDRSEND,YESQ        SET TO SEND INVENTORY HEADER                
         MVC   OHDRSTA,DUMSTAT                                                  
         CLI   OHDRSTA+4,C'T'                                                   
         BNE   *+8                                                              
         MVI   OHDRSTA+4,0                                                      
         MVC   OHDRDAY,RFTFDTDY     SET DAYS                                    
                                                                                
         MVC   OHDRSTIM,RFTFDTST    SET START TIME                              
         MVC   OHDRETIM,RFTFDTEN    SET END TIME                                
         OC    OHDRSTIM,OHDRSTIM    MIDNIGHT IS 2400                            
         BNZ   *+10                                                             
         MVC   OHDRSTIM,=H'2400'                                                
         OC    OHDRETIM,OHDRETIM                                                
         BNZ   *+10                                                             
         MVC   OHDRETIM,=H'2400'                                                
         CLC   =C'CC',OHDRETIM                                                  
         BNE   *+10                                                             
         MVC   OHDRETIM,=AL2(200)                                               
         MVC   OHDRDYPT,RFTFDTM                                                 
         CLI   OVRFLAG,C'Y'                                                     
         BE    FHOOK60                                                          
                                                                                
                                                                                
         TM    SELWPROF,X'02'                    SET PRINTCIPAL DAYPART         
         BNO   FHOOK40                                                          
         LA    RF,TMPDYPT                                                       
                                                                                
FHOOK38  CLI   0(RF),0                                                          
         BE    FHOOK40                                                          
         CLC   0(1,RF),RFTFDPTS                                                 
         BNE   *+14                                                             
         MVC   OHDRDYPT,0(RF)                                                   
         B     FHOOK40                                                          
         AHI   RF,1                                                             
         B     FHOOK38                                                          
                                                                                
FHOOK40  MVC   OHDRPRG(L'RFTFPGMS),RFTFPGMS                                     
         LA    RF,RFTFPGMS+L'RFTFPGMS                                           
         CLC   0(L'RFTFPGMS,RF),SPACES                                          
         BNH   FHOOK46                                                          
         LA    RE,OHDRPRG+L'RFTFPGMS-1       FIND LAST CHAR OF LINE             
FHOOK42  CLI   0(RE),X'40'                                                      
         BH    FHOOK43                                                          
         SHI   RE,1                                                             
         B     FHOOK42                                                          
FHOOK43  AHI   RE,2                                                             
* COUNT SPACES BEGINNING OF NEXT LINE                                           
         LA    R1,L'RFTFPGMS                                                    
FHOOK44  CLI   0(RF),X'40'                                                      
         BH    FHOOK45                                                          
         AHI   RF,1                                                             
         SHI   R1,1                                                             
         B     FHOOK44                                                          
FHOOK45  SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)                                                    
FHOOK46  MVC   OHDRINVN,RFTFINV                                                 
         GOTO1 VDATCON,DMCB,(2,RFTFEFST),(5,OHDREFDT)                           
         OC    RFTFEFEN,RFTFEFEN                                                
         BZ    FHOOK60                                                          
         MVI   OHDREFDT+8,C'-'                                                  
         GOTO1 VDATCON,DMCB,(2,RFTFEFEN),(5,OHDREFDT+9)                         
FHOOK60  MVC   OHDRNUM,=X'0001'                                                 
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
FHOOK80  DS    0H                                                               
FHOOKX   J     EXITY                                                            
         DROP  R5                                                               
                                                                                
**********************************************************************          
*                                                                    *          
*     PROCESS PROGRAM HEADER INFORMATION                             *          
*                                                                    *          
**********************************************************************          
PAVHDR   NTR1                                                                   
         XC    TMPUINDX,TMPUINDX                                                
         MVI   DBSRC,C'N'                                                       
         MVI   DBMED,C'T'                                                       
         MVI   RECALFLG,C'N'                                                    
                                                                                
         MVI   DMCB,5                             5 BYTES KEY AND REC           
         MVC   DMCB+1(2),=X'0005'                                               
         MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
         MVI   HDRFLAG,C'Y'                       SET DOING HEADER FLAG         
PAVHDR05 LA    RE,DEMODEMS                       JUST FUDGE IN ONE DEMO         
         LA    R5,DMHDVALS                                                      
         USING DMHDVALS,R5                                                      
         NI    MISCFLG1,X'FF'-MF1WHHD            ASSUME NO WKLY DEMOS           
PAVHDR07 MVC   0(4,RE),=X'00D901FF'              3 BYTE DEMO CODE               
                                                                                
PAVHDR10 L     R5,=A(DBLOCK1-WORKD)                GET STATION                  
         LA    R5,WORKD(R5)                                                     
         USING DBLOCK,R5                                                        
         SR    R4,R4                                                            
         ICM   R4,7,ASTAT                                                       
         LA    R2,1                                                             
         TM    STAIND,LQ_TSINQ                                                  
         BNO   PAVHDR20                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
         J     PAVHDR30                                                         
PAVHDR20 TM    STAIND,LQ_TLSTQ                                                  
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
PAVHDR30 XC    ALFMKTS,ALFMKTS                                                  
         XC    DBSELALF,DBSELALF                                                
         ST    R4,MYSTAPTR                                                      
                                                                                
         L     R4,MYSTAPTR                                                      
         USING PAVHDRD,R4                                                       
PAVHDR40 XC    DUMSTATH,DUMSTATH                                                
         XC    DUMSTAT,DUMSTAT                                                  
         ZIC   RE,PAVSTALN                       LENGTH OF INPUT STRING         
         STC   RE,DUMSTATH+5                                                    
         MVI   DUMSTATH,L'DUMSTATH+L'DUMSTAT                                    
         ZIC   RE,PAVSTALN                       LENGTH OF INPUT STRING         
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMSTAT(0),PAVSTAT                                               
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMSTATH,(R0),C',=,/'                              
         L     R1,AIO1                                                          
         USING SCANBLKD,R1                                                      
         MVC   DBSELSTA,SC1STFLD                                                
         OC    DBSELSTA(5),=C'     '                                            
                                                                                
PAVHDR43 CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   TMPSTA,DBSELSTA                                                  
                                                                                
PAVHDR44 MVC   PAVSTAT,DUMSTAT                                                  
         MVC   ALFMKTS,SC2NDFLD                                                 
                                                                                
         LA    RF,SC2NDFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC2NDLEN                                                      
         OR    RF,RF                                                            
         BZ    PAVHDR45                                                         
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
         GOTOR (#VALAMKT,AVALAMKT),DMCB                                         
         XC    SPILLMKT,SPILLMKT                                                
         OC    WORK(2),WORK                      BINARY MKT RETURNED?           
         BZ    PAVHDR45                          IF SO MKT IS NUMERIC           
         MVC   SPILLMKT,WORK                                                    
         XC    ALFMKTS,ALFMKTS                                                  
         B     PAVHDR50                                                         
         DROP  R1                                                               
PAVHDR45 OC    ALFMKTS,ALFMKTS                                                  
         BZ    PAVHDR50                                                         
         MVI   DBFUNCT,DBCNVA2N                  TRANSLATE ALPHA TO             
                                                                                
         OC    ALFMKTS,=X'404040'                NUMERIC MARKET                 
         MVC   DBSELALF,ALFMKTS                                                 
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                         ANY ERRORS?                    
         BNE   *+10                              NO, SET NUMERIC MKT            
         MVC   SPILLMKT,DBSELRMK                 AS SPILL                       
PAVHDR50 XC    DBSELRMK,DBSELRMK                 CLEAR FIELD IN CASE            
         MVI   DBERROR,0                         'TWAS FUDGED                   
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELMK,SPILLMKT                                                 
         MVC   TMPDYTIM(L'PAVDYTM),PAVDYTM                                      
PAVHDR80 MVC   TMPBOOK,PAVBOOK                   BOOK                           
         MVC   TMPBKTYP,PAVBKTY                  BOOKTYPE                       
         MVC   TMPBKWK,PAVWEEK                   WEEK SPECIFIED                 
         CLI   PAVWEEK,C'W'                      IF ALL WEEKS WANTED            
         BNE   PAVHDR81                          DONT STRIP HIGH ORDER          
         MVI   TMPBKWK,1                                                        
         B     PAVHDR82                                                         
PAVHDR81 NI    TMPBKWK,X'0F'                     NIBBLE                         
PAVHDR82 MVC   INPFIL,=C'PAV'                    FILE                           
         MVI   TMPBEST,C'A'                      SET DBBEST TO BEST             
         GOTOR =A(PAVLOOK),RR=SRVRRELO                                          
PAVHDR86 DS    0H                                                               
         CLI   PAVWEEK,C'W'                                                     
         BNE   PAVHDR88                                                         
         ZIC   RE,TMPBKWK                                                       
         AHI   RE,1                                                             
         STC   RE,TMPBKWK                                                       
         CLI   TMPBKWK,5                                                        
         BL    PAVHDR82                                                         
         MVI   TMPBKWK,1                         RESET FOR NEXT FILE            
                                                                                
*  DO NEXT ENTRY  OF STATION/DYTIME/BOOK                                        
                                                                                
PAVHDR88 L     R4,MYSTAPTR                                                      
         AHI   R4,PAVHDRL                                                       
         ST    R4,MYSTAPTR                                                      
         MVI   DMCB,5                             5 BYTES KEY AND REC           
         MVC   DMCB+1(2),=X'0005'                 CLEAR TSAR BUFFS              
         MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
         BCT   R2,PAVHDR30                                                      
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
**********************************************************************          
*                                                                    *          
*     START PROGRAM DETAILS                                          *          
*                                                                    *          
**********************************************************************          
RUNPGDET NTR1  BASE=*                                                           
         MVI   DBSRC,C'N'                                                       
         MVI   DBMED,C'T'                                                       
*                                                                               
*                                                                               
         MVI   RECALFLG,C'N'                     SET NOT RECALC                 
                                                                                
*    FOR OFF-LINE NO RELO                                                       
         TM    OFFLFLAG,LP_FOFFL                                                
         JZ    RUNPG02                                                          
         XC    SRVRRELO,SRVRRELO                                                
         GOTOR INITBUFF                                                         
         J     RUNPG03                                                          
                                                                                
RUNPG02  MVI   DMCB,2                                                           
         MVC   DMCB+1(2),=X'0002'                                               
         MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
RUNPG03  XC    WORK,WORK                                                        
         MVC   WORK+0(4),=C'S01W'                                               
         MVC   WORK+4(2),AGYALPH                                                
         MVI   WORK+6,C'T'                                                      
         GOTOR VGETPROF,DMCB,WORK,PROF1W,VDATAMGR                               
         MVI   HDRFLAG,C'N'                      INIT FLAG- NOT HEADER          
*                                                                               
* SEE IF WE WANT 2 DECIMAL                                                      
         SR    R4,R4                             GET DECIMAL                    
         XC    OPTDEC,OPTDEC                                                    
         ICM   R4,7,ADECIMAL                                                    
         BZ    *+14                                                             
         LA    R4,LW_DATA1-LW_D(R4)                                             
         MVC   OPTDEC,0(R4)                                                     
                                                                                
                                                                                
         SR    R4,R4                             GET DEMO                       
         ICM   R4,7,ADEMO                                                       
         LA    R2,1                                                             
         TM    DEMOIND,LQ_TSINQ                                                 
         BNO   RUNPG04                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     RUNPG05                                                          
RUNPG04  TM    DEMOIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
RUNPG05  LA    R6,DEMODEMS                                                      
         STCM  R2,3,NUMDEMO                                                     
         XC    NUMDEMO2,NUMDEMO2                                                
*                                                                               
         CHI   R2,14                                                            
         BNH   RUNPG06                                                          
         CLI   OVSYS,8                         REP COMPARGRAPGH ONLY            
         BE    *+14                            SUPPORTS MAX 14 DEMOS            
         CLC   VERSNUM,=AL4(SPVER26)           IF SPOT DESKTOP VERSION          
         BNL   RUNPG5C                         < 2.6 THEN ONLY PROCESS          
         LA    R2,14                           UP TO 14 DEMOS                   
*                                                                               
RUNPG5C  LR    RE,R2                                                            
         SHI   RE,14                            REMAINING # OF DEMOS            
         STCM  RE,3,NUMDEMO2                                                    
         LA    R2,14                            MAX 14 AT A TIME                
         STCM  R2,3,NUMDEMO                                                     
RUNPG06  DS    0H                                                               
*                                                                               
*                                                                               
                                                                                
         LA    R5,DMHDVALS                                                      
         USING DMHDVALS,R5                                                      
RUNPG07  MVC   0(3,R6),0(R4)                     3 BYTE DEMO CODE               
         MVC   3(3,R6),0(R4)                                                    
         MVI   4(R6),C'S'                        TSA IMPRESSION                 
         CLI   1(R4),C'I'                        IF REQUEST IMPRESSION          
         BNE   *+8                                                              
         MVI   4(R6),C'X'                                                       
         MVC   6(3,R6),0(R4)                                                    
         MVI   7(R6),C'P'                                                       
         CLI   1(R4),C'I'                        TSA IMPRESSION                 
         BNE   *+8                                                              
         MVI   7(R6),C'Q'                                                       
         MVC   DMHDDEM,4(R4)                                                    
* FOR PROPOSER  DONT REPLACE HOMES STRING                                       
         CLC   VERSNUM,=AL4(PPVER30)                                            
         BNL   RUNPG7A                                                          
*                                                                               
         CLC   =C'HOMES',DMHDDEM+1                                              
         BNE   *+10                                                             
         MVC   DMHDDEM+1(5),=C'HH   '                                           
                                                                                
         CLC   =C'HOMES',DMHDDEM                                                
         BNE   *+10                                                             
         MVC   DMHDDEM(5),=C'HH   '                                             
                                                                                
RUNPG7A  MVC   DMCB+0(1),1(R4)                   MOD FOR REQUESTED DEMO         
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
*                                                                               
* SPOT SYSTEM - IF VERSION 2.6 OR HIGHER THEN PASS BACK SHARE AND PUTS          
* BACK AUTOMATICALLY                                                            
         ST    R4,DEMOPTR                                                       
         CLI   OVSYS,8                                                          
         BE    RUNPG7C                                                          
         CLC   VERSNUM,=AL4(SPVER26)                                            
         BNL   RUNPG7C                                                          
         AHI   R6,3                                                             
         AHI   R4,24                                                            
         B     RUNPG7D                                                          
                                                                                
*                                                                               
RUNPG7C  MVC   DMHDDEM(5),=C'Share'                                             
         MVC   DMCB+0(1),4(R6)                   MOD FOR SHARE                  
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
         MVC   DMHDDEM(7),=C'HUT/PUT'                                           
         MVC   DMCB+0(1),7(R6)                   MOD FOR HPT                    
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R4,24                                                            
         ST    R4,DEMOPTR                                                       
         AHI   R6,9                                                             
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
         ZICM  R0,NUMDEMO,(3)                                                   
         AHI   R0,2                                                             
         STCM  R0,3,NUMDEMO                                                     
RUNPG7D  BCT   R2,RUNPG07                                                       
         MVI   0(R6),X'FF'                                                      
         MVI   DMHDSEND,C'Y'                                                    
         MVC   DMHDNUM,NUMDEMO                                                  
         DROP  R5                                                               
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
         SR    R4,R4                             PROCESS FILES                  
         ICM   R4,7,AFILE                                                       
         LA    R2,1                                                             
         TM    FILEIND,LQ_TSINQ                                                 
         BNO   RUNPG08                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         ST    R4,FILBKPTR                                                      
         J     RUNPG09                                                          
RUNPG08  TM    FILEIND,LQ_TLSTQ                                                 
         JO    *+8                               IF NO BOOKS AT ALL             
         J     EXITY                             JUST EXIT                      
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,FILBKPTR                                                      
RUNPG09  DS    0H                                                               
         ST    R2,TOTALFBK                                                      
                                                                                
                                                                                
RUNPG15  XC    ROWCOUNT,ROWCOUNT                INITIALIZE COUNTS               
         XC    FBKCOUNT,FBKCOUNT                                                
                                                                                
RUNPG17  L     R5,=A(DBLOCK1-WORKD)             GET STATION                     
         LA    R5,WORKD(R5)                                                     
         USING DBLOCK,R5                                                        
         SR    R4,R4                                                            
         ICM   R4,7,ASTAT                                                       
         LA    R2,1                                                             
         TM    STAIND,LQ_TSINQ                                                  
         BNO   RUNPG20                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
         J     RUNPG30                                                          
RUNPG20  TM    STAIND,LQ_TLSTQ                                                  
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
RUNPG30  XC    ALFMKTS,ALFMKTS                                                  
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         XC    DBSELALF,DBSELALF                                                
         ST    R4,MYSTAPTR                                                      
         L     R4,MYSTAPTR                                                      
         USING STATTABD,R4                                                      
                                                                                
         L     R2,FILBKPTR                                                      
         USING FILBKD,R2                                                        
RUNPG35  MVC   TMPSTA,STATION                                                   
         MVC   TMPDYTIM(6),STADYTM                                              
         MVC   TOTALROW,STATNROW                                                
         MVC   TMPINVNM,STAINVN                 INVENTORY NUMBER                
         MVC   TMPEFFDT,STAEFFDT                EFFECTIVE DATE                  
         MVC   TMPSYSC,STASYSC                                                  
*  CHECK # OF ROW FOR THIS STAT/DAYTIME TO BE ZERO                              
         OC    TOTALROW,TOTALROW                                                
         BNZ   *+14                                                             
         MVC   ROWCOUNT,=X'FFFFFFFF'            FUDGE TO -1 FOR LATER           
         B     RUNPG170                         COMPARISION                     
                                                                                
         MVC   TOTALROW,STATNROW                                                
RUNPG40  DS    0H                                                               
         XC    DUMSTATH,DUMSTATH                                                
         XC    DUMSTAT,DUMSTAT                                                  
         ZIC   RE,STALEN                         LENGTH OF INPUT STRING         
         STC   RE,DUMSTATH+5                                                    
         MVI   DUMSTATH,L'DUMSTATH+L'DUMSTAT                                    
         ZIC   RE,STALEN                         LENGTH OF INPUT STRING         
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMSTAT(0),STATION                                               
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMSTATH,(R0),C',=,/'                              
         L     R1,AIO1                                                          
         USING SCANBLKD,R1                                                      
         MVC   DBSELSTA,SC1STFLD                                                
         OC    DBSELSTA(5),=C'     '                                            
         CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   TMPSTA,DBSELSTA                                                  
         MVC   STATION,DUMSTAT                                                  
         MVC   ALFMKTS,SC2NDFLD                                                 
                                                                                
         LA    RF,SC2NDFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC2NDLEN                                                      
         OR    RF,RF                                                            
         BZ    RUNPG45                                                          
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
                                                                                
         GOTOR (#VALAMKT,AVALAMKT),DMCB                                         
         XC    SPILLMKT,SPILLMKT                                                
         OC    WORK(2),WORK                      BINARY MKT RETURNED?           
         BZ    RUNPG45                           IF SO MKT IS NUMERIC           
         MVC   SPILLMKT,WORK                                                    
         XC    ALFMKTS,ALFMKTS                                                  
         B     RUNPG50                                                          
                                                                                
         DROP  R1                                                               
RUNPG45  OC    ALFMKTS,ALFMKTS                                                  
         BZ    RUNPG50                                                          
         MVI   DBFUNCT,DBCNVA2N                  TRANSLATE ALPHA TO             
         MVC   DBFILE,=C'TP '                                                   
         OC    ALFMKTS,=X'404040'                NUMERIC MARKET                 
         MVC   DBSELALF,ALFMKTS                                                 
         MVC   DBCOMFCS,ACOMFACS                 A(COMFACS)                     
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                         ANY ERRORS?                    
         BNE   *+10                              NO, SET NUMERIC MKT            
         MVC   SPILLMKT,DBSELRMK                 AS SPILL                       
RUNPG50  XC    DBSELRMK,DBSELRMK                 CLEAR FIELD IN CASE            
         MVI   DBERROR,0                         'TWAS FUDGED                   
                                                                                
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELMK,SPILLMKT                                                 
                                                                                
         MVC   TMPDYTIM(L'STADYTM),STADYTM                                      
                                                                                
*    NOW FIGURE OUT WHICH ENTRY OF THE FILE BK ENTRY WE SHOULD PROCESS          
*    BASED ON OUR COUNTERS                                                      
*    R2 POINTS TO CURRENT FIL/BK TABLE ENTRY                                    
         LA    R1,FILBKL                                                        
         SR    RE,RE                                                            
         L     RF,ROWCOUNT                       CURRENT FILBK TABLE            
         MR    RE,R1                             ENTRY POINTED TO FOR           
         LR    RE,R2                             STATION POINTED TO             
         AR    R2,RF                             FILEBK ENTRY                   
RUNPG80  MVC   TMPBOOK,FBKBOOK                   BOOK                           
         MVC   TMPBKTYP,FBKBKTY                  BOOKTYPE                       
         MVC   TMPBKWK,FBKWEEK                   WEEK SPECIFIED                 
         MVC   TMPMBKS,FBKMULBK                  MULTIBOOK AVERAGE BKS          
         MVC   TMPUINDX,FBKUPIND                 UPGRADE INDEX NUMBER           
         MVI   TMPLATBN,0                                                       
         MVI   TMPLATBT,0                                                       
         MVI   NUMLATBK,0                                                       
*                                                                               
         CLC   =X'404040',FBKFILE                                               
         JNL   EXITY                                                            
*                                                                               
                                                                                
         CLI   DBMED,C'T'                        FOR USTV ONLY                  
         BNE   RUNPG80B                          IF ITS SPOT DESKTOP'S          
         CLI   FBKLATBN,0                        LATEST BOOKS FEATURE           
         BE    RUNPG80B                          SET THE BOOKTYPE TO            
         MVC   TMPLATBN,FBKLATBN                                                
         CLI   FBKLATBT,0                        LOOK FOR LATEST BOOK           
         BE    RUNPG80C                                                         
         MVC   DBBTYPE,FBKLATBT                                                 
         MVC   TMPLATBT,FBKLATBT                                                
         B     RUNPG80C                                                         
RUNPG80B DS    0X                                                               
* ---- GET BOOK FOR LATEST BOOK ONLY                                            
                                                                                
         MVI   LATFLAG,C'N'                                                     
         OC    TMPBOOK,TMPBOOK                                                  
         BNZ   RUNPG80D                                                         
         OC    TMPUINDX,TMPUINDX                                                
         BNZ   RUNPG82                                                          
RUNPG80C MVI   LATFLAG,C'Y'                      SET LASTEST FLAG               
         L     R0,AIO1                                                          
         ST    R0,DBAREC                                                        
         MVC   DBFILE,=C'TP '                    FILE                           
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVI   DBFUNCT,DBGETTLB                                                 
         MVC   DBSELMK,SPILLMKT                                                 
         XC    DBSELBK,DBSELBK                                                  
         CLI   DBSRC,C'N'                                                       
         BNE   *+18                                                             
         OC    TMPSYSC,TMPSYSC                                                  
         BZ    *+8                                                              
         MVI   DBBTYPE,C'W'                                                     
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                         SHOULD BE ABLE TO              
         BNE   EXITY                                                            
*                                                                               
         CLI   TMPLATBN,0            SPOTDESKTOPS- LATEST BOOKS FEATURE         
         BE    *+8                   IS NOT OUR TRADITIONAL LATEST              
         MVI   LATFLAG,C'N'          LOOKUP- FLAG IT AS SO                      
*                                                                               
         XC    TMPBOOK,TMPBOOK                                                  
         MVC   TMPBOOK+1(2),DBACTBK                                             
         XC    TMPBKTYP,TMPBKTYP                                                
         XC    TMPBKWK,TMPBKWK                                                  
                                                                                
         CLI   TMPLATBN,0            THE NUMBER OF LATEST BOOKS FEATURE         
         BE    RUNPG80D              IS NOT OUR TRADITIONAL LATEST              
         MVI   LATFLAG,C'N'          LOOKUP- FLAG IT AS SO                      
                                                                                
         MVC   SVLATBK,DBACTBK                                                  
         MVC   SVLATBT,TMPLATBT                                                 
         XC    TMPBOOK,TMPBOOK                                                  
         MVC   TMPBOOK+1(2),SVLATBK                                             
         MVC   TMPBKTYP,TMPLATBT                                                
                                                                                
RUNPG80D MVC   INPWEEK,TMPBKWK                                                  
         CLI   FBKWEEK,C'W'                      IF ALL WEEKS WANTED            
         BNE   RUNPG81                           DONT STRIP HIGH ORDER          
         MVI   TMPBKWK,1                                                        
         B     RUNPG82                                                          
RUNPG81  NI    TMPBKWK,X'0F'                     NIBBLE                         
RUNPG82  MVC   INPFIL,FBKFILE                    FILE                           
         MVI   DBMED,C'T'                        DEFAULT MED                    
         MVC   TMPFILE,=C'T4 '                                                  
         CLC   =C'TT',INPFIL                     TT DO T4 NEXT                  
         BE    *+10                              T4 DO TT NEXT                  
         MVC   TMPFILE,=C'TT '                                                  
         MVC   OUTPCID,FBKPCID                   PCID                           
         CLC   =C'PAV',INPFIL                                                   
         BNE   *+10                                                             
         MVC   TMPFILE,=C'PAV'                                                  
         MVI   TMPBEST,C'B'                      SET DBBEST TO BEST             
         CLC   =C'INV',INPFIL                                                   
         BNE   *+10                                                             
         MVC   TMPFILE,=C'INV'                                                  
         CLC   =C'WTP',INPFIL                                                   
         BNE   *+14                                                             
         MVC   TMPFILE,=C'WTP'                                                  
         MVI   DBMED,C'W'                                                       
         CLC   =C'OTP',INPFIL                                                   
         BNE   *+14                                                             
         MVC   TMPFILE,=C'OTP'                                                  
         MVI   DBMED,C'O'                                                       
         CLC   =C'OPA',INPFIL                                                   
         BNE   *+14                                                             
         MVC   TMPFILE,=C'OPA'                                                  
         MVI   DBMED,C'O'                                                       
         CLC   =C'TF',INPFIL                   FUSION                           
         BNE   *+10                                                             
         MVC   TMPFILE,INPFIL                                                   
                                                                                
                                                                                
         L     R1,ALP                            DOWNLOAD PCID                  
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
*                                                                               
         CLI   TMPLATBN,0            FOR SPOT DESKTOP FEATURE LATEST            
         BE    *+12                  BOOKS                                      
         CLI   DBACTBK,X'5A'         DONT READ PASS 1996                        
         BL    RUNPG170                                                         
*                                                                               
                                                                                
         CLI   NUMLATBK,0                       DONT UPDATE THE TOTAL           
         BNE   RUNPG88                          COUNT OF FILE/BK COMBOS         
         L     RE,FBKCOUNT                      PROCESSED IN MIDST OF           
         AHI   RE,1                             SPOT DESKTOPS LATEST            
         ST    RE,FBKCOUNT                      BOOK FEATURE                    
RUNPG88  MVI   ALLTPFLG,C'N'                     INITIALIZE  FLAG               
RUNPG90  OC    TMPMBKS,TMPMBKS                                                  
         BNZ   RUNPG97                                                          
                                                                                
RUNPG96  OC    TMPUINDX,TMPUINDX                 ANY UPGRADES??                 
         BZ    RUNPG100                                                         
         CLC   =C'PAV',INPFIL                                                   
         BE    RUNPG100                                                         
         CLC   =C'OPA',INPFIL                                                   
         BE    RUNPG100                                                         
RUNPG97  GOTOR DEMUPGD                                                          
         J     RUNPG140                                                         
                                                                                
RUNPG100 DS    0X                                                               
         CLC   =C'INV',INPFIL                                                   
         BNE   RUNPG120                                                         
         GOTOR =A(INVLOOK),RR=SRVRRELO                                          
         J     RUNPG140                                                         
RUNPG120 CLC   =C'PAV',INPFIL                                                   
         BE    *+10                                                             
         CLC   =C'OPA',INPFIL                                                   
         BNE   RUNPG130                                                         
         GOTOR =A(PAVLOOK),RR=SRVRRELO                                          
         J     RUNPG140                                                         
RUNPG130 GOTOR DEMLK,RR=SRVRRELO                                                
RUNPG140 DS    0H                                                               
         CLI   FBKWEEK,C'W'                                                     
         BNE   RUNPG150                                                         
         ZIC   RE,TMPBKWK                                                       
         AHI   RE,1                                                             
         STC   RE,TMPBKWK                                                       
         CLI   TMPBKWK,5                                                        
         BL    RUNPG120                                                         
         MVI   TMPBKWK,1                         RESET FOR NEXT FILE            
                                                                                
RUNPG150 CLC   =C'INV',INPFIL                    IF PAV SOURCE ,                
         BE    RUNPG170                                                         
         CLC   =C'PAV',INPFIL                    IF PAV SOURCE ,                
         BNE   RUNPG160                          DO DBBEST = ALL                
         CLI   TMPBEST,C'B'                                                     
         BNE   RUNPG170                                                         
         MVI   TMPBEST,C'A'                                                     
         J     RUNPG120                                                         
                                                                                
RUNPG160 CLC   INPFIL,TMPFILE                                                   
         BE    RUNPG170                                                         
         MVC   INPFIL,TMPFILE                                                   
         MVI   ALLTPFLG,C'Y'                     SET DID ALL TP FLAG            
         J     RUNPG90                                                          
                                                                                
* NOW CHECK IF STATION CHANGED. IF IT IS THE SAME JUST KEEP PROCESSING          
* SAME STAION SAME BOOK WITH THE NEXT DAYTIME                                   
* IF IT CHANGES , PROCESS THE NEXT BOOK FOR THE STATION/DAYTIME AGAIN           
                                                                                
RUNPG170 LA    RE,STATTABL                                                      
         AR    R4,RE                                                            
         LA    R1,FILBKL                                                        
         L     RF,TOTALROW                                                      
         MR    RE,R1                                                            
         L     R2,FILBKPTR                                                      
         AR    R2,RF                                                            
                                                                                
         TM    OFFLFLAG,LP_FOFFL                                                
         JZ    RUNPG180                                                         
         XC    SRVRRELO,SRVRRELO                                                
         GOTOR INITBUFF                                                         
         J     RUNPG182                                                         
                                                                                
RUNPG180 MVI   DMCB,2                            RESET TSAR BUFF                
         MVC   DMCB+1(2),=X'0002'                                               
         MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
*                                                                               
* TEST TO SEE IF WE ARE PROCESSING SPOT DESKTOP'S LATEST BOOKS FEATURE          
RUNPG182 DS    0X                                                               
         CLI   TMPLATBN,0                     NOT PROCESSING SPOT               
         JE    RUNPG186                       DESKTOP LATEST BK FEATURE         
         CLI   ANYTPFLG,C'Y'                                                    
         JE    RUNPG184                                                         
*DIDNT GET THE BOOK BACK FOR LAST READ - DONT UPDATE THE COUNT                  
*OF LATEST BOOKS WE HAVE READ - KEEP SAME COUNT AND REREAD A MONTH BACK         
         J     RUNPG185                                                         
*                                                                               
RUNPG184 ZIC   RE,NUMLATBK                                                      
         AHI   RE,1                                                             
         STC   RE,NUMLATBK                                                      
         CLC   NUMLATBK,TMPLATBN               READ ALL THE LATEST BKS          
         JNL   RUNPG186                        REQUIRED?                        
RUNPG185 XC    DMCB,DMCB                                                        
         MVC   DMCB(2),TMPBOOK+1                                                
         GOTOR =A(GETNLATB),DMCB,RR=SRVRRELO   GET NEXT LATEST BOOK             
         MVC   TMPBOOK+1(2),DMCB+4             NEXT BOOK TO READ                
         CLI   TMPBOOK+1,X'5A'                 DONT READ PASS 90 FOR            
         BL    RUNPG186                        LAST FEATURE                     
         L     R2,FILBKPTR                                                      
*    NOW FIGURE OUT WHICH ENTRY OF THE FILE BK ENTRY WE SHOULD PROCESS          
*    BASED ON OUR COUNTERS                                                      
*    R2 POINTS TO CURRENT FIL/BK TABLE ENTRY                                    
         LA    R1,FILBKL                                                        
         SR    RE,RE                                                            
         L     RF,ROWCOUNT                       CURRENT FILBK TABLE            
         MR    RE,R1                             ENTRY POINTED TO FOR           
         LR    RE,R2                             STATION POINTED TO             
         AR    R2,RF                             FILEBK ENTRY                   
         L     R4,MYSTAPTR                                                      
         J     RUNPG80B                                                         
*                                                                               
*                                                                               
*                                                                               
* TEST TO SEE OF WE HAVE PROCESS ALL THE ROWS FOR A COLUMN                      
* PROCESS ALL FIL/BKS FOR EACH STATION/DAYTIME COMBINATION                      
RUNPG186 L     RE,ROWCOUNT                                                      
         AHI   RE,1                                                             
         ST    RE,ROWCOUNT                                                      
         CLC   ROWCOUNT,TOTALROW                                                
         BE    RUNPG190                                                         
         L     R4,MYSTAPTR                       POINT TO BACK TO STAT          
         L     R2,FILBKPTR                       AND PROCESS NEXT BK            
         B     RUNPG30                                                          
*  IF WE PROCESSED THE ALL ROWS FOR THE STATION THEN PROCESS NEXT               
*  STATION UNTIL ALL THE FILE BKS ARE PROCESSED                                 
RUNPG190 ST    R4,MYSTAPTR                        KEEP POINTED TO NEXT          
         ST    R2,FILBKPTR                       STATION AND FIL/BK             
         XC    ROWCOUNT,ROWCOUNT                 ENTRY ASSIOCAITED WITH         
         CLC   FBKCOUNT,TOTALFBK                 IT AND DO ALL ROWS             
         BNE   RUNPG30                           FOT THAT STATION               
*        BE    EXITY                                                            
*        B     RUNPG30                                                          
*                                                                               
         ZICM  R2,NUMDEMO2,(3)                ANY REMAINING DEMOS?              
         L     R4,DEMOPTR                                                       
         LR    R2,R2                                                            
         BNZ   RUNPG05                                                          
         DROP  R2                                                               
*                                                                               
                                                                                
         J     EXITY                                                            
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
YESQ     EQU   C'Y'                                                             
SPSYSQ   EQU   2                                                                
RESYSQ   EQU   8                                                                
NINVINDQ EQU   RFTCHDRQ+RFTCDEMQ+RFTCSLVQ+RFTCFTNQ+RFTCRTEQ+RFTCRNWQ+RF+        
               TCTXTQ                                                           
EXITN    CR    RB,RE                                                            
EXITY    CR    RE,RE                                                            
EXIT     XIT1  ,                                                                
                                                                                
***********************************************************************         
* LIST OF MEDIA FILES TO OPEN IN ALL SYSTEMS                          *         
***********************************************************************         
*                                                                               
FILESPOT DS    0X                                ** FILE INFO **                
         DC    C'SPOT   '                        SYSTEM NAME FOR OPEN           
                                                                                
         DC    C'NSPTDIR '                                                      
         DC    C'NSPTFIL '                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NDEMDIRN'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
*                                                                               
FILESREP DS    0X                                ** FILE INFO **                
         DC    C'REP    '                        SYSTEM NAME FOR OPEN           
                                                                                
         DC    C'NREPDIR '                                                      
         DC    C'NREPFIL '                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NDEMDIRN'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
FACS     DS    0X                                                               
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,VDEMAND-SYSADDR)               
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,VDEMOUT-SYSADDR)               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,VDEMAINT-SYSADDR)            
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD,0)                              
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDEFINE),AL2(CDEFINE-COMFACSD,VDEFINE-SYSADDR)               
         DC    AL1(QDEMTABS),AL2(CDEMTABS-COMFACSD,0)                           
         DC    AL1(0),AL2(CDATAMGR-COMFACSD,VDATAMGR-SYSADDR)                   
         DC    AL1(0),AL2(CDATAMGR-COMFACSD,VDATAMGR-SYSADDR)                   
**       DC    AL1(0),AL2(CADDAY-COMFACSD,VADDAY-SYSADDR)                       
**       DC    AL1(0),AL2(CGETDAY-COMFACSD,VGETDAY-SYSADDR)                     
FACSX    DC    AL1(0)                                                           
         EJECT                                                                  
                                                                                
         DROP  RB                                                               
                                                                                
**********************************************************************          
*     CANADIAN LOOKUP FOR  SPOTDESKTOP                               *          
**********************************************************************          
CANLOOK  NTR1  BASE=*                                                           
         ICM   R4,7,ARSTAMKT            RATING SERVICE/STATION/AMKT             
         LA    R2,1                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
         STCM  R2,3,NUMSTAT                                                     
                                                                                
         USING CANRSRVD,R4                                                      
CANLK02  MVC   NMAMKTS,CANNMKTS                  NUMBER OF ALPHA MKTS           
                                                                                
         SR    R5,R5                             GET DEMO                       
         ICM   R5,7,ADEMO                                                       
         LA    R2,1                                                             
         TM    DEMOIND,LQ_TSINQ                                                 
         ICM   R2,3,LW_NUMN-LW_D(R5)                                            
         LA    R5,LW_DATA2-LW_D(R5)                                             
         LA    R6,DEMODEMS                                                      
         STCM  R2,3,NUMDEMO                                                     
                                                                                
CANLK05  MVC   0(3,R6),0(R5)                                                    
         AHI   R6,3                                                             
         AHI   R5,3                                                             
         BCT   R2,CANLK05                                                       
         MVI   0(R6),X'FF'                        EOL                           
                                                                                
                                                                                
CANLK06  L     R5,=A(DBLOCK1-WORKD)                                             
         LA    R5,WORKD(R5)                                                     
         USING SPDEMLKD,R5                                                      
                                                                                
* PASS DOWN THE DEMO HEADER RECORD ONCE                                         
                                                                                
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
* SET RESEARCH APPLICATION FLAG                                                 
         LA    RE,SYSCEXT                                                       
         USING SPLKXTD,RE                                                       
         XC    SPLKXTND,SPLKXTND                                                
         OI    SPXTFLAG,SPXTRAPP       SET CALLING RESEARCH APPL FLAG           
         ST    RE,SPLKXTND                                                      
*                                                                               
         L     R0,AIO1                                                          
         ST    R0,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         LA    R0,DEMODEMS                                                      
                                                                                
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL                                                      
         LA    R0,CANLKHK                                                       
         ST    R0,SPLKHOOK                                                      
         MVC   MYPROF1W,CANPROF                  THIS IS TO INCORPORATE         
         MVI   MYPROF1W+5,C'I'                   PRECISION PROFILE IF           
                                                                                
*   FUDGE THE PROFILES FOR MY TESTING                                           
**       XC    MYPROF1W,MYPROF1W                                                
**       MVC   MYPROF1W(10),=X'D5E85CD55AC9D5E8D4D5'                            
*                                                                               
         LA    R0,MYPROF1W                       CONNECTED TO REP               
         ST    R0,SPLKA1W                        ALWAYS DMA=I                   
                                                                                
         MVI   SPLKFIL,C'T'                                                     
                                                                                
         MVI   SPLKMED,C'C'                      ALWAYS C FOR NOW               
         MVI   SPLKSRC,C'A'                      0=CSI  1=BBM                   
         CLI   CANRSRV,C'0'                                                     
         BNE   *+8                                                              
         MVI   SPLKSRC,C'N'                                                     
                                                                                
         MVC   SPLKAGY,AGYALPH                                                  
         MVC   SPLKDBK,CANBOOK                                                  
                                                                                
                                                                                
         MVC   SPLKSTA,CANSTAT                                                  
         MVI   SPLKSTA+4,C'T'                                                   
                                                                                
         MVC   SPLKDAY,CANDAY                                                   
         MVC   SPLKSTIM,CANSTIM                                                 
         MVC   SPLKETIM,CANETIM                                                 
                                                                                
         MVI   SPLKSVI,X'FF'                                                    
         MVI   SPLKBEST,C'M'                                                    
         LA    RE,CANAMKTS                                                      
         ST    RE,AMKTPTR                                                       
         MVC   NMAMKTS,CANNMKTS                  NUMBER OF ALPHA MKTS           
         MVC   SPLKALF,0(RE)                                                    
         AHI   RE,3                                                             
         ST    RE,AMKTPTR                                                       
         ZIC   RE,NMAMKTS                                                       
         SHI   RE,1                                                             
         STC   RE,NMAMKTS                                                       
CANLK20  MVC   SPLKUID,USERID                                                   
         CLI   CAN2DEC,C'Y'                                                     
         BNE   CANLK21                                                          
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
                                                                                
         ICM   R1,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
         MVI   DBXTSCTL,C'2'                                                    
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         MVI   TMPWHOLE,WHOLERTG                                                
         OI    TMPWHOLE,WHOLESHR                                                
         OI    TMPWHOLE,WHOLEPUT                                                
         TM    TMPWHOLE,WHOLERTG           ROUND RATINGS TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTRC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLESHR           ROUND SHARES  TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTSC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLEPUT           ROUND PUTS    TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTPC2T,C'Y'                YEP                                 
         DROP  R1                                                               
                                                                                
CANLK21  MVI   ANYTPFLG,C'N'                      ASSUME NO DATA                
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD)                                   
         CLI   0(R1),X'80'                                                      
         BE    CANLK33                                                          
                                                                                
CANLK22  LA    RF,DROPSVI2                                                      
         BASR  RE,RF                                                            
                                                                                
CANLK33  MVC   CANPROG(L'SPLKPRG),SPLKPRG                                       
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    CANPROG,CANPROG                                                  
***      MVC   CANPROG(15),=C'NO DOMINANT PGM'                                  
*                                                                               
         L     R3,SPLKDBLK                                                      
         USING DBLOCKD,R3                                                       
         OC    DBACTBK,DBACTBK                                                  
         BZ    CALK30                                                           
         MVC   TMPBOOK+1(L'DBACTBK),DBACTBK                                     
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
CALK23   MVC   CANLKBK(L'MYBK),MYBK                                             
         DROP  R3                                                               
                                                                                
CALK30   LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
CALK40   MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    SDEMOS,SDEMOS                                                    
*                                                                               
         AHI   R3,SDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,CALK40                                                        
                                                                                
         MVC   SDEMNUM,NUMDEMO                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         DROP  R3                                                               
                                                                                
*  NOW PROCESS ALL THE ALPHA MARKETS                                            
         CLI   NMAMKTS,0                                                        
         JE    CANLK80                                                          
         L     RE,AMKTPTR                                                       
         MVC   SPLKALF,0(RE)                                                    
         AHI   RE,3                                                             
         ST    RE,AMKTPTR                                                       
         ZIC   RE,NMAMKTS                                                       
         SHI   RE,1                                                             
         STC   RE,NMAMKTS                                                       
         XC    SPLKSPL,SPLKSPL                                                  
         XC    THISDEMS,THISDEMS                                                
         J     CANLK21                                                          
* GO BACK AND PROCESS REST OF RATING SERVICE/STATIONS                           
CANLK80  XC    SPLKSPL,SPLKSPL                                                  
         XC    SPLKALF,SPLKALF                                                  
         AHI   R4,CANRSRVL                                                      
         ZICM  RE,NUMSTAT,(3)                                                   
         SHI   RE,1                                                             
         STCM  RE,3,NUMSTAT                                                     
         OC    NUMSTAT,NUMSTAT                   IF NO MORE STATION             
         JZ    CANLKX                                                           
         MVC   NMAMKTS,CANNMKTS                  NUMBER OF ALPHA MKTS           
         XC    THISDEMS,THISDEMS                                                
         J     CANLK06                                                          
*                                                                               
CANLKX   J     EXITY                                                            
                                                                                
***********************************************************************         
* ROUTINE TO PROCESS VALUES RETURNED FROM SPDEMLK                               
* BE CAREFUL NOT TO CLOBBER DMCB, SINCE SPDEMLK'S CALLER DEPENDS ON IT          
***********************************************************************         
CANLKHK  NTR1                                                                   
         MVC   SAVEDMCB(24),DMCB                                                
         L     R4,SPLKDBLK                                                      
         USING DBLOCKD,R4                                                       
         TM    SPLKDAY,X'90'                       CANT HAVE VAR,AGN            
         JO    EXITY                               FOR TT,T4                    
                                                                                
         MVI   ANYTPFLG,C'Y'                                                    
         LA    RF,DROPSVI2                                                      
         BASR  RE,RF                                                            
         CLC   DBFACTOR,=H'1'                                                   
         BE    CALKHK30                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         LH    R1,DBFACTOR                                                      
         LA    R2,THISDEMS                                                      
CALKHK20 L     RF,0(R2)                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,CALKHK20                                                      
                                                                                
CALKHK30 GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
         J     EXITY                                                            
                                                                                
DROPSVI2 LA    R1,THISDEMS                                                      
         LA    RF,8(R1)                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         MVC   4(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R4,R5                                                         
***********************************************************************         
* ROUTINE INITIALIZES BUFFERIN CALL                                   *         
***********************************************************************         
INITBUFF NTR1  BASE=*                                                           
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         DROP  R1                                                               
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         DROP  RF                                                               
         USING RUNFACSD,RE         R6=A(RUNFACS)                                
         L     RF,RBUFFRIN                                                      
         DROP  RE                                                               
         GOTOR (RF),DMCB,('BUFFAINI',ADEMBUFF),WORK,ACOMFACS                    
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* ROUTINE ADDS RECORD TO ARRAY IN BUFFER                              *         
***********************************************************************         
PUTBUFF  NTR1  BASE=*                                                           
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         DROP  R1                                                               
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         DROP  RF                                                               
         USING RUNFACSD,RE         R6=A(RUNFACS)                                
         L     RF,RBUFFRIN                                                      
         DROP  RE                                                               
         GOTOR (RF),DMCB,('BUFFAPUT',ADEMBUFF),WORK,ACOMFACS                    
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* ROUTINE CHECKS IF RECORD EXIST IN BUFFER                            *         
***********************************************************************         
GETBUFF  NTR1  BASE=*                                                           
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         DROP  R1                                                               
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         DROP  RF                                                               
         USING RUNFACSD,RE         R6=A(RUNFACS)                                
         L     RF,RBUFFRIN                                                      
         DROP  RE                                                               
         MVC   WORK2(2),WORK                                                    
         GOTOR (RF),DMCB,('BUFFARDH',ADEMBUFF),WORK,ACOMFACS                    
         CLC   WORK(2),WORK2                                                    
         BE    GETBUF10                                                         
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
                                                                                
GETBUF10 CLI   BUFFERRS-BUFFPARM(R1),0        SET  CONDITION CODE               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* ROUTINE TO GET DEMO PRECISION                                                 
* ENTRY   DMCB+0(1)=MODIFIER TO LOOK UP                                         
* EXIT    DMCB+4(1)=PRECISION                                                   
***********************************************************************         
GETPREC  NTR1  BASE=*                                                           
         L     RE,=A(PRECTAB2)                                                  
         CLI   OPTDEC,C'2'                                                      
         BE    *+8                                                              
         L     RE,=A(PRECTAB)                                                   
         A     RE,SRVRRELO                                                      
GETP10   CLI   0(RE),X'FF'                     DEMO MOD BETTER BE IN            
         BNE   *+6                             TABLE OR SOMETHING IS            
         DC    H'0'                            WRONG                            
         CLC   0(1,RE),DMCB+0                                                   
         BE    GETP20                                                           
         AHI   RE,L'PRECTAB                                                     
         J     GETP10                                                           
GETP20   MVC   DMCB+4(1),1(RE)                                                  
GETPRCY  J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
PRECTAB  DS    0CL2                             DEMO PRECISION TABLE            
         DC    C'R',X'01'                       DEMOMOD/PRECISION               
         DC    C'S',X'01'                                                       
         DC    C'P',X'01'                                                       
         DC    C'I',X'01'                                                       
         DC    C'T',X'01'                                                       
         DC    C'Q',X'01'                                                       
****     DC    C'D',X'00'                                                       
         DC    C'D',X'01'                                                       
         DC    C'X',X'01'                                                       
         DC    C'FF'                                                            
PRECTAB2 DS    0CL2                             DEMO PRECISION TABLE            
         DC    C'R',X'02'                       DEMOMOD/PRECISION               
         DC    C'S',X'01'                                                       
         DC    C'P',X'01'                                                       
         DC    C'I',X'01'                                                       
         DC    C'T',X'01'                                                       
         DC    C'Q',X'01'                                                       
****     DC    C'D',X'00'                                                       
         DC    C'D',X'01'                                                       
         DC    C'X',X'01'                                                       
         DC    C'FF'                                                            
**********************************************************************          
*                                                                    *          
*     RECALC DETAILS                                                 *          
*                                                                    *          
**********************************************************************          
                                                                                
RECALC   NTR1  BASE=*                                                           
         MVI   DBSRC,C'N'                                                       
         MVI   DBMED,C'T'                                                       
                                                                                
         MVI   HDRFLAG,C'N'                                                     
         MVI   RECALFLG,C'Y'                     SET RECALC FLAG                
         SR    R4,R4                             GET DEMO                       
         ICM   R4,7,ADEMO                                                       
         LA    R2,1                                                             
         TM    DEMOIND,LQ_TSINQ                                                 
         BNO   RECAL03                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     RECAL05                                                          
RECAL03  TM    DEMOIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
RECAL05  LA    R6,DEMODEMS                                                      
         STCM  R2,3,NUMDEMO                                                     
                                                                                
         LA    R5,DMHDVALS                                                      
         USING DMHDVALS,R5                                                      
RECAL07  MVC   0(3,R6),0(R4)                     3 BYTE DEMO CODE               
         MVC   3(3,R6),0(R4)                                                    
         MVI   4(R6),C'S'                        TSA IMPRESSION                 
         CLI   1(R4),C'I'                        IF REQUEST IMPRESSION          
         BNE   *+8                                                              
         MVI   4(R6),C'X'                                                       
         MVC   6(3,R6),0(R4)                                                    
         MVI   7(R6),C'P'                                                       
         CLI   1(R4),C'I'                        TSA IMPRESSION                 
         BNE   *+8                                                              
         MVI   7(R6),C'Q'                                                       
         MVC   DMHDDEM,4(R4)                                                    
         CLC   =C'HOMES',DMHDDEM                                                
         BNE   *+10                                                             
         MVC   DMHDDEM(5),=C'HH   '                                             
         CLC   =C'HOMES',DMHDDEM+1                                              
         BNE   *+10                                                             
         MVC   DMHDDEM+1(5),=C'HH   '                                           
                                                                                
         MVC   DMCB+0(1),1(R4)                   MOD FOR REQUESTED DEMO         
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
         MVC   DMHDDEM(5),=C'Share'                                             
         MVC   DMCB+0(1),4(R6)                   MOD FOR SHARE                  
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
         MVC   DMHDDEM(7),=C'HUT/PUT'                                           
         MVC   DMCB+0(1),7(R6)                   MOD FOR HPT                    
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R4,24                                                            
         AHI   R6,9                                                             
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
         ZICM  R0,NUMDEMO,(3)                                                   
         AHI   R0,2                                                             
         STCM  R0,3,NUMDEMO                                                     
         BCT   R2,RECAL07                                                       
         MVI   0(R6),X'FF'                                                      
         MVI   DMHDSEND,C'Y'                                                    
         MVC   DMHDNUM,NUMDEMO                                                  
         DROP  R5                                                               
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
         L     R5,=A(DBLOCK1-WORKD)             GET STATION,FILE,BOOK           
         LA    R5,WORKD(R5)                                                     
         USING DBLOCK,R5                                                        
         SR    R4,R4                                                            
         ICM   R4,7,ASTAT                                                       
         LA    R2,1                                                             
         TM    STAIND,LQ_TSINQ                                                  
         BNO   RECAL20                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
         J     RECAL30                                                          
RECAL20  TM    STAIND,LQ_TLSTQ                                                  
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
RECAL30  XC    ALFMKTS,ALFMKTS                                                  
         XC    DBSELALF,DBSELALF                                                
         ST    R4,MYSTAPTR                                                      
                                                                                
         L     R4,MYSTAPTR                                                      
         USING RECALCD,R4                                                       
                                                                                
RECAL40  DS    0H                                                               
         XC    DUMSTATH,DUMSTATH                                                
         XC    DUMSTAT,DUMSTAT                                                  
         ZIC   RE,RCSTALEN                       LENGTH OF INPUT STRING         
         STC   RE,DUMSTATH+5                                                    
         MVI   DUMSTATH,L'DUMSTATH+L'DUMSTAT                                    
         ZIC   RE,RCSTALEN                       LENGTH OF INPUT STRING         
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMSTAT(0),RCSTAT                                                
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMSTATH,(R0),C',=,/'                              
         L     R1,AIO1                                                          
         USING SCANBLKD,R1                                                      
         MVC   DBSELSTA,SC1STFLD                                                
         OC    DBSELSTA(5),=C'     '                                            
         CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   TMPSTA,DBSELSTA                                                  
         MVC   RCSTAT,DUMSTAT                                                   
         MVC   ALFMKTS,SC2NDFLD                                                 
                                                                                
         LA    RF,SC2NDFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC2NDLEN                                                      
         OR    RF,RF                                                            
         BZ    RECAL45                                                          
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
         GOTOR (#VALAMKT,AVALAMKT),DMCB                                         
         XC    SPILLMKT,SPILLMKT                                                
         OC    WORK(2),WORK                      BINARY MKT RETURNED?           
         BZ    RECAL45                           IF SO MKT IS NUMERIC           
         MVC   SPILLMKT,WORK                                                    
         XC    ALFMKTS,ALFMKTS                                                  
         B     RECAL50                                                          
                                                                                
         DROP  R1                                                               
RECAL45  OC    ALFMKTS,ALFMKTS                                                  
         BZ    RECAL50                                                          
         MVI   DBFUNCT,DBCNVA2N                  TRANSLATE ALPHA TO             
         OC    ALFMKTS,=X'404040'                NUMERIC MARKET                 
         MVC   DBSELALF,ALFMKTS                                                 
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                         ANY ERRORS?                    
         BNE   *+10                              NO, SET NUMERIC MKT            
         MVC   SPILLMKT,DBSELRMK                 AS SPILL                       
RECAL50  XC    DBSELRMK,DBSELRMK                 CLEAR FIELD IN CASE            
         MVI   DBERROR,0                         'TWAS FUDGED                   
                                                                                
         MVC   TMPUINDX,RCUPINDX                 UPGRADE INDEX                  
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELMK,SPILLMKT                                                 
RECAL80  MVC   TMPBOOK,RCBOOK                    BOOK                           
         MVC   TMPBKTYP,RCBKTYP                  BOOKTYPE                       
         MVC   TMPBKWK,RCWEEK                    WEEK SPECIFIED                 
         MVC   INPWEEK,TMPBKWK                                                  
         MVC   TMPMBKS,RCMULBK                   MULTIBOOK                      
         CLI   RCWEEK,C'W'                       IF ALL WEEKS WANTED            
         BNE   RECAL81                           DONT STRIP HIGH ORDER          
         MVI   TMPBKWK,1                                                        
         B     RECAL82                                                          
RECAL81  NI    TMPBKWK,X'0F'                     NIBBLE                         
RECAL82  DS    0H                                                               
         MVC   INPFIL,RCFILE                     FILE                           
RECAL84  CLC   =C'PAV',INPFIL                                                   
         BNE   RECAL90                                                          
         MVI   TMPBEST,C'A'                                                     
         GOTOR =A(PAVLOOK),RR=SRVRRELO                                          
         J     RECAL100                                                         
RECAL90  OC    TMPMBKS,TMPMBKS                                                  
         BNZ   RECAL92                                                          
         OC    TMPUINDX,TMPUINDX                                                
         JZ    RECAL94                                                          
RECAL92  GOTOR DEMUPGD                                                          
         J     RECAL100                                                         
RECAL94  GOTOR DEMLK,RR=SRVRRELO                                                
RECAL100 DS    0H                                                               
         CLI   RCWEEK,C'W'                                                      
         BNE   RECALX                                                           
         ZIC   RE,TMPBKWK                                                       
         AHI   RE,1                                                             
         STC   RE,TMPBKWK                                                       
         CLI   TMPBKWK,5                                                        
         BL    RECAL84                                                          
         MVI   TMPBKWK,1                         RESET FOR NEXT FILE            
                                                                                
RECALX   J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
********************************************************************            
*              DEMLOOK ROUTINE                                     *            
********************************************************************            
DEMLK    NTR1  BASE=*                                                           
         L     R5,=A(DBLOCK1-WORKD)                                             
         LA    R5,WORKD(R5)                                                     
         USING SPDEMLKD,R5                                                      
                                                                                
         MVI   ANYTPFLG,C'N'                      ASSUME NO DATA                
                                                                                
* PASS DOWN THE DEMO HEADER RECORD ONCE                                         
                                                                                
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         L     R0,AIO1                                                          
         ST    R0,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         LA    R0,DEMODEMS                                                      
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL                                                      
         LA    R0,DEMLKHK                                                       
         ST    R0,SPLKHOOK                                                      
         MVC   MYPROF1W,PROF1W                   THIS IS TO INCORPORATE         
         MVI   MYPROF1W+5,C'I'                   PRECISION PROFILE IF           
         LA    R0,MYPROF1W                       CONNECTED TO REP               
         ST    R0,SPLKA1W                        ALWAYS DMA=I                   
                                                                                
         MVC   SPLKFIL,INPFIL                                                   
         CLC   =C'OTP',INPFIL                                                   
         BE    *+10                                                             
         CLC   =C'WTP',INPFIL                                                   
         BNE   *+8                                                              
         MVI   SPLKFIL,C'T'                                                     
                                                                                
         MVC   SPLKMED,DBMED                                                    
         MVI   SPLKSRC,C'N'                                                     
         MVC   SPLKTPTT,DBTPTTS                                                 
         CLC   =C'T4',INPFIL                     T4                             
         BNE   *+8                                                              
         MVI   SPLKTPTT,C'P'                                                    
         CLC   =C'TF',INPFIL                     FUSION?                        
         BNE   *+8                                                              
         MVI   SPLKSRC,C'F'                                                     
         MVC   SPLKAGY,AGYALPH                                                  
         MVC   SPLKDBK,TMPBOOK+1                                                
         MVC   SPLKWKN,TMPBKWK                                                  
         MVC   SPLKBTYP,TMPBKTYP                                                
         MVC   SPLKSTA,TMPSTA                                                   
         CLI   SPLKSTA+3,C'+'                                                   
         BNE   *+8                                                              
         MVI   SPLKSTA+3,C' '                                                   
         CLI   SPLKSTA+4,C'+'                                                   
         BNE   *+8                                                              
         MVI   SPLKSTA+4,C' '                                                   
         MVC   SPLKSPL,SPILLMKT                                                 
                                                                                
         LA    R3,TMPDYTIM                                                      
         ST    R3,DYTMPTR                                                       
DMLK05   CLI   RECALFLG,C'Y'                    RECALC JUST DO ENTIRE           
         BE    DMLK06                            ROTATION                       
         CLI   0(R3),X'FF'                       INDIVIDUAL COMPONENT           
         JNE   DMLK10                            UNTIL END OF DYTIME            
                                                                                
DMLK06   LA    R3,TMPDYTIM                       DO ENTIRE ROTATION             
                                                                                
                                                                                
DMLK6B   XC    DUB,DUB                                                          
         LA    R0,MYDBXTRA                                                      
         ST    R0,DUB+4                                                         
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
         ICM   RF,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTLD,RF                                                        
         LA    RE,DBXTLIST                                                      
         MVC   DBXTLID,=C'DYTM'                                                 
         MVI   DBXTLIDX,0                                                       
         LA    R0,0                                                             
                                                                                
         CLI   RECALFLG,C'Y'                                                    
         BNE   DMLK08                                                           
                                                                                
*      GET MAINFRAME ID'S   (DAY,STIME,ETIME,PURE NUMBER)                       
                                                                                
         SR    R3,R3                                                            
         ICM   R3,7,AMFID                                                       
         LA    R2,1                                                             
         TM    MFIDIND,LQ_TSINQ                                                 
         BNO   DMLK07                                                           
         LA    R3,LW_DATA1-LW_D(R3)                                             
         J     DMLK7A                                                           
DMLK07   TM    MFIDIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R3)                                            
         LA    R3,LW_DATA2-LW_D(R3)                                             
                                                                                
DMLK7A   PACK  DUB,0(3,R3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,TMPMFID                                                       
         XC    DUB,DUB                                                          
         PACK  DUB,3(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+1                                                   
         XC    DUB,DUB                                                          
         PACK  DUB,7(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+3                                                   
         MVC   0(5,RE),TMPMFID                                                  
                                                                                
         AHI   RE,5                                                             
         AHI   R3,15                                                            
         BCT   R2,DMLK7A                                                        
         MVI   0(RE),0                                                          
         MVI   ROTFLAG,C'Y'                     SET DID ROTATION FLAG           
         J     DMLK12                                                           
                                                                                
*                                               SET UP ROTATION LINK            
DMLK08   MVC   0(5,RE),0(R3)                    DAY,START END TIME              
         AHI   RE,5                                                             
         AHI   R3,5                                                             
         CLI   0(R3),X'FF'                                                      
         BNE   DMLK08                                                           
         MVI   0(RE),0                                                          
         MVI   ROTFLAG,C'Y'                     SET DID ROTATION FLAG           
         J     DMLK12                                                           
         DROP  RF                                                               
                                                                                
DMLK10   MVC   SPLKDAY,0(R3)                                                    
         MVC   SPLKTIM,1(R3)                                                    
         AHI   R3,5                             BUMP NEXT DAYTIME               
         ST    R3,DYTMPTR                       COMPONENT                       
         MVI   ROTFLAG,C'N'                     UNSET ROTATION FLAG             
                                                                                
DMLK12   MVI   SPLKSVI,X'FF'                                                    
         CLC   ALFMKTS,SPACES                                                   
         BNE   *+10                                                             
         XC    ALFMKTS,ALFMKTS                                                  
         OC    ALFMKTS,ALFMKTS                                                  
         BZ    DEMLK20                                                          
         MVC   SPLKALF,ALFMKTS                                                  
DEMLK20  MVC   SPLKUID,USERID                                                   
*  SYSCODE                                                                      
         LA    RE,SYSCEXT                                                       
         USING SPLKXTD,RE                                                       
         XC    SPLKXTND,SPLKXTND   DEFAULT NO EXTENSION                         
         XC    SPXTSYSC,SPXTSYSC                                                
         MVC   SPXTSYSC,TMPSYSC                                                 
         OI    SPXTFLAG,SPXTRAPP  SET CALLING RESEARCH APPL FLAG                
         ST    RE,SPLKXTND                                                      
         DROP  RE                                                               
                                                                                
*                                                                               
*                                                                               
*  2 DECIMAL DEMOS                                                              
         CLI   OPTDEC,C'2'                                                      
         BNE   DEMLK21                                                          
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
                                                                                
         ICM   R1,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
         MVI   DBXTSCTL,C'2'                                                    
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         MVI   TMPWHOLE,WHOLERTG                                                
         OI    TMPWHOLE,WHOLESHR                                                
         OI    TMPWHOLE,WHOLEPUT                                                
         TM    TMPWHOLE,WHOLERTG           ROUND RATINGS TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTRC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLESHR           ROUND SHARES  TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTSC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLEPUT           ROUND PUTS    TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTPC2T,C'Y'                YEP                                 
         DROP  R1                                                               
*                                                                               
DEMLK21  GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD)                                   
         CLI   0(R1),X'80'                                                      
         BE    DMLK33                                                           
                                                                                
DEMLK22  LA    RF,DROPSVIS                                                      
         BASR  RE,RF                                                            
         CLI   ROTFLAG,C'Y'                                                     
         JE    DMLK35                                                           
                                                                                
* PROCESS THE DETAIL                                                            
                                                                                
         CLI   ANYTPFLG,C'N'                      NO DETAILS                    
         BE    DMLK33                             THEN DO NEXT                  
         LA    R3,PGDTVALS                                                      
         USING PGDTVALS,R3                                                      
         MVI   PGDTSEND,C'Y'                                                    
         MVC   PFILE,INPFIL                                                     
         MVI   PACTF,C'N'                                                       
         CLI   ALLTPFLG,C'Y'                                                    
         BE    *+8                                                              
         MVI   PACTF,C'Y'                                                       
         MVC   PSTAT(L'DUMSTAT),DUMSTAT                                         
                                                                                
         CLI   PSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   PSTAT+4,0                                                        
                                                                                
         MVC   PPROG(L'SPLKPRG),SPLKPRG                                         
         MVC   PWEEKS(1),TMPBKWK                                                
         OI    PWEEKS,X'F0'                                                     
         MVC   PDAYS,SPLKDAY                                                    
         MVC   PSTIME,SPLKTIM                                                   
         MVC   PETIME,SPLKTIM+2                                                 
         OC    PSTIME,PSTIME                                                    
         BNZ   *+10                                                             
         MVC   PSTIME,=H'2400'                                                  
         OC    PETIME,PETIME                                                    
         BNZ   *+10                                                             
         MVC   PETIME,=H'2400'                                                  
                                                                                
         XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',PFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         CLC   =C'WTP',PFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_WKLY                                                     
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,PBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),INPWEEK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*&&DO                                                                           
*################################################################               
* CHECK TO SEE IF WE ARE PROCESSING WEEKLY FILE                                 
* IF SO WE HAVE TP CALL DATCON                                                  
         MVI   BYTE1,1           BYTE1 = MONDAY FOR OVERNIGHTS                  
         CLC   =C'OTP',PFILE                                                    
         BE    DMLK22A                                                          
         CLC   =C'WTP',PFILE                                                    
         BNE   DMLK22D                                                          
         MVI   BYTE1,0           BYTE1 = DEFAULT = SAT                          
DMLK22A  GOTO1 =V(NSIWEEK),DMCB,(C'D',TMPBOOK+1),(BYTE1,VGETDAY),VADDAY+        
               ,VDATCON,RR=SRVRRELO                                             
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(6),0(R1)                                                    
         GOTO1 VDATCON,DMCB,(X'80',WORK),(5,PBOOK)                              
         CLI   TMPBKTYP,0                                                       
         BE    DMLK25                                                           
         LA    RE,PBOOK+8                                                       
         MVI   0(RE),C'('                                                       
*****    MVC   1(1,RE),TMPBKTYP                                                 
         LA    R2,TMPBKTYP                                                      
         LA    R4,1(RE)                                                         
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,(R2),1,(R4),12                          
         CLI   0(R4),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,R4),=C'??'        JUST PASS ??                               
         LA    RE,PBOOK+8                                                       
         CLI   0(R4),X'40'                                                      
         BH    DMLK22B                                                          
         MVI   2(RE),C')'            1 CHARACTER BOOKTYPE DISPLAY               
         AHI   RE,3                                                             
         B     DMLK25                                                           
DMLK22B  MVI   3(RE),C')'            2 CHARACTER BOOKTYPE DISPLAY               
         AHI   RE,4                                                             
****     MVI   2(RE),C')'                                                       
         B     DMLK25                                                           
****                                                                            
DMLK22D  MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         CLI   LATFLAG,C'Y'                                                     
         BNE   *+14                                                             
         MVC   PBOOK(6),=C'LATEST'                                              
         B     DMLK25                                                           
         CLI   TMPBOOK+1,X'FF'                                                  
         BNE   DMLK23                                                           
         MVC   PBOOK(3),=C'LAT'                                                 
         MVC   PBOOK+3(1),TMPBOOK+2                                             
         B     DMLK25                                                           
DMLK23   MVC   PBOOK(L'MYBK),MYBK                                               
         CLI   PBOOK,C'P'                                                       
         BE    *+8                                                              
         CLI   PBOOK,C'E'                                                       
         BNE   DMLK23A                                                          
         MVI   ANYTPFLG,C'N'                                                    
         B     DMLK35A                                                          
DMLK23A  LA    RE,PBOOK+5                                                       
         CLI   TMPBKTYP,0                                                       
         BE    DMLK24                                                           
         MVI   0(RE),C'('                                                       
         LA    R2,TMPBKTYP                                                      
         LA    R4,1(RE)                                                         
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,(R2),1,(R4),12                          
*****    MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         CLI   0(R4),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,R4),=C'??'        JUST PASS ??                               
         LA    RE,PBOOK+5                                                       
         CLI   0(RE),X'40'                                                      
         BH    DMLK23D                                                          
         MVI   2(RE),C')'            1 CHARACTER BOOKTYPE DISPLAY               
         AHI   RE,3                                                             
         B     DMLK24                                                           
DMLK23D  MVI   3(RE),C')'            2 CHARACTER BOOKTYPE DISPLAY               
         AHI   RE,4                                                             
DMLK24   CLI   INPWEEK,0                                                        
         BE    DMLK25                                                           
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),INPWEEK                                                  
*&&                                                                             
*################################################################               
DMLK25   MVC   PGDTNUM,=H'1'                                                    
         LA    R4,PMFID                                                         
         USING MFIDD,R4                                                         
         EDIT  (B1,PDAYS),(3,MFDAY),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0             
         EDIT  (B2,PSTIME),(4,MFSTIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         EDIT  (B2,PETIME),(4,MFETIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         DROP  R3                                                               
         DROP  R4                                                               
         LA    R3,DDEMVALS                                                      
         USING DDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DMLK30   MVI   DDEMSEND,C'Y'                                                    
         MVC   DDEMOS,0(R2)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DMLK30                                                        
         MVC   DDEMNUM,NUMDEMO                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
DMLK33   CLI   ROTFLAG,C'Y'                                                     
         JE    DMLK35                                                           
         L     R3,DYTMPTR                                                       
         J     DMLK05                                                           
         DROP  R3                                                               
                                                                                
* PROCESS SUMMARY RECORD                                                        
                                                                                
DMLK35   CLI   RECALFLG,C'Y'                     RECALC  ONLY HAS               
         BE    DMLK35A                           SUMMARY RECORD TO PASS         
         CLI   ALLTPFLG,C'Y'                     IF WE ARE DOING SECOND         
         JE    DEMLKX                            TP READ, T4-TP, TP-T4          
DMLK35A  LA    R3,SUMMVALS                       WE DONT NEED 2ND SUMM          
         USING SUMMVALS,R3                                                      
         MVI   SUMMSEND,C'Y'                                                    
         MVC   SFILE,INPFIL                                                     
         MVC   SSTAT(L'DUMSTAT),DUMSTAT                                         
         CLI   SSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   SSTAT+4,0                                                        
         MVC   SPROG(L'SPLKPRG),SPLKPRG                                         
*                                                                               
         OC    TMPSYSC,TMPSYSC                                                  
         BZ    DMLK35C                                                          
         EDIT  (B2,TMPSYSC),(5,SSYSC),ALIGN=LEFT,ZERO=BLANK                     
                                                                                
*                                                                               
                                                                                
DMLK35C  CLI   ANYTPFLG,C'N'                                                    
         BNE   DMLK35F                                                          
         CLI   TMPLATBN,0       IF NOTHING FOUND AND WE ARE PROCESSING          
         BNE   DEMLKX           SPOT DESKTOP'S LATEST BOOKS REQUEST             
         XC    SPROG,SPROG      THEN JUST EXIT- DONT PASS DOWN                  
         MVC   SPROG(15),=C'NO DOMINANT PGM'                                    
*                                                                               
DMLK35F  XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         CLC   =C'WTP',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_WKLY                                                     
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,SBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),INPWEEK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*&&DO                                                                           
*********************************************************************           
* CHECK TO SEE IF WE ARE PROCESSING WEEKLY FILE                                 
* IF SO WE HAVE TP CALL DATCON                                                  
DMLK35F  MVI   BYTE1,1           BYTE1 = MONDAY FOR OVERNIGHTS                  
         CLC   =C'OTP',SFILE                                                    
         BE    DMLK35H                                                          
         CLC   =C'WTP',SFILE                                                    
         BNE   DMLK35K                                                          
*                                                                               
         MVI   BYTE1,0           BYTE1 = DEFAULT = SAT                          
DMLK35H  GOTO1 =V(NSIWEEK),DMCB,(C'D',TMPBOOK+1),(BYTE1,VGETDAY),VADDAY+        
               ,VDATCON,RR=SRVRRELO                                             
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(6),0(R1)                                                    
         GOTO1 VDATCON,DMCB,(X'80',WORK),(5,SBOOK)                              
         CLI   TMPBKTYP,0                                                       
         BE    DMLK38                                                           
         LA    RE,SBOOK+8                                                       
         MVI   0(RE),C'('                                                       
*****    MVC   1(1,RE),TMPBKTYP                                                 
         LA    R4,1(RE)                                                         
         LA    R2,TMPBKTYP                                                      
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,(R2),1,(R4),12                          
         CLI   0(R4),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,R4),=C'??'        JUST PASS ??                               
         LA    RE,SBOOK+8                                                       
         CLI   0(R4),X'40'                                                      
         BH    DMLK35I                                                          
         MVI   2(RE),C')'            1 CHARACTER BOOKTYPE DISPLAY               
         AHI   RE,3                                                             
         B     DMLK38                                                           
DMLK35I  MVI   3(RE),C')'            2 CHARACTER BOOKTYPE DISPLAY               
         AHI   RE,4                                                             
*****    MVI   2(RE),C')'                                                       
         B     DMLK38                                                           
*                                                                               
                                                                                
DMLK35K  MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         CLI   LATFLAG,C'Y'                                                     
         BNE   *+14                                                             
         MVC   SBOOK(6),=C'LATEST'                                              
         B     DMLK38                                                           
         CLI   TMPBOOK+1,X'FF'                                                  
         BNE   DMLK36                                                           
         MVC   SBOOK(3),=C'LAT'                                                 
         MVC   SBOOK+3(1),TMPBOOK+2                                             
         B     DMLK38                                                           
DMLK36   MVC   SBOOK(L'MYBK),MYBK                                               
         LA    RE,SBOOK+5                                                       
         CLI   TMPBKTYP,0                                                       
         BE    DMLK37                                                           
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
DMLK37   CLI   INPWEEK,0                                                        
         BE    DMLK38                                                           
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),INPWEEK                                                  
*&&                                                                             
****************************************************************                
DMLK38   MVC   SUMMNUM,=H'1'                                                    
         DROP  R3                                                               
         LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DMLK40   MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    SDEMOS,SDEMOS                                                    
         AHI   R3,SDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DMLK40                                                        
         MVC   SDEMNUM,NUMDEMO                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         DROP  R3                                                               
                                                                                
DEMLKX   J     EXITY                                                            
***********************************************************************         
* ROUTINE TO PROCESS VALUES RETURNED FROM SPDEMLK                               
* BE CAREFUL NOT TO CLOBBER DMCB, SINCE SPDEMLK'S CALLER DEPENDS ON IT          
***********************************************************************         
DEMLKHK  NTR1                                                                   
         MVC   SAVEDMCB(24),DMCB                                                
         L     R4,SPLKDBLK                                                      
         USING DBLOCKD,R4                                                       
         TM    SPLKDAY,X'90'                       CANT HAVE VAR,AGN            
         JO    EXITY                               FOR TT,T4                    
                                                                                
         MVI   ANYTPFLG,C'Y'                                                    
         LA    RF,DROPSVIS                                                      
         BASR  RE,RF                                                            
         CLC   DBFACTOR,=H'1'                                                   
         BE    DMLKHK30                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         LH    R1,DBFACTOR                                                      
         LA    R2,THISDEMS                                                      
DMLKHK20 L     RF,0(R2)                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,DMLKHK20                                                      
                                                                                
DMLKHK30 GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
         J     EXITY                                                            
                                                                                
DROPSVIS LA    R1,THISDEMS                                                      
         LA    RF,8(R1)                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         MVC   4(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
                                                                                
         LTORG                                                                  
         DROP  RB,R4,R5                                                         
                                                                                
*---------------------------- GET PROFILE ----------------------------*         
* At entry,                                                                     
*   DUB(4) = profile id                                                         
                                                                                
GETPROFL NTR1  BASE=*                                                           
                                                                                
         DS    0H                  FIND ENTRY FOR PROFILE                       
         LA    RE,PROFTAB                                                       
PROF     USING PROFTAB,RE                                                       
GPRF010  CLI   0(RE),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PROF.PRFTBID,DUB                                                 
         BE    *+12                                                             
         LA    RE,PRFTBQ(RE)                                                    
         B     GPRF010                                                          
                                                                                
         ZICM  RF,PROF.PRFTBASV,(3)                                             
         LA    RF,WORKD(RF)         RF-->AREA TO HOLD PROFILE                   
         ST    RF,APROFILE                                                      
         ZIC   R1,PROF.PRFTBLSV                                                 
         STC   R1,LPROFILE          R1 = L(AREA TO HOLD PROFILE)                
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RF),0(RF)        CLEAR AREA USED TO HOLD PROFILE             
                                                                                
         ZICM  RF,PROF.PRFTBRTN,(3)                                             
         LA    RF,GETPROFL(RF)                                                  
         BR    RF                   GO TO APPROPRIATE GET-PROFILE RTN           
         DC    H'0'                                                             
         DROP  PROF                                                             
                                                                                
GPRFRRMP00 DS  0H                   REP RMP PROFILE                             
         XC    WORK,WORK            BUILD KEY OF REP RECD IN WORK               
         LA    RE,WORK                                                          
         USING RREPREC,RE                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGYALPH                                                 
         DROP  RE                                                               
                                                                                
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+08,X'10'                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 (RF),(R1),=C'GETREC',=C'REPFILE',WORK+28,AIO1,MYDMWORK           
         CLI   DMCB+08,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                  LOOK FOR PROFILE IN RECORD                   
         L     R3,AIO1              R3-->REP RECORD                             
         MVI   MYELCODE,X'04'                                                   
         LA    R0,RREPELEM-RREPREC                                              
         STH   R0,MYDATDSP                                                      
         BAS   RE,GETEL                                                         
         BNE   GPRFRRMPX                                                        
         ZIC   R0,(RREPPGM#-RREPPGMP)(R3)  R0 = # OF PROGRAM PROFILES           
         LA    RE,(RREPPGM1-RREPPGMP)(R3)  R6-->PROGRAM PROFILES LIST           
                                                                                
GPRFRRMP40 DS  0H                                                               
         CLI   0(RE),RREPQSEL       LOOK FOR SEL PROGRAM PROFILE                
         BE    *+16                                                             
         LA    RE,RREPPGML(RE)                                                  
         BCT   R0,GPRFRRMP40                                                    
         B     GPRFRRMPX                                                        
                                                                                
         L     RF,APROFILE                                                      
         ZIC   R1,LPROFILE                                                      
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),2(RE)       MOVE PROFILE INTO STORAGE AREA              
                                                                                
GPRFRRMPX EQU  *                                                                
         B     GETPRFLX                                                         
                                                                                
GETPRFLX DS    0H                                                               
         J     EXITY                             TABLE OF PROFILE INFO          
         GETEL R3,MYDATDSP,MYELCODE                                             
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
MYELCODE DS    C                                                                
MYDMWORK DS    12D                                                              
EOT      EQU   0                                                                
PROFTAB  DS    0X                                PROFILE IF                     
PRFTBID  DS    CL4                               ROUTINE TO GET PROFILE         
PRFTBRTN DS    AL2                               A(FIELD) TO STORE PROF         
PRFTBASV DS    AL2                               L(FIELD) TO STORE PROF         
PRFTBLSV DS    XL1                               L(PROFILE INFO ENTRY)          
PRFTBQ   EQU   *-PROFTAB                                                        
         ORG   PROFTAB                                                          
         DC    C'SELW',AL2(GPRFRRMP00-GETPROFL),AL2(PROFRRMP-WORKD)             
         DC    AL1(L'PROFRRMP)                                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
*   FORMAT BOOK STRING ROUTINE                                        *         
*   ENTRY PARAMATER LIST AS DEFINED BY INPARAM                        *         
*   EXIT  - DMCB(4)= A(END OF FORMATTED BOOK STRING)                            
***********************************************************************         
FORMATBK NTR1  BASE=*                                                           
                                                                                
         LA    R6,DMCB                                                          
         MVC   SAVEDMCB(24),DMCB                                                
         USING IN_PARAM,R6                                                      
*                                                                               
FORMAT10 CLI   INBKTYPE,0            ANY INTERNAL BOOKTYPE  ?                   
         BE    FORMAT20                                                         
         LA    R4,TWOCHRBT                                                      
         LA    R2,INBKTYPE                                                      
         GOTOR (#TRNSBKT,ATRNSBKT),MYDMCB,(R2),1,(R4),12                        
         MVC   DMCB(24),SAVEDMCB                                                
         CLI   0(R4),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,R4),=C'??'        JUST PASS ??                               
*                                                                               
FORMAT20 XC    MYDMCB,MYDMCB                                                    
         MVI   BYTE1,1           BYTE1 = MONDAY FOR OVERNIGHTS                  
         CLI   INFORMAT,BK_OVERN                                                
         BE    FORMAT25                                                         
         CLI   INFORMAT,BK_WKLY                                                 
         BNE   FORMAT38                                                         
         MVI   BYTE1,0           BYTE1 = DEFAULT = SAT                          
FORMAT25 GOTO1 =V(NSIWEEK),MYDMCB,(C'D',INBOOK+1),(BYTE1,VGETDAY),     +        
               VADDAY,VDATCON,RR=SRVRRELO                                       
         ZICM  R1,MYDMCB+1,(7)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(6),0(R1)                                                    
                                                                                
         L     R2,INOUTPUT           R2 POINTS TO OUTPUT AREA                   
                                                                                
         GOTO1 VDATCON,MYDMCB,(X'80',WORK),(5,0(R2))                            
         CLI   INBKTYPE,0            ANY BOOKTYPE?                              
         BE    FORMATX                                                          
         LA    R2,8(R2)                                                         
         MVI   0(R2),C'('                                                       
         CLI   TWOCHRBT+1,X'40'                                                 
         BH    FORMAT30                                                         
         MVC   1(1,R2),TWOCHRBT                                                 
         MVI   2(R2),C')'            1 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,3                                                             
         B     FORMATX                                                          
FORMAT30 MVC   1(2,R2),TWOCHRBT                                                 
         MVI   3(R2),C')'            2 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,4                                                             
         B     FORMATX                                                          
*=====================================================================          
FORMAT38 MVI   MYBKH,18              CREATE FAKE FIELD                          
         GOTOR =V(UNBOOK),MYDMCB,(1,INBOOK),MYBKH,0,(C'+',=CL6' '),    +        
               RR=SRVRRELO                                                      
         L     R2,INOUTPUT           R2 POINTS TO OUTPUT AREA                   
         CLI   LATFLAG,C'Y'                                                     
         BNE   *+18                                                             
         MVC   0(6,R2),=C'LATEST'                                               
         AHI   R2,6                                                             
         B     FORMATX                                                          
         CLI   DMCB+2,X'FF'                                                     
         BNE   FORMAT42                                                         
         MVC   0(3,R2),=C'LAT'                                                  
         MVC   3(1,R2),INBOOK+2                                                 
         AHI   R2,4                                                             
         B     FORMATX                                                          
FORMAT42 MVC   0(L'MYBK,R2),MYBK                                                
         CLI   INFORMAT,BK_RMINV     INVENTORY BOOK                             
         BE    FORMAT46                                                         
         CLI   0(R2),C'P'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'E'                                                       
         BNE   FORMAT46                                                         
         MVI   ANYTPFLG,C'N'                                                    
         B     FORMATX                                                          
FORMAT46 L     R2,INOUTPUT                                                      
         AHI   R2,5                                                             
         CLI   INBKTYPE,0                                                       
         BE    FORMAT60                                                         
         MVI   0(R2),C'('                                                       
         CLI   TWOCHRBT+1,X'40'                                                 
         BH    FORMAT52                                                         
         MVC   1(1,R2),TWOCHRBT                                                 
         MVI   2(R2),C')'            1 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,3                                                             
         B     FORMAT60                                                         
FORMAT52 MVC   1(2,R2),TWOCHRBT                                                 
         MVI   3(R2),C')'            2 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,4                                                             
FORMAT60 CLI   INWKNUM,0                                                        
         BE    FORMATX                                                          
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),INWKNUM                                                  
FORMATX  ST    R2,DMCB                                                          
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
TWOCHRBT DS    CL2                                                              
*                                                                               
IN_PARAM DSECT                                                                  
INFORMAT DS    XL1                                                              
BK_OVERN EQU   C'O'                                                             
BK_WKLY  EQU   C'W'                                                             
BK_MONTH EQU   C'M'                                                             
BK_RMINV EQU   X'01'                                                            
INBOOK   DS    XL3                                                              
INOUTPUT DS    A                                                                
INBKTYPE DS    X                                                                
INWKNUM  DS    C                                                                
         EJECT                                                                  
SVRDEF   CSECT                                                                  
***********************************************************************         
*   -------------- CLEAR OUTPUT AREAS ------------------------------- *         
***********************************************************************         
CLRAREA  NTR1  BASE=*                                                           
         LA    R0,OUTVALS                        CLEAR DOWN SAVE AREA           
         LHI   R1,OUTVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*   -------------- CONVERT   DAY      ------------------------------- *         
*   ENTRY = DMCB(1) = INTERNAL DAY CODE                               *         
*   EXIT  = DMCB+4(1)= 1 BYTE DAY CODE                                *         
***********************************************************************         
*                                  TABLE TO CONVERT KEY DAY VALUES              
CNVDAY   NTR1   BASE=*                                                          
         LA     RE,KDAYTAB                                                      
CNVD10   CLI    0(RE),X'FF'                                                     
         BNE    *+6                                                             
         DC     H'0'                                                            
         CLC    1(1,RE),DMCB                                                    
         BE     CNVD30                                                          
         AHI    RE,L'KDAYTAB                                                    
         J      CNVD10                                                          
CNVD30   MVC    DMCB+4(1),5(RE)                                                 
         J      EXITY                                                           
         LTORG                                                                  
KDAYTAB  DS     0XL(1+1+3+1)                                                    
         DC     X'0',X'10',C'MON',X'40'                                         
         DC     X'0',X'20',C'TUE',X'20'                                         
         DC     X'0',X'30',C'WED',X'10'                                         
         DC     X'0',X'40',C'THU',X'08'                                         
         DC     X'0',X'50',C'FRI',X'04'                                         
         DC     X'0',X'60',C'SAT',X'02'                                         
         DC     X'0',X'70',C'SUN',X'01'                                         
         DC     C'T',X'95',C'M-F',X'7C'                                         
         DC     C'T',X'FF',C'VAR',X'00'                                         
         DC     C'P',X'00',C'M-F',X'7C'                                         
         DC     C'P',X'80',C'M-S',X'7F'                                         
         DC     C'P',X'90',C'VAR',X'90'                                         
         DC     C'P',X'FF',C'VAR',X'00'                                         
         DC     C'R',X'95',C'M-F',X'7C'                                         
         DC     X'FF',X'FF',C'???',X'00'                                        
         DROP   RB                                                              
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
*------------------------- SET DBEXTEND AREA -------------------------*         
                                                                                
* Sets up a link in the DBEXTEND area.                                          
* At entry,                                                                     
*   DUB(4)   = link identification if want to match to an existing link         
*            = x'00000000' if link must be added,                               
*   DUB+4(4) = A(link),                                                         
*   DUB1     = A(PARAMETER BLOCK) ACCORDING TO SDBXF:                           
*            = A(SPDEMLK block) when SDBXF=C'S'                                 
*            = A(DBLOCK)        when SDBXF=C'D'                                 
*            = A(SPDEMUP block) when SDBXF=C'U'                                 
* At exit,                                                                      
*   FULL1    = ADDRESS OF LINK, ZEROES IF NO LINK SET UP.                       
                                                                                
STDBXLNK NTR1  BASE=*                                                           
         L     R6,DUB1                                                          
         XC    FULL1,FULL1                                                      
                                                                                
         CLI   SDBXF,C'S'                                                       
         BE    SDBX010                                                          
         CLI   SDBXF,C'D'                                                       
         BE    SDBX015                                                          
         CLI   SDBXF,C'U'                                                       
         BE    SDBX080                                                          
         DC    H'0'                                                             
                                                                                
                                                                                
SDBX010  DS    0H                  GET R2 TO START OF EXTENSION AREA            
         DS    0H                  GET R2 TO START OF EXTENSION AREA            
         USING SPDEMLKD,R6                                                      
         L     RF,SPLKAREC                                                      
         ICM   R0,15,8(RF)                                                      
         CLC   0(8,RF),=C'DBEXTEND'                                             
         BE    SDBX022                                                          
                                                                                
         MVC   0(8,RF),=C'DBEXTEND'                                             
         L     R2,DUB+4            USE A(CALLER'S LINK) IF DBEXTEND=0           
         STCM  R2,15,8(RF)                                                      
         MVC   0(4,R2),DUB+0        PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R6                                                               
                                                                                
SDBX015  DS    0H                  R6-->DBLOCK                                  
         USING DBLOCKD,R6                                                       
         ICM   R0,15,DBEXTEND                                                   
         BNZ   SDBX022                                                          
                                                                                
         L     R2,DUB+4            USE A(CALLER'S LINK) IF DBEXTEND=0           
         STCM  R2,15,DBEXTEND                                                   
         MVC   0(4,R2),DUB          PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R6                                                               
                                                                                
SDBX020  DS    0H                  BUMP TO APPROPRIATE LINK                     
         ICM   R0,15,4(R2)          GET ADDRESS OF NEXT LINK                    
         BZ    SDBX030               IF ZERO, ADD CALLER'S LINK                 
                                                                                
SDBX022  DS    0H                                                               
         LR    R2,R0                "BUMPED" TO NEXT LINK                       
         OC    DUB(4),DUB           IF LOOKING FOR MATCH,                       
         BZ    SDBX020                                                          
         CLC   DUB(4),0(R2)          AND LINKS' IDS MATCH,                      
         BNE   SDBX020                                                          
         B     SDBX050               PASS BACK ADDR OF CURRENT LINK             
                                                                                
SDBX030  DS    0H                  ADD CALLER'S LINK TO END                     
         MVC   4(4,R2),DUB+4        SET THE NEXT ADDR INTO LAST LINK            
         ICM   R2,15,4(R2)          BUMP TO THE NEW LAST LINK                   
         MVC   0(4,R2),DUB          PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040                                                          
                                                                                
SDBX040  DS    0H                  R2-->LAST LINK                               
         XC    4(4,R2),4(R2)        ZERO OUT THE NEXT ADDRESS                   
         B     SDBX050               NOPE                                       
                                                                                
SDBX050  DS    0H                                                               
         ST    R2,FULL1            RETURN ADDRESS OF MATCHED/ADDED LINK         
         B     SDBXX                                                            
                                                                                
                                                                                
SDBX080  DS    0H                  R6-->SPDEMUPD                                
         USING SPDEMUPD,R6                                                      
         ICM   R0,15,SPUPEXTN                                                   
         BNZ   SDBX022                                                          
                                                                                
         L     R2,DUB+4            USE A(CALLER'S LINK) IF SPUPEXTN=0           
         STCM  R2,15,SPUPEXTN                                                   
         MVC   0(4,R2),DUB          PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R6                                                               
                                                                                
                                                                                
SDBXX    DS    0H                                                               
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
*            PAV LOOKUP                                               *         
***********************************************************************         
PAVLOOK  NTR1  BASE=*                                                           
         L     R6,=A(DBLOCK1-WORKD)                                             
         LA    R6,WORKD(R6)                                                     
         USING DBLOCKD,R6                                                       
         L     RE,=A(VUTLIST)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,AVUTLIST                                                      
         L     RE,=A(PUTLIST)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,APUTLIST                                                      
         L     RE,=A(SHRLIST)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,ASHRLIST                                                      
         L     RE,=A(NUMWKTAB)                                                  
         A     RE,SRVRRELO                                                      
         ST    RE,ANUMWKTB                                                      
         L     RE,=A(FRSTWKTB)                                                  
         A     RE,SRVRRELO                                                      
         ST    RE,A1STWKTB                                                      
         XC    CUMFCTR,CUMFCTR                                                  
         MVC   CUMPNAM1,SPACES                                                  
         MVC   CUMPNAM2,SPACES                                                  
         MVI   ANYDETLS,C'N'      ASSUME NO DETAILS                             
                                                                                
         DS    0H                 CLEAR DBLOCK                                  
         LR    R0,R6               R0-->DESTINATION                             
         LA    R1,DBLOCK1X-DBLOCK1 R1 = L(DESTINATION)                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     R0,AIO3             R0-->DESTINATION                             
         LR    R5,R0                                                            
         LA    R1,L'IOAREA3        R1 = L(DESTINATION)                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BUFFER FOR ACCUMULATING RECDS          
         MVI   0(R5),RINVKTYQ      AND MAKE IT AN INVENTORY RECD                
                                                                                
         CLI   RECALFLG,C'Y'                                                    
         BE    PVLK80                                                           
                                                                                
         DS    0H                 BUILD DBLOCK                                  
         LA    R3,TMPDYTIM                                                      
         ST    R3,DYTMPTR                                                       
PVLK050  CLI   0(R3),X'FF'        IF END OF DAYTIME LIST THEN                   
         BE    PVLK110            PROCESS CUMMULATIVE RECORD                    
         MVC   TMPDAY,0(R3)       MOVE IN SINGLE DAY                            
         MVC   TMPSETIM,1(R3)     MOVE IN SINGLE START END TIME                 
         MVC   SVSETIM,TMPSETIM   SAVE REQUESTED TIME                           
                                                                                
         ST    R6,ADBLOCK                                                       
         GOTOR =A(BLDBLOCK),RR=SRVRRELO                                         
                                                                                
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCK,PAVHOOK,0                                    
                                                                                
         L     R3,DYTMPTR                                                       
         AHI   R3,5                                                             
         ST    R3,DYTMPTR                                                       
         B     PVLK050                                                          
                                                                                
*      GET MAINFRAME ID'S   (DAY,STIME,ETIME,PURE NUMBER)                       
                                                                                
PVLK80   SR    R3,R3                                                            
         ICM   R3,7,AMFID                                                       
         LA    R2,1                                                             
         TM    MFIDIND,LQ_TSINQ                                                 
         BNO   PVLK87                                                           
         LA    R3,LW_DATA1-LW_D(R3)                                             
         J     PVLK89                                                           
PVLK87   TM    MFIDIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R3)                                            
         LA    R3,LW_DATA2-LW_D(R3)                                             
                                                                                
PVLK89   PACK  DUB,0(3,R3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,TMPMFID                                                       
         XC    DUB,DUB                                                          
         PACK  DUB,3(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+1                                                   
         XC    DUB,DUB                                                          
         PACK  DUB,7(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+3                                                   
         MVC   TMPPURE,11(R3)                                                   
                                                                                
         MVC   TMPDAY,TMPMFID             MOVE IN SINGLE DAY                    
         MVC   TMPSETIM,TMPMFID+1         MOVE IN SINGLE START END TIME         
                                                                                
         ST    R6,ADBLOCK                                                       
         GOTOR =A(BLDBLOCK),RR=SRVRRELO                                         
                                                                                
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCK,PAVHOOK,0                                    
                                                                                
         AHI   R3,15                                                            
         BCT   R2,PVLK89                                                        
                                                                                
*  PROCESS CUMULATIVE RECORD **                                                 
                                                                                
PVLK110  DS    0H                                                               
         MVC   MTHCFACS,ADBLOCK                                                 
         MVC   MTHFCTR,CUMFCTR                                                  
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),DMCB,=C'DIVIDE',AIO3,AIO3,MATHFAC                           
         DROP  R6                                                               
         OC    TMPUINDX,TMPUINDX                                                
         BZ    PVLK110A                                                         
         GOTOR DEMUPGD                                                          
PVLK110A DS    0H                  EXTRACT DEMO VALUES                          
         LA    R0,DEMODEMS                                                      
         ST    R0,ADEMLIST                                                      
         LA    R0,DEMODEMS                                                      
         ST    R0,ADEMLIST          SET A(DEMO LIST)                            
         MVC   AXTRCREC,AIO3                                                    
         GOTOR =A(XTRCTDMV),RR=SRVRRELO                                         
                                                                                
         CLI   RECALFLG,C'Y'                      IF RECALC WE NEED             
         BE    PVLK111                            SUMMARY                       
         CLI   HDRFLAG,C'Y'                       IF PROCESSING HEADERS         
         BE    PVLKX                              WE DONT NEED SUMMARY          
         CLI   TMPBEST,C'B'                       IF DBBEST NOT BEST            
         BNE   PVLKX                              DONT NEED SUMMARY             
                                                                                
PVLK111  LA    R3,SUMMVALS                        WE DONT NEED 2ND SUMM         
         USING SUMMVALS,R3                                                      
         MVI   SUMMSEND,C'Y'                                                    
*****    MVC   SFILE,=C'PAV'                                                    
         MVC   SFILE,INPFIL                                                     
         MVC   SSTAT(L'DUMSTAT),DUMSTAT                                         
         CLI   SSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   SSTAT+4,0                                                        
                                                                                
         MVC   SPROG,CUMPNAM1                                                   
         CLC   CUMPNAM1,CUMPNAM2                                                
         BE    *+14                                                             
         MVI   SPROG+(L'SPROG/2),C'/'                                           
         MVC   SPROG+((L'SPROG/2)+1)((L'SPROG/2)-1),CUMPNAM2                    
                                                                                
         CLI   ANYDETLS,C'Y'                                                    
         BE    *+10                                                             
         MVC   SPROG(15),=C'NO DOMINANT PGM'                                    
* NEW CALL TO FORMATBK ROUTINE TO FORMAT BOOK OUTPUT STRING                     
         XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,SBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),INPWEEK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*                                                                               
********************************************************************            
* TAKE OUT CODE AND REPLACE WITH FORMATBK ROUTINE CALL                          
*&&DO                                                                           
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         CLI   LATFLAG,C'Y'                                                     
         BNE   *+14                                                             
         MVC   SBOOK(6),=C'LATEST'                                              
         B     PVLK115                                                          
         CLI   TMPBOOK+1,X'FF'                                                  
         BNE   PVLK112                                                          
         MVC   SBOOK(3),=C'LAT'                                                 
         MVC   SBOOK+3(1),TMPBOOK+2                                             
         B     PVLK115                                                          
PVLK112  MVC   SBOOK(L'MYBK),MYBK                                               
         LA    RE,SBOOK+5                                                       
         CLI   TMPBKTYP,0                                                       
         BE    PVLK113                                                          
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
PVLK113  CLI   INPWEEK,0                                                        
         BE    PVLK115                                                          
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),INPWEEK                                                  
***************************************************************                 
*&&                                                                             
PVLK115  MVC   SUMMNUM,=H'1'                                                    
         OC    TMPUINDX,TMPUINDX                                                
         BZ    *+10                                                             
         MVC   SBOOK(L'TMPUPNAM),TMPUPNAM                                       
         DROP  R3                                                               
                                                                                
         LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
PVLK120  MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         CLI   ANYDETLS,C'N'                                                    
         BNE   *+10                                                             
         XC    SDEMOS,SDEMOS                                                    
         AHI   R3,SDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,PVLK120                                                       
         MVC   SDEMNUM,NUMDEMO                                                  
PVLK130  L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
PVLKX    J     EXITY                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
PAVHOOK  NTR1                                                                   
         OC    TMPUINDX,TMPUINDX                 ANY UPGRADES??                 
         BZ    PVHK00                            MUST HAVE SHARE BOOK           
         OC    TMPBOOK,TMPBOOK                   FOR DETAILS                    
         JZ     EXITY                                                           
PVHK00   L     R6,=A(DBLOCK1-WORKD)                                             
         LA    R6,WORKD(R6)                                                     
         USING DBLOCKD,R6                                                       
                                                                                
         DS    0H                  TEST FOR CORRECT WEEK                        
         MVC   WORK,SPACES                                                      
***      GOTO1 =V(DEFINE),MYDMCB,=C'WEEK',DBLOCK,WORK,RR=SRVRRELO               
         GOTO1 VDEFINE,MYDMCB,=C'WEEK',DBLOCK,WORK                              
         SR    R1,R1                                                            
         ICM   R1,1,TMPBKWK        R1 = WEEK REQUESTED                          
         BZ    *+22                 OK IF NONE REQUESTED                        
         LA    R1,WORK(R1)         R1-->ACTIVE WEEK                             
         NI    0(R1),X'0F'          (REMOVE ZONE)                               
         CLC   0(1,R1),TMPBKWK     MATCH WEEK TO CHECK FOR ACTIVE-NESS          
         BNE   PVHKX                EXIT HOOK IF WEEK WASN'T ACTIVE             
                                                                                
         MVC   WORK,SPACES                                                      
**       GOTO1 =V(DEFINE),MYDMCB,=C'TIME',DBLOCK,WORK,RR=SRVRRELO               
         GOTO1 VDEFINE,MYDMCB,=C'TIME',DBLOCK,WORK                              
         MVC   TMPSETIM,WORK+2                                                  
                                                                                
         MVC   WORK,SPACES                                                      
**       GOTO1 =V(DEFINE),MYDMCB,=C'DAY',DBLOCK,WORK,RR=SRVRRELO                
         GOTO1 VDEFINE,MYDMCB,=C'DAY',DBLOCK,WORK                               
         XC    FULL2,FULL2                                                      
         CLC   =C'AV',WORK+2                                                    
         BNE   PVHOOK1                                                          
         MVC   FULL2(3),WORK+2                                                  
         MVC   TMPDAY,WORK                                                      
         B     PVHOOK2                                                          
PVHOOK1  ZIC   R1,WORK+1                                                        
         SLL   R1,4                (UNDO WHAT DEFINE DID)                       
         STC   R1,DMCB                                                          
         GOTOR CNVDAY,DMCB                                                      
         MVC   TMPDAY,DMCB+4                                                    
PVHOOK2  XC    WORK,WORK                                                        
         GOTO1 (RF),MYDMCB,=C'PURE',DBLOCK,WORK                                 
         MVC   BINPURE,WORK                                                     
                                                                                
         CLI   RECALFLG,C'Y'         RECALC DO FOR EXACT PURE                   
         BNE   *+14                  NUMBER                                     
         CLC   TMPPURE,WORK+3                                                   
         BNE   PVHKX                                                            
         MVC   TMPPURE,WORK+3                                                   
         CLI   HDRFLAG,C'Y'                      IF PROCESSING HEADERS          
         BNE   PAVHOOK3                                                         
         MVC   WORK+0(L'TMPDAY),TMPDAY                                          
         MVC   WORK+1(L'TMPSETIM),TMPSETIM                                      
         J     PVHOOK14                          IF EXIST EXIT                  
                                                                                
PAVHOOK3 CLI   RECALFLG,C'Y'                     RECALC DONT NEED TO            
         BE    PVHOOK18                          SAVE JUST PROCEED              
                                                                                
         MVC   WORK(L'BINPURE),BINPURE                                          
         CLI   TMPBEST,C'A'                      IF WANT DBBEST =ALL            
         BE    PVHOOK14                          THEN  DONT ADD TO TSAR         
* CHECK OFFLINE OR ONLINE - ONLINE USE BUFFERIN                                 
         TM    OFFLFLAG,LP_FOFFL                                                
         JZ    PVHOOK10                                                         
         XC    SRVRRELO,SRVRRELO                                                
         GOTOR PUTBUFF                                                          
         J     PVHOOK18                                                         
PVHOOK10 LA    RE,WORK                           JUST READ TO SEE IF IT         
         ST    RE,ATSIOREC                       ALREADY EXIST                  
         GOTOR (#WRITTSR,AWRITTSR),DMCB                                         
         J     PVHOOK18                                                         
                                                                                
PVHOOK14 TM    OFFLFLAG,LP_FOFFL                                                
         JZ    PVHOOK15                                                         
         XC    SRVRRELO,SRVRRELO                                                
         MVC   WORK2,WORK                                                       
         GOTOR GETBUFF                                                          
         BE    PVHKX                                                            
         MVC   WORK,WORK2                                                       
         GOTOR PUTBUFF                                                          
         J     PVHOOK18                                                         
                                                                                
PVHOOK15 LA    RE,WORK                         JUST READ TO SEE IF IT           
                                                                                
         ST    RE,ATSIOREC                       ALREADY EXIST                  
         MVC   WORK2,WORK                                                       
         GOTOR (#READTSR,AREADTSR),DMCB                                         
         CLI   DMCB,C'Y'                                                        
         BE    PVHKX                                                            
                                                                                
         LA    RE,WORK                           JUST READ TO SEE IF IT         
         ST    RE,ATSIOREC                       ALREADY EXIST                  
         MVC   WORK,WORK2                                                       
         GOTOR (#WRITTSR,AWRITTSR),DMCB                                         
                                                                                
** EXTRACT DATA FOR PAV FILE **                                                 
                                                                                
PVHOOK18 MVC   WORK,SPACES                                                      
**       GOTO1 =V(DEFINE),MYDMCB,=C'PROG+',DBLOCK,WORK,RR=SRVRRELO              
         GOTO1 VDEFINE,MYDMCB,=C'PROG+',DBLOCK,WORK                             
         MVC   THISPROG,WORK        SET THIS PROGRAM'S NAME                     
         MVC   CUMPNAM2,WORK                                                    
         CLC   CUMPNAM1,SPACES                                                  
         BNE   *+10                                                             
         MVC   CUMPNAM1,WORK                                                    
*&&DO                                                                           
                                                                                
         DS    0H                  GET TOTAL DURATION                           
         MVI   WORK,0                                                           
         GOTO1 (RF),MYDMCB,=C'TOTDUR',DBLOCK,WORK                               
         MVC   THISNQH,WORK         SET THIS PROGRAM'S DURATION                 
                                                                                
         DS    0H                  GET NUMBER OF DAYS                           
         MVI   WORK,0                                                           
         GOTO1 (RF),MYDMCB,=C'NDAYS',DBLOCK,WORK                                
         MVC   THISNDYS,WORK        SET THIS PROGRAM'S # OF DAYS                
*&&                                                                             
                                                                                
         MVI   WORK,0              GET NUMBER OF WEEKS                          
**       GOTO1 =V(DEFINE),MYDMCB,=C'WEEK',DBLOCK,WORK,RR=SRVRRELO               
         GOTO1 VDEFINE,MYDMCB,=C'WEEK',DBLOCK,WORK                              
         ZICM  RE,WORK,(1)                                                      
         BNZ   PVHOOK20                                                         
         CLI   DBSELMED,C'N'       ZERO=5 WEEKS FOR NETWORK                     
         BE    *+6                                                              
         DC    H'0'                NON-NETWORK -> 0 WKS IS INVALID              
         LA    RE,X'10'            SET A 5TH WK BIT                             
                                                                                
PVHOOK20 STC   RE,TMPWKS                                                        
         LR    RF,RE                                                            
         A     RF,A1STWKTB                                                      
         MVC   THIS1WK,0(RF)        SET THIS PROGRAM'S 1ST WEEK                 
         A     RE,ANUMWKTB                                                      
         MVC   THISNWK,0(RE)        SET THIS PROGRAM'S # OF WEEKS               
                                                                                
         DS    0H                  LOOK UP VUTS (TO DERIVE SHARES)              
         GOTO1 VDEMOUT,MYDMCB,(C'L',AVUTLIST),DBLOCK,QHVUTS                     
         OC    QHVUTS(4),QHVUTS    IF NO VUTS,                                  
         BNZ   PVHOOK30                                                         
         GOTO1 VDEMOUT,MYDMCB,(C'L',APUTLIST),DBLOCK,QHVUTS                     
PVHOOK30 DS    0H                  CALL REGETIUN FOR DEMO VALUES                
         LA    RE,DEMODEMS                                                      
         ST    RE,ADEMLIST          SET A(DEMO LIST)                            
         LA    R0,QHVUTS                                                        
         ST    R0,AVUTS             SET A(VUTS TO USE)                          
         ST    R6,ADBLOCK                                                       
         GOTOR =A(PROCIUN),RR=SRVRRELO                                          
                                                                                
         DS    0H                                                               
         LA    R3,CUMSHR                                                        
         ST    R6,ADBLOCK                                                       
         GOTOR =A(GETSHR),RR=SRVRRELO                                           
                                                                                
         DS    0H                  GO EXTRACT DEMO VALUES                       
         LA    R0,DEMODEMS                                                      
         ST    R0,ADEMLIST          SET A(DEMO LIST)                            
         MVC   AXTRCREC,AIUNWRK     R5-->RECORD CONTAINING DEMO VALUES          
         GOTOR =A(XTRCTDMV),RR=SRVRRELO                                         
         DS    0H                  MAD IUNWRK RECD TO CUMULATIVE AREA           
         MVC   MTHCFACS,ADBLOCK                                                 
         ZICM  R0,DBFACTOR,(3)                                                  
         ST    R0,MTHFCTR           MATH FACTOR                                 
         A     R0,CUMFCTR                                                       
         ST    R0,CUMFCTR           UPDATE CUMULATIVE FACTOR AS WELL            
         MVC   MTHIFIL,=C'INV'                                                  
         MVC   MTHOFIL,=C'INV'                                                  
         MVC   MTHOSRC,=C'NSI'                                                  
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,(CDEMOMTH-COMFACSD)(RF)                                       
         GOTO1 (RF),MYDMCB,=C'MAD',AIUNWRK,AIO3,MATHFAC                         
                                                                                
PVHOOK32 CLI   RECALFLG,C'Y'         RECALC DONT PASS BACK                      
         BNE   *+12                  DETAILS RECORD BUT SET FLAG                
         MVI   ANYDETLS,C'Y'         FOR SUMMARY TO BE PASSED                   
         B     PVHKX                                                            
                                                                                
         CLI   SAVGFLAG,C'Y'         SAVG/SHARE INDEX                           
         BNE   *+12                  DETAILS RECORD BUT SET FLAG                
         MVI   ANYDETLS,C'Y'         FOR SUMMARY TO BE PASSED                   
         B     PVHKX                                                            
                                                                                
         LA    R3,PGDTVALS                                                      
         USING PGDTVALS,R3                                                      
         LA    R4,OHDRVALS                                                      
OHDR     USING OHDRVALS,R4                                                      
         MVC   OHDR.OHDRDAY,TMPDAY                                              
         MVC   PDAYS,TMPDAY                                                     
         MVC   PDYTIM(3),FULL2                                                  
         MVC   OHDR.OHDRDYCD,FULL2                                              
         MVC   PSTIME,TMPSETIM                                                  
         MVC   PETIME,TMPSETIM+2                                                
         OC    PSTIME,PSTIME                                                    
         BNZ   *+10                                                             
         MVC   PSTIME,=H'2400'                                                  
         OC    PETIME,PETIME                                                    
         BNZ   *+10                                                             
         MVC   PETIME,=H'2400'                                                  
         MVC   OHDR.OHDRSTIM,PSTIME                                             
         MVC   OHDR.OHDRETIM,PETIME                                             
         OC    TMPBKWK,TMPBKWK                                                  
         BZ    PVHOOK34                                                         
         XC    PWEEKS,PWEEKS                                                    
         MVC   PWEEKS(1),TMPBKWK                                                
         OI    PWEEKS,X'F0'                                                     
         B     PVHOOK36                                                         
PVHOOK34 GOTOR =A(TRSLTWKS),RR=SRVRRELO                                         
         ZICM  RE,HALF1,(3)                                                     
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PWEEKS(0),WORK                                                   
*                                                                               
* NEW CALL TO FORMATBK ROUTINE TO FORMAT BOOK OUTPUT STRING                     
PVHOOK36 XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OPA',PFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,PBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),INPWEEK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*                                                                               
*&&DO                                                                           
**********************************************************************          
PVHOOK36 MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         CLI   LATFLAG,C'Y'                                                     
         BNE   *+14                                                             
         MVC   PBOOK(6),=C'LATEST'                                              
         B     PVHOOK38                                                         
         CLI   TMPBOOK+1,X'FF'                                                  
         BNE   PVHK36A                                                          
         MVC   PBOOK(3),=C'LAT'                                                 
         MVC   PBOOK+3(1),TMPBOOK+2                                             
         B     PVHOOK38                                                         
PVHK36A  MVC   PBOOK(L'MYBK),MYBK                                               
         CLI   PBOOK,C'P'                         NO P/E BOOKS FOR              
         BE    *+8                                PAV AND SET FLAG              
         CLI   PBOOK,C'E'                         NOT TO HAVE SUMMARY           
         BNE   PVHK36B                                                          
         MVI   ANYDETLS,C'N'                                                    
         B     PVHKX                                                            
PVHK36B  LA    RE,PBOOK+5                                                       
         CLI   TMPBKTYP,0                                                       
         BE    PVHOOK37                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
PVHOOK37 CLI   INPWEEK,0                                                        
         BE    PVHOOK38                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),INPWEEK                                                  
****************************************************************                
*&&                                                                             
PVHOOK38 MVC   PPROG,THISPROG                                                   
         MVI   PACTF,C'Y'                                                       
         CLI   TMPBEST,C'B'                                                     
         BE    *+8                                                              
         MVI   PACTF,C'N'                                                       
         MVC   PSTAT,DUMSTAT                                                    
         CLI   PSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   PSTAT+4,0                                                        
         MVC   OHDR.OHDRSTA(L'DUMSTAT),DUMSTAT                                  
         DROP  OHDR                                                             
*                                                                               
         CLI   HDRFLAG,C'Y'                                                     
         BE    PVHOOK60                                                         
         MVC   PFILE,=C'PAV'                                                    
         MVC   PGDTNUM,=X'0001'                                                 
         MVI   PGDTSEND,C'Y'                                                    
         LA    R4,PMFID                                                         
         USING MFIDD,R4                                                         
         EDIT  (B1,PDAYS),(3,MFDAY),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0             
         EDIT  (B2,PSTIME),(4,MFSTIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
                                                                                
         EDIT  (B2,PETIME),(4,MFETIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
*&&DO                                                                           
         OC    TMPUINDX,TMPUINDX                                                
         BZ    PVHOOK39                                                         
         CLC   PETIME,SVSETIM+2                       END TIME CANT BE          
         BL    PVHOOK39                               > THEN REQUESTED          
         EDIT  (B2,SVSETIM+2),(4,MFETIME),ALIGN=RIGHT,ZERO=NOBLANK,             
               FILL=0                                                           
*&&                                                                             
PVHOOK39 MVC   MFPURE,TMPPURE                                                   
         MVC   PFACT,DBFACTOR                                                   
         DROP  R4                                                               
                                                                                
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         DROP  R3                                                               
                                                                                
         LA    R3,DDEMVALS                                                      
         USING DDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
PVHOOK40 MVI   DDEMSEND,C'Y'                                                    
         MVC   DDEMOS(5),0(R2)                                                  
         AHI   R3,DDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,PVHOOK40                                                      
         MVC   DDEMNUM,NUMDEMO                                                  
         J     PVHOOK80                                                         
PVHOOK60 MVI   OHDRSEND,C'Y'                                                    
         MVC   OHDRNUM,=X'0001'                                                 
PVHOOK80 CLI   TMPBEST,C'B'                                                     
         BNE   *+8                                                              
         MVI   ANYDETLS,C'Y'       WE HAVE DETAILS                              
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         DROP  R6                                                               
                                                                                
PVHKX    J     EXITY                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
VUTLIST  DC    X'81',C'V',AL1(1)   RATING TIMES SHARE                           
         DC    X'81',C'V',AL1(2)                                                
         DC    X'81',C'V',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
PUTLIST  DC    X'81',C'P',AL1(1)   STRAIGHT PUT                                 
         DC    X'81',C'P',AL1(2)                                                
         DC    X'81',C'P',AL1(3)                                                
         DC    X'FF'                                                            
                                                                                
SHRLIST  DC    X'00',C'S',AL1(1)   SHARES                                       
         DC    X'00',C'S',AL1(2)                                                
         DC    X'00',C'S',AL1(3)                                                
         DC    X'FF'                                                            
NUMWKTAB DC    AL1(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4)                             
FRSTWKTB DC    AL1(0,4,3,3,2,2,2,2,1,1,1,1,1,1,1,1)                             
                                                                                
**********************************************************************          
*                                                                    *          
*     PROCESS INVENTORY DETAILS INFORMATION                          *          
*                                                                    *          
**********************************************************************          
INVLOOK  NTR1  BASE=*                                                           
         OC    TMPINVNM,TMPINVNM             NO INVENTORY NUMBER                
         BZ    INVLK200                      REQUESTED                          
         LA    R5,FETCHBLK                                                      
         USING RFTBLKD,R5                                                       
         LA    R0,RFTBLKD          INITIALIZE FETCH BLOCK                       
         LHI   R1,RFTBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   RFTACOM,ACOMFACS                                                 
         MVC   RFTAIO1,AIO1                                                     
         MVC   RFTAIO2,AIO3                                                     
         LA    R0,FETCHWRK                                                      
         STCM  R0,15,RFTAWRK                                                    
         MVC   RFTCREP,AGYALPH                                                  
         MVI   RFTCSRC,C'N'                                                     
         LA    RE,INVHOOK                                                       
         STCM  RE,15,RFTHOOKA                                                   
                                                                                
         MVC   RFTCSTAT,TMPSTA                                                  
         CLI   RFTCSTAT+L'RFTCSTAT-1,C' '                                       
         BNE   *+8                                                              
         MVI   RFTCSTAT+L'RFTCSTAT-1,C'T'                                       
*&&DO                                                                           
         CLC   =C'B1',AGYALPH                    TEST REP IS TELEMUDO           
         BE    *+14                              OR EJOR (TEST SYSTEM)          
         CLC   =C'B3',AGYALPH                                                   
         BNE   *+10                                                             
         CLC   =C'TELE',RFTCSTAT                 YES-TEST STAT=TELE             
         BNE   *+10                                                             
         MVC   RFTCSTAT,=C'TEL H'                YES -SET LOOKUP TO             
*&&                                                                             
         CLI   RFTCSTAT+3,C'+'                                                  
         BNE   INVLK034                                                         
         MVC   RFTCSTAT+3(2),=C' 1'                                             
         B     INVLK036                                                         
INVLK034 CLI   RFTCSTAT+4,C'+'                                                  
         BNE   INVLK036                                                         
         MVC   RFTCSTAT+4(1),=C'1'                                              
                                                                                
INVLK036 MVC   RFTCEFST,TMPEFFDT                                                
         MVC   RFTCEFEN,TMPEFFDT+2                                              
***      MVI   RFTAMODE,RFTADIRQ                                                
         MVI   RFTAMODE,RFTAMSTQ         USE DAYTIME LOOKUP?                    
         MVI   RFTCDCTL,RFTCDC1Q                 FETCH METHOD                   
         MVI   RFTCNTL,RFTCHDRQ                                                 
         OI    RFTCNTL,RFTCDEMQ                                                 
         OI    RFTCNTL,RFTCFTNQ                                                 
         OI    RFTCNTL,RFTCTXTQ                                                 
         OI    RFTCNTL,RFTCSLVQ                  INCLUDE SHARES/LVLS            
         MVC   RFTCINV,TMPINVNM                  INVENTORY NUM                  
                                                                                
         L     R2,AIO2                                                          
*&&DO                                                                           
         MVI   RFTCDTMS,X'FF'                                                   
         STCM  R2,15,RFTCDTMS+1                                                 
         USING RFTCDTMS,R2                                                      
         XC    RFTCDTMS(RFTCDTLQ),RFTCDTMS                                      
         MVC   RFTCDTDY(5),TMPDYTIM               1 DYTIME AT A TIME            
         AHI   R2,RFTCDTLQ                                                      
         XC    RFTCDTMS(RFTCDTLQ),RFTCDTMS                                      
         AHI   R2,RFTCDTLQ                                                      
         DROP  R2                                                               
*&&                                                                             
         MVI   RFTCBKS,X'FF'                                                    
         STCM  R2,15,RFTCBKS+1                                                  
CBKS     USING RFTCBKS,R2                                                       
         XC    CBKS.RFTCBKS(RFTCBKLQ),CBKS.RFTCBKS                              
         MVC   CBKS.RFTCBKS(L'TMPBOOK),TMPBOOK                                  
         MVI   CBKS.RFTCBKFL,C'I'                                               
         MVC   CBKS.RFTCBKSV,TMPBKTYP                                           
         AHI   R2,RFTCBKLQ                                                      
         XC    CBKS.RFTCBKS(RFTCBKLQ),CBKS.RFTCBKS                              
         DROP  CBKS                                                             
                                                                                
         LA    RE,RFTCDEMS                                                      
         LA    R1,DEMODEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
INVLK60  MVC   0(L'RFTCDEMS,RE),0(R1)                                           
         AHI   RE,L'RFTCDEMS                                                    
         AHI   R1,L'DEMODEMS                                                    
*   NEW CODE FETCH DOESNT NEED US TO ASK FOR SHARES AND PUTS                    
*   IN DEMO LIST IT GIVES IT ANYWAYS                                            
         AHI   R1,L'DEMODEMS*2                                                  
         SHI   R0,2                                                             
                                                                                
         BCT   R0,INVLK60                                                       
         MVI   0(RE),X'FF'                                                      
                                                                                
         MVI   INVFLAG,C'N'                       ASSUME NO INVENTORY           
         GOTOR VFETCH,PARM,RFTBLKD                                              
         CLI   INVFLAG,C'N'                                                     
         BE    INVLK200                                                         
         J     INVLKX                                                           
                                                                                
INVLK200 LA    R3,SUMMVALS                                                      
         USING SUMMVALS,R3                                                      
         MVI   SUMMSEND,C'Y'                                                    
         MVC   SFILE,=C'INV'                                                    
         MVC   SSTAT(L'DUMSTAT),DUMSTAT                                         
                                                                                
         CLI   SSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   SSTAT+4,0                                                        
                                                                                
         MVC   SPROG(11),=C'NO INV DATA'                                        
* 2 CHARACTER BOOKTYPE CODE                                                     
         XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_RMINV                     REP MONTHLY INVENTORY          
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,SBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*&&DO                                                                           
*********************************************************************           
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         MVC   SBOOK(L'MYBK),MYBK                                               
         CLI   TMPBKTYP,0                                                       
         BE    INVLK220                                                         
         MVC   SBOOK+5(1),=C'('                                                 
         MVC   SBOOK+6(1),TMPBKTYP                                              
*                                                                               
*                                                                               
         MVC   SBOOK+7(1),=C')'                                                 
********************************************************************            
*&&                                                                             
INVLK220 MVC   SUMMNUM,=H'1'                                                    
                                                                                
         LA    R3,SDEMVALS                        PASS ZEROS TO PC FOR          
         USING SDEMVALS,R3                        NO DATA FOR NOW               
         LA    R2,RFTFDEMS                                                      
         XC    SDEMOS,SDEMOS                                                    
         MVC   SDEMNUM,NUMDEMO                                                  
                                                                                
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
INVLKX   J     EXITY                                                            
                                                                                
                                                                                
INVHOOK  NTR1                                                                   
         OC    RFTERR,RFTERR       DIE IF ANY ERRORS RETURNED                   
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   RFTMODE,RFTNBKQ     NEW BOOK                                     
         BE    INVHK10                                                          
         B     INVHKX                                                           
                                                                                
INVHK10  OC    RFTFDEMS(RFTCNDQ*L'RFTFDEMS),RFTFDEMS                            
         BNZ   *+10                                                             
         OC    RFTFSHRS(RFTCNDQ*L'RFTFSHRS),RFTFSHRS                            
         BNZ   *+10                                                             
         OC    RFTFLVLS(RFTCNDQ*L'RFTFLVLS),RFTFLVLS                            
         BZ    INVHKX                                                           
         MVI   INVFLAG,C'Y'                                                     
*                                                                               
* NEW CODE - REFETCH DOES A POOR JOB FILTERING THE EFFECTIVE DATES.             
* FILTER IT OUT OURSELVES. WE HAD A PROBLEM WITH GETTING MULTIPLE INV           
* RECORDS WITH THE SAME INV NUMBER BUT DIFFERENT EFF DATES                      
* FILTER OUT START DATES                                                        
*                                                                               
*                                                                               
         L     R3,RFTAIO1                                                       
         USING REINVRCD,R3                                                      
*&&DO                                                                           
         GOTO1 VDATCON,DMCB,(2,RFTCEFST),(3,FULL)                               
         CLC   RINVKSTD,FULL                                                    
         BL    INVHKX                                                           
         CLC   RINVKSTD,FULL                                                    
         BNE   INVHKX                                                           
         XC    FULL,FULL                                                        
*&&                                                                             
**       GOTO1 VDATCON,DMCB,(2,RFTCEFEN),(3,FULL)                               
         CLC   RFTCEFST,RINVPEFF                                                
         BNE   INVHKX                                                           
         CLC   RFTCEFEN,RINVPEFF+2                                              
         BNE   INVHKX                                                           
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
         LA    R3,PGDTVALS                                                      
         USING PGDTVALS,R3                                                      
         MVI   PGDTSEND,C'Y'                                                    
         MVC   PFILE,=C'INV'                                                    
         MVI   PACTF,C'Y'                                                       
         MVC   PSTAT(L'DUMSTAT),DUMSTAT                                         
                                                                                
         CLI   PSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   PSTAT+4,0                                                        
                                                                                
         L     RE,RFTFTX1A                                                      
         MVC   PPROG,3(RE)                                                      
         MVC   PDAYS,TMPDYTIM                                                   
         MVC   PSTIME,TMPDYTIM+1                                                
         MVC   PETIME,TMPDYTIM+3                                                
         OC    PSTIME,PSTIME                                                    
         BNZ   *+10                                                             
         MVC   PSTIME,=H'2400'                                                  
         OC    PETIME,PETIME                                                    
         BNZ   *+10                                                             
         MVC   PETIME,=H'2400'                                                  
*                                                                               
* 2 CHARACTER BOOKTYPE CODE                                                     
         XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_RMINV            REP MONTHLY INVENTORY                   
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,PBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*********************************************************************           
*&&DO                                                                           
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         MVC   PBOOK(L'MYBK),MYBK                                               
         CLI   TMPBKTYP,0                                                       
         BE    INVHK37                                                          
         LA    RE,PBOOK                                                         
INVHK34  CLI   0(RE),X'40'                                                      
         BNH   INVHK36                                                          
         AHI   RE,1                                                             
         B     INVHK34                                                          
INVHK36  MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
*&&                                                                             
*******************************************************************             
                                                                                
INVHK37  MVC   PGDTNUM,=H'1'                                                    
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         DROP  R3                                                               
                                                                                
         OC    RFTFDEMS(RFTCNDQ*L'RFTFDEMS),RFTFDEMS                            
         BNZ   *+10                                                             
         OC    RFTFSHRS(RFTCNDQ*L'RFTFSHRS),RFTFSHRS                            
         BNZ   *+10                                                             
         OC    RFTFLVLS(RFTCNDQ*L'RFTFLVLS),RFTFLVLS                            
         LA    R3,DDEMVALS                                                      
         USING DDEMVALS,R3                                                      
         LA    R2,RFTFDEMS                                                      
         LA    RE,RFTFSHRS                                                      
         LA    RF,RFTFLVLS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
INVHK40  MVI   DDEMSEND,C'Y'                                                    
         MVC   DDEMOS,0(R2)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   R2,4                                                             
*  SHARES                                                                       
         MVC   DDEMOS,0(RE)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   RE,4                                                             
*  PUTS                                                                         
         MVC   DDEMOS,0(RF)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   RF,4                                                             
                                                                                
         SHI   R0,2                               SUB 2 FOR SHR/LVLS            
         BCT   R0,INVHK40                                                       
         MVC   DDEMNUM,NUMDEMO                                                  
         DROP  R3                                                               
                                                                                
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
         LA    R3,SUMMVALS                                                      
         USING SUMMVALS,R3                                                      
         MVI   SUMMSEND,C'Y'                                                    
         MVC   SFILE,=C'INV'                                                    
         MVC   SSTAT(L'DUMSTAT),DUMSTAT                                         
         CLI   SSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   SSTAT+4,0                                                        
         L     RE,RFTFTX1A                                                      
         MVC   SPROG,3(RE)                                                      
*                                                                               
*                                                                               
* 2 CHARACTER BOOKTYPE CODE                                                     
         XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_RMINV                                                    
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,SBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
***********************************************************                     
*&&DO                                                                           
         MVC   SBOOK(L'MYBK),MYBK                                               
         CLI   TMPBKTYP,0                                                       
         BE    INVHK74                                                          
         LA    RE,SBOOK                                                         
INVHK64  CLI   0(RE),X'40'                                                      
         BNH   INVHK70                                                          
         AHI   RE,1                                                             
         B     INVHK64                                                          
INVHK70  MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
*&&                                                                             
***********************************************************                     
INVHK74  MVC   SUMMNUM,=H'1'                                                    
         DROP  R3                                                               
         LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,RFTFDEMS                                                      
         LA    RE,RFTFSHRS                                                      
         LA    RF,RFTFLVLS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
INVHK80  MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   R2,4                                                             
*  SHARES                                                                       
         MVC   SDEMOS,0(RE)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   RE,4                                                             
*  PUTS                                                                         
         MVC   SDEMOS,0(RF)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   RF,4                                                             
                                                                                
         SHI   R0,2                               SUB 2 FOR SHR/LVLS            
         BCT   R0,INVHK80                                                       
         MVC   SDEMNUM,NUMDEMO                                                  
                                                                                
         MVC   OUTUPEXP,RFTFUPGR                  UPGRADE EXPRESSION            
                                                                                
         DROP  R3                                                               
                                                                                
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
INVHKX   J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB,R5                                                            
                                                                                
**********************************************************************          
DEMUPGD  NTR1  BASE=*                                                           
         XC    TMPUPNAM,TMPUPNAM                                                
         OC    TMPMBKS,TMPMBKS              MULTIBOOK AVERAGE?                  
         JNZ   DEMUP95                                                          
*  LOOK FOR CORRECT UPGRADE FORMULA                                             
         SR    RE,RE                                                            
         ICM   RE,7,AUPGRD                                                      
         LA    R1,1                                                             
         TM    UPGRIND,LQ_TSINQ                                                 
         BNO   DEMUP90                                                          
         LA    RE,LW_DATA1-LW_D(RE)                                             
         J     DEMUP92                                                          
DEMUP90  TM    UPGRIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,3,LW_NUMN-LW_D(RE)                                            
         LA    RE,LW_DATA2-LW_D(RE)                                             
*  NOW FIND THE CORRECT PROJECTION FORMULA BASE ON THE PROJECTION INDEX         
*  CURRENTLY SEEKING  INDEX START AT 1                                          
         USING UPGRADD,RE                                                       
DEMUP92  CLC   UPINDEX,TMPUINDX                                                 
         BE    DEMUP94                                                          
         AHI   RE,UPGRADX                                                       
         BCT   R1,DEMUP92                                                       
         DC    H'0'                         MUST FIND UPGRADE INDEX             
*  FOUND THE CORRECT PROJECTION                                                 
DEMUP94  STCM  RE,15,AUPGRADE                                                   
         MVC   TMPUPNAM,UPNAME                                                  
DEMUP95  CLC   =C'PAV',INPFIL                                                   
         JE    DEMUP400                                                         
         L     R6,=A(SPDEMUP1-WORKD)                                            
         LA    R6,WORKD(R6)                                                     
         USING SPDEMUPD,R6                                                      
         XC    0(SPDEMUP2,R6),0(R6)                                             
* MULTIBOOK AVERAGE                                                             
         XC    SPDEMUPD,SPDEMUPD                                                
         MVC   SPUPAREC,AIO1                                                    
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGYALPH                                                  
         MVC   SPUPMED,DBMED                                                    
         MVC   SPUPSTA,TMPSTA                                                   
****** SINCE COMPARAGRAPH IS ONLY USED BY THE REP GROUP ALWAYS SET              
******  SPUPSYS TO "R"                                                          
         MVI   SPUPSYS,C'R'                                                     
                                                                                
         OC    TMPMBKS,TMPMBKS              MULTIBOOK AVERAGE?                  
         JZ    DEMUP98                                                          
         MVC   SPUPFBK,TMPBOOK+1                                                
         MVC   SPUPBTYP,TMPBKTYP                                                
         MVC   SPUPFBKL,TMPMBKS                                                 
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPTYPE(8),=XL8'0400006400000000'                               
         CLI   RECALFLG,C'Y'                                                    
         JE    DEMUP110                                                         
         J     DEMUP99                                                          
                                                                                
DEMUP98  MVC   SPUPFBK,UPSHRBK                                                  
                                                                                
         MVC   SPUP2YRP,UP2YRP                                                  
         CLI   UP2YRP,C'2'                                                      
         BNE   *+10                                                             
         XC    SPUP2YRP,SPUP2YRP                                                
         MVC   SPUP2YRR,UP2YRR                                                  
         CLI   UP2YRR,C'2'                                                      
         BNE   *+10                                                             
         XC    SPUP2YRR,SPUP2YRR                                                
         MVC   SPUPUDAY,UPDYTIM                                                 
         MVC   SPUPUTIM,UPDYTIM+1                                               
         MVC   SPUPTYPE(L'UPBKVAL),UPBKVAL                                      
         MVC   SPUPBTYP,UPBKTYP                                                 
         MVI   SPUPFIL,C'T'                                                     
         MVC   TMPBKWK,UPWEEK                                                   
         XC    TMPBOOK,TMPBOOK                                                  
         MVC   TMPBOOK+1(2),SPUPFBK                                             
         MVC   TMPBKTYP,UPBKTYP                                                 
         MVC   BYTE2,UPWEEK                                                     
         NI    BYTE2,X'0F'                                                      
         PACK  BYTE1,BYTE2                                                      
         OC    SPUPFBK+1(1),BYTE1                                               
         DROP  RE                                                               
                                                                                
                                                                                
*  IF ROTATION THEN PROCESS ROTATION                                            
*  ROTATION HAS MORE THAN ONE DAYTIME (5 BYTES) IN TMPDYTIM                     
DEMUP99  LA    R3,TMPDYTIM                                                      
         MVI   ROTFLAG,C'N'                                                     
         CLI   TMPDYTIM+5,X'FF'                                                 
         BNE   DEMUP100                                                         
         MVC   SPUPDAY,TMPDYTIM                                                 
         MVC   SPUPTIM,TMPDYTIM+1                                               
         MVC   TMPDAY,SPUPDAY                                                   
         MVC   TEMPSTIM,SPUPTIM                                                 
         MVC   TEMPETIM,SPUPTIM+2                                               
         B     DEMUP124                                                         
* PROCESS ROTATION COMPONENTS FIRST THEN DO ENTIRE ROTATION                     
* UNLESS RECALC JUST DO ENTIRE ROTATION AND GET SUMMARY                         
                                                                                
DEMUP100 MVI   ROTFLAG,C'Y'                                                     
         CLI   RECALFLG,C'Y'       RECALC JUST DO SUMMARY                       
         JE    DEMUP110                                                         
*                                                                               
DEMUP108 CLI   0(R3),X'FF'         IF DONE COMPONENTS THEN                      
         BE    DEMUP110            SET UP ENTIRE ROTATION                       
         MVC   SPUPDAY,0(R3)                                                    
         MVC   SPUPTIM,1(R3)                                                    
         MVC   TMPDAY,SPUPDAY                                                   
         MVC   TEMPSTIM,SPUPTIM                                                 
         MVC   TEMPETIM,SPUPTIM+2                                               
         AHI   R3,5                                                             
         ST    R3,DYTMPTR                                                       
         B     DEMUP124                                                         
                                                                                
DEMUP110 LA    R3,TMPDYTIM                                                      
                                                                                
DEMUP112 MVI   ROTFLAG,C'D'         SET TO DONE                                 
         XC    SPUPEXTN,SPUPEXTN                                                
         XC    DUB,DUB                                                          
         LA    R0,DBXTROTN                                                      
         ST    R0,DUB+4                                                         
         MVI   SDBXF,C'U'                                                       
         ST    R6,DUB1                                                          
         GOTOR STDBXLNK                                                         
         ICM   RF,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTLD,RF                                                        
         LA    RE,DBXTLIST                                                      
         MVC   DBXTLID,=C'DYTM'                                                 
         MVI   DBXTLIDX,0                                                       
* IS THIS RECALC AVERAGE  ??                                                    
         CLI   RECALFLG,C'Y'                                                    
         BNE   DEMUP123                                                         
                                                                                
*      GET MAINFRAME ID'S   (DAY,STIME,ETIME,PURE NUMBER)                       
         SR    R3,R3                                                            
         ICM   R3,7,AMFID                                                       
         LA    R2,1                                                             
         TM    MFIDIND,LQ_TSINQ                                                 
         BNO   DEMUP114                                                         
         LA    R3,LW_DATA1-LW_D(R3)                                             
         J     DEMUP116                                                         
DEMUP114 TM    MFIDIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R3)                                            
         LA    R3,LW_DATA2-LW_D(R3)                                             
                                                                                
DEMUP116 PACK  DUB,0(3,R3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,TMPMFID                                                       
         XC    DUB,DUB                                                          
         PACK  DUB,3(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+1                                                   
         XC    DUB,DUB                                                          
         PACK  DUB,7(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+3                                                   
                                                                                
         MVC   0(5,RE),TMPMFID                                                  
                                                                                
         AHI   RE,5                                                             
         AHI   R3,15                                                            
         BCT   R2,DEMUP116                                                      
         B     DEMUP124                                                         
                                                                                
DEMUP123 MVC   0(5,RE),0(R3)                                                    
         AHI   RE,5                                                             
         AHI   R3,5                                                             
         CLI   0(R3),X'FF'                                                      
         BNE   DEMUP123                                                         
         MVI   0(RE),0                                                          
DEMUP124 OI    SPUPOPTS,SPOPEXT                   EXTENDED BLK PRESENT          
         MVC   SPUPSPL,SPILLMKT                                                 
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPSRC,DBSRC                                                    
         CLC   =C'T4',INPFIL                                                    
         BNE   *+8                                                              
         MVI   SPUPTPTT,C'P'                                                    
         CLC   =C'TF',INPFIL                     FUSION                         
         BNE   *+8                                                              
         MVI   SPUPSRC,C'F'                                                     
         MVC   SPUPSYSC,TMPSYSC                  SYSCODE                        
                                                                                
         OI    SPUPOPTS,SPOPDMAI                 IMPRESSIONS                    
         CLI   PROF1W+7,C'Y'                                                    
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                 IF REP SYSTEM                  
                                                                                
         CLI   OVSYS,8                           ALWAYS NORMALIZE               
         JNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
**       CLI   SPUPSRC,C'F'                      FUSION                         
**       BNE   *+16                              UNLESS YOU ASK FOR             
**       CLI   OPTDEC,C'1'                       DEC=1 THEN DEFAULT             
**       BE    *+14                              TO DEC=2                       
**       OI    SPUPOPTS,X'01'                                                   
**       MVC   OPTDEC,C'2'                                                      
         CLI   OPTDEC,C'2'                                                      
         BNE   *+8                                                              
         OI    SPUPOPTS,X'01'                                                   
*                                                                               
         MVC   SPUPUID,USERID                                                   
         GOTO1 VSPDEMUP,DMCB,SPDEMUPD,DEMODEMS,THISDEMS                         
***      CLI   0(R1),0                                                          
***      JNE   EXITY                                                            
         MVI   ANYTPFLG,C'Y'                                                    
         CLI   0(R1),0                                                          
         JE    DEMUP128                                                         
         MVI   ANYTPFLG,C'N'                                                    
         J     DEMUP165                                                         
* NOW SEND RECORDS TO PC                                                        
* PROCESS THE DETAIL                                                            
DEMUP128 CLI   ROTFLAG,C'D'                       NO DETAILS                    
         BE    DEMUP165                                                         
                                                                                
         LA    R3,PGDTVALS                                                      
         USING PGDTVALS,R3                                                      
         MVI   PGDTSEND,C'Y'                                                    
         MVC   PFILE,INPFIL                                                     
         MVI   PACTF,C'N'                                                       
         CLI   ALLTPFLG,C'Y'                                                    
         BE    *+8                                                              
         MVI   PACTF,C'Y'                                                       
         MVC   PSTAT(L'DUMSTAT),DUMSTAT                                         
                                                                                
         CLI   PSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   PSTAT+4,0                                                        
                                                                                
         MVC   PPROG(L'SPUPPRG),SPUPPRG                                         
         MVC   PWEEKS(1),TMPBKWK                                                
         OI    PWEEKS,X'F0'                                                     
         MVC   PDAYS(1),TMPDAY                                                  
         MVC   PSTIME,TEMPSTIM                                                  
         MVC   PETIME,TEMPETIM                                                  
         OC    PSTIME,PSTIME                                                    
         BNZ   *+10                                                             
         MVC   PSTIME,=H'2400'                                                  
         OC    PETIME,PETIME                                                    
         BNZ   *+10                                                             
         MVC   PETIME,=H'2400'                                                  
*                                                                               
*                                                                               
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,PBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),TMPBKWK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
*                                                                               
*&&DO                                                                           
DEMUP138 MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         MVC   PBOOK(L'MYBK),MYBK                                               
         CLI   PBOOK,C'P'                                                       
         BE    *+8                                                              
         CLI   PBOOK,C'E'                                                       
         BNE   DEMUP146                                                         
         MVI   ANYTPFLG,C'N'                                                    
         B     DEMUP157                                                         
DEMUP146 LA    RE,PBOOK+5                                                       
         CLI   SPUPBTYP,0                                                       
         BE    DEMUP156                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
DEMUP156 CLI   TMPBKWK,0                                                        
         BE    DEMUP157                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),TMPBKWK                                                  
*******************************************************************             
*&&                                                                             
                                                                                
* multibook code                                                                
DEMUP157 OC    TMPMBKS,TMPMBKS                                                  
         BZ    DEMUP162                                                         
         LA    R4,TMPMBKS                                                       
******** LR    R2,RE                                                            
         LA    R0,3                                                             
*                                                                               
DEMUP158 MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),WORK                                                   
         ST    R2,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
***********************************************************************         
*&&DO                                                                           
DEMUP158 MVI   MYBKH,18                          CREATE FAKE FIELD              
         MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         GOTOR =V(UNBOOK),DMCB,(1,WORK),MYBKH,0,(C'+',=CL6' '),        +        
               RR=SRVRRELO                                                      
         MVC   0(L'MYBK,R2),MYBK                                                
         LA    RE,5(R2)                                                         
         CLI   SPUPBTYP,0                                                       
         BE    DEMUP159                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
****************************************************************                
*&&                                                                             
DEMUP159 AHI   R4,2                                                             
*****    LR    R2,RE                                                            
         OC    0(2,R4),0(R4)                                                    
         BZ    DEMUP162                                                         
         BCT   R0,DEMUP158                                                      
DEMUP162 MVC   PGDTNUM,=H'1'                                                    
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'PBOOK),PBOOK                                             
         LA    R4,PMFID                                                         
         USING MFIDD,R4                                                         
         EDIT  (B1,PDAYS),(3,MFDAY),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0             
         EDIT  (B2,PSTIME),(4,MFSTIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         EDIT  (B2,PETIME),(4,MFETIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         DROP  R3                                                               
         DROP  R4                                                               
         LA    R3,DDEMVALS                                                      
         USING DDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DEMUP164 MVI   DDEMSEND,C'Y'                                                    
         CLI   OPTDEC,C'2'              TURN OF THE X'40' INDICATOR             
         BNE   *+8                      FOR EXTENDED PRECISION SET              
         NI    0(R2),X'FF'-X'40'        BY SPDEMUP                              
         MVC   DDEMOS,0(R2)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DEMUP164                                                      
         MVC   DDEMNUM,NUMDEMO                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
         CLI   ROTFLAG,C'Y'                                                     
         JNE   DEMUP165                                                         
         L     R3,DYTMPTR                                                       
         J     DEMUP100                                                         
         DROP  R3                                                               
                                                                                
* PROCESS SUMMARY RECORD                                                        
                                                                                
DEMUP165 CLI   RECALFLG,C'Y'                     RECALC  ONLY HAS               
         BE    DEMUP170                          SUMMARY RECORD TO PASS         
         CLI   ALLTPFLG,C'Y'                     IF WE ARE DOING SECOND         
         JE    DEMUPX                            TP READ, T4-TP, TP-T4          
DEMUP170 LA    R3,SUMMVALS                       WE DONT NEED 2ND SUMM          
         USING SUMMVALS,R3                                                      
         MVI   SUMMSEND,C'Y'                                                    
         MVC   SFILE,INPFIL                                                     
         MVC   SSTAT(L'DUMSTAT),DUMSTAT                                         
         CLI   SSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   SSTAT+4,0                                                        
         MVC   SPROG(L'SPUPPRG),SPUPPRG                                         
                                                                                
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+16                                                             
         XC    SPROG,SPROG                                                      
         MVC   SPROG(17),=C'NO DATA AVAILABLE'                                  
         XC    SBOOK,SBOOK                                                      
         MVC   SBOOK(L'TMPUPNAM),TMPUPNAM                                       
*  ONLY MULTIBOOK AVERAGE GETS REAL BOOKS                                       
         OC    TMPMBKS,TMPMBKS                                                  
         BZ    DEMUP260                                                         
*                                                                               
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,SBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),TMPBKWK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
**********************************************************************          
*&&DO                                                                           
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         MVC   SBOOK(L'MYBK),MYBK                                               
         CLI   SBOOK,C'P'                                                       
         BE    *+8                                                              
         CLI   SBOOK,C'E'                                                       
         BNE   DEMUP176                                                         
         MVI   ANYTPFLG,C'N'                                                    
         B     DEMUP190                                                         
DEMUP176 LA    RE,SBOOK+5                                                       
         CLI   SPUPBTYP,0                                                       
         BE    DEMUP186                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
DEMUP186 CLI   TMPBKWK,0                                                        
         BE    DEMUP190                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),TMPBKWK                                                  
********************************************************************            
*&&                                                                             
                                                                                
DEMUP190 LA    R4,TMPMBKS                                                       
******   LR    R2,RE                                                            
         LA    R0,3                                                             
*                                                                               
DEMUP198 MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),WORK                                                   
         ST    R2,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),TMPBKWK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
*&&DO                                                                           
******************************************************************              
DEMUP198 MVI   MYBKH,18                          CREATE FAKE FIELD              
         MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         GOTOR =V(UNBOOK),DMCB,(1,WORK),MYBKH,0,(C'+',=CL6' '),        +        
               RR=SRVRRELO                                                      
         MVC   0(L'MYBK,R2),MYBK                                                
         LA    RE,5(R2)                                                         
         CLI   SPUPBTYP,0                                                       
         BE    DEMUP199                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                                                 
*                                                                               
*                                                                               
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
*********************************************************************           
*&&                                                                             
DEMUP199 AHI   R4,2                                                             
******   LR    R2,RE                                                            
         OC    0(2,R4),0(R4)                                                    
         BZ    DEMUP260                                                         
         BCT   R0,DEMUP198                                                      
                                                                                
                                                                                
DEMUP260 MVC   SUMMNUM,=H'1'                                                    
         DROP  R3                                                               
         LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DEMUP280 MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    SDEMOS,SDEMOS                                                    
         AHI   R3,SDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DEMUP280                                                      
         MVC   SDEMNUM,NUMDEMO                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         J     EXITY                                                            
* ---------------------------------------                                       
*    UPGRADE FOR PAV                    |                                       
* ---------------------------------------                                       
DEMUP400 L     R5,AIO3             INVENTORY-IZE CUMULATIVE RECORD              
         USING REINVRCD,R5                                                      
                                                                                
         MVC   RINVKSTA,TMPSTA        INVENTORY KEY FIELD: STATION              
                                                                                
         DS    0H                                                               
GKS      USING GKSPARMD,GKSPARM                                                 
         MVC   GKS.GKSPRSVC,DBSRC                                               
         MVI   GKS.GKSPQLFY,C' '                                                
         MVC   GKS.GKSPBTYP,TMPBKTYP                                            
                                                                                
*  THIS CODE IS FROM $DEM                                                       
*    BECAUSE WE CAN NOT FIT A BOOKTYPE OF "D" INTO THE                          
*    KEYSOURCE FIELD, WE SHOULD FOLLOW THE ADVICE WE                            
*    GIVE TO OUR REP CLIENTS--USE AN "S" QUALIFIER AND                          
*    DON'T GIVE IT A BOOKTYPE                                                   
                                                                                
         CLI   TMPBKTYP,C'd'                    CHECK LOWERCASE TOO             
         BE    GKSFUD10                                                         
         CLI   TMPBKTYP,C'p'                    ALSO CANT FIT BKTYPE P          
         BE    GKSFUD10                                                         
         CLI   TMPBKTYP,C'D'                                                    
         BE    GKSFUD10                                                         
         CLI   TMPBKTYP,C'P'                    ALSO CANT FIT BKTYPE P          
         BE    GKSFUD10                                                         
         CLI   TMPBKTYP,BOOKTYPE_L3                                             
         BE    GKSFUD10                                                         
         CLI   TMPBKTYP,BOOKTYPE_C3                                             
         BE    GKSFUD10                                                         
         CLI   TMPBKTYP,BOOKTYPE_W3                                             
         BE    GKSFUD10                                                         
         B     GKSFUDGEX                                                        
GKSFUD10 MVI   GKS.GKSPQLFY,C'S'                                                
         MVI   GKS.GKSPBTYP,0                                                   
GKSFUDGEX EQU  *                                                                
                                                                                
         GOTO1 =V(GETKSRC),DMCB,(C'Q',GKSPARM),GKSPARM,ACOMFACS,       X        
               RR=SRVRRELO                                                      
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   RINVKRSR,GKS.GKSPRSVC  INVENTORY KEY FIELD: RTG SOURCE           
         MVC   RINVKQLF,GKS.GKSPBKBT  INVENTORY KEY FIELD: QUALIFIER            
         MVC   RINVKBTP,GKS.GKSPBTYP  INVENTORY KEY FIELD: BOOKTYPE             
         DROP  GKS                                                              
                                                                                
         XC    WORK2,WORK2                                                      
RIZ      USING RINVZEL,WORK2                                                    
         MVI   RIZ.RINVZCOD,X'CE'                                               
         MVI   RIZ.RINVZLEN,10                                                  
                                                                                
         DS    0H                                                               
         LA    R0,5                                                             
         LR    R6,R2                                                            
         LA    R6,TMPDYTIM                                                      
                                                                                
         CLC   VERSNUM,=AL4(VERS76)                                             
         BH    DEMUP425                                                         
                                                                                
*===========================================================                    
* SAVE THIS CODE AROUND..TURNS OUT IT IS MORE OF                                
* A MULTI-HUT UPGRADE                                                           
* FOR NOW VERSION CONTROL IT SINCE IT WORKS LIKE THIS                           
* FOR OLDER BUILDS                                                              
*===========================================================                    
         CLI   RECALFLG,C'Y'                                                    
         BNE   DEMUP445                                                         
         SR    R3,R3                                                            
         ICM   R3,7,AMFID                                                       
         LA    R2,1                                                             
         TM    MFIDIND,LQ_TSINQ                                                 
         BNO   DEMUP410                                                         
         LA    R3,LW_DATA1-LW_D(R3)                                             
         J     DEMUP414                                                         
DEMUP410 TM    MFIDIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R3)                                            
         LA    R3,LW_DATA2-LW_D(R3)                                             
                                                                                
DEMUP414 CHI   R2,50                         PAV UPGRADE CAN ONLY               
         BNH   *+6                           SUPPORT 12 DAYTIMES                
         DC    H'0'                                                             
                                                                                
         PACK  DUB,0(3,R3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,TMPMFID                                                       
         XC    DUB,DUB                                                          
         PACK  DUB,3(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+1                                                   
         XC    DUB,DUB                                                          
         PACK  DUB,7(4,R3)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,TMPMFID+3                                                   
                                                                                
         MVC   RIZ.RINVZDAY,TMPMFID                                             
         MVC   RIZ.RINVZTIM,TMPMFID+1                                           
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFIL'),RINVREC,RIZ.RINVZEL,0              
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         AHI   R3,15                                                            
         BCT   R2,DEMUP414                                                      
         MVI   ROTFLAG,C'Y'                     SET DID ROTATION FLAG           
         J     DEMUP450                                                         
*===================================================================            
                                                                                
*===================================================================            
*  RECALCULATE AVERAGE CODE                                                     
*===================================================================            
DEMUP425 CLI   RECALFLG,C'Y'                                                    
         BNE   DEMUP445                                                         
         SR    R3,R3                                                            
         ICM   R3,7,ADAYTIME                                                    
         TM    DYTMIND,LQ_TSINQ                                                 
         BNO   DEMUP430                                                         
         LA    R3,LW_DATA1-LW_D(R3)                                             
         J     DEMUP434                                                         
DEMUP430 TM    DYTMIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         LA    R3,LW_DATA2-LW_D(R3)                                             
                                                                                
DEMUP434 MVC   RIZ.RINVZDAY,0(R3)                                               
         MVC   RIZ.RINVZTIM,1(R3)                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFIL'),RINVREC,RIZ.RINVZEL,0              
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         AHI   R3,5                                                             
         CLI   0(R3),X'FF'                                                      
         BNE   DEMUP434                                                         
         MVI   ROTFLAG,C'Y'                     SET DID ROTATION FLAG           
         J     DEMUP450                                                         
                                                                                
*  THIS PART IS NONE RECALC AVG CODE                                            
                                                                                
DEMUP445 DS    0H                  R6-->DAYS/TIMES                              
         MVC   RIZ.RINVZDAY,0(R6)                                               
         MVC   RIZ.RINVZTIM,1(R6)                                               
                                                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'REPFIL'),RINVREC,RIZ.RINVZEL,0              
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         DS    0H                                                               
         LA    R6,5(R6)                                                         
         CLI   0(R6),X'FF'                                                      
         BNE   DEMUP445                                                         
         DROP  RIZ                                                              
                                                                                
DEMUP450 MVC   DIVISOR,CUMFCTR                                                  
         GOTOR DIVSHR                                                           
                                                                                
         DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    RF,WORK             BUILD UPGRADE ELEMENT IN  WORK               
         USING RAVLNEL,RF                                                       
         ZICM  RE,AUPGRADE,(15)                                                 
         USING UPGRADD,RE                                                       
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVC   RAVLNBT,UPBKTYP                                                  
         MVC   RAVLNTYP(L'UPBKVAL),UPBKVAL                                      
         DROP  RF,RE                                                            
                                                                                
         LA    R0,RINVPEL-RINVREC                                               
         A     R0,AIO3             R0-->1ST ELEMENT OF RECORD                   
         MVI   FROMTYPE,C'I'                                                    
         MVI   BYTE,C'I'                                                        
         MVC   DUB+0(4),=C'RID='                                                
         MVC   DUB+4(2),AGYALPH                                                 
         GOTO1 VREDEMUP,DMCB,(FROMTYPE,(R0)),(BYTE,WORK),ACOMFACS,DUB, +        
               HOMSHR                                                           
                                                                                
         MVC   AXTRCREC,AIO3                                                    
         LA    R0,DEMODEMS                                                      
         ST    R0,ADEMLIST                                                      
         GOTOR XTRCTDMV                                                         
                                                                                
                                                                                
         DROP  R5                                                               
                                                                                
DEMUPX   J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
*--------------------------- DIVIDE SHARES ---------------------------*         
* Routine to divide accumulated shares by accumulated weighting factor.         
* At entry,                                                                     
*   CUMSHR  = A(accumulated shares),                                            
*   DIVISOR = accumulated factor.                                               
* At entry,                                                                     
*   HOMSHR  will contain the homes shares unweighted                            
DIVSHR   NTR1  BASE=*                                                           
         LA    R0,3                R0 = COUNTER                                 
         LA    R2,CUMSHR           R2-->ACCUMULATED SHARES                      
         LA    R3,HOMSHR           R3-->UNWEIGHTED HOME SHARES                  
                                                                                
DIVSHR10 DS    0H                                                               
         SR    RE,RE                                                            
         L     RF,0(R2)                                                         
         STM   RE,RF,DIVIDEND      SET TOTAL WEIGHTED SHARE IN DIVIDEND         
         GOTOR DIVIDE                                                           
         MVC   0(4,R3),QUOTIENT    QUOTIENT HAS UNWEIGHTED SHARE                
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,DIVSHR10                                                      
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
*------------------------ GENERIC DIVIDE LOGIC -----------------------*         
                                                                                
* At entry, DIVIDEND, DIVISOR are set.                                          
* At exit, QUOTIENT and REMAINDR are set.                                       
                                                                                
DIVIDE   NTR1  BASE=*                                                           
         XC    QUOTIENT,QUOTIENT                                                
         XC    REMAINDR,REMAINDR                                                
                                                                                
         ICM   RF,15,DIVISOR       IF DIVISOR IS ZERO,                          
         JZ    EXIT                 CALLER GETS ZERO BACK                       
         OC    DIVIDEND,DIVIDEND   IF DIVIDEND IS ZERO                          
         JZ    EXIT                 CALLER GETS ZERO BACK ALSO                  
                                                                                
         DS    0H                  CALCULATE QUOTIENT                           
         LM    R0,R1,DIVIDEND                                                   
         SLDA  R0,1                DOUBLE DIVIDEND                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,QUOTIENT                                                      
                                                                                
         DS    0H                  CALCULATE REMAINDER                          
         AH    R0,=H'1'                                                         
         SRA   R0,1                                                             
         ST    R0,REMAINDR                                                      
                                                                                
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
*-------------------------- TRANSLATE WEEKS --------------------------*         
                                                                                
* Translates active week bits into printable format                             
* At entry,                                                                     
*   TMPWKS   = active week bits                                                 
* At exit,                                                                      
*   WORK     = formatted active weeks                                           
*   HALF1    = length of formatted active weeks                                 
                                                                                
TRSLTWKS NTR1  BASE=*                                                           
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
                                                                                
         DS    0H                  FORMAT BOOK                                  
         LA    RE,B'00001000'      TRANSLATE BITS (STARTING W/ X'08')           
         LA    RF,C'1'              INTO EBCDIC DIGITS                          
TWK012   EX    RE,*+8              JUST PASS THOSE WEEK NOS. OF                 
         B     *+8                  THOSE WEEKS THAT WERE ACTIVE                
         TM    TMPWKS,0             (eg. X'0B' ==> C'134' )                     
         BNZ   TWK014                                                           
         MVI   0(R4),C'.'           MOVE IN A "." FOR NON ACTIVE WK             
         B     TWK016                                                           
TWK014   STC   RF,0(R4)                                                         
TWK016   LA    R4,1(R4)                                                         
         LA    RF,1(RF)                                                         
         SRL   RE,1                                                             
         OR    RE,RE                                                            
         BNZ   TWK012                                                           
                                                                                
         TM    TMPWKS,X'80'         IF DIFFERING ACTIVE WEEKS,                  
         BZ    *+12                                                             
         MVI   0(R4),C'*'           INDICATE IT SO WITH AN "*"                  
         LA    R4,1(R4)                                                         
                                                                                
TWKX     DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         STH   R4,HALF1            SET LENGTH INTO HALF                         
                                                                                
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
*  ---------------- PROCESS IUN STUFF ------------------------------- *         
* At entry,                                                                     
*   R6-->DBLOCK,                                                                
*   IUNOLD values are set,                                                      
*   AVUTS-->VUT values to use in computing home shares,                         
*   DEMODEMS = list of requested demos.                                         
* At exit,                                                                      
*   THISDEMS = output demo values.                                              
*   IUNWRK contains demo values in IUN format                                   
PROCIUN  NTR1  BASE=*                                                           
         L     R6,ADBLOCK                                                       
         USING DBLOCKD,R6                                                       
                                                                                
* SAVE DBLOCK *                                                                 
                                                                                
         LA    R0,SVDBLOCK          DESTINATION                                 
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,DBLOCK            SOURCE                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
* SET UP IUN AREAS *                                                            
                                                                                
         L     R4,AIUNWRK                                                       
         LR    R0,R4                                                            
         LA    R1,IUNWRKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR IUN WORK AREA                          
                                                                                
         MVI   0(R4),RINVKTYQ                                                   
         LA    R4,500(R4)                                                       
         USING IUNRECD,R4                                                       
                                                                                
* EXPLODE DEMO RECORD INTO IUN BUCKETS *                                        
                                                                                
         GOTO1 VREGTIUN,MYDMCB,(10,DBLOCK),IUNRECD                              
                                                                                
* COPY OLD TO NEW TO COMPLETE IUN RECORD *                                      
                                                                                
         LA    R0,IUNNEW                                                        
         LA    R1,IUNNEWX-IUNNEW                                                
         LA    RE,IUNOLD                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
* GET HOME SHARES *                                                             
                                                                                
         GOTO1 VDEMOUT,DMCB,(C'L',ASHRLIST),DBLOCKD,ISHOMES,0                   
                                                                                
* BUILD IUN RECORD *                                                            
                                                                                
         DS    0H                  SET DBLOCK VALUES                            
         L     RE,AIUNWRK                                                       
         ST    RE,DBAREC            SET A(RECORD)                               
         LA    RE,(RINVPEL-RINVREC)(RE)                                         
         MVI   0(RE),0                                                          
         ST    RE,DBAQUART          SET A(QH ELEM)                              
         LA    R0,IUNRECL/4                                                     
         STH   R0,DBNUMVLS          SET NUMBER OF VALUES                        
                                                                                
         L     RF,DBCOMFCS                                                      
         L     RF,(CDEMAINT-COMFACSD)(RF)                                       
         XC    WORK,WORK                                                        
         MVC   WORK(10),OFORMAT                                                 
         GOTO1 (RF),MYDMCB,=C'REP',DBLOCK,IUNRECD,WORK                          
                                                                                
* RESTORE DBLOCK *                                                              
                                                                                
         L     R0,ADBLOCK                                                       
         LA    R1,DBLOCK1X-DBLOCK1                                              
         LA    RE,SVDBLOCK          SOURCE                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     EXITY                                                            
         DROP  R4,R6                                                            
         LTORG                                                                  
OFORMAT  DC    C'INVUIUN',X'5A0B00'                                             
         DROP  RB                                                               
***********************************************************************         
* Routine to get shares and update shares accumulators.                         
* At entry,                                                                     
*   R3-->output area                                                            
*   R6-->DBLOCK.                                                                
*   MTHFCTR+2 = half-word aligned factor to use in weighting                    
GETSHR   NTR1  BASE=*                                                           
         L     R6,ADBLOCK                                                       
         USING DBLOCKD,R6                                                       
         LA    R3,CUMSHR                                                        
                                                                                
         MVC   HALF1,DBACTBK        SAVE ACTUAL BOOK                            
         XC    HOMSHR(HOMSHRL),HOMSHR                                           
                                                                                
         OC    DBAQUART,DBAQUART   IF NO DBAQUART,                              
         BZ    GETSHR05X            THERE BE NO SHARES                          
         GOTO1 VDEMOUT,MYDMCB,(C'P',DEMOSHR),DBLOCKD,HOMSHR                     
GETSHR05X EQU  *                                                                
                                                                                
         MVC   DBACTBK,HALF1       RESTORE ACTUAL BOOK VALUE                    
         DROP  R6                                                               
                                                                                
         DS    0H                  ACCUMULATE THE SHARES                        
         LA    R0,3                 R0 = COUNTER                                
         SR    R1,R1                R1 = INDEX INTO VALUES                      
                                                                                
GETSHR10 DS    0H                                                               
         L     RE,HOMSHR(R1)                                                    
         MH    RE,MTHFCTR+2                                                     
         LR    RF,RE                                                            
         A     RE,0(R1,R3)                                                      
         ST    RE,0(R1,R3)         UPDATE SHARES                                
         LA    R1,4(R1)                                                         
         BCT   R0,GETSHR10                                                      
                                                                                
         J     EXITY                                                            
         LTORG                                                                  
DEMOSHR  DS    0XL3                                                             
         DC    X'81',C'S',AL1(1)                                                
         DC    X'81',C'S',AL1(2)                                                
         DC    X'81',C'S',AL1(3)                                                
         DC    X'FF'                                                            
         DROP  RB                                                               
***********************************************************************         
*------------------------ EXTRACT DEMO VALUES ------------------------*         
                                                                                
* Extracts demo values from an inventory track record.                          
* Indicates if there are any demo overrides.                                    
* At entry,                                                                     
*   ADEMLIST = list of demos to get,                                            
*   AXTRCREC = A(record to extract demos from)                                  
*   DBLOCK1  = the DBLOCK in effect                                             
* At exit,                                                                      
*   THISDEMS = extracted demo values.                                           
* NOTE:  SVDBLOCK area will get creamed here.                                   
                                                                                
XTRCTDMV NTR1  BASE=*                                                           
         XC    THISDEMS,THISDEMS   CLEAR OUTPUT AREA FOR DEMO VALUES            
         L     R5,AXTRCREC          R5-->RECORD CONTAINING DEMO VALUES          
                                                                                
         DS    0H                  CLEAR DBLOCK                                 
         LA    R0,SVDBLOCK                                                      
         ST    R0,ADBLOCK                                                       
                                                                                
         L     R0,ADBLOCK                                                       
         LA    R1,DBLOCK1X-DBLOCK1                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         DS    0H                  SET UP DBLOCK                                
         LA    R6,SVDBLOCK                                                      
         USING DBLOCKD,R6                                                       
                                                                                
         LA    R0,(RINVPEL-RINVREC)(R5)                                         
         LTR   R1,R5                                                            
         BNZ   XDV012X                                                          
         L     RE,=A(DBLOCK1-WORKD)                                             
         LA    RE,WORKD(RE)                                                     
MYDBK1   USING DBLOCKD,RE                                                       
         L     R0,MYDBK1.DBAQUART                                               
         L     R1,MYDBK1.DBAREC                                                 
         DROP  MYDBK1                                                           
XDV012X  EQU   *                                                                
                                                                                
         ST    R5,DBAREC                 A(RECORD)                              
         LA    R0,(RINVPEL-RINVREC)(R5)  A(1ST ELEMENT)                         
         CLI   0(R5),RINVKTYQ                                                   
         BE    *+8                                                              
         LA    R0,(DRFRSTEL-DRKEY)(R5)                                          
                                                                                
         ST    R1,DBAREC                 A(RECORD)                              
         ST    R0,DBAQUART               A(QH OR 1ST ELEMENT)                   
         MVC   DBCOMFCS,ACOMFACS         A(COMFACS)                             
                                                                                
XDV031   DS    0H                                                               
         CLI   OPTDEC,C'2'                                                      
         BNE   XDV041                                                           
**       L     RE,=A(DBLOCK1-WORKD)                                             
**       LA    RE,WORKD(RE)                                                     
**       ST    RE,DUB1                                                          
         ST    R6,DUB1                                                          
         LA    R1,MYDBXTRA                                                      
         ST    R1,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
         MVI   SDBXF,C'D'                                                       
*                                                                               
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
                                                                                
         ICM   R1,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
         MVC   DBXTSCTL,OPTDEC                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         TM    TMPWHOLE,WHOLERTG           ROUND RATINGS TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTRC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLESHR           ROUND SHARES  TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTSC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLEPUT           ROUND PUTS    TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTPC2T,C'Y'                YEP                                 
         DROP  R1                                                               
*                                                                               
XDV041   GOTO1 VDEMOUT,MYDMCB,(C'L',ADEMLIST),DBLOCKD,THISDEMS                  
         DROP  R6                                                               
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
*-------------------------- BUILD DEMOBLOCK --------------------------*         
                                                                                
* Builds the DBLOCK in the area addressed by ADBLOCK                            
* At entry,                                                                     
*   TMPBEST =DBBEST VALUE TO USE                                                
                                                                                
BLDBLOCK NTR1  BASE=*                                                           
                                                                                
         L     R6,ADBLOCK                                                       
         USING DBLOCKD,R6                                                       
                                                                                
         DS    0H                 CLEAR DBLOCK                                  
         LR    R0,R6               R0-->DESTINATION                             
         LA    R1,DBLOCK1X-DBLOCK1 R1 = L(DESTINATION)                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         DS    0H                 BUILD DBLOCK                                  
         LHI   RE,IOAREA1-WORKD                                                 
         LA    RE,WORKD(RE)                                                     
                                                                                
         ST    RE,DBAREC           A(I/O AREA)                                  
         MVC   DBCOMFCS,ACOMFACS   A(COMFACS)                                   
         MVC   DBFILE,INPFIL       FILE                                         
         MVI   DBFUNCT,DBGETDEM    DEMAND REQUEST FUNCTION                      
         MVI   DBSELSRC,C'N'       ONLY NIELSON                                 
         MVI   DBSELMED,C'T'           "     MEDIA                              
         CLC   =C'OPA',INPFIL                                                   
         BNE   *+8                                                              
         MVI   DBSELMED,C'O'                                                    
         MVC   DBSELSTA,TMPSTA         "     STATION                            
                                                                                
         CLI   DBSELSTA+3,C'+'                                                  
         BNE   BDB04                                                            
         MVC   DBSELSTA+3(2),=C' 1'                                             
         B     BDB05                                                            
BDB04    CLI   DBSELSTA+4,C'+'                                                  
         BNE   BDB05                                                            
         MVI   DBSELSTA+4,C'1'                                                  
                                                                                
BDB05    MVC   DBSELMK,SPILLMKT        "     SPILL MARKET                       
                                                                                
         DS    0H                                                               
         MVC   DBSELBK,TMPBOOK+1       "     BOOK                               
                                                                                
         MVC   DBSELDAY,TMPDAY         "     DAY                                
                                                                                
         MVC   DBSELTIM(4),TMPSETIM                                             
         CLC   INPFIL,=C'PAV'                                                   
         BNE   BDB010                                                           
                                                                                
BDB010   MVC   DBSELWKN,TMPBKWK         "     WEEK NUMBER                       
         B     BDB016X                                                          
                                                                                
         MVC   DUB(4),=C'DYTM'     LINK IDENTIFICATION                          
BDB016   DS   0H                                                                
                                                                                
         LA    R3,TMPDYTIM                       DO ENTIRE ROTATION             
         XC    DUB,DUB                                                          
         LA    R0,MYDBXTRA                                                      
         ST    R0,DUB+4                                                         
         MVI   SDBXF,C'D'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
         ICM   RF,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTLD,RF                                                        
         LA    RE,DBXTLIST                                                      
         MVC   DBXTLID,=C'DYTM'                                                 
         MVI   DBXTLIDX,0                                                       
         LA    R0,0                                                             
*                                               SET UP ROTATION LINK            
BDB016D  MVC   0(5,RE),0(R3)                    DAY,START END TIME              
         AHI   RE,5                                                             
         AHI   R3,5                                                             
         CLI   0(R3),X'FF'                                                      
         BNE   BDB016D                                                          
         MVI   0(RE),0                                                          
                                                                                
BDB016X  EQU   *                                                                
                                                                                
*&&DO                                                                           
         DS    0H                  NEED LONGER DAYS/QHS TABLE                   
         CLI   DBMED,C'T'           FOR MEDIA=USTV                              
         BNE   BDBDQX                                                           
         CLI   DBSRC,C'N'           FOR SOURCE=NSI                              
         BNE   BDBDQX                                                           
         CLC   DBFIL,=C'TP '        FOR TP FILE                                 
         BNE   BDBDQX                                                           
                                                                                
         MVC   DUB+0(4),=C'DBD3'                                                
         LA    RE,DBXTDQT                                                       
         ST    RE,DUB+4                                                         
         XC    0(DBXTDQTL,RE),0(RE)                                             
         MVI   SDBXF,C'D'                                                       
         MVI   GOSUBN,SDBX#         SET UP MULTI-D/T LINK                       
         GOTO1 AGOSUB                                                           
         ICM   RE,15,FULL                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
BDBDQX   EQU   *                                                                
                                                                                
         MVC   DBBTYPE,TMPBKTYP     BOOK TYPE                                   
         MVC   DBSELAGY,AGYALPH     AGENCY                                      
         MVC   DBBEST,TMPBEST                                                   
                                                                                
BDB020   MVI   DBTAPEP,C'Y'        WE WANT TAPE PRECISION                       
         OC    SPILLMKT,SPILLMKT   IF NO NUMERIC SPILL MARKET,                  
         BNZ   *+20                                                             
         OC    ALFMKTS,ALFMKTS      AND THERE WAS ALPHA MKT INPUT,              
         BZ    *+10                                                             
         MVC   DBSELALF,ALFMKTS     USE IT                                      
         CLC   DBSELALF,=X'404040'                                              
         BNE   *+10                                                             
         XC    DBSELALF,DBSELALF                                                
         MVC   DBSELMK,SPILLMKT                                                 
                                                                                
*  LOOK FOR CORRECT UPGRADE FORMULA                                             
         MVI   SAVGFLAG,C'N'                                                    
         OC    TMPUINDX,TMPUINDX                                                
         BZ    BDBX                                                             
         SR    RE,RE                                                            
         ICM   RE,7,AUPGRD                                                      
         LA    R1,1                                                             
         TM    UPGRIND,LQ_TSINQ                                                 
         BNO   BDB90                                                            
         LA    RE,LW_DATA1-LW_D(RE)                                             
         J     BDB92                                                            
BDB90    TM    UPGRIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,3,LW_NUMN-LW_D(RE)                                            
         LA    RE,LW_DATA2-LW_D(RE)                                             
*  NOW FIND THE CORRECT PROJECTION FORMULA BASE ON THE PROJECTION INDEX         
*  CURRENTLY SEEKING  INDEX START AT 1                                          
         USING UPGRADD,RE                                                       
BDB92    CLC   TMPUINDX,UPINDEX                                                 
         BE    BDB93                                                            
         AHI   RE,UPGRADX                                                       
         BCT   R1,BDB92                                                         
         DC    H'0'                             SOMETHING AMISS                 
BDB93    MVC   DBSELBK,UPSHRBK                                                  
         MVC   DBBTYPE,UPBKTYP                                                  
         MVC   DBSELWKN,UPWEEK                                                  
         MVC   TMPBKWK,UPWEEK                                                   
         NI    TMPBKWK,X'0F'                     NIBBLE                         
         STCM  RE,15,AUPGRADE                                                   
         XC    TMPBOOK,TMPBOOK                                                  
         MVC   TMPBOOK+1(2),UPSHRBK                                             
         MVC   TMPBKTYP,UPBKTYP                                                 
                                                                                
*  DETERMINE IF THIS IS A SAVG (SHARE MULTIBOOK UPGRADE)                        
                                                                                
         CLI   UPBKVAL,X'0E'                      SAVG?                         
         BE    BDB98                                                            
         CLI   UPBKVAL,X'0C'                      SHARE INDEX?                  
         BE    BDB98                                                            
         B     BDBX                                                             
BDB98    OC    UPBKVAL+4(2),UPBKVAL+4                                           
         BZ    BDBX                                                             
         MVI   SAVGFLAG,C'Y'                                                    
BDBX     DS    0H                                                               
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB,R6,RE                                                         
***********************************************************************         
* ROUTINE TO DETERMINE THE NEXT LASTEST BOOK                                    
* ENTRY DMCB=TMPBOOK+1 THE BOOK TO ADJUST                                       
* EXIT  DMCB+4(2) - NEXT BOOK                                                   
***********************************************************************         
GETNLATB NTR1  BASE=*                                                           
         MVC   DMCB+4(2),DMCB                                                   
         ZIC   RE,DMCB+1                                                        
         SHI   RE,1                                                             
         STC   RE,DMCB+5                                                        
                                                                                
         CLI   DMCB+5,1                                                         
         BNL   GETNLATX                                                         
                                                                                
         MVI   DMCB+5,12             RESET FOR PREVIOUS YEAR                    
         ZIC   RE,DMCB                                                          
         SHI   RE,1                                                             
         STC   RE,DMCB+4                                                        
                                                                                
                                                                                
GETNLATX J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
***********************************************************************         
* REQUEST DEFINITIONS                                                 *         
***********************************************************************         
LVALUES  DS    0D                                                               
         DC    A(DEMBUFF)                                                       
LVALUESX DS    0X                                                               
DEMBUFF  BUFFD TYPE=D,KEYLEN=2,COMLEN=2,BUFFERS=6                               
                                                                                
**REQUEST  DS    0X                                                             
***********************************************************************         
* REQUEST FOR CANADIAN SPOT DESKTOP LOOKUP                            *         
* THE SPEC WAS FOR MULTIPLE STATION EACH WITH MULTIPLE SPILL          *         
* MARKETS FOR ONE DAYTIME, ONE BOOK                                   *         
***********************************************************************         
M#CANLK  EQU   24                                                               
*                                                                               
                                                                                
REQCANL  LKREQ H,I#DECAND,OUTCANLK                                              
                                                                                
RMED     LKREQ F,1,(D,B#SAVED,CANMED),CHAR,OLEN=1,TEXT=SP#MED,COL=*             
                                                                                
RRSMKT   LKREQ F,2,(I,B#SAVED,RRSMIND),CHAR,OLEN=CANRSRVL,SORT=N,      *        
               TEXT=(*,RSATEXT),LIST=F,COL=*                                    
                                                                                
*&&DO                                                                           
DAY      LKREQ F,5,(D,B#SAVED,CANDAY),LBIN,OLEN=1,TEXT=SP#DAY,COL=*             
                                                                                
                                                                                
STIME    LKREQ F,6,(D,B#SAVED,CANSTIM),LBIN,                           *        
               OLEN=L'CANSTIM,TEXT=(*,STIMTXT),COL=*                            
ETIME    LKREQ F,7,(D,B#SAVED,CANETIM),LBIN,                           *        
               OLEN=L'CANETIM,TEXT=(*,ETIMTXT),COL=*                            
                                                                                
BOOKS    LKREQ F,8,(D,B#SAVED,CANBOOK),BMON,                           *        
               OLEN=L'CANBOOK,TEXT=SP#1BBOK,COL=*                               
*&&                                                                             
                                                                                
DPREC    LKREQ F,9,(D,B#SAVED,CAN2DEC),CHAR,OLEN=1,TEXT=SP#DPREC,COL=*          
                                                                                
PROF1W   LKREQ F,10,(D,B#SAVED,CANPROF),CHAR,OLEN=L'CANPROF,           *        
               TEXT=(*,PROFLIT),COL=* ,                                         
DEMOS    LKREQ F,11,(I,B#SAVED,DEMOIND),CHAR,LIST=F,                   *        
               OLEN=3,SORT=N,TEXT=SP#DEMO,COL=*                                 
                                                                                
         LKREQ E                                                                
*                                                                               
***********************************************************************         
*  REQUEST FOR PROGRAM HEADERS  FOR PAV                               *         
***********************************************************************         
M#PAVHD  EQU   21                                                               
REQPAVH  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQPAVHX+1-*)                                                
         DC    AL2(M#PAVHD)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTPRGHD-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
EC#STAT  EQU   1                                                                
EQ#STAT  EQU   1                                                                
         DC    AL2(EQ#STAT)                                                     
         DC    CL5'STAT '                                                       
         DC    AL1(EC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    AL1(600,11)                                                      
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    AL1(RESYSQ),AL2(RE#STA)                                          
         DC    XL4'00'                                                          
                                                                                
* DAYTIME *                                                                     
EC#DYTM  EQU   2                                                                
EQ#DYTM  EQU   2                                                                
         DC    AL2(EQ#DYTM)                                                     
         DC    CL5'DYTM '                                                       
         DC    AL1(EC#DYTM)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MVTBUFF,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DYTMIND-SAVED)                                               
         DC    AL1(61)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(40,300)                                                      
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#1BDYT)                                        
         DC    AL1(RESYSQ),AL2(RE#DAYTM)                                        
         DC    XL4'00'                                                          
                                                                                
* BOOK *                                                                        
EC#BOOK  EQU   3                                                                
EQ#BOOK  EQU   3                                                                
         DC    AL2(EQ#BOOK)                                                     
         DC    CL5'BOOK '                                                       
         DC    AL1(EC#BOOK)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALBOOK,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(BOOKIND-SAVED)                                               
         DC    AL1(6)                                                           
         DC    AL1(LD_USERQ+LD_ECOLQ)                                           
         DC    AL1(400,15)                                                      
         DC    XL2'00'                                                          
***      DC    AL1(SPSYSQ),AL2(SP#1BBOK)                                        
         DC    AL1(RESYSQ),AL2(RE#BOOKS)                                        
         DC    XL4'00'                                                          
                                                                                
REQPAVHX DC    AL1(LD_EOTQ)                                                     
***********************************************************************         
***********************************************************************         
*  REQUEST FOR INVENTORY HEADERS                                      *         
***********************************************************************         
M#INVHD  EQU   22                                                               
REQINVH  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQINVHX+1-*)                                                
         DC    AL2(M#INVHD)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTPRGHD-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
* EFFECTIVE DATE                                                                
                                                                                
IC#EFFDT EQU   1                                                                
IQ#EFFDT EQU   1                                                                
         DC    AL2(IQ#EFFDT)                                                    
         DC    CL5'EFFDT'                                                       
         DC    AL1(IC#EFFDT)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VAL1WK,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(EFFDTIND-SAVED)                                              
         DC    AL1(4)                                                           
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(1,50)                                                        
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#DATE)                                         
         DC    AL1(RESYSQ),AL2(RE#EFFDT)                                        
         DC    XL4'00'                                                          
                                                                                
IC#STAT  EQU   2                                                                
IQ#STAT  EQU   2                                                                
         DC    AL2(IQ#STAT)                                                     
         DC    CL5'STAT '                                                       
         DC    AL1(IC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(11)                                                          
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    AL1(RESYSQ),AL2(RE#STA)                                          
         DC    XL4'00'                                                          
                                                                                
IC#OSTAT EQU   3                                  OVERRIDE STATION              
IQ#OSTAT EQU   3                                                                
         DC    AL2(IQ#OSTAT)                                                    
         DC    CL5'OSTAT'                                                       
         DC    AL1(IC#OSTAT)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(OSTATIND-SAVED)                                              
         DC    AL1(5)                                                           
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    AL1(RESYSQ),AL2(RE#OSTA)                                         
         DC    XL4'00'                                                          
                                                                                
* DAYPARTS                                                                      
                                                                                
IC#DYPT  EQU   4                                                                
IQ#DYPT  EQU   4                                                                
         DC    AL2(IQ#DYPT)                                                     
         DC    CL5'DYPT '                                                       
         DC    AL1(IC#DYPT)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(DYPTIND-SAVED)                                               
         DC    AL1(100)                                                         
         DC    AL1(LD_CHARQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#DAYPT)                                        
         DC    AL1(RESYSQ),AL2(RE#DPT)                                          
         DC    XL4'00'                                                          
                                                                                
REQINVHX DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
*  REQUEST FOR PROGRAM DETAILS                                        *         
***********************************************************************         
M#PGDET  EQU   20                                                               
REQPDET  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQPDETX+1-*)                                                
         DC    AL2(M#PGDET)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTPGDET-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
CC#DEMO  EQU   1                                                                
CQ#DEMO  EQU   1                                                                
         DC    AL2(CQ#DEMO)                                                     
         DC    CL5'DEMO '                                                       
         DC    AL1(CC#DEMO)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALDEMO,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DEMOIND-SAVED)                                               
         DC    AL1(24)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(60,20)                                                       
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#DEMO)                                         
         DC    AL1(RESYSQ),AL2(RE#DEMOS)                                        
         DC    XL4'00'                                                          
                                                                                
CC#STAT  EQU   2                                                                
CQ#STAT  EQU   2                                                                
         DC    AL2(CQ#STAT)                                                     
         DC    CL5'STA  '                                                       
         DC    AL1(CC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    AL1(RESYSQ),AL2(RE#STA)                                          
         DC    XL4'00'                                                          
                                                                                
* DAYTIME *                                                                     
                                                                                
CC#DYTM  EQU   3                                                                
CQ#DYTM  EQU   3                                                                
         DC    AL2(CQ#DYTM)                                                     
         DC    CL5'DYTM '                                                       
         DC    AL1(CC#DYTM)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MVTBUFF,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DYTMIND-SAVED)                                               
         DC    AL1(61)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#1BDYT)                                        
         DC    AL1(RESYSQ),AL2(RE#DAYTM)                                        
         DC    XL4'00'                                                          
                                                                                
* EFFECTIVE DATE                                                                
                                                                                
CC#EFFDT EQU   8                                                                
CQ#EFFDT EQU   8                                                                
         DC    AL2(CQ#EFFDT)                                                    
         DC    CL5'EFFDT'                                                       
         DC    AL1(CC#EFFDT)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VAL1WK,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(EFFDTIND-SAVED)                                              
         DC    AL1(4)                                                           
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#DATE)                                         
         DC    AL1(RESYSQ),AL2(RE#EFFDT)                                        
         DC    XL4'00'                                                          
                                                                                
* INVENTORY NUMBER                                                              
* PC ID ASSOCIATED WITH EACH FILE BOOK                                          
                                                                                
IC#INVNM EQU   9                                                                
IQ#INVNM EQU   9                                                                
         DC    AL2(IQ#INVNM)                                                    
         DC    CL5'INVNM'                                                       
         DC    AL1(IC#INVNM)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(INVNMIND-SAVED)                                              
         DC    AL1(4)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#INVNM)                                        
         DC    XL4'00'                                                          
*                                                                               
CC#SYSC  EQU   15                                                               
CQ#SYSC  EQU   15                                                               
         DC    AL2(CQ#SYSC)                                                     
         DC    CL5'SYSC '                                                       
         DC    AL1(CC#SYSC)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SYSCIND-SAVED)                                               
         DC    AL1(2)                                                           
         DC    AL1(LD_UBINQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
***      DC    AL1(RESYSQ),AL2(RE#SYSC)                                         
         DC    AL1(RESYSQ),AL2(272)                                             
         DC    XL4'00'                                                          
*                                                                               
* NUM OF ROWS FROM PC COMPARAGRAH REPORT                                        
                                                                                
CC#NROW  EQU   4                                                                
CQ#NROW  EQU   4                                                                
         DC    AL2(CQ#NROW)                                                     
         DC    CL5'NROW '                                                       
         DC    AL1(CC#NROW)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(NROWIND-SAVED)                                               
         DC    AL1(4)                                                           
         DC    AL1(LD_UBINQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#ROWS)                                         
         DC    XL4'00'                                                          
                                                                                
* FILE *                                                                        
                                                                                
CC#FILE  EQU   5                                                                
CQ#FILE  EQU   5                                                                
         DC    AL2(CQ#FILE)                                                     
         DC    CL5'FILE '                                                       
         DC    AL1(CC#FILE)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALFILE,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(FILEIND-SAVED)                                               
         DC    AL1(3)                                                           
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#1BFIL)                                        
         DC    AL1(RESYSQ),AL2(RE#FILE)                                         
         DC    XL4'00'                                                          
                                                                                
* BOOK *                                                                        
                                                                                
CC#BOOK  EQU   6                                                                
CQ#BOOK  EQU   6                                                                
         DC    AL2(CQ#BOOK)                                                     
         DC    CL5'BOOK '                                                       
         DC    AL1(CC#BOOK)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
*******  DC    AL1(#VALBOOK,0)                                                  
         DC    AL1(#MULTBKS,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(BOOKIND-SAVED)                                               
******   DC    AL1(6)                                                           
         DC    AL1(12)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#1BBOK)                                        
         DC    AL1(RESYSQ),AL2(RE#BOOKS)                                        
         DC    XL4'00'                                                          
                                                                                
* PC ID ASSOCIATED WITH EACH FILE BOOK                                          
                                                                                
CC#PCID  EQU   7                                                                
CQ#PCID  EQU   7                                                                
         DC    AL2(CQ#PCID)                                                     
         DC    CL5'PCID '                                                       
         DC    AL1(CC#PCID)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PCIDIND-SAVED)                                               
         DC    AL1(5)                                                           
**       DC    AL1(LD_CHARQ+LD_ECOLQ)                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#PCID)                                         
         DC    XL4'00'                                                          
                                                                                
* UPGRADE INDEX NUMBER                                                          
                                                                                
CC#UINDX EQU   10                                                               
CQ#UINDX EQU   10                                                               
         DC    AL2(CQ#UINDX)                                                    
         DC    CL5'UINDX'                                                       
         DC    AL1(CC#UINDX)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UINDXIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#UPIDX)                                        
         DC    XL4'00'                                                          
*                                                                               
* LATEST BOOK NUMBER- NUMBER OF LATEST BOOKS TO PROCESS                         
* SPOT DESKTOP FEATURE MAPCODE                                                  
CC#LATBN EQU   16                                                               
CQ#LATBN EQU   16                                                               
         DC    AL2(CQ#LATBN)                                                    
         DC    CL5'LATBN'                                                       
         DC    AL1(CC#LATBN)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(LATBNIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#LATES)                                        
         DC    XL4'00'                                                          
* LATEST BOOK TYPE                                                              
* FOR MAPCODE 15 LATEST BOOK  NUMBER                                            
CC#LATBT EQU   17                                                               
CQ#LATBT EQU   17                                                               
         DC    AL2(CQ#LATBT)                                                    
         DC    CL5'LATBT'                                                       
         DC    AL1(CC#LATBT)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(LATBTIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#BKTY)                                         
         DC    XL4'00'                                                          
*                                                                               
                                                                                
* UPGRADE                                                                       
                                                                                
CC#UPGR  EQU   11                                                               
CQ#UPGR  EQU   11                                                               
         DC    AL2(CQ#UPGR)                                                     
         DC    CL5'UPGR '                                                       
         DC    AL1(CC#UPGR)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#PARSEUP,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(UPGRIND-SAVED)                                               
         DC    AL1(19)                                                          
****     DC    AL1(LD_USERQ)                                                    
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#OPT1)                                         
         DC    AL1(RESYSQ),AL2(RE#UPGDE)                                        
         DC    XL4'00'                                                          
                                                                                
CC#UPNAM EQU   12                              UPGRADE NAME                     
CQ#UPNAM EQU   12                              CREATED BY PC                    
         DC    AL2(CQ#UPNAM)                                                    
         DC    CL5'UPNAM'                                                       
         DC    AL1(CC#UPNAM)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UPNAMIND-SAVED)                                              
         DC    AL1(15)                                                          
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#RQNAM)                                        
         DC    AL1(RESYSQ),AL2(RE#UPNAM)                                        
         DC    XL4'00'                                                          
                                                                                
CC#UPIND EQU   13                              UPGRADE KEY NUMBER               
CQ#UPIND EQU   13                                                               
         DC    AL2(CQ#UPIND)                                                    
         DC    CL5'UPIND'                                                       
         DC    AL1(CC#UPIND)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UPININD-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#UPNUM)                                        
         DC    XL4'00'                                                          
CC#DEC   EQU   14                                                               
CQ#DEC   EQU   14                                                               
         DC    AL2(CQ#DEC)                                                      
         DC    CL5'DEC  '                                                       
         DC    AL1(CC#DEC)                                                      
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(DECIND-SAVED)                                                
**       DC    AL2(OPTDEC-SAVED)                                                
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#DECML)                                        
         DC    XL4'00'                                                          
                                                                                
REQPDETX DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
*  REQUEST FOR DETAIL RECALCULATION                                   *         
***********************************************************************         
M#RECAL  EQU   23                                                               
REQRECL  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQRECLX+1-*)                                                
         DC    AL2(M#RECAL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTPGDET-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
RC#DEMO  EQU   1                                                                
RQ#DEMO  EQU   1                                                                
         DC    AL2(RQ#DEMO)                                                     
         DC    CL5'DEMO '                                                       
         DC    AL1(RC#DEMO)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALDEMO,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DEMOIND-SAVED)                                               
         DC    AL1(24)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(60,20)                                                       
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#DEMO)                                         
         DC    AL1(RESYSQ),AL2(RE#DEMOS)                                        
         DC    XL4'00'                                                          
                                                                                
                                                                                
RC#STAT  EQU   2                                                                
RQ#STAT  EQU   2                                                                
         DC    AL2(RQ#STAT)                                                     
         DC    CL5'STA  '                                                       
         DC    AL1(RC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    AL1(RESYSQ),AL2(RE#STA)                                          
         DC    XL4'00'                                                          
                                                                                
* FILE *                                                                        
                                                                                
RC#FILE  EQU   3                                                                
RQ#FILE  EQU   3                                                                
         DC    AL2(RQ#FILE)                                                     
         DC    CL5'FILE '                                                       
         DC    AL1(RC#FILE)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALFILE,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(FILEIND-SAVED)                                               
         DC    AL1(3)                                                           
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#1BFIL)                                        
         DC    AL1(RESYSQ),AL2(RE#FILE)                                         
         DC    XL4'00'                                                          
                                                                                
* BOOK *                                                                        
                                                                                
RC#BOOK  EQU   4                                                                
RQ#BOOK  EQU   4                                                                
         DC    AL2(RQ#BOOK)                                                     
         DC    CL5'BOOK '                                                       
         DC    AL1(RC#BOOK)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MULTBKS,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(BOOKIND-SAVED)                                               
         DC    AL1(12)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#1BBOK)                                        
         DC    AL1(RESYSQ),AL2(RE#BOOKS)                                        
         DC    XL4'00'                                                          
                                                                                
*                                                                               
RC#UINDX EQU   10                                                               
RQ#UINDX EQU   10                                                               
         DC    AL2(RQ#UINDX)                                                    
         DC    CL5'UINDX'                                                       
         DC    AL1(RC#UINDX)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UINDXIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#UPIDX)                                        
         DC    XL4'00'                                                          
                                                                                
*                                                                               
*   MAINFRAME ID MAX 7 BYTES FOR PAV  5 BYTES FOR TT,T4                         
                                                                                
RC#MFID  EQU   5                                                                
RQ#MFID  EQU   5                                                                
         DC    AL2(RQ#MFID)                                                     
         DC    CL5'MFID '                                                       
         DC    AL1(RC#MFID)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(MFIDIND-SAVED)                                               
         DC    AL1(15)                                                          
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(300,15)                                                      
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#MFID)                                         
         DC    XL4'00'                                                          
                                                                                
* UPGRADE                                                                       
                                                                                
RC#UPGR  EQU   11                                                               
RQ#UPGR  EQU   11                                                               
         DC    AL2(RQ#UPGR)                                                     
         DC    CL5'UPGR '                                                       
         DC    AL1(RC#UPGR)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#PARSEUP,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(UPGRIND-SAVED)                                               
         DC    AL1(19)                                                          
****     DC    AL1(LD_USERQ)                                                    
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#OPT1)                                         
         DC    AL1(RESYSQ),AL2(RE#UPGDE)                                        
         DC    XL4'00'                                                          
                                                                                
RC#UPNAM EQU   12                              UPGRADE NAME                     
RQ#UPNAM EQU   12                              CREATED BY PC                    
         DC    AL2(RQ#UPNAM)                                                    
         DC    CL5'UPNAM'                                                       
         DC    AL1(RC#UPNAM)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UPNAMIND-SAVED)                                              
         DC    AL1(15)                                                          
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#RQNAM)                                        
         DC    AL1(RESYSQ),AL2(RE#UPNAM)                                        
         DC    XL4'00'                                                          
                                                                                
RC#UPIND EQU   13                              UPGRADE KEY NUMBER               
RQ#UPIND EQU   13                                                               
         DC    AL2(RQ#UPIND)                                                    
         DC    CL5'UPIND'                                                       
         DC    AL1(RC#UPIND)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UPININD-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#UPNUM)                                        
         DC    XL4'00'                                                          
                                                                                
*  DAYTIME OR ROTATION                                                          
                                                                                
RC#DYTM  EQU   14                                                               
RQ#DYTM  EQU   14                                                               
         DC    AL2(RQ#DYTM)                                                     
         DC    CL5'DYTM '                                                       
         DC    AL1(RC#DYTM)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MVTBUFF,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DYTMIND-SAVED)                                               
         DC    AL1(61)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#1BDYT)                                        
         DC    AL1(RESYSQ),AL2(RE#DAYTM)                                        
         DC    XL4'00'                                                          
                                                                                
REQRECLX DC    AL1(LD_EOTQ)                                                     
REQUESTX DC    AL1(0)                                                           
                                                                                
***********************************************************************         
*======================================================================         
* OUTPUT MAP FOR PROGRAM DETAILS                                      |         
*======================================================================         
                                                                                
OUTPGDET DS    0X                                                               
                                                                                
O#PGDET  EQU   20                                                               
         DC    AL2(OUTPDETX-*)                                                  
         DC    AL2(O#PGDET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
O#DEMHD  EQU   1                                 DEMO HEADER                    
         DC    AL2(O#DEMHDX-*)                                                  
         DC    AL2(O#DEMHD),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#DEMHD)                                                     
         DC    CL5'DEMHD'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDEMHD-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
O#DEMHDX DS    0X                                                               
                                                                                
O#PC_ID  EQU   2                                 PC_ID                          
         DC    AL2(O#PC_IDX-*)                                                  
         DC    AL2(O#PC_ID),C'PC_ID'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(O#PC_ID)                                                     
         DC    CL5'PC_ID'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OUTPCID-SAVED),AL1(LD_CHARQ,5)                               
         DC    XL4'00'                                                          
O#PC_IDX DS    0X                                                               
                                                                                
O#PGHDR  EQU   3                                 PROG DETAIL HEADER             
         DC    AL2(O#PGHDRX-*)                                                  
         DC    AL2(O#PGHDR),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#PGHDR)                                                     
         DC    CL5'PGHDR'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYPGDET-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
O#PGHDRX DS    0X                                                               
                                                                                
O#SUMHDR EQU   4                                 SUMMARY HEADER INFO            
         DC    AL2(O#SUMHDX-*)                                                  
         DC    AL2(O#SUMHDR),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#SUMHDR)                                                    
         DC    CL5'SUMHD'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYSUMM-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
O#SUMHDX DS    0X                                                               
                                                                                
O#SDEMO  EQU   5                                  PROG SUMMARY DEMOS            
         DC    AL2(O#SDEMOX-*)                                                  
         DC    AL2(O#SDEMO),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#SDEMO)                                                     
         DC    CL5'SDEMO'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYSDEM-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
O#SDEMOX DS    0X                                                               
                                                                                
O#DDEMO  EQU   6                                 PROGRAM DETAIL DEMOS           
         DC    AL2(O#DDEMOX-*)                                                  
         DC    AL2(O#DDEMO),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#DDEMO)                                                     
         DC    CL5'DDEMO'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDDEM-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
O#DDEMOX DS    0X                                                               
                                                                                
O#UPEXP  EQU   8                                 UPGRADE EXPRESSION             
         DC    AL2(O#UPEXPX-*)                                                  
         DC    AL2(O#UPEXP),C'UPEXP'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(O#UPEXP)                                                     
         DC    CL5'UPEXP'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OUTUPEXP-SAVED),AL1(LD_CHARQ,L'RFTFUPGR)                     
         DC    XL4'00'                                                          
O#UPEXPX DS    0X                                                               
                                                                                
OUTPDETX DS    0X                                                               
                                                                                
                                                                                
*======================================================================         
* OUTPUT MAP FOR PROGRAM HEADERS INFORMATION                          |         
*======================================================================         
                                                                                
OUTPRGHD DS    0X                                                               
O#PRGHD  EQU   21                                                               
         DC    AL2(OUTPRGHX-*)                                                  
         DC    AL2(O#PRGHD)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
O#PRGHDR EQU   07                                PROGRAM HEADER                 
         DC    AL2(O#PRGHDX-*)                                                  
         DC    AL2(O#PRGHDR),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#DDEMO)                                                     
         DC    CL5'PRGHD'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYPRGHD-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
                                                                                
O#PRGHDX DS    0X                                                               
OUTPRGHX DS    0X                                                               
                                                                                
*======================================================================         
* OUTPUT MAP FOR CANADIAN SPOT DESKTOP LOOKUP                         |         
*======================================================================         
OUTCANLK LKOUT H                                                                
OUTCAN   LKOUT R,1                                                              
OUTPROG  LKOUT C,1,(D,B#SAVED,CANPROG),CHAR                                     
ARRAY    LKOUT C,2,(A,ARYCDEM)                                                  
OUTLKBK  LKOUT C,3,(D,B#SAVED,CANLKBK),CHAR                                     
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*======================================================================         
* ARRAY DEFINITION DEMO VALUES FOR CANADIAN LOOKUPS                   |         
*======================================================================         
ARYCDEM  LKOUT A,(D,B#SAVED,SDEMVALS),ROWWIDTH=SDEMVALL,               *        
               NROWS=(B#SAVED,SDEMNUM)                                          
         LKOUT C,2,(D,,SDEMOS),UBIN,LEN=L'SDEMOS                                
         LKOUT E                                                                
*======================================================================         
* ARRAY DEFINITION PROGRAM HEADER  INFO                               |         
*======================================================================         
                                                                                
ARYPRGHD DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(OHDRVALS-SAVED)                                              
         DC    AL2(OHDRNUM-SAVED)                  # OF ROWS                    
         DC    AL2(OHDRVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(OHDRCOLN)                                                    
         DC    XL4'00'                                                          
OHDRCOL  DC    0X                                                               
                                                                                
                                                                                
DD#OSTAT EQU   1                                   STATION                      
         DC    AL2(DD#PSTAT),C'STAT '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRSTA-OHDRVALS),AL1(L'OHDRSTA)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#OPROG EQU   2                                   PROGRAM                      
         DC    AL2(DD#OPROG),C'PROG '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRPRG-OHDRVALS),AL1(L'OHDRPRG)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#OINVN EQU   6                                   INVENTORY NUMBER             
         DC    AL2(DD#OINVN),C'INVN '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRINVN-OHDRVALS),AL1(L'OHDRINVN)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#EFFDT EQU   7                                   EFFECTIVE DATE               
         DC    AL2(DD#EFFDT),C'EFFDT'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDREFDT-OHDRVALS),AL1(L'OHDREFDT)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#ODYPT EQU   8                                   DAYPART                      
         DC    AL2(DD#ODYPT),C'DYPT '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRDYPT-OHDRVALS),AL1(L'OHDRDYPT)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#ODYCD EQU   9                                   DAYCODE ALPHA                
         DC    AL2(DD#ODYCD),C'DYCD '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRDYCD-OHDRVALS),AL1(L'OHDRDYCD)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#OSTIM EQU   3                                   START TIME                   
         DC    AL2(DD#OSTIM),C'STIM '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRSTIM-OHDRVALS),AL1(L'OHDRSTIM)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#OETIM EQU   4                                   END TIME                     
         DC    AL2(DD#OETIM),C'ETIM '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRETIM-OHDRVALS),AL1(L'OHDRETIM)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#ODAYS EQU   5                                   DAYS                         
         DC    AL2(DD#ODAYS),C'DAYS '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(OHDRDAY-OHDRVALS),AL1(L'OHDRDAY)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
OHDRCOLN EQU   (*-OHDRCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION PROGRAM DETAILS INFO                               |         
*======================================================================         
                                                                                
ARYPGDET DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(PGDTVALS-SAVED)                                              
         DC    AL2(PGDTNUM-SAVED)                  # OF ROWS                    
         DC    AL2(PGDTVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(PGDTCOLN)                                                    
         DC    XL4'00'                                                          
PGDTCOL  DC    0X                                                               
                                                                                
DD#PSTAT EQU   1                                   STATION                      
         DC    AL2(DD#PSTAT),C'STAT '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PSTAT-PGDTVALS),AL1(L'PSTAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PPROG EQU   2                                   PROGRAM                      
         DC    AL2(DD#PPROG),C'PROG '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PPROG-PGDTVALS),AL1(L'PPROG)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PFILE EQU   3                                   FILE                         
         DC    AL2(DD#PFILE),C'FILE '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PFILE-PGDTVALS),AL1(L'PFILE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PBOOK EQU   4                                   BOOK                         
         DC    AL2(DD#PBOOK),C'BOOK '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBOOK-PGDTVALS),AL1(L'PBOOK)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PSTIM EQU   5                                   START TIME                   
         DC    AL2(DD#PSTIM),C'STIM '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PSTIME-PGDTVALS),AL1(L'PSTIME)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PETIM EQU   6                                   END TIME                     
         DC    AL2(DD#PETIM),C'ETIM '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PETIME-PGDTVALS),AL1(L'PETIME)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#PWEEK EQU   7                                   WEEK                         
         DC    AL2(DD#PWEEK),C'WEEK '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PWEEKS-PGDTVALS),AL1(L'PWEEKS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#DYTIM EQU   10                                  DAYTIME STRING               
         DC    AL2(DD#DYTIM),C'DYTIM'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PDYTIM-PGDTVALS),AL1(L'PDYTIM)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#PDAYS EQU   8                                   DAYS                         
         DC    AL2(DD#PDAYS),C'DAYS '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PDAYS-PGDTVALS),AL1(L'PDAYS)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#PACTF EQU   9                                   ACTIVE FLAG                  
         DC    AL2(DD#PACTF),C'ACTF '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PACTF-PGDTVALS),AL1(L'PACTF)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#MFID  EQU   11                                  MAINFRAME ID                 
         DC    AL2(DD#MFID),C'MFID '                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(PMFID-PGDTVALS),AL1(L'PMFID)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#FACT  EQU   12                                  FACTOR                       
         DC    AL2(DD#FACT),C'FACT '                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(PFACT-PGDTVALS),AL1(L'PFACT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
PGDTCOLN EQU   (*-PGDTCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION SUMMARY INFO                                       |         
*======================================================================         
                                                                                
ARYSUMM  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(SUMMVALS-SAVED)                                              
         DC    AL2(SUMMNUM-SAVED)                  # OF ROWS                    
         DC    AL2(SUMMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(SUMMCOLN)                                                    
         DC    XL4'00'                                                          
SUMMCOL  DC    0X                                                               
                                                                                
DD#SSTAT EQU   1                                   STATION                      
         DC    AL2(DD#PSTAT),C'STAT '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SSTAT-SUMMVALS),AL1(L'SSTAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SPROG EQU   2                                   PROGRAM/HIST PROGRAM         
         DC    AL2(DD#PPROG),C'PROG '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SPROG-SUMMVALS),AL1(L'SPROG)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SFILE EQU   3                                   FILE                         
         DC    AL2(DD#PFILE),C'FILE '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SFILE-SUMMVALS),AL1(L'SFILE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SBOOK EQU   4                                   BOOK                         
         DC    AL2(DD#PBOOK),C'BOOK '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SBOOK-SUMMVALS),AL1(L'SBOOK)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SSYSC EQU   5                                   SYSCODE                      
         DC    AL2(DD#SSYSC),C'SYSCD'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SSYSC-SUMMVALS),AL1(L'SSYSC)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
SUMMCOLN EQU   (*-SUMMCOL)/LX_COLSL                                             
*======================================================================         
* ARRAY DEFINITION FOR DEMOS PROGRAM SUMMARY                          |         
*======================================================================         
                                                                                
ARYSDEM  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(SDEMVALS-SAVED)                                              
         DC    AL2(SDEMNUM-SAVED)                  # OF ROWS                    
         DC    AL2(SDEMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(SDEMCOLN)                                                    
         DC    XL4'00'                                                          
SDEMCOL  DC    0X                                                               
                                                                                
DD#SDEMS EQU   1                                   DEMOS                        
         DC    AL2(DD#SDEMS),C'SDEMO'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SDEMOS-SDEMVALS),AL1(L'SDEMOS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
SDEMCOLN EQU   (*-SDEMCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION FOR DEMOS     PROGRAM DETAIL                       |         
*======================================================================         
                                                                                
ARYDDEM  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DDEMVALS-SAVED)                                              
         DC    AL2(DDEMNUM-SAVED)                  # OF ROWS                    
         DC    AL2(DDEMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DDEMCOLN)                                                    
         DC    XL4'00'                                                          
DDEMCOL  DC    0X                                                               
                                                                                
DD#DDEMS EQU   1                                   DEMOS                        
         DC    AL2(DD#DDEMS),C'DDEMO'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DDEMOS-DDEMVALS),AL1(L'DDEMOS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DDEMCOLN EQU   (*-DDEMCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION FOR DEMOS HEADER RECORD                            |         
*======================================================================         
                                                                                
ARYDEMHD LKOUT A,(D,B#SAVED,DMHDVALS),NROWS=(B#SAVED,DMHDNUM),         +        
               ROWWIDTH=DMHDVALL                                                
*                                                                               
DD#DHPRE EQU   1                                   PRECISION                    
         LKOUT C,DD#DHPRE,(D,,DMHDPRE),UBIN                                     
*                                                                               
DD#DHDEM EQU   2                                   DEMO NAME                    
         LKOUT C,DD#DHDEM,(D,,DMHDDEM),(R,EDTDEML)                              
*                                                                               
         LKOUT E                                                                
*                                                                               
EDTDEML  L     R1,ALP                                                           
         USING LP_D,R1                                                          
         LM    R2,R4,LP_AINP                                                    
         MVC   0(L'DMHDDEM,R4),0(R2)                                            
         LHI   RE,L'DMHDDEM                                                     
         LA    RF,(L'DMHDDEM-1)(R4)                                             
EDTDEML5 CLI   0(RF),X'FF'                                                      
         JE    EDTDEML6                                                         
         CLI   0(RF),C' '                                                       
         JH    EDTDEMLX                                                         
EDTDEML6 BCTR  RE,0                                                             
         CHI   RE,0                                                             
         JNH   EDTDEMLX                                                         
         MVI   0(RF),0             No trailing spaces                           
         BCTR  RF,0                                                             
         J     EDTDEML5                                                         
EDTDEMLX STCM  RE,15,LP_OLEN       Set output field length                      
         J     EXITY                                                            
*                                                                               
EDTDMMY  L     R1,ALP                                                           
         USING LP_D,R1                                                          
         LM    R2,R4,LP_AINP                                                    
         MVI   0(R4),C'U'          U in dUmmy                                   
         LHI   RE,1                                                             
         STCM  RE,15,LP_OLEN       Set output field length                      
         J     EXITY                                                            
*                                                                               
                                                                                
PROFLIT  DC    C'1W PROFILE'                                                    
RSATEXT  DC    C'RATING SERVICE, STATION, ALPHA MKTS'                           
STIMTXT  DC    C'START TIME'                                                    
ETIMTXT  DC    C'END TIME'                                                      
LATBNTXT DC    C'NUMBER OF LATEST BOOKS'                                        
LATBTTXT DC    C'BOOKTYPE FOR LATEST BOOK'                                      
*======================================================================         
SAVED    DSECT                                                                  
SVVALUES DS    0D                                                               
RELOLST2 DS    0A                                                               
ADEMBUFF DS    A                                                                
RLOLSTN2 EQU   (*-RELOLST2)/L'RELOLST2                                          
SVVALUESL EQU   *-SVVALUES                                                      
                                                                                
REQVALS  DS    0A                                                               
                                                                                
                                                                                
BYTE     DS    X                                                                
BOOKIND  DS    XL1                                                              
ABOOK    DS    AL3                                                              
DAYIND   DS    XL1                                                              
ADAY     DS    AL3                                                              
DEMOIND  DS    XL1                                                              
ADEMO    DS    AL3                                                              
TIMEIND  DS    XL1                                                              
ATIME    DS    AL3                                                              
BKTYIND  DS    XL1                                                              
ABKTY    DS    AL3                                                              
DATEIND  DS    XL1                                                              
ADATE    DS    AL3                                                              
FUNCIND  DS    XL1                                                              
AFUNCT   DS    AL3                                                              
FILEIND  DS    XL1                                                              
AFILE    DS    AL3                                                              
STAIND   DS    XL1                                                              
ASTAT    DS    AL3                                                              
OSTATIND DS    XL1                                                              
AOSTAT   DS    AL3                                                              
AMKTIND  DS    XL1                                                              
AAMKT    DS    AL3                                                              
PROGIND  DS    XL1                                                              
APROG    DS    AL3                                                              
AGYIND   DS    XL1                                                              
AAGY     DS    AL3                                                              
DYTMIND  DS    XL1                                                              
ADAYTIME DS    AL3                                                              
NROWIND  DS    XL1                                                              
ANROW    DS    AL3                                                              
PCIDIND  DS    XL1                                                              
APCID    DS    AL3                                                              
DYPTIND  DS    XL1                                                              
ADYPT    DS    AL3                                                              
EFFDTIND DS    XL1                                                              
AEFFDT   DS    AL3                                                              
INVNMIND DS    XL1                                                              
AINVNUM  DS    AL3                                                              
MFIDIND  DS    XL1                                                              
AMFID    DS    AL3                                                              
UPGRIND  DS    XL1                                                              
AUPGRD   DS    AL3                                                              
UINDXIND DS    XL1                                                              
AUINDX   DS    AL3                                                              
UPININD  DS    XL1                                                              
AUPIND   DS    AL3                                                              
UPNAMIND DS    XL1                                                              
AUPNAME  DS    AL3                                                              
DECIND   DS    XL1                                                              
ADECIMAL DS    AL3                                                              
SYSCIND  DS    XL1                                                              
ASYSCODE DS    AL3                                                              
RRSMIND  DS    XL1                                                              
ARSTAMKT DS    AL3                                                              
LATBNIND DS    XL1                                                              
ALATBKNM DS    AL3                                                              
LATBTIND DS    XL1                                                              
ALATBKTP DS    AL3                                                              
                                                                                
CANMED   DS    C                                                                
*CANDAY   DS    X                                                               
*CANSTIM  DS    XL2                                                             
*CANETIM  DS    XL2                                                             
*CANBOOK  DS    CL2                                                             
CANPROF  DS    CL16                                                             
CAN2DEC  DS    C                                                                
REQVALSL EQU   *-REQVALS                                                        
                                                                                
VERS76   EQU   X'0100004D'                                                      
SPVER26  EQU   X'02060000'         SPOT DESKTOP 2.6                             
PPVER30  EQU   X'03000000'         PROPOSER  3.0                                
VERSNUM  DS    XL4                                                              
ELCODE   DS    C                                                                
MYSTAPTR DS    A                                                                
AMKTPTR  DS    A                                                                
MFIDPTR  DS    A                                                                
DYTMPTR  DS    A                                                                
NUMDEMO  DS    AL2                                                              
NUMDEMO2 DS    AL2                                                              
NUMSTAT  DS    AL2                                                              
DEMOPTR  DS    A                                                                
NMAMKTS  DS    X                                                                
INPSTA   DS    CL(L'STATION)                                                    
INPWEEK  DS    CL1                                                              
TMPDYPT  DS    CL(L'INVDYPT)                                                    
TMPFILE  DS    CL3                                                              
TMPEFFDT DS    XL4                                                              
TMPBOOK  DS    XL3                                                              
TMPBKTYP DS    C                                                                
TMPBKWK  DS    CL1                     WEEK                                     
TMPDAY   DS    CL1                                                              
TMPSETIM DS    XL4                                                              
TMPINVNM DS    CL4                                                              
TMPMFID  DS    CL5                                                              
TMPPURE  DS    CL4                                                              
TMPUINDX DS    XL1                                                              
TMPLATBN DS    XL1                                                              
TMPLATBT DS    C                                                                
BINPURE  DS    XL2                                                              
TMPSTA   DS    CL(L'DBSELSTA)                                                   
TMPDYTIM DS    200CL5                                                           
TMPSYSC  DS    XL2                                                              
MYDMCB   DS    6F                                                               
SAVEDMCB DS    6F                                                               
TEMPSTIM DS    XL2                                                              
TEMPETIM DS    XL2                                                              
TEMPIDAY DS    XL1                                                              
DAYTIME  DS    CL13                                                             
TMPKSE   DS    0XL4                                                             
TMPKDAY  DS    XL1                                                              
TMPSQH   DS    XL1                                                              
TMPEQH   DS    XL1                                                              
TMPWKS   DS    XL1                                                              
THIS1WK  DS    XL1                               CURRENT 1ST WK OF PRG          
THISNWK  DS    XL1                               CURRENT NUM WKS OF PRG         
FNDX     DS    XL1                                                              
THISNDYS DS    XL1                                                              
MAXDAYS  EQU   20                                                               
DAYS     DS    (MAXDAYS)XL5                                                     
FILBKPTR DS    A                                                                
THISQHAF DS    XL1                                                              
ROWCOUNT DS    F                                 CURRENT ROW COUNT              
FBKCOUNT DS    F                                 CURRENT FILE BK COUNT          
**COLCOUNT DS    F                                 CURRENT COLUMN COUNT         
NUMMFID  DS    F                                 NUMBER OF MFIDS'               
TOTALROW DS    F                                 TOTAL NUMBER OF ROWS           
TOTALFBK DS    F                                 TOTAL NUMBER OF FIL/BK         
DYTMNUM  DS    F                                                                
ROTFLAG  DS    C                                                                
ALLTPFLG DS    C                                 ALL TP PROCESSED?              
HDRFLAG  DS    C                                                                
THISPROG DS    CL17                                                             
TMPBEST  DS    C                                                                
ANYDETLS DS    C                                 DETAILS FLAG                   
INVFLAG  DS    C                                                                
ANYTPFLG DS    C                                                                
RECALFLG DS    C                                                                
SAVGFLAG DS    C                                                                
SVSETIM  DS    XL(L'TMPSETIM)                                                   
SVLATBK  DS    XL2                                                              
SVLATBT  DS    C                                                                
NUMLATBK DS    X                                                                
LATBKTYP DS    C                                                                
OVRFLAG  DS    C                                 OVERRIDE STATION FLAG          
LATFLAG  DS    C                                                                
FROMTYPE DS    CL1                                                              
TMPUPNAM DS    CL15                                                             
TMPMBKS  DS    XL6                                                              
OFFLFLAG DS    CL1                                                              
TMPWHOLE DS    XL(L'WHOLEFLG)      WHOLE # FLAG FOR EXTRACTING DEMOS            
WHOLEFLG DS     XL1                 MODIFIERS TO DISPLAY AS WHOLE #S            
WHOLERTG EQU    X'80'               RTGS DISPLAYED AS XX.0                      
WHOLESHR EQU    X'40'               SHRS DISPLAYED AS XX.0                      
WHOLEPUT EQU    X'20'               PUTS DISPLAYED AS XX.0                      
OPTDEC   DS    C                                                                
SYSCEXT  DS    0X                                                               
FULL     DS    F                                                                
         DS    XL(L'SPXTAREA)                                                   
                                                                                
MYBKH    DS    CL8                                                              
MYBK     DS    CL10                                                             
DUMSTATH DS    CL8                                                              
DUMSTAT  DS    CL20                                                             
ALFMKTS  DS    CL3                                                              
SPILLMKT DS    XL2                                                              
                                                                                
*--------------------------------------------------------------------           
MAXDEMS  EQU   60                                                    |          
THISDEMS DS    (MAXDEMS*3)XL4          CURRENT DEMOUT DEMOVALUES     |          
THISDEMX DS    0X                                                    |          
THISDEML EQU   THISDEMX-THISDEMS                                                
*---------------------------------------------------------------------          
DEMODEMS DS    (MAXDEMS*3)XL3,X        DEMO VALUES (+LIST TERMINATOR)|          
DEMODEML EQU   *-DEMODEMS                                            |          
*---------------------------------------------------------------------          
MYPROF1W DS    CL(L'PROF1W)                                                     
PRECPROF DS    CL1                     PRECISION SET IN PROFILE                 
SELWPROF DS    CL1                     SELLERS WORKSHEET PROFILE                
VGETPROF DS    A                                                                
MYDATDSP DS    H                                                                
SDBXF    DS    C                       SET DBLOCK EXTENSION FLAG                
MYDBXTRA DS    XL128                   DBLOCK EXTENSION                         
         DS    0F                      (FORCE FULL-WORD ALIGNMENT)              
DBXTROTN DS    0X                      DBLOCK EXTENSION FOR ROTATIONS           
         DS    CL4                     LINK ID                                  
         DS    A                       A(NEXT LINK)                             
DBXTDYTM DS    (MAXDTCMP)XL5,XL1       DAY/TIME LINK                            
MAXDTCMP EQU   (255)/(19+1)            L(MTWTFSS/1130A-1230P) = 19              
                                                                                
ADBLOCK  DS    A                                                                
QHVUTS   DS    3F                                VUT VALUES                     
*                                     **** MATH BLOCK ***********               
HOMSHR   DS    3F                                SHARES VALUES                  
HOMSHRL  EQU   *-HOMSHR                                                         
CUMSHR   DS    3F                                CUMULATIVE SHARES              
CUMSHRL  EQU   *-CUMSHR                                                         
         DS    0F                                                               
MATHFAC  DS    0XL17                   MATH BLOCK                               
MTHCFACS DS    A                       A(DBLOCK)                                
MTHFCTR  DS    F                       WEIGHTING FACTOR FOR MULTI&DIV           
MTHIFIL  DS    CL3                     INPUT FILE                               
MTHOFIL  DS    CL3                     OUTPUT FILE                              
MTHOSRC  DS    CL3                     OUTPUT  SOURCE                           
                                                                                
CUMFCTR  DS    F                       CUMULATIVE MATH FACTOR                   
CUMPNAM1 DS    CL(L'THISPROG)          CUMULATIVE PROGRAM NAME #1               
CUMPNAM2 DS    CL(L'THISPROG)          "         "     "   #2                   
*CUMWKS   DS    XL(L'THISWKS)          CUMULATIVE ACTIVE WEEKS                  
*                                      X'80' = MULTIPLE PROGRAMS WITH           
*                                      DIFFERING ACTIVE WEEKS                   
GKSPARM  DS    XL5                                                              
AIUNWRK  DS    A                       A(WORK AREA FOR IUN STUFF)               
IUNWRKL  EQU   IUNRECL+500                                                      
AVUTLIST DS    A                                 A(VUTLIST)                     
APUTLIST DS    A                                 A(PUTLIST)                     
ASHRLIST DS    A                                 A(SHRLIST)                     
AVUTS    DS    A                                 VUT VALUES PASSED TO           
A1STWKTB DS    A                                 A(FRSTWKTB)                    
ANUMWKTB DS    A                                 A(NUMWKTAB)                    
AXTRCREC DS    A                                 A(RECORD TO EXTRACT)           
AUPGRADE DS    A                                                                
                                                                                
                                                                                
SVDBLOCK DS    XL(DBLOCK1X-DBLOCK1)                                             
OUTVALS  DS    0X                                                               
CANPROG  DS     CL(L'THISPROG)                                                  
OUTPCID  DS    CL5                               PC_ID                          
OUTUPEXP DS    CL(L'RFTFUPGR)                    UPGRADE EXPRESSION             
OUTMFID  DS    CL14                              OUT MAINFRAME ID               
CANLKBK  DS    CL12                                                             
                                                                                
DDEMNUM  DS    AL2                                                              
DDEMVALS DS    0X                                                               
DDEMSEND DS    CL1                                                              
DDEMOS   DS    CL4                                                              
DDEMVALL EQU   *-DDEMVALS                                                       
         DS    60XL(DDEMVALL)                                                   
                                                                                
SDEMNUM  DS    AL2                                                              
SDEMVALS DS    0X                                                               
SDEMSEND DS    CL1                                                              
SDEMOS   DS    CL4                                                              
SDEMVALL EQU   *-SDEMVALS                                                       
         DS    60XL(SDEMVALL)                                                   
                                                                                
DMHDNUM  DS    AL2                                                              
DMHDVALS DS    0X                                                               
DMHDSEND DS    CL1                                                              
DMHDPRE  DS    XL1                                                              
DMHDDEM  DS    CL20                                                             
DMHDVALL EQU   *-DMHDVALS                                                       
         DS    60XL(DMHDVALL)                                                   
                                                                                
PGDTNUM  DS    AL2                                                              
PGDTVALS DS    0X                                                               
PGDTSEND DS    CL1                                                              
PSTAT    DS    CL10                                                             
PPROG    DS    CL(L'THISPROG)                                                   
*****PBOOK    DS    CL10                                                        
PBOOK    DS    CL50                                                             
PSTIME   DS    XL2                                                              
PETIME   DS    XL2                                                              
PWEEKS   DS    CL4                                                              
PDAYS    DS    XL1                                                              
PFILE    DS    CL3                                                              
PACTF    DS    CL1                                                              
PDYTIM   DS    CL20                                                             
*PMFID    DS    CL14                                                            
PMFID    DS    CL15                                                             
PFACT    DS    XL2                                                              
PGDTVALL EQU   *-PGDTVALS                                                       
                                                                                
SUMMNUM  DS    AL2                                                              
SUMMVALS DS    0X                                                               
SUMMSEND DS    CL1                                                              
SSTAT    DS    CL10                                                             
SPROG    DS    CL(L'THISPROG)                                                   
SBOOK    DS    CL50                                                             
SFILE    DS    CL3                                                              
SSYSC    DS    CL5                                                              
SUMMVALL EQU   *-SUMMVALS                                                       
                                                                                
                                                                                
OHDRNUM  DS    AL2                                                              
OHDRVALS DS    0X                                                               
OHDRSEND DS    CL1                                                              
OHDRSTA  DS    CL10                                                             
OHDRPRG  DS    CL60                                                             
OHDRSTIM DS    XL2                                                              
OHDRETIM DS    XL2                                                              
OHDRDAY  DS    XL1                                                              
OHDRINVN DS    CL4                                                              
OHDREFDT DS    CL20                                                             
OHDRDYPT DS    C                                                                
OHDRDYCD DS    CL3                                                              
OHDRVALL EQU   *-OHDRVALS                                                       
                                                                                
OUTVALSL EQU   *-OUTVALS                                                        
                                                                                
                                                                                
FETCHWKD DSECT                                                                  
FETCHBLK DS    XL(RFTBLKL)                       FETCH BLOCK                    
FETCHWRK DS    XL(6*ONEK)                                                       
FETCHWKL EQU   *-FETCHWKD                                                       
                                                                                
         PRINT OFF                                                              
                                                                                
       ++INCLUDE REFETCHD                                                       
       ++INCLUDE DELNKWRK                                                       
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE REDDEQUS                                                       
       ++INCLUDE DEDEMEQUS                                                      
REINVRCD DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
                                                                                
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENIBKL                                                      
                                                                                
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE REGENREPA                                                      
                                                                                
SPDEMLKD DSECT                                                                  
       ++INCLUDE SPDEMLK                                                        
                                                                                
RRDPRECD DSECT                                                                  
       ++INCLUDE REGENRDP                                                       
       ++INCLUDE FAFACTS                                                        
                                                                                
       ++INCLUDE DEDEMFILE                                                      
                                                                                
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDBUFFD                                                        
SPDEMLKD DSECT                                                                  
       ++INCLUDE SPDEMLKXTD                                                     
DEMTABD  DSECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
                                                                                
IOWORKD  DSECT                                                                  
IOWORK   DS    CL96                FOR GETREC/SPTFIL                            
IOPARM   DS    6F                  DATAMGR PARM LIST                            
IOWORK1  DS    X                   I/O COMMAND FLAG                             
IOWORK2  DS    X                   FILE/DIR NUMBER                              
IOWORK3  DS    X                   COMMAND NUMBER                               
IOFILE   DS    CL7                 I/O FILE NAME                                
IODIR    DS    CL7                 I/O DIRECTORY NAME                           
IOCMND   DS    CL7                 I/O DATAMGR COMMAND                          
IOINDS   DS    0XL5                I/O INDICATORS                               
IOFILTY  DS    XL1                 FILE/DIRECTORY TYPE                          
IOKEYLN  DS    XL1                 L'KEY (DANDX INCLUDES DBMINKEY)              
IODADSP  DS    XL1                 DISPLACEMENT TO D/A IN KEY                   
IOFSDSP  DS    XL1                 DISPLACEMENT TO FIL STATUS BYTE              
IODSDSP  DS    XL1                 DISPLACEMENT TO DIR STATUS BYTE              
IOWORKX  EQU   *                                                                
                                                                                
*******************************************************************             
*========================== IUN RECORD DSECT =========================*         
                                                                                
IUNRECD  DSECT                                                                  
                                                                                
IUNIVS   DS    (IUNNVALS)F        UNIVERSES                                     
                                                                                
IUNOLD   DS    0F                 ORIGINAL (OLD) BOOK VALUES                    
IOLDRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   IOLDRTG+IUNHMDSP                                                 
IUORHOME DS    F                                                                
         ORG                                                                    
IOLDIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
IOLDHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   IOLDHPT+IUNHMDSP                                                 
IUOPHOME DS    F                                                                
         ORG                                                                    
IOLDTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   IOLDTOT+IUNHMDSP                                                 
IUOQHOME DS    F                                                                
         ORG                                                                    
IUNOLDX  EQU   *                                                                
                                                                                
IUNNEW   DS    0F                 NEW VALUES                                    
INEWRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   INEWRTG+IUNHMDSP                                                 
IUNRHOME DS    F                                                                
         ORG                                                                    
INEWIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
INEWHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   INEWHPT+IUNHMDSP                                                 
IUNPHOME DS    F                                                                
         ORG                                                                    
INEWTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   INEWTOT+IUNHMDSP                                                 
IUNQHOME DS    F                                                                
         ORG                                                                    
IUNNEWX  EQU   *                                                                
                                                                                
         DS    0CL((IUNOLDX-IUNOLD)-(IUNNEWX-IUNNEW)+1)                         
         DS    0CL((IUNNEWX-IUNNEW)-(IUNOLDX-IUNOLD)+1)                         
                                                                                
IUNOTH   DS    0F                 OTHER VALUES                                  
ISHOMES  DS    F                                                                
ISMETA   DS    F                                                                
ISMETB   DS    F                                                                
                                                                                
ILUNVS   DS    (IUNNVALS)F         LOONEYVERSES                                 
ILUNVX   EQU   *                                                                
                                                                                
IUNRECL  EQU   *-IUNRECD                                                        
                                                                                
                                                                                
IUNNVALS EQU   32                  # OF IUN VALUES                              
IUNLVALS EQU   IUNNVALS*4          LENGTH OF IUN VALUES                         
IUNHMNDX EQU   20                  INDEX TO HOMES RTGS IN IUN RTGS AREA         
IUNHMDSP EQU   IUNHMNDX*4          DISPL TO HOMES RTGS IN IUN RTGS AREA         
***********************************************************************         
*******************************************************************             
*  2 DSECTS TO COVER TABLES BUILT BY DDLINK                       *             
*  DSECT TO COVER UPGRADES AND INDEX NUMBER                       *             
*******************************************************************             
UPGRADD  DSECT                                                                  
UPBKTYP  DS    CL1                                                              
UPBKVAL  DS    XL8                                                              
UPSHRBK  DS    XL2                                                              
UPDYTIM  DS    XL5                                                              
UP2YRP   DS    CL1                                                              
UP2YRR   DS    CL1                                                              
UPWEEK   DS    XL1                                                              
UPNAME   DS    XL15                              UPGRADE NAME                   
UPINDEX  DS    XL1                                                              
UPGRADX  EQU   *-UPGRADD                                                        
*                                                                               
FILBKD   DSECT                                                                  
*                                          DSECT TO COVER FILE  BOOK            
FBKFILE  DS    CL3                               FILE CODE                      
FBKBKTY  DS    CL1                               BOOKTYPE                       
FBKWEEK  DS    CL1                               WEEK                           
FBKVBFLG DS    XL1                               VALID BOOK FLAG                
FBKBOOK  DS    XL3                               3 BYTE BOOK VALUE              
*  NEWEST                                                                       
FBKMULBK DS    XL6                               3( 2 BYTES BOOKS)              
*                                                                               
FBKPCID  DS    CL5                               PC ID                          
*                                                                               
FBKUPIND DS    XL1                               UPGRADE INDEX                  
*                                                                               
FBKLATBN DS    XL1                               NUM OF LATEST BKS              
FBKLATBT DS    XL1                               BOOKTYPE FOR LATEST BK         
*                                                                               
*                                                                               
FILBKL   EQU   *-FILBKD                                                         
                                                                                
STATTABD DSECT                                                                  
*                               DSECT TO COVER STATION HEADER                   
STATION  DS    CL11                 INPUT STATION                               
STAFLG   DS    CL1                  VALID STATION FLAG                          
STALEN   DS    XL1                  LENGTH OF STATION INPUT                     
STASPLLF DS    XL1                  SPILL STAT FLAG                             
STADYTM  DS    CL61                 LIST OF DAY/TIME CODE                       
*                                                                               
STAEFFDT DS    XL4                  EFFECTIVE DATE                              
STAINVN  DS    CL4                  INVENTORY NUMBER                            
STASYSC  DS    CL2                  SYSCODE                                     
                                                                                
STATNROW DS    XL4                  NUMBER OF ROWS                              
STATTABL EQU   *-STATTABD                                                       
                                                                                
PAVHDRD  DSECT                      DSECT TO COVER PAV                          
*                                   PROGRAM HEADER                              
PAVSTAT  DS    CL11                 INPUT STATION                               
PAVSFLAG DS    CL1                  VALID STATION FLAG                          
PAVSTALN DS    XL1                  LENGTH OF INPUT STAT                        
PAVSPLLF DS    XL1                  SPILL STAT FLAG                             
PAVDYTM  DS    CL61                 LIST OF DAY/TIME CODE                       
PAVBKTY  DS    CL1                  BOOKTYPE                                    
PAVWEEK  DS    CL1                  WEEK                                        
PAVVBFLG DS    XL1                  VALID BOOK FLAG                             
PAVBOOK  DS    XL3                  3 BYTE BOOK VALUE                           
PAVHDRL  EQU   *-PAVHDRD                                                        
                                                                                
                                                                                
INVHDRD  DSECT                     DSECT TO COVER INV                           
*                                  PROGRAM HEADER                               
INVSTAT  DS    CL11                INPUT STATION                                
INVOSTAT DS    CL5                 OVERRIDE STATION                             
INVDYPT  DS    CL100               DAYPARTS                                     
INVHDRL  EQU   *-INVHDRD                                                        
                                                                                
RECALCD  DSECT                     DSECT TO COVER PART                          
RCSTAT   DS    CL11                OF RECALCULATION                             
RCSTAFLG DS    CL1                 VALID STATION FLAG                           
RCSTALEN DS    XL1                 LENGTH OF INPUT STAT                         
RCSPILL  DS    XL1                 SPILL STAT FLAG                              
RCFILE   DS    CL3                                                              
RCBKTYP  DS    CL1                 BOOKTYPE                                     
RCWEEK   DS    CL1                 WEEK                                         
RCBKFLG  DS    XL1                 VALID BOOK FLAG                              
RCBOOK   DS    XL3                 3 BYTE BOOK VALUE                            
* NEWEST                                                                        
RCMULBK  DS    XL6                 3 MULTIBOOKS                                 
RCUPINDX DS    XL1                 UPGRADE INDEX NUMBER                         
                                                                                
VDOUTD   DSECT                     ** DEMO OUTPUT ROW **                        
VDODEMCD DS    0XL3                ** DEMO CODE **                              
         DS    X                                                                
VDODEMTY DS    C                   DEMO TYPE                                    
VDODEMNO DS    X                   DEMO NUMBER                                  
VDODEMYN DS    C                   ** DEMO IS VALID INDICATOR **                
VDODEMYQ EQU   C'Y'                DEMO IS VALID                                
VDODEMNQ EQU   C'N'                DEMO IS INVALID                              
VDODEMNM DS    CL20                DEMO NAME/INPUT STRING                       
VDOUTL   EQU   *-VDOUTD            WIDTH OF DEMO ROW                            
VDTOTLN  EQU   MAXDEMN*VDOUTL+1    LENGTH OF INPUT VALIDATED DEMO BLOCK         
MAXDEMN  EQU   42                                                               
                                                                                
CANRSRVD DSECT                     CANADIAN LOOKUP SOURCE/STA/AMKT              
CANRSRV  DS    CL1                 RATING SERVICE                               
CANSTAT  DS    CL5                 STATION                                      
CANNMKTS DS    XL1                 NUMBER OF ALPHA MARKETS                      
CANAMKTS DS    17CL3               INPUT ALPHA MKT (17 MAX)                     
CANDAY   DS    X                                                                
CANSTIM  DS    XL2                                                              
CANETIM  DS    XL2                                                              
CANBOOK  DS    CL2                                                              
CANRSRVL EQU   *-CANRSRVD          WIDTH                                        
                                                                                
MFIDD    DSECT                     MAINFRAME ID                                 
MFDAY    DS    CL3                 DAYCODE                                      
MFSTIME  DS    CL4                 START TIME                                   
MFETIME  DS    CL4                 END TIME                                     
MFPURE   DS    CL4                 PURE NUMBER                                  
                                                                                
GKSPARMD DSECT                                                                  
GKSPRSVC DS    CL1                 RATING SERVICE                               
GKSPQLFY DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GKSPBKBT DS    XL1                 BOOKVAL BITS                                 
GKSPBTYP DS    CL1                 BOOK TYPE                                    
         DS    XL1                 SPARE                                        
GKSPARMX EQU   *                                                                
GKSPARML EQU   GKSPARMX-GKSPARMD                                                
         DS    0XL(L'GKSPARM-GKSPARML+1)                                        
         DS    0XL(GKSPARML-L'GKSPARM+1)                                        
                                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029DELNK11   06/08/17'                                      
         END                                                                    
