*          DATA SET PPLNK10    AT LEVEL 028 AS OF 05/10/16                      
*PHASE T41410A                                                                  
PPLNK10  TITLE '- CLT/PRD/EST DOWNLOAD - DRAFT WIZARD - ADCODE VERIFY'          
                                                                                
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,REQUEST=*,CODE=CODE,SYSTEM=PRTSYSQ,              *        
               LOADFACSOFF=Y,SERVERTYPE=TSTADBY,IDF=Y,WORKERKEY=PPWD,  *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED,                    *        
               B#CLT,PCLTKEY,B#PRD,PPRDKEY,B#EST,PESTKEY,              *        
               B#PUB,PUBRECD,B#REP,PREPRECD,B#ADC,PJOBRECD,            *        
               B#AD2,ADC_OUTD,B#CON,CON_OUTD,B#COM,PCOMRECD)                    
                                                                                
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**PL10**,RR=RE                                                 
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
                                                                                
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(DDLINK PARAMETER BLOCK)                 
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         L     R8,LP_ABLK2                                                      
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
         USING SAVED,R8            R8=A(SAVE W/S)                               
         USING OFFICED,OFCBLK                                                   
         ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
INIT04   MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         DROP  R6,R7                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNSTR02            NO                                           
         MVC   AROUT1,LP_AUIR1     SET LOCAL INDEX ROUTINE ADDRESSES            
         MVC   AROUT2,LP_AUIR2     WHICH WERE LOADED BY MASTER SERVER           
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE SERVER WORKING STORAGE            
                                                                                
RUNSTR02 MVC   LP_BLKS+((B#MED-1)*L'LP_BLKS),AIO1                               
         MVC   LP_BLKS+((B#CLT-1)*L'LP_BLKS),AIO2                               
         MVC   LP_BLKS+((B#PRD-1)*L'LP_BLKS),AIO3                               
         MVC   LP_BLKS+((B#EST-1)*L'LP_BLKS),AIO4                               
         MVC   LP_BLKS+((B#PUB-1)*L'LP_BLKS),AIO5                               
         MVC   LP_BLKS+((B#REP-1)*L'LP_BLKS),AIO6                               
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
                                                                                
         LHI   R0,QPUBVAL          LOAD PUBVAL                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   VPUBVAL,0(R1)                                                    
                                                                                
         LHI   R0,QPUBEDIT         LOAD PUBEDIT                                 
         ICM   R0,B'1110',T00A                                                  
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   VPUBEDIT,0(R1)                                                   
                                                                                
         LHI   R0,QPGETADR         LOAD GETADR                                  
         ICM   R0,B'1110',T00A                                                  
         GOTOR (RF),(R1),0,(R0),0                                               
         MVC   VPPGETAD,0(R1)                                                   
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         LA    RE,REQVALS                                                       
         LHI   RF,REQVALSL                                                      
         XCEFL                                                                  
         LA    RE,PAYEE_VS                                                      
         LHI   RF,PAYEE_LQ                                                      
         XCEFL                                                                  
         MVC   ENDDATE,EFFS                                                     
         L     RF,AIO2                                                          
         XC    0(256,RF),0(RF)     CLEAR I/O AREA SENDING VALUES                
         L     RF,AIO3                                                          
         XC    0(256,RF),0(RF)                                                  
         L     RF,AIO4                                                          
         XC    0(256,RF),0(RF)                                                  
         J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
         MVC   AGY,LP_AGY                                                       
         MVC   MAP#,LP_QMAPN       SET PROCESSING MAP NUMBER                    
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         BZ    RUNREQ01                                                         
         GOTOR VDATAMGR,DMCB,DMKEY,PRTDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PRTFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,PUBFIL,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,GENFIL,(4,0),0                               
                                                                                
RUNREQ01 CLC   MAP#,=AL2(M#DLVER)                                               
         BE    *+10                                                             
         CLC   MAP#,=AL2(M#DLWIZ)                                               
         BE    *+10                                                             
         CLC   MAP#,=AL2(M#DLAVD)                                               
         BNE   RUNREQ18                                                         
                                                                                
         LA    R2,IOKEY                                                         
         USING PAGYRECD,R2                                                      
         XC    PAGYKEY,PAGYKEY                                                  
         MVC   PAGYKAGY,AGY                                                     
         ICM   RE,7,AMED                                                        
         BZ    INVMED                                                           
         MVC   PAGYKMED,LW_DATA1-LW_D(RE)                                       
         MVI   PAGYKRCD,PAGYKIDQ                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO1'                            
         BE    RUNREQ02                                                         
         CLC   MAP#,=AL2(M#DLVER)                                               
         JE    EXITY                                                            
         B     INVMED                                                           
                                                                                
RUNREQ02 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO1'                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ICM   RF,7,ACLT                                                        
         BZ    RUNREQ14                                                         
         LA    R2,IOKEY                                                         
         USING PCLTRECD,R2                                                      
         XC    PCLTKEY,PCLTKEY                                                  
         MVC   PCLTKAGY,AGY                                                     
         ICM   RE,7,AMED                                                        
         MVC   PCLTKMED,LW_DATA1-LW_D(RE)                                       
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,LW_DATA1-LW_D(RF)                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO2'                            
         BE    RUNREQ04                                                         
         CLC   MAP#,=AL2(M#DLVER)                                               
         BE    RUNREQ14                                                         
         B     INVCLT                                                           
                                                                                
RUNREQ04 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
                                                                                
         TM    PCLTSTAT,X'04'      COS2 $?                                      
         JZ    *+8                                                              
         MVI   COS2FLAG,COS2DOLQ                                                
         TM    PCLTSTAT,X'08'      COS2 Factor?                                 
         JZ    *+8                                                              
         MVI   COS2FLAG,COS2FACQ                                                
                                                                                
         TM    PCLTSTAT,X'02'      TEST CLIENT IS FROZEN                        
         BZ    RUNREQ08                                                         
         MVI   CLTFRZTY,CLTFRZTA   SET ALL MONTHS ARE FROZEN                    
         TM    PCLTSTAT,X'10'      TEST MONTH SPECIFIED                         
         BZ    RUNREQ08                                                         
         LA    R3,PCLTELEM                                                      
         SR    R0,R0                                                            
         USING PCLTFEL,R3                                                       
RUNREQ06 CLI   PCLTFEL,0           TEST END OF RECORD                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   PCLTFEL,PCLTFELQ    TEST FREEZE STATUS ELEMENT                   
         BE    *+14                                                             
         IC    R0,PCLTFLEN         NO - BUMP TO NEXT ELEMENT                    
         AR    R3,R0                                                            
         B     RUNREQ06                                                         
                                                                                
         MVC   CLTFRZMO,PCLTFDTE                                                
         MVI   CLTFRZTY,CLTFRZTM                                                
         TM    PCLTFIND,X'02'      TEST MONTH FROZEN                            
         BNZ   RUNREQ08                                                         
         MVI   CLTFRZTY,CLTFRZTP                                                
         TM    PCLTFIND,X'04'      TEST MONTH AND PRIOR FROZEN                  
         BNZ   RUNREQ08                                                         
         MVI   CLTFRZTY,CLTFRZTS                                                
         TM    PCLTFIND,X'08'      TEST MONTH AND SUBSEQUENT FROZEN             
         BNZ   RUNREQ08                                                         
         DC    H'0'                                                             
         DROP  R3                                                               
                                                                                
RUNREQ08 ICM   RF,7,APRD                                                        
         BZ    RUNREQ14                                                         
         LA    R2,IOKEY                                                         
         USING PPRDRECD,R2                                                      
         XC    PPRDKEY,PPRDKEY                                                  
         MVC   PPRDKAGY,AGY                                                     
         ICM   RE,7,AMED                                                        
         MVC   PPRDKMED,LW_DATA1-LW_D(RE)                                       
         MVI   PPRDKRCD,X'06'                                                   
         ICM   RE,7,ACLT                                                        
         MVC   PPRDKCLT,LW_DATA1-LW_D(RE)                                       
         MVC   PPRDKPRD,LW_DATA1-LW_D(RF)                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO3'                            
         BE    RUNREQ10                                                         
         CLC   MAP#,=AL2(M#DLVER)                                               
         BE    RUNREQ14                                                         
         B     INVPRD                                                           
                                                                                
RUNREQ10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO3'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PRDSEND,YESQ                                                     
                                                                                
         ICM   RF,7,AEST                                                        
         BZ    RUNREQ14                                                         
         LA    R2,IOKEY                                                         
         USING PESTRECD,R2                                                      
         XC    PESTKEY,PESTKEY                                                  
         MVC   PESTKAGY,AGY                                                     
         ICM   RE,7,AMED                                                        
         MVC   PESTKMED,LW_DATA1-LW_D(RE)                                       
         MVI   PESTKRCD,X'07'                                                   
         ICM   RE,7,ACLT                                                        
         MVC   PESTKCLT,LW_DATA1-LW_D(RE)                                       
         ICM   RE,7,APRD                                                        
         MVC   PESTKPRD,LW_DATA1-LW_D(RE)                                       
         MVC   PESTKEST,LW_DATA1-LW_D(RF)                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO4'                            
         BE    RUNREQ12                                                         
         CLC   MAP#,=AL2(M#DLVER)                                               
         BE    RUNREQ14                                                         
         B     INVEST                                                           
                                                                                
RUNREQ12 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO4'                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
         TM    PESTTEST,X'80'      TEST 'TEST ESTIMATE'                         
         BZ    *+8                                                              
         MVI   ESTTEST,ESTTESTY                                                 
         TM    PESTTEST,X'20'      iDesk Esimate?                               
         JZ    *+8                                                              
         MVI   EST_TYPE,IDESKESQ                                                
         TM    PESTTEST,X'40'      TEST 'STEWARDSHIP ESTIMATE'                  
         BZ    *+8                                                              
         MVI   ESTTEST,ESTSTEWQ                                                 
         TM    PESTSTAT,X'03'      TEST 'LOCKED ESTIMATE'                       
         BZ    *+8                                                              
         MVI   ESTLOCK,ESTLOCKY                                                 
         OC    PESTCF,PESTCF       Have COS2 Factor?                            
         JZ    *+8                                                              
         MVI   COS2FLAG,COS2FACQ                                                
         BRAS  RE,SETEACTZ         SET ESTIMATE ACTUALIZE DATA                  
         MVI   ESTSEND,YESQ                                                     
                                                                                
RUNREQ14 OC    PUBCODE,PUBCODE                                                  
         BZ    RUNREQ18                                                         
         LA    R1,PUBCODE                                                       
         LA    R0,L'PUBCODE                                                     
         SR    RF,RF                                                            
         BASR  RE,0                                                             
         CLI   0(R1),C' '                                                       
         BNH   *+14                                                             
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
         LTR   R0,R0                                                            
         BZ    INVPUB                                                           
         GOTOR VPUBVAL,DMCB,((RF),PUBCODE),PUBKCODE                             
         CLI   0(R1),X'FF'                                                      
         BE    INVPUB                                                           
         LA    R2,IOKEY                                                         
         USING PUBRECD,R2                                                       
         XC    PUBKEY,PUBKEY                                                    
         ICM   RE,7,AMED                                                        
         MVC   PUBKMED,LW_DATA1-LW_D(RE)                                        
         MVC   PUBKPUB(L'PUBKPUB+L'PUBKZON+L'PUBKED),PUBKCODE                   
         MVC   PUBKAGY,AGY                                                      
         MVI   PUBKCOD,X'81'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPUBDIR+IO5'                            
         BE    RUNREQ16                                                         
         CLC   MAP#,=AL2(M#DLVER)                                               
         BE    RUNREQ18                                                         
         B     INVPUB                                                           
                                                                                
RUNREQ16 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPUBFIL+IO5'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PUBSEND,YESQ                                                     
                                                                                
         CLC   MAP#,=AL2(M#DLVER)                                               
         BNE   RUNREQ18                                                         
                                                                                
         L     R2,IOADDR                                                        
         ICM   R0,15,PUBPLSH       TEST PUBLISHER/REP GIVEN                     
         BZ    RUNREQ17                                                         
         LA    R2,IOKEY                                                         
         USING PREPRECD,R2                                                      
         XC    PREPKEY,PREPKEY                                                  
         MVC   PREPKAGY,AGY                                                     
         ICM   RE,7,AMED                                                        
         MVC   PREPKMED,LW_DATA1-LW_D(RE)                                       
         MVI   PREPKRCD,X'11'                                                   
         STCM  R0,15,PREPKREP                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO6'                            
         BNE   RUNREQ17                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO6'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   REPSEND,YESQ                                                     
                                                                                
RUNREQ17 BRAS  RE,SETPAYEE         SET PAYEE INFORMATION                        
                                                                                
RUNREQ18 CLC   MAP#,=AL2(M#DLAVD)                                               
         BNE   RUNREQ20                                                         
                                                                                
         OC    ADCODE,ADCODE                                                    
         BZ    INVADC                                                           
         LA    R2,IOKEY                                                         
         USING PJOBRECD,R2                                                      
         XC    PJOBKEY,PJOBKEY                                                  
         MVC   PJOBKAGY,AGY                                                     
         ICM   RE,7,AMED                                                        
         MVC   PJOBKMED,LW_DATA1-LW_D(RE)                                       
         MVI   PJOBKRCD,X'15'                                                   
         ICM   RE,7,ACLT                                                        
         BZ    INVCLT                                                           
         MVC   PJOBKCLT,LW_DATA1-LW_D(RE)                                       
         ICM   RE,7,APRD                                                        
         BZ    INVPRD                                                           
         MVC   PJOBKPRD,LW_DATA1-LW_D(RE)                                       
         MVC   PJOBKJOB,ADCODE                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO6'                            
         BNE   INVADC                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO6'                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,IOADDR                                                        
         MVC   ADCSTRDT,PJOBSTA                                                 
         MVC   ADCENDDT,PJOBEND                                                 
         MVC   ADCSPDSC,PJOBSPC                                                 
                                                                                
         OC    PUBKCODE,PUBKCODE   IF PUBLICATION CODE GIVEN                    
         BZ    RUNREQ20                                                         
         LA    R2,IOKEY            READ FOR INSTRUCTIONS RECORD                 
         MVC   PJOBKPUB,PUBKCODE                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO6'                            
         BE    RUNREQ20                                                         
         MVC   PJOBKPUB,EFFS       TRY FOR ALL PUB INSTRUCTIONS                 
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         BNE   INVNIR                                                           
                                                                                
RUNREQ20 L     R1,ALP              CALL DDLINK OUTPUT PROCESSOR                 
         GOTOR LP_APUTO                                                         
         J     EXITY               EXIT BACK TO DDLINK                          
                                                                                
INVMED   LHI   R0,13               INVALID OR MISSING MEDIA                     
         B     INVALL                                                           
INVCLT   LHI   R0,14               INVALID OR MISSING CLIENT                    
         B     INVALL                                                           
INVPRD   LHI   R0,15               INVALID OR MISSING PRODUCT                   
         B     INVALL                                                           
INVEST   LHI   R0,16               INVALID OR MISSING ESTIMATE                  
         B     INVALL                                                           
INVPUB   LHI   R0,18               INVALID OR MISSING PUBLICATION               
         B     INVALL                                                           
INVADC   LHI   R0,185              INVALID OR MISSING ADCODE                    
         B     INVALL                                                           
INVNIR   LHI   R0,194              MISSING INSTRUCTIONS                         
         B     INVALL                                                           
NOMATR   LHI   R0,544              NO MATCHING RECORD FOUND                     
         B     INVALL                                                           
                                                                                
INVALL   L     R1,ALP              SET ERROR AND EXIT BACK TO DDLINK            
         STCM  R0,3,LP_ERROR                                                    
         J     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET CLIENT RECORDS AND APPLY LIMIT ACCESS FILTERS                   *         
***********************************************************************         
                                                                                
GETCLT   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',CLTTAB),                 *        
               ('B#CLT',0),SAVED,('#LIMACC',ALIMACC)                            
         JNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         USING PCLTRECD,R2                                                      
         TM    PCLTSTAT,X'04'      COS2 $?                                      
         JZ    *+8                                                              
         MVI   COS2FLAG,COS2DOLQ                                                
         TM    PCLTSTAT,X'08'      COS2 Factor?                                 
         JZ    *+8                                                              
         MVI   COS2FLAG,COS2FACQ                                                
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* GET PRODUCT RECORDS FOR A CLIENT                                    *         
***********************************************************************         
                                                                                
GETPRD   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PRDTAB),                 *        
               ('B#PRD',SVCLTKEY),SAVED,0                                       
         MVI   PRDSEND,YESQ                                                     
         J     EXITY                                                            
                                                                                
***********************************************************************         
* GET ESTIMATE RECORDS FOR A PRODUCT AND APPLY DATE FILTERS           *         
***********************************************************************         
                                                                                
GETEST   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ESTTAB),                 *        
               ('B#EST',SVPRDKEY),SAVED,0                                       
         JNE   EXITY                                                            
                                                                                
         L     R2,IOADDR                                                        
         USING PESTRECD,R2         R2=A(ESTIMATE RECORD)                        
         CLC   PESTST,ENDDATE      TEST ESTIMATE OVERLAPS                       
         BH    GETEST              START AND END DATES                          
         CLC   PESTEND,STRDATE                                                  
         BL    GETEST                                                           
         XC    ESTVALS(ESTVALSL),ESTVALS                                        
         TM    PESTTEST,X'80'      TEST 'TEST ESTIMATE'                         
         BZ    *+8                                                              
         MVI   ESTTEST,ESTTESTY                                                 
         TM    PESTTEST,X'20'      iDesk Esimate?                               
         JZ    *+8                                                              
         MVI   EST_TYPE,IDESKESQ                                                
         TM    PESTTEST,X'40'      TEST 'STEWARDSHIP ESTIMATE'                  
         BZ    *+8                                                              
         MVI   ESTTEST,ESTSTEWQ                                                 
         TM    PESTSTAT,X'02'      TEST 'LOCKED ESTIMATE'                       
         BZ    *+8                                                              
         MVI   ESTLOCK,ESTLOCKY                                                 
         OC    PESTCF,PESTCF       Have COS2 Factor?                            
         JZ    *+8                                                              
         MVI   COS2FLAG,COS2FACQ                                                
         BRAS  RE,SETEACTZ         SET ESTIMATE ACTUALIZE DATA                  
         MVI   ESTSEND,YESQ                                                     
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PRCCPGRP BRAS  RE,GETCPGRP         PROCESS CLIENT/PRODUCT GROUP RECORD          
         JE    EXITY                                                            
         J     EXITN                                                            
                                                                                
PRCPOREC BRAS  RE,GET_PO#R         PROCESS PURCHASE ORDER RECORD                
         JE    EXITY                                                            
         J     EXITN                                                            
                                                                                
GET_REPR GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',REPTAB),                 +        
               ('B#REP',0),SAVED,0                                              
         JNE   EXITN                                                            
         MVI   REPSEND,YESQ                                                     
         J     EXITY                                                            
                                                                                
TSTTREP  CLI   REPIND,0            REP RECORD DOWNLOAD?                         
         JNE   *+8                                                              
         LTR   RE,RE               SET CC NOT EQUAL                             
         BR    RE                                                               
         CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
                                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
EXITN    LHI   RE,1                                                             
         J     EXITCC                                                           
EXITY    SR    RE,RE                                                            
EXITCC   LTR   RE,RE                                                            
EXIT     XIT1  ,                                                                
                                                                                
XCOLEN   XC    LP_OLEN,LP_OLEN     Clear output field length & exit             
         J     EXITY                                                            
                                                                                
SETOLENX STCM  R0,15,LP_OLEN       Set output field length & exit               
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
REPTAB   LKKEY H,PREPKEY,SAVED                                                  
         LKKEY SIN,PREPKAGY,AGY                                                 
         LKKEY WMP,PREPKMED,AMED                                                
         LKKEY LIT,PREPKRCD,PREPKRCQ                                            
         LKKEY WMP,PREPKREP,AREP                                                
         LKKEY E                                                                
                                                                                
COMKEYT  LKKEY H,PCOMKEY,SAVED                                                  
         LKKEY SIN,PCOMKAGY,AGY                                                 
         LKKEY SIN,PCOMKMED,QCOMKEY+0                                           
         LKKEY LIT,PCOMKRCD,PCOMKRCQ                                            
         LKKEY SIN,PCOMKNUM,QCOMKEY+1                                           
         LKKEY E                                                                
                                                                                
PUBKEYT  LKKEY H,PUBKEY,SAVED                                                   
         LKKEY SIN,PUBKMED,QMEDCOD                                              
         LKKEY ALL,PUBKPUB                                                      
         LKKEY ALL,PUBKZON                                                      
         LKKEY ALL,PUBKED                                                       
         LKKEY SIN,PUBKAGY,AGY                                                  
         LKKEY LIT,PUBKCOD,PUBKCODQ                                             
         LKKEY E                                                                
                                                                                
CLTTAB   DS    0X                  ** CLIENT KEY DRIVER TABLE **                
         DC    AL2(PCLTKCLT+L'PCLTKCLT-PCLTKEY)                                 
                                                                                
         DC    AL1(PCLTKAGY-PCLTKEY,L'PCLTKAGY-1)                               
         DC    AL2(AGY-SAVED)                                                   
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PCLTKMED-PCLTKEY,L'PCLTKMED-1)                               
         DC    AL2(AMED-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
         DC    AL1(PCLTKRCD-PCLTKEY,L'PCLTKRCD-1)                               
         DC    X'02',AL1(0)                                                     
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PCLTKCLT-PCLTKEY,L'PCLTKCLT-1)                               
         DC    AL2(ACLT-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
CLTTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
PRDTAB   DS    0X                  ** PRODUCT KEY DRIVER TABLE **               
         DC    AL2(PPRDKPRD+L'PPRDKPRD-PPRDKEY)                                 
                                                                                
         DC    AL1(PPRDKAGY-PPRDKEY,L'PPRDKAGY-1)                               
         DC    AL2(SVCLTKEY+(PCLTKAGY-PCLTKEY)-SAVED)                           
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PPRDKMED-PPRDKEY,L'PPRDKMED-1)                               
         DC    AL2(SVCLTKEY+(PCLTKMED-PCLTKEY)-SAVED)                           
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PPRDKRCD-PPRDKEY,L'PPRDKRCD-1)                               
         DC    X'06',AL1(0)                                                     
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PPRDKCLT-PPRDKEY,L'PPRDKCLT-1)                               
         DC    AL2(SVCLTKEY+(PCLTKCLT-PCLTKEY)-SAVED)                           
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PPRDKPRD-PPRDKEY,L'PPRDKPRD-1)                               
         DC    AL2(APRD-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
PRDTABX  DC    AL1(LK_EOTQ)                                                     
                                                                                
ESTTAB   DS    0X                  ** ESTIMATE KEY DRIVER TABLE **              
         DC    AL2(PESTKEST+L'PESTKEST-PESTKEY)                                 
                                                                                
         DC    AL1(PESTKAGY-PESTKEY,L'PESTKAGY-1)                               
         DC    AL2(SVPRDKEY+(PPRDKAGY-PPRDKEY)-SAVED)                           
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PESTKMED-PESTKEY,L'PESTKMED-1)                               
         DC    AL2(SVPRDKEY+(PPRDKMED-PPRDKEY)-SAVED)                           
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PESTKRCD-PESTKEY,L'PESTKRCD-1)                               
         DC    X'07',AL1(0)                                                     
         DC    AL1(LK_ILITQ)                                                    
                                                                                
         DC    AL1(PESTKCLT-PESTKEY,L'PESTKCLT-1)                               
         DC    AL2(SVPRDKEY+(PPRDKCLT-PPRDKEY)-SAVED)                           
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PESTKPRD-PESTKEY,L'PESTKPRD-1)                               
         DC    AL2(SVPRDKEY+(PPRDKPRD-PPRDKEY)-SAVED)                           
         DC    AL1(LQ_TSINQ)                                                    
                                                                                
         DC    AL1(PESTKEST-PESTKEY,L'PESTKEST-1)                               
         DC    AL2(AEST-SAVED)                                                  
         DC    AL1(LK_IWMPQ)                                                    
                                                                                
ESTTABX  DC    AL1(LK_EOTQ)                                                     
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR CLIENT/PRODUCT/ESTIMATE DOWNLOAD                   *         
***********************************************************************         
                                                                                
REQCPE   LKREQ H,M#DLCPE,OUTCLT                                                 
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,LIST=NOD,DEFAULT=Y,TEXT=PP#MEDCS,COL=*, *        
               MAXLEN=1                                                         
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,LIST=Y,DEFAULT=Y,TEXT=PP#CLTCS,COL=*,   *        
               MAXLEN=3                                                         
                                                                                
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,OLEN=L'PPRDKPRD,     *        
               LIST=Y,DEFAULT=Y,TEXT=PP#PRDCS,COL=*                             
                                                                                
EstNo    LKREQ F,D#ESTNUM,(I,B#SAVED,ESTIND),UBIN,OLEN=L'PESTKEST,     *        
               LIST=Y,DEFAULT=Y,RANGE=Y,TEXT=PP#ESTNS,COL=*                     
                                                                                
StrDt    LKREQ F,D#STRDAT,(D,B#SAVED,STRDATE),EDAT,TEXT=PP#STDAT,COL=*          
EndDt    LKREQ F,D#ENDDAT,(D,B#SAVED,ENDDATE),EDAT,TEXT=PP#ENDAT,COL=*          
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
                                                                                
***********************************************************************         
* Request maps for Accessible client codes download                   *         
***********************************************************************         
                                                                                
REQACL   LKREQ H,M#DLACL,OUTACL                                                 
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),CHAR,OLEN=L'PAGYKMED,     *        
               LIST=NOD,DEFAULT=Y,TEXT=PP#MEDCS,COL=*,MAXLEN=1                  
DLOpt    LKREQ F,004,(D,B#SAVED,QACCDOPT),CHAR,TEXT=PP#DATA,COL=*               
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Request maps for Buy Serial# Verifier                               *         
***********************************************************************         
                                                                                
REQOST   LKREQ *,M#DLOST,OUTOST                                                 
         EJECT                                                                  
                                                                                
***********************************************************************         
* Request maps for Buy Serial# Verifier                               *         
***********************************************************************         
                                                                                
REQBS#   LKREQ H,M#DLBS#,OUTBS#                                                 
BuyS#    LKREQ F,D#INSKEY,(I,B#SAVED,BS#IND),CHAR,TEXT=PP#KEY,         +        
               OLEN=L'BUYSERKY,LIST=F,SORT=N                                    
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,TEXT=PP#PUBCD,COL=*          
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Request maps for Buy Serial# Verifier                               *         
***********************************************************************         
                                                                                
REQBST   LKREQ H,M#BYSTA,OUTBST                                                 
BuyS#    LKREQ F,D#INSKEY,(I,B#SAVED,BS#IND),CHAR,TEXT=PP#KEY,         +        
               OLEN=L'BUYSERKY,LIST=F,SORT=N                                    
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR DOWNLOAD WIZARD                                    *         
***********************************************************************         
                                                                                
REQWIZ   LKREQ H,M#DLWIZ,OUTWIZ                                                 
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,TEXT=PP#MED,COL=*,MAXLEN=1                       
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,TEXT=PP#CLTC,COL=*,MAXLEN=3                      
                                                                                
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,OLEN=L'PPRDKPRD,     *        
               TEXT=PP#PRDC,COL=*                                               
                                                                                
EstNo    LKREQ F,D#ESTNUM,(I,B#SAVED,ESTIND),UBIN,OLEN=L'PESTKEST,     *        
               TEXT=PP#ESTNO,COL=*                                              
                                                                                
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,OLEN=L'PUBCODE,     *        
               TEXT=PP#PUBCD,COL=*                                              
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR RECORD VERIFIER                                    *         
***********************************************************************         
                                                                                
REQVER   LKREQ H,M#DLVER,OUTVER                                                 
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,TEXT=PP#MED,COL=*,MAXLEN=1                       
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,TEXT=PP#CLTC,COL=*,MAXLEN=3                      
                                                                                
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,OLEN=L'PPRDKPRD,     *        
               TEXT=PP#PRDC,COL=*                                               
                                                                                
EstNo    LKREQ F,D#ESTNUM,(I,B#SAVED,ESTIND),UBIN,OLEN=L'PESTKEST,     *        
               TEXT=PP#ESTNO,COL=*                                              
                                                                                
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,OLEN=L'PUBCODE,     *        
               TEXT=PP#PUBCD,COL=*                                              
                                                                                
RepCd    LKREQ F,D#SPREP,(I,B#SAVED,REPIND),CHAR,OLEN=L'PREPKREP,      *        
               LIST=NOD,TEXT=PP#REP,COL=*                                       
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR ADCODE VERIFY                                      *         
***********************************************************************         
                                                                                
REQAVD   LKREQ H,M#DLAVD,OUTAVD                                                 
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,TEXT=PP#MED,COL=*,MAXLEN=1                       
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,TEXT=PP#CLTC,COL=*,MAXLEN=3                      
                                                                                
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,OLEN=L'PPRDKPRD,     *        
               TEXT=PP#PRDC,COL=*                                               
                                                                                
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,OLEN=L'PUBCODE,     *        
               TEXT=PP#PUBCD,COL=*                                              
                                                                                
AdCod    LKREQ F,D#ADCODE,(D,B#SAVED,ADCODE),CHAR,OLEN=L'ADCODE,       *        
               TEXT=PP#ADCOD,COL=*                                              
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR CLT GRP AND PRD GRP VERIFIER                       *         
***********************************************************************         
                                                                                
REQCPG   LKREQ H,M#DLCPG,OUTCPG                                                 
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,LIST=NOD,DEFAULT=Y,TEXT=PP#MED,COL=*             
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
                                                                                
GrpID    LKREQ F,D#GRP_ID,(D,B#SAVED,GROUP_ID),CHAR,TEXT=PP#GRPID,     *        
               COL=*,MAXLEN=L'GROUP_ID                                          
                                                                                
GrpCd    LKREQ F,D#GRPCOD,(D,B#SAVED,GROUPCOD),CHAR,TEXT=PP#GRPCD,     *        
               COL=*,MAXLEN=L'GROUPCOD                                          
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR PURCHASE ORDER RECORD DOWNLOAD                     *         
***********************************************************************         
                                                                                
REQPO#   LKREQ H,M#DLPO#,OUTPO#                                                 
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,DEFAULT=Y,TEXT=PP#MED,COL=*                      
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
                                                                                
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,                     *        
               OLEN=L'PPRDKPRD,LIST=NOD,DEFAULT=Y,TEXT=PP#PRDCS,COL=*           
                                                                                
EstNo    LKREQ F,D#ESTNUM,(I,B#SAVED,ESTIND),UBIN,OLEN=L'PESTKEST,     *        
               LIST=NOD,DEFAULT=Y,RANGE=Y,TEXT=PP#ESTNS,COL=*                   
                                                                                
POSq#    LKREQ F,D#PO#SQ#,(D,B#SAVED,FLTPOSQ#),UBIN,TEXT=PP#PONUM,COL=*         
                                                                                
PONum    LKREQ F,D#PO#NUM,(D,B#SAVED,FLTPONUM),CHAR,TEXT=PP#PONUM,COL=*         
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR AD CODE RECORD DOWNLOAD                            *         
***********************************************************************         
                                                                                
REQADC   LKREQ H,M#ADF_DL,OUTADC                                                
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,TEXT=PP#MED,COL=*,MAXLEN=1                       
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,TEXT=PP#CLTC,COL=*,MAXLEN=3                      
                                                                                
PrdCd    LKREQ F,D#PRDCOD,(I,B#SAVED,PRDIND),CHAR,OLEN=L'PPRDKPRD,     *        
               LIST=NOD,DEFAULT=Y,TEXT=PP#PRDCS,COL=*                           
                                                                                
AdCod    LKREQ F,D#ADCODE,(D,B#SAVED,ADCODE),CHAR,OLEN=L'ADCODE,       *        
               TEXT=PP#ADCOD,COL=*                                              
                                                                                
Ad_ID    LKREQ F,D#AD_ID,(D,B#SAVED,AD_ID),CHAR,OLEN=L'AD_ID,          *        
               TEXT=PP#AD_ID,COL=*                                              
                                                                                
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,OLEN=L'PUBCODE,     *        
               TEXT=PP#PUBCD,COL=*                                              
                                                                                
RecTy    LKREQ F,D#RECTYP,(D,B#SAVED,ADCRTYP),UBIN,                    *        
               TEXT=PP#RCDTY,COL=*                                              
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR FOREIGN EXCHANGE RATE DOWNLOAD                     *         
***********************************************************************         
                                                                                
REQFXR   LKREQ H,M#FXR_DL,OUTFXR                                                
                                                                                
CurFr    LKREQ F,D#CURFRO,(D,B#SAVED,FXCURFRO),CHAR,TEXT=PP#FROM,COL=*          
CurTo    LKREQ F,D#CUR_TO,(D,B#SAVED,FXCUR_TO),CHAR,TEXT=PP#__TO,COL=*          
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),CHAR,                     *        
               OLEN=L'PCLTKCLT,LIST=NOD,DEFAULT=Y,TEXT=PP#CLTCS,COL=*           
                                                                                
EndDt    LKREQ F,D#ENDDAT,(D,B#SAVED,BINENDDT),BDAT,TEXT=PP#ENDAT,COL=*         
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST MAPS FOR AD CODE RECORD DOWNLOAD                            *         
***********************************************************************         
                                                                                
REQCON   LKREQ H,M#CON_DL,OUTCON                                                
                                                                                
MedCd    LKREQ F,D#MEDCOD,(I,B#SAVED,MEDIND),(U,#VALMED,$VALMED),      *        
               OLEN=L'PAGYKMED,TEXT=PP#MED,COL=*,MAXLEN=1                       
                                                                                
CltCd    LKREQ F,D#CLTCOD,(I,B#SAVED,CLTIND),(U,#VALCLT,$VALCLT),      *        
               OLEN=L'PCLTKCLT,TEXT=PP#CLTC,COL=*,MAXLEN=3                      
                                                                                
PubCd    LKREQ F,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,OLEN=L'PUBCODE,     *        
               TEXT=PP#PUBCD,COL=*                                              
                                                                                
ConNo    LKREQ F,D#CONNUM,(D,B#SAVED,CONTNUM),UBIN,OLEN=L'PCONNUM,     *        
               TEXT=PP#CNTNO,COL=*                                              
                                                                                
StrDt    LKREQ F,D#CONSDT,(D,B#SAVED,BINSTRDT),BDAT,TEXT=PP#STDAT,COL=*         
EndDt    LKREQ F,D#CONEDT,(D,B#SAVED,BINENDDT),BDAT,TEXT=PP#ENDAT,COL=*         
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Request maps for Comment record download                            *         
***********************************************************************         
                                                                                
REQCOM   LKREQ H,M#DLCOM,OUTCOM                                                 
Key      LKREQ F,001,(D,B#SAVED,QCOMKEY),CHAR,TEXT=PP#KEY,COL=*                 
Optn1    LKREQ F,002,(D,B#SAVED,QCOMOP1),UBIN,TEXT=PP#OPTNS,COL=*               
         LKREQ E                                                                
                                                                                
***********************************************************************         
* Request maps for Publication record download                        *         
***********************************************************************         
                                                                                
REQPUB   LKREQ H,X'0350',OUTPUB                                                 
Media    LKREQ F,003,(D,B#SAVED,QMEDCOD),CHAR,TEXT=PP#MED,COL=*                 
         LKREQ E                                                                
                                                                                
***********************************************************************         
* End of Request                                                      *         
***********************************************************************         
         LKREQ X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR CLIENT/PRODUCT/ESTIMATE DOWNLOAD                    *         
***********************************************************************         
                                                                                
OUTCLT   LKOUT H                                                                
                                                                                
CLTC     LKOUT R,E#CLT                                                          
Array    LKOUT C,E#CLT,(A,ARYCLT)                                               
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* Output maps for Accessible client codes download                    *         
***********************************************************************         
                                                                                
OUTACL   LKOUT H                                                                
ACLC     LKOUT R,E#CLT                                                          
Array    LKOUT C,E#CLT,(A,ARYACL)                                               
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* Array definition for Accessible client codes download               *         
***********************************************************************         
                                                                                
ARYACL   LKOUT A,(R,GETCLT),MULTIROW=Y                                          
Array    LKOUT C,E#CLT,(A,ARYACLV)                                              
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Output maps for OS Tool Initial download                            *         
***********************************************************************         
                                                                                
OUTOST   LKOUT H                                                                
OSTRPY   LKOUT R,E#OST                                                          
AgyAlp   LKOUT C,001,(D,B#SAVED,AGY),CHAR                                       
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
***********************************************************************         
* Output maps for Buy Serial# Verifier                                *         
***********************************************************************         
                                                                                
OUTBS#   LKOUT H                                                                
BSE#     LKOUT R,E#BS#                                                          
Array    LKOUT C,E#BS#,(A,ARYBS#)                                               
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYBS#   LKOUT A,(R,NXTBS#),MULTIROW=Y,ROWNAME=SAVED                            
BuyS#    LKOUT C,D#INSKEY,(D,B#SAVED,BUYSERKY),CHAR                             
ErrNm    LKOUT C,D#ERRNUM,(D,B#SAVED,ERRMCNUM),UBIN,ND=Y                        
ErrDs    LKOUT C,D#ERRDSC,(D,B#SAVED,ERRORMSG),CHAR,ND=Y                        
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Output maps for Buy Status download                                 *         
***********************************************************************         
                                                                                
OUTBST   LKOUT H                                                                
BSTA     LKOUT R,E#BST                                                          
Array    LKOUT C,E#BST,(A,ARYBST)                                               
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYBST   LKOUT A,(R,NXTBST),MULTIROW=Y,ROWNAME=SAVED                            
BuyS#    LKOUT C,001,(D,B#SAVED,BUYSERKY),CHAR                                  
PdSta    LKOUT C,020,(D,B#SAVED,BUYPAYSW),CHAR                                  
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CLIENT DOWNLOAD                                *         
***********************************************************************         
                                                                                
ARYCLT   LKOUT A,(R,GETCLT),MULTIROW=Y                                          
Array    LKOUT C,E#CLT,(A,ARYCLTV)                                              
Array    LKOUT C,E#PRD,(A,ARYPRD)                                               
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PRODUCT DOWNLOAD                               *         
***********************************************************************         
                                                                                
ARYPRD   LKOUT A,(R,GETPRD),MULTIROW=Y                                          
Array    LKOUT C,E#PRD,(A,ARYPRDV)                                              
Array    LKOUT C,E#EST,(A,ARYEST)                                               
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ESTIMATE DOWNLOAD                              *         
***********************************************************************         
                                                                                
ARYEST   LKOUT A,(R,GETEST),MULTIROW=Y                                          
Array    LKOUT C,E#EST,(A,ARYESTV)                                              
         LKOUT E                                                                
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR DOWNLOAD WIZARD VERIFIER                            *         
***********************************************************************         
                                                                                
OUTWIZ   LKOUT H                                                                
                                                                                
WCLTC    LKOUT R,E#CLT                                                          
Array    LKOUT C,E#CLT,(A,ARYCLTV)                                              
         LKOUT E                                                                
                                                                                
WPRDC    LKOUT R,E#PRD                                                          
Array    LKOUT C,E#PRD,(A,ARYPRDV)                                              
         LKOUT E                                                                
                                                                                
WESTC    LKOUT R,E#EST                                                          
Array    LKOUT C,E#EST,(A,ARYESTV)                                              
         LKOUT E                                                                
                                                                                
WPUBC    LKOUT R,E#PUB                                                          
Array    LKOUT C,E#PUB,(A,ARYPUBV)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR DOWNLOAD RECORD VERIFIER                            *         
***********************************************************************         
                                                                                
OUTVER   LKOUT H                                                                
                                                                                
RCLTC    LKOUT R,E#CLT                                                          
Array    LKOUT C,E#CLT,(A,ARYCLTV)                                              
         LKOUT E                                                                
                                                                                
RPRDC    LKOUT R,E#PRD                                                          
Array    LKOUT C,E#PRD,(A,ARYPRDV)                                              
         LKOUT E                                                                
                                                                                
RESTC    LKOUT R,E#EST                                                          
Array    LKOUT C,E#EST,(A,ARYESTV)                                              
         LKOUT E                                                                
                                                                                
RPUBC    LKOUT R,E#PUB                                                          
Array    LKOUT C,E#PUB,(A,ARYPUBV)                                              
         LKOUT E                                                                
                                                                                
RPAYEE   LKOUT R,E#PAYADR                                                       
Array    LKOUT C,E#PAYADR,(A,ARYPAYEV),PCVERSION=3.3.0.6                        
         LKOUT E                                                                
                                                                                
REPINF   LKOUT R,E#SPREP                                                        
Array    LKOUT C,E#SPREP,(A,ARYREPR),FILTROUT=TSTTREP,                 +        
               PCVERSION=3.5.0.6                                                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR ADCODE VERIFY                                       *         
***********************************************************************         
                                                                                
OUTAVD   LKOUT H                                                                
                                                                                
ADCOD    LKOUT R,E#ADC                                                          
Array    LKOUT C,E#ADC,(A,ARYADCV)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR CLIENT GROUP AND PRODUCT GROUP VERIFIER             *         
***********************************************************************         
                                                                                
OUTCPG   LKOUT H                                                                
                                                                                
CPGRP    LKOUT R,E#CPGRPY                                                       
Array    LKOUT C,E#CPGRPY,(A,ARYCPG)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR PURCHASE ORDER RECORD DOWNLOAD                      *         
***********************************************************************         
                                                                                
OUTPO#   LKOUT H                                                                
                                                                                
POREC    LKOUT R,E#PO#RPY                                                       
Array    LKOUT C,E#PO#RPY,(A,ARYPO#)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR AD CODE RECORD DOWNLOAD                             *         
***********************************************************************         
                                                                                
OUTADC   LKOUT H                                                                
                                                                                
ADCD2    LKOUT R,E#ADF_DL                                                       
Array    LKOUT C,E#ADF_DL,(A,ARYADCR),PCVERSION=3.5.0.41                        
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR FOREIGN EXCHANGE DOWNLOAD                           *         
***********************************************************************         
                                                                                
OUTFXR   LKOUT H                                                                
                                                                                
FXRAT    LKOUT R,E#FXR_DL                                                       
Array    LKOUT C,E#FXR_DL,(A,ARYFXRT)                                           
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* OUTPUT MAPS FOR CONTRACT RECORD DOWNLOAD                            *         
***********************************************************************         
                                                                                
OUTCON   LKOUT H                                                                
                                                                                
CONTR    LKOUT R,E#CON_DL                                                       
Array    LKOUT C,E#CON_DL,(A,ARYCONR),PCVERSION=3.6.0.7                         
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Output maps for comment record download                             *         
***********************************************************************         
                                                                                
OUTCOM   LKOUT H                                                                
                                                                                
COMVR    LKOUT R,E#COMVAR                                                       
Array    LKOUT C,D#COMMNT,(A,ARYCOMR),FILTROUT=TSTCMVAR                         
         LKOUT E                                                                
                                                                                
COMSD    LKOUT R,E#STDCOM                                                       
Array    LKOUT C,D#COMMNT,(A,ARYCOMR),FILTROUT=TSTCMSTD                         
         LKOUT E                                                                
                                                                                
COMRQ    LKOUT R,E#REQDCM                                                       
Array    LKOUT C,D#COMMNT,(A,ARYCOMR),FILTROUT=TSTCMREQ                         
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Output maps for Publication record download                         *         
***********************************************************************         
                                                                                
OUTPUB   LKOUT H                                                                
                                                                                
PUBRC    LKOUT R,X'0351'                                                        
Array    LKOUT C,X'0351',(A,ARYPUBR)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CLIENT DOWNLOAD COLUMNS                        *         
***********************************************************************         
                                                                                
ARYCLTV  LKOUT A,(D,B#SAVED,SAVED),NEWEL=Y,NROWS=1                              
MedCd    LKOUT C,D#MEDCOD,(D,B#CLT,PCLTKMED),CHAR,ND=Y                          
CltCd    LKOUT C,D#CLTCOD,(D,B#CLT,PCLTKCLT),CHAR,ND=Y                          
CltNm    LKOUT C,D#CLTNAM,(D,B#CLT,PCLTNAME),CHAR,ND=Y                          
CltOC    LKOUT C,015,(D,B#CLT,PCLTOFF),(R,EDTCOF),ND=Y,                +        
               PCVERSION=4.0.1.6                                                
CltAO    LKOUT C,017,(D,B#CLT,PCLTAOFC),CHAR,ND=Y,PCVERSION=4.0.1.6             
CltFt    LKOUT C,D#CLTFZT,(D,B#SAVED,CLTFRZTY),CHAR,ND=Y                        
CltFm    LKOUT C,D#CLTFZM,(D,B#SAVED,CLTFRZMO),BMON,ND=Y                        
         LKOUT E                                                                
                                                                                
EDTCOF   LM    R2,R4,LP_AINP                                                    
         CLC   0(L'PCLTOFF,R2),SPACES                                           
         JNH   XCOLEN                                                           
         XC    OFCBLK,OFCBLK       Init officer block                           
         MVI   OFCSYS,PRTLETQ      Print system                                 
         MVC   OFCAGY,AGY                                                       
         MVC   OFCOFC,0(R2)                                                     
         GOTOR VOFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                        
         TM    OFCINDS,OFCINOLA    Using two chars office codes?                
         JNZ   EDTCOF6                                                          
         MVC   0(L'OFCOFC2,R4),OFCOFC2                                          
         LHI   R0,L'OFCOFC2                                                     
         J     SETOLENX                                                         
EDTCOF6  MVC   0(L'PCLTOFF,R4),0(R2)                                            
         LHI   R0,L'PCLTOFF                                                     
         J     SETOLENX                                                         
                                                                                
***********************************************************************         
* Array definition for Accessible Client codes download               *         
***********************************************************************         
                                                                                
ARYACLV  LKOUT A,(D,B#SAVED,SAVED),NEWEL=Y,NROWS=1                              
MedCd    LKOUT C,D#MEDCOD,(D,B#CLT,PCLTKMED),CHAR,ND=Y                          
CltCd    LKOUT C,D#CLTCOD,(D,B#CLT,PCLTKCLT),CHAR,ND=Y                          
CltNm    LKOUT C,D#CLTNAM,(D,B#CLT,PCLTNAME),CHAR,ND=Y,FILTROUT=TSTACOP         
         LKOUT E                                                                
                                                                                
TSTACOP  CLI   QACCDOPT,YESQ                                                    
         BR    RE                                                               
                                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PRODUCT DOWNLOAD COLUMNS                       *         
***********************************************************************         
                                                                                
ARYPRDV  LKOUT A,(D,B#SAVED,PRDSEND),NEWEL=Y,NROWS=1,                  *        
               ROWID=(PRDSEND,YESQ)                                             
PrdCd    LKOUT C,D#PRDCOD,(D,B#PRD,PPRDKPRD),CHAR,ND=Y                          
PrdNm    LKOUT C,D#PRDNAM,(D,B#PRD,PPRDNAME),CHAR,ND=Y                          
Array    LKOUT C,017,(A,ARYPAOC),PCVERSION=4.0.1.6                              
         LKOUT E                                                                
                                                                                
ARYPAOC  LKOUT A,(D,B#PRD,PPRDELEM),EOT=EOR,ROWID=(PPRDAOEL,X'35'),    +        
               ROWWIDTH=(V,PPRDAOEL+1)                                          
PPrAOfC  LKOUT C,017,PPRDAOFC,CHAR,ND=Y                                         
         LKOUT E                                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ESTIMATE DOWNLOAD COLUMNS                      *         
***********************************************************************         
                                                                                
ARYESTV  LKOUT A,(D,B#SAVED,ESTSEND),NEWEL=Y,NROWS=1,                  *        
               ROWID=(ESTSEND,YESQ)                                             
EstNo    LKOUT C,D#ESTNUM,(D,B#EST,PESTKEST),UBIN,ND=Y                          
EstNm    LKOUT C,D#ESTNAM,(D,B#EST,PESTNAME),CHAR,ND=Y                          
EstDs    LKOUT C,D#ESTDSC,(D,B#EST,PESTNAM2),CHAR,ND=Y                          
StrDt    LKOUT C,D#ESTSTD,(D,B#EST,PESTST),EDAT,ND=Y                            
EndDt    LKOUT C,D#ESTEND,(D,B#EST,PESTEND),EDAT,ND=Y                           
EStat    LKOUT C,D#ESTSTA,(D,B#SAVED,ESTTEST),CHAR,ND=Y                         
ELock    LKOUT C,D#ESTLOK,(D,B#SAVED,ESTLOCK),CHAR,ND=Y                         
AcTDt    LKOUT C,D#ACTZDT,(D,B#SAVED,EACTTHDT),BDAT,ND=Y                        
LCpid    LKOUT C,D#LCHPID,(D,B#SAVED,EACLCPID),CHAR,ND=Y                        
LCdat    LKOUT C,D#LCHDAT,(D,B#SAVED,EACLCDAT),BDAT,ND=Y                        
RateT    LKOUT C,D#RATTYP,(D,B#EST,PESTRTYP),CHAR,ND=Y,                +        
               PCVERSION=3.6.0.13                                               
COS2T    LKOUT C,D#COS2TY,(D,B#SAVED,COS2FLAG),CHAR,ND=Y,              +        
               PCVERSION=4.0.0.4                                                
IDKES    LKOUT C,D#IDKEST,(D,B#SAVED,EST_TYPE),CHAR,ND=Y,              +        
               PCVERSION=4.0.0.9                                                
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PUBLICATION DOWNLOAD COLUMNS                   *         
***********************************************************************         
                                                                                
ARYPUBV  LKOUT A,(D,B#SAVED,PUBSEND),NEWEL=Y,NROWS=1,                  *        
               ROWID=(PUBSEND,YESQ)                                             
MedCd    LKOUT C,D#MEDCOD,(D,B#PUB,PUBKMED),CHAR,ND=Y                           
PubCd    LKOUT C,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,ND=Y                         
PubNm    LKOUT C,D#PUBNAM,(D,B#PUB,PUBNAME),CHAR,ND=Y                           
PZnam    LKOUT C,D#PZNAME,(D,B#PUB,PUBZNAME),CHAR,ND=Y                          
Pcity    LKOUT C,D#PCITY,(D,B#PUB,PUBCITY),CHAR,ND=Y                            
Pstat    LKOUT C,D#STATE,(D,B#PUB,PUBSTATE),CHAR,ND=Y                           
PNStC    LKOUT C,D#PNSTCD,(D,B#PUB,PUBSTACD),CHAR,ND=Y,                *        
               PCVERSION=3.5.0.41                                               
Array    LKOUT C,E#REP,(A,ARYREPV)                                              
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR REP RECORD DOWNLOAD                            *         
***********************************************************************         
                                                                                
ARYREPR  LKOUT A,(R,GET_REPR),MULTIROW=Y                                        
         LKOUT C,E#SPREP,(A,ARYREP2)                                            
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AD CODE RECORD DOWNLOAD                        *         
***********************************************************************         
                                                                                
ARYADCR  LKOUT A,(R,GET_ADCR),MULTIROW=Y,ROWNAME=ADC_OUTD                       
MedCd    LKOUT C,D#MEDCOD,(D,,ADC_MEDC),CHAR,ND=Y                               
CltCd    LKOUT C,D#CLTCOD,(D,,ADC_CLTC),CHAR,ND=Y                               
PrdCd    LKOUT C,D#PRDCOD,(D,,ADC_PRDC),CHAR,ND=Y                               
AdCod    LKOUT C,D#ADCODE,(D,,ADC_ADCD),CHAR,ND=Y                               
Ad_ID    LKOUT C,D#AD_ID,(D,,ADC_ADID),CHAR,ND=Y                                
PubCd    LKOUT C,D#PUBCOD,(D,,ADC_PUBC),CHAR,ND=Y                               
ACap1    LKOUT C,D#ADCAP,(D,,ADC_CAP1),CHAR,ND=Y                                
ACap2    LKOUT C,D#ADCAP2,(D,,ADC_CAP2),CHAR,ND=Y                               
CpyNo    LKOUT C,D#CPYNUM,(D,,ADC_CPY#),CHAR,ND=Y                               
SpDsc    LKOUT C,D#SPCDSC,(D,,ADC_SPDS),CHAR,ND=Y                               
NumLn    LKOUT C,D#NUMLIN,(D,,ADC_#LIN),CHAR,ND=Y                               
NumCl    LKOUT C,D#NUMCOL,(D,,ADC_#COL),CHAR,ND=Y                               
LnXCl    LKOUT C,D#LNXCOL,(D,,ADC_LNXC),CHAR,ND=Y                               
Premn    LKOUT C,D#PREMUM,(D,,ADC_PREM),CHAR,ND=Y                               
SDate    LKOUT C,D#STRDAT,(D,,ADC_STRD),BDAT,ND=Y                               
EDate    LKOUT C,D#ENDDAT,(D,,ADC_ENDD),BDAT,ND=Y                               
Phous    LKOUT C,D#PRDUCD,(D,,ADC_PHOU),CHAR,ND=Y                               
AgySi    LKOUT C,D#AGYSIG,(D,,ADC_AGYS),CHAR,ND=Y                               
PrJob    LKOUT C,D#PRDJOB,(D,,ADC_PJOB),CHAR,ND=Y                               
Alloc    LKOUT C,D#ALLOCS,(D,,ADC_ALLO),CHAR,ND=Y                               
BiCon    LKOUT C,D#BILCON,(D,,ADC_BCON),CHAR,ND=Y                               
Filtr    LKOUT C,D#ADFILT,(D,,ADC_FILT),CHAR,ND=Y                               
FSIyn    LKOUT C,D#FSI_YN,(D,,ADC_FSI_),CHAR,ND=Y                               
BiRep    LKOUT C,D#BILREP,(D,,ADC_BREP),CHAR,ND=Y                               
PubLs    LKOUT C,D#PUBLST,(D,,ADC_PUBL),CHAR,ND=Y                               
AdSta    LKOUT C,D#ADCSTA,(D,,ADC_STAT),LBIN,ND=Y                               
                                                                                
InsC1    LKOUT C,D#COMMNT,(D,,ADC_COM1),CHAR,FILTROUT=TSTTCOM                   
InsC2    LKOUT C,D#COMMNT,(D,,ADC_COM2),CHAR,FILTROUT=TSTTCOM                   
InsC3    LKOUT C,D#COMMNT,(D,,ADC_COM3),CHAR,FILTROUT=TSTTCOM                   
InsC4    LKOUT C,D#COMMNT,(D,,ADC_COM4),CHAR,FILTROUT=TSTTCOM                   
InsC5    LKOUT C,D#COMMNT,(D,,ADC_COM5),CHAR,FILTROUT=TSTTCOM                   
InsC6    LKOUT C,D#COMMNT,(D,,ADC_COM6),CHAR,FILTROUT=TSTTCOM                   
InsC7    LKOUT C,D#COMMNT,(D,,ADC_COM7),CHAR,FILTROUT=TSTTCOM                   
InsC8    LKOUT C,D#COMMNT,(D,,ADC_COM8),CHAR,FILTROUT=TSTTCOM                   
InsC9    LKOUT C,D#COMMNT,(D,,ADC_COM9),CHAR,FILTROUT=TSTTCOM                   
InsCA    LKOUT C,D#COMMNT,(D,,ADC_COMA),CHAR,FILTROUT=TSTTCOM                   
InsCB    LKOUT C,D#COMMNT,(D,,ADC_COMB),CHAR,FILTROUT=TSTTCOM                   
InsCC    LKOUT C,D#COMMNT,(D,,ADC_COMC),CHAR,FILTROUT=TSTTCOM                   
InsCD    LKOUT C,D#COMMNT,(D,,ADC_COMD),CHAR,FILTROUT=TSTTCOM                   
InsCE    LKOUT C,D#COMMNT,(D,,ADC_COME),CHAR,FILTROUT=TSTTCOM                   
InsCF    LKOUT C,D#COMMNT,(D,,ADC_COMF),CHAR,FILTROUT=TSTTCOM                   
InsCG    LKOUT C,D#COMMNT,(D,,ADC_COMG),CHAR,FILTROUT=TSTTCOM                   
                                                                                
SubC1    LKOUT C,D#SUBADC,(D,,ADC_ADC1),CHAR,ND=Y                               
SubC1    LKOUT C,D#SUBADI,(D,,ADC_AID1),CHAR,ND=Y                               
SubC1    LKOUT C,D#PCTSHR,(D,,ADC_SHR1),CHAR,ND=Y                               
SubC2    LKOUT C,D#SUBADC,(D,,ADC_ADC2),CHAR,ND=Y                               
SubC2    LKOUT C,D#SUBADI,(D,,ADC_AID2),CHAR,ND=Y                               
SubC2    LKOUT C,D#PCTSHR,(D,,ADC_SHR2),CHAR,ND=Y                               
SubC3    LKOUT C,D#SUBADC,(D,,ADC_ADC3),CHAR,ND=Y                               
SubC3    LKOUT C,D#SUBADI,(D,,ADC_AID3),CHAR,ND=Y                               
SubC3    LKOUT C,D#PCTSHR,(D,,ADC_SHR3),CHAR,ND=Y                               
SubC4    LKOUT C,D#SUBADC,(D,,ADC_ADC4),CHAR,ND=Y                               
SubC4    LKOUT C,D#SUBADI,(D,,ADC_AID4),CHAR,ND=Y                               
SubC4    LKOUT C,D#PCTSHR,(D,,ADC_SHR4),CHAR,ND=Y                               
SubC5    LKOUT C,D#SUBADC,(D,,ADC_ADC5),CHAR,ND=Y                               
SubC5    LKOUT C,D#SUBADI,(D,,ADC_AID5),CHAR,ND=Y                               
SubC5    LKOUT C,D#PCTSHR,(D,,ADC_SHR5),CHAR,ND=Y                               
SubC6    LKOUT C,D#SUBADC,(D,,ADC_ADC6),CHAR,ND=Y                               
SubC6    LKOUT C,D#SUBADI,(D,,ADC_AID6),CHAR,ND=Y                               
SubC6    LKOUT C,D#PCTSHR,(D,,ADC_SHR6),CHAR,ND=Y                               
SubC7    LKOUT C,D#SUBADC,(D,,ADC_ADC7),CHAR,ND=Y                               
SubC7    LKOUT C,D#SUBADI,(D,,ADC_AID7),CHAR,ND=Y                               
SubC7    LKOUT C,D#PCTSHR,(D,,ADC_SHR7),CHAR,ND=Y                               
SubC8    LKOUT C,D#SUBADC,(D,,ADC_ADC8),CHAR,ND=Y                               
SubC8    LKOUT C,D#SUBADI,(D,,ADC_AID8),CHAR,ND=Y                               
SubC8    LKOUT C,D#PCTSHR,(D,,ADC_SHR8),CHAR,ND=Y                               
SubC9    LKOUT C,D#SUBADC,(D,,ADC_ADC9),CHAR,ND=Y                               
SubC9    LKOUT C,D#SUBADI,(D,,ADC_AID9),CHAR,ND=Y                               
SubC9    LKOUT C,D#PCTSHR,(D,,ADC_SHR9),CHAR,ND=Y                               
SubCA    LKOUT C,D#SUBADC,(D,,ADC_ADCA),CHAR,ND=Y                               
SubCA    LKOUT C,D#SUBADI,(D,,ADC_AIDA),CHAR,ND=Y                               
SubCA    LKOUT C,D#PCTSHR,(D,,ADC_SHRA),CHAR,ND=Y                               
                                                                                
         LKOUT E                                                                
                                                                                
TSTTCOM  CLI   DL_FLTR1,ADCINSRQ   DOWNLOADING INSTRUCTION RECORD?              
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CONTRACT RECORD DOWNLOAD                       *         
***********************************************************************         
                                                                                
ARYCONR  LKOUT A,(R,GET_CONR),MULTIROW=Y,ROWNAME=CON_OUTD                       
MedCd    LKOUT C,D#MEDCOD,(D,,CON_MEDC),CHAR,ND=Y                               
CltCd    LKOUT C,D#CLTCOD,(D,,CON_CLTC),CHAR,ND=Y                               
PubCd    LKOUT C,D#PUBCOD,(D,,CON_PUBC),CHAR,ND=Y                               
ConNo    LKOUT C,D#CONNUM,(D,,CON_CON#),UBIN,ND=Y                               
SDate    LKOUT C,D#CONSDT,(D,,CON_STRD),BDAT,ND=Y                               
EDate    LKOUT C,D#CONEDT,(D,,CON_ENDD),BDAT,ND=Y                               
PrdCd    LKOUT C,D#PRDCOD,(D,,CON_PRDC),CHAR,ND=Y                               
         LKOUT E                                                                
                                                                                
***********************************************************************         
* Array definition for Comment record download                        *         
***********************************************************************         
                                                                                
ARYCOMR  LKOUT A,(R,NXTCOMM),MULTIROW=Y,ROWNAME=PCOMRECD                        
Array    LKOUT C,D#COMMNT,(A,ARYCOML)                                           
         LKOUT E                                                                
                                                                                
TSTCMVAR CLI   QCOMOP1,QCOMVARQ                                                 
         BR    RE                                                               
TSTCMSTD CLI   QCOMOP1,QCOMSTDQ                                                 
         BR    RE                                                               
TSTCMREQ CLI   QCOMOP1,QCOMREQQ                                                 
         BR    RE                                                               
                                                                                
ARYCOML  LKOUT A,(D,B#COM,PCOMCELM),EOT=EOR,ROWID=(PCOMCELM,X'40'),    +        
               ROWWIDTH=(V,PCOMCELM+1)                                          
         LKOUT C,D#COMMNT,PCOMCELM,(R,EDTCOM)                                   
         LKOUT E                                                                
                                                                                
         USING PCOMCELM,R2                                                      
EDTCOM   LM    R2,R4,LP_AINP                                                    
         CLI   PCOMDT,C'+'         Special skipping lines notation?             
         JNE   EDTCOM12                                                         
         CLI   PCOMCELM+1,4        Special +n notation?                         
         JH    EDTCOM12                                                         
         CLI   PCOMDT+1,C'1'       Skipping lines?                              
         JNL   XCOLEN                                                           
EDTCOM12 LLC   RF,PCOMCELM+1                                                    
         AHI   RF,-2               Length of comment                            
         LR    R0,RF               Set output length                            
         AHI   RF,-1                                                            
         JNM   *+6                                                              
         DC    H'0'                Zero length entry                            
         BASR  RB,0                                                             
         MVC   0(0,R4),PCOMDT      Move text to output string                   
         EX    RF,0(RB)                                                         
         J     SETOLENX                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Read comment records                                                *         
***********************************************************************         
                                                                                
NXTCOMM  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',COMKEYT),                +        
               ('B#COM',0),SAVED,0,0                                            
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* Array definition for Publication record download                    *         
***********************************************************************         
                                                                                
ARYPUBR  LKOUT A,(R,NXTPUBR),MULTIROW=Y,ROWNAME=PUBRECD                         
PubMed   LKOUT C,003,PUBKMED,CHAR,ND=Y                                          
PubCod   LKOUT C,029,PUBKEY,(R,EDTPBC)                                          
Array    LKOUT C,X'0351',(A,ARYPUBN)                                            
         LKOUT E                                                                
                                                                                
ARYPUBN  LKOUT A,(D,B#PUB,PUBNAMEL),EOT=EOR,ROWID=(PUBNAMEL,X'10'),    +        
               ROWWIDTH=(V,PUBNAMEL+1)                                          
         LKOUT C,031,PUBNAME,CHAR,ND=Y                                          
         LKOUT E                                                                
                                                                                
         USING PUBKEY,R2                                                        
EDTPBC   LM    R2,R4,LP_AINP                                                    
         MVC   0(20,R4),SPACES                                                  
         GOTOR VPUBEDIT,DMCB,(X'08',PUBKPUB),(C'S',(R4))                        
         LHI   R0,20                                                            
         J     SETOLENX                                                         
         DROP  R2                                                               
                                                                                
***********************************************************************         
* Read Publication records                                            *         
***********************************************************************         
                                                                                
NXTPUBR  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',PUBKEYT),                +        
               ('PUB_DFQ+B#PUB',0),SAVED,0,0                                    
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PUBLISHER/REP DOWNLOAD COLUMNS                 *         
***********************************************************************         
                                                                                
ARYREPV  LKOUT A,(D,B#SAVED,REPSEND),NEWEL=Y,NROWS=1,                  *        
               ROWID=(REPSEND,YESQ)                                             
Rcode    LKOUT C,D#PUBREP,(D,B#REP,PREPKREP),CHAR,ND=Y                          
Rname    LKOUT C,D#REPNAM,(D,B#REP,PREPNAME),CHAR,ND=Y                          
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR REP RECORD DOWNLOAD COLUMNS                    *         
***********************************************************************         
                                                                                
ARYREP2  LKOUT A,(D,B#SAVED,REPSEND),NEWEL=Y,NROWS=1,                  *        
               ROWID=(REPSEND,YESQ)                                             
Media    LKOUT C,D#MEDCOD,(D,B#REP,PREPKMED),CHAR                               
RCode    LKOUT C,D#SPREP,(D,B#REP,PREPKREP),CHAR                                
RName    LKOUT C,D#REPNAM,(D,B#REP,PREPNAME),CHAR                               
Atten    LKOUT C,D#ATTENT,(D,B#REP,PREPATTN),CHAR,ND=Y                          
Alin1    LKOUT C,D#ADDRL1,(D,B#REP,PREPLIN1),CHAR,ND=Y                          
Alin2    LKOUT C,D#ADDRL2,(D,B#REP,PREPLIN2),CHAR,ND=Y                          
Alin3    LKOUT C,D#ADDRL3,(D,B#REP,PREPLIN3),CHAR,ND=Y                          
StaCd    LKOUT C,D#STATE,(D,B#REP,PREPSTAC),CHAR,ND=Y                           
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PUBLICATION PAYEE INFORMATION DOWNLOAD         *         
***********************************************************************         
                                                                                
ARYPAYEV LKOUT A,(D,B#SAVED,PUBSEND),NEWEL=Y,NROWS=1,                  *        
               ROWID=(PUBSEND,YESQ)                                             
MedCd    LKOUT C,D#MEDCOD,(D,B#PUB,PUBKMED),CHAR,ND=Y                           
PCode    LKOUT C,D#PUBCOD,(D,B#SAVED,PUBCODE),CHAR,ND=Y                         
PName    LKOUT C,D#PUBNAM,(D,B#SAVED,PAYEENAM),CHAR,ND=Y                        
PAdL1    LKOUT C,D#ADDRL1,(D,B#SAVED,PAYEEAL1),CHAR,ND=Y                        
PAdL2    LKOUT C,D#ADDRL2,(D,B#SAVED,PAYEEAL2),CHAR,ND=Y                        
PAttn    LKOUT C,D#ATTENT,(D,B#SAVED,PAYEEATN),CHAR,ND=Y                        
PTel#    LKOUT C,D#TELNUM,(D,B#SAVED,PAYEETEL),CHAR,ND=Y                        
PFax#    LKOUT C,D#FAXNUM,(D,B#SAVED,PAYEEFAX),CHAR,ND=Y                        
PE-ml    LKOUT C,D#E_MAIL,(D,B#SAVED,PAYEEEML),CHAR,ND=Y                        
PayRp    LKOUT C,D#PAYREP,(D,B#SAVED,PAYEECOD),CHAR,ND=Y,              +        
               PCVERSION=3.5.0.6                                                
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ADCODE DOWNLOAD COLUMNS                        *         
***********************************************************************         
                                                                                
ARYADCV  LKOUT A,(D,B#SAVED,SAVED),NEWEL=Y,NROWS=1                              
SDate    LKOUT C,D#STRDAT,(D,B#SAVED,ADCSTRDT),BDAT,ND=Y                        
EDate    LKOUT C,D#ENDDAT,(D,B#SAVED,ADCENDDT),BDAT,ND=Y                        
SpDsc    LKOUT C,D#SPCDSC,(D,B#SAVED,ADCSPDSC),CHAR,ND=Y                        
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CLT/PRD GROUP VERIFIER                         *         
***********************************************************************         
                                                                                
ARYCPG   LKOUT A,(R,PRCCPGRP),MULTIROW=Y                                        
                                                                                
CltC1    LKOUT C,D#CLTCOD,(D,B#SAVED,GRPCLTCD),CHAR,ND=Y                        
CltC2    LKOUT C,D#CLTCD2,(D,B#SAVED,GRPCLTC2),CHAR,ND=Y                        
PrdCd    LKOUT C,D#PRDCOD,(D,B#SAVED,GRPPRDCD),CHAR,ND=Y                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PURCHASE ORDER RECORD DOWNLOAD                 *         
***********************************************************************         
                                                                                
ARYPO#   LKOUT A,(R,PRCPOREC),MULTIROW=Y                                        
                                                                                
POSq#    LKOUT C,D#PO#SQ#,(D,B#SAVED,PO#SEQNO),UBIN,ND=Y                        
PONum    LKOUT C,D#PO#NUM,(D,B#SAVED,PO#NUMBR),CHAR,ND=Y                        
POSDt    LKOUT C,D#STRDAT,(D,B#SAVED,PO#STRDT),BDAT,ND=Y                        
POEDt    LKOUT C,D#ENDDAT,(D,B#SAVED,PO#ENDDT),BDAT,ND=Y                        
POAmt    LKOUT C,D#PO#AMT,(D,B#SAVED,PO#AMOUT),CHAR,ND=Y                        
POSta    LKOUT C,D#PO#STA,(D,B#SAVED,PO#STATU),UBIN,ND=Y                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR FOREIGN EXCHANGE DOWNLOAD                      *         
***********************************************************************         
                                                                                
ARYFXRT  LKOUT A,(R,GET_FXRT),MULTIROW=Y                                        
                                                                                
FXRat    LKOUT C,D#FXRATE,(D,B#SAVED,FX_RATE_),CHAR,ND=Y                        
StrDt    LKOUT C,D#STRDAT,(D,B#SAVED,BINSTRDT),BDAT,ND=Y                        
EndDt    LKOUT C,D#ENDDAT,(D,B#SAVED,BINENDDT),BDAT,ND=Y                        
ErMSG    LKOUT C,D#ERRDSC,(D,B#SAVED,ERRORMSG),CHAR,ND=Y                        
                                                                                
         LKOUT E                                                                
                                                                                
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTBS#   NTR1  BASE=*,LABEL=*      Validate Buy Serial#                         
*                                                                               
         LA    RE,SAVED                                                         
         STCM  RE,15,LP_ADATA                                                   
         XC    BUYSERKY,BUYSERKY   Init Buy serial# key                         
         XC    ERRMCNUM,ERRMCNUM   Init error map code                          
         XC    ERRORMSG,ERRORMSG   Init error message                           
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXBS#50                                                          
         SR    RE,RE                                                            
         ICM   RE,7,ABS#                                                        
         JZ    NXBS#_X                                                          
         MVC   NUMBS#,LW_NUMN-LW_D(RE)                                          
         AHI   RE,LW_LN2Q                                                       
*                                                                               
NXBS#20  MVC   BUYSERKY,0(RE)      Get Buy ser# key from current list           
         SR    R0,R0                                                            
         ICM   R0,3,NUMBS#         R0=remaining Buy serial# key count           
         JZ    NXBS#_X             Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMBS#                                                      
         AHI   RE,L'BUYSERKY       Bump to next Buy serial# key                 
         ST    RE,ANXTBS#          Set A(next Buy serial# key)                  
*                                                                               
         CLC   BUYSERKY,SPACES     Have Buy Serial# key?                        
         JNH   NBS#_E01                                                         
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PSERRECD,R2         READ SERIAL PASSIVE POINTER                  
         MVC   PSERKAGY,AGY                                                     
         MVC   PSERKMED,BUYSERKY+0                                              
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKCLT,BUYSERKY+1                                              
         PACK  DUB,BUYSERKY+4(9)                                                
         TP    DUB                 Serial# numeric?                             
         JNZ   NBS#_E02                                                         
         ZAP   WORK(L'DUB),=P'1000000000'                                       
         SP    WORK(L'DUB),DUB                                                  
         MVC   PSERKNUM,WORK+3                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IODIR+IO2'                              
         JNE   NBS#_E03                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         JNE   NBS#_E03                                                         
*                                                                               
         XC    PUBKCODE,PUBKCODE                                                
         OC    PUBCODE,PUBCODE                                                  
         JZ    NXBS#30                                                          
         LA    R1,PUBCODE                                                       
         LA    R0,L'PUBCODE                                                     
         SR    RF,RF                                                            
         BASR  RE,0                                                             
         CLI   0(R1),C' '                                                       
         JNH   *+14                                                             
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
         LTR   R0,R0                                                            
         JZ    NBS#_E04                                                         
         GOTOR VPUBVAL,DMCB,((RF),PUBCODE),PUBKCODE                             
         CLI   0(R1),X'FF'                                                      
         JE    NBS#_E04                                                         
*                                                                               
NXBS#30  OC    PUBKCODE,PUBKCODE   Have pub to match?                           
         JZ    NXBS#40                                                          
         L     RE,AIO2                                                          
         CLC   (PBUYKPUB-PBUYREC)(L'PBUYKPUB,RE),PUBKCODE                       
         JNE   NBS#_E05                                                         
*                                                                               
NXBS#40  DS    0H                                                               
         J     NXBS#_Y                                                          
*                                                                               
NXBS#50  L     RE,ANXTBS#                                                       
         J     NXBS#20                                                          
*                                                                               
*                                                                               
NXBS#_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
*                                                                               
NXBS#_Y  J     EXITY                                                            
*                                                                               
NXBS#_N  J     EXITN                                                            
*                                                                               
*                                                                               
NBS#_E01 LHI   RF,001              Missing input field                          
         MVC   ERRMCNUM,=AL2(D#INSKEY)                                          
         J     NBS#_E_X                                                         
*                                                                               
NBS#_E02 LHI   RF,559              Buy serial# must be numeric                  
         MVC   ERRMCNUM,=AL2(D#INSKEY)                                          
         J     NBS#_E_X                                                         
*                                                                               
NBS#_E03 LHI   RF,200              No buy found                                 
         MVC   ERRMCNUM,=AL2(D#INSKEY)                                          
         J     NBS#_E_X                                                         
*                                                                               
NBS#_E04 LHI   RF,018              Invalid Publication                          
         MVC   ERRMCNUM,=AL2(D#PUBCOD)                                          
         J     NBS#_E_X                                                         
*                                                                               
NBS#_E05 LHI   RF,620              Pub does not match                           
         MVC   ERRMCNUM,=AL2(D#PUBCOD)                                          
         J     NBS#_E_X                                                         
*                                                                               
NBS#_E_X BRAS  RE,GET_ETXT                                                      
         J     EXITY                                                            
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTBST   NTR1  BASE=*,LABEL=*      Buy status download                          
*                                                                               
         LA    RE,SAVED                                                         
         STCM  RE,15,LP_ADATA                                                   
         XC    BUYSERKY,BUYSERKY   Init Buy serial# key                         
         MVI   BUYPAYSW,C'N'       Init Buy pay switch                          
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NXBST50                                                          
         SR    RE,RE                                                            
         ICM   RE,7,ABS#                                                        
         JZ    NXBST_X                                                          
         MVC   NUMBS#,LW_NUMN-LW_D(RE)                                          
         AHI   RE,LW_LN2Q                                                       
*                                                                               
NXBST20  MVC   BUYSERKY,0(RE)      Get Buy ser# key from current list           
         SR    R0,R0                                                            
         ICM   R0,3,NUMBS#         R0=remaining Buy serial# key count           
         JZ    NXBST_X             Exit if all done                             
         BCTR  R0,0                                                             
         STCM  R0,3,NUMBS#                                                      
         AHI   RE,L'BUYSERKY       Bump to next Buy serial# key                 
         ST    RE,ANXTBS#          Set A(next Buy serial# key)                  
*                                                                               
         CLC   BUYSERKY,SPACES     Have Buy Serial# key?                        
         JNH   NXBST50                                                          
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING PSERRECD,R2         READ SERIAL PASSIVE POINTER                  
         MVC   PSERKAGY,AGY                                                     
         MVC   PSERKMED,BUYSERKY+0                                              
         MVI   PSERKRCD,PSERKIDQ                                                
         MVC   PSERKCLT,BUYSERKY+1                                              
         PACK  DUB,BUYSERKY+4(9)                                                
         TP    DUB                 Serial# numeric?                             
         JNZ   NXBST50                                                          
         ZAP   WORK(L'DUB),=P'1000000000'                                       
         SP    WORK(L'DUB),DUB                                                  
         MVC   PSERKNUM,WORK+3                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORDD+IODIR+IO2'                              
         JNE   NXBST50                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         JNE   NXBST50                                                          
         DROP  R2                                                               
*                                                                               
         L     R2,AIO2                                                          
         LA    R2,(PBDELEM-PBUYREC)(R2)                                         
         USING PPAYELEM,R2                                                      
         MVI   ELCODE,PPAYELCQ                                                  
         BRAS  RE,NXTEL            Pay elem found?                              
         JNE   *+18                                                             
         OC    PPDDATE,PPDDATE     Have pay date?                               
         JZ    *-14                                                             
         MVI   BUYPAYSW,C'Y'       Set pay switch to yes                        
         J     NXBST_Y                                                          
         DROP  R2                                                               
*                                                                               
NXBST50  L     RE,ANXTBS#                                                       
         J     NXBST20                                                          
*                                                                               
NXBST_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
NXBST_Y  J     EXITY                                                            
NXBST_N  J     EXITN                                                            
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCPGRP NTR1  BASE=*,LABEL=*      GET CLT/PRD GROUP                            
*                                                                               
         LA    RE,SAVED                                                         
         STCM  RE,15,LP_ADATA                                                   
         LA    R2,IOKEY                                                         
         USING GRPGKEY,R2                                                       
*                                                                               
         XC    GRP_RDAT(GRP_RLNQ),GRP_RDAT                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   G_CPG40                                                          
         XC    IOKEY,IOKEY                                                      
*                                                                               
         USING LW_D,RE                                                          
         ICM   RE,7,AMED                                                        
         BZ    G_CPG_X                                                          
         LHI   RF,1                                                             
         LA    R1,LW_DATA1         SET N'ENTRIES TO 1                           
         CLI   LW_TYPE,LQ_TSINQ    TEST SINGLE ENTRY                            
         BE    *+12                                                             
         ICM   RF,3,LW_NUMN        NO - PICK UP N'ENTRIES FROM POOL             
         LA    R1,LW_DATA2                                                      
         STH   RF,NMED             SET N'ENTRIES TO BE PROCESSED                
         ST    R1,ANXTMED          SET A(NEXT ONE TO BE PROCESSED)              
         OC    0(L'PCLTKMED,R1),0(R1)                                           
         BZ    G_CPG_X             NO CLT CODE, NOT DOING PRD GRP               
         MVC   GRPGMED,0(R1)                                                    
         DROP  RE                                                               
*                                                                               
G_CPG06  MVC   GRPGAGY,AGY                                                      
         MVI   GRPGTYP,GRPGCGQ     CLT GROUP                                    
         MVI   GRPSTAT1,GRPCGRPQ                                                
*                                                                               
         USING LW_D,RE                                                          
         ICM   RE,7,ACLT                                                        
         BZ    G_CPG08                                                          
         LHI   RF,1                                                             
         LA    R1,LW_DATA1         SET N'ENTRIES TO 1                           
         CLI   LW_TYPE,LQ_TSINQ    TEST SINGLE ENTRY                            
         BE    *+12                                                             
         ICM   RF,3,LW_NUMN        NO - PICK UP N'ENTRIES FROM POOL             
         LA    R1,LW_DATA2                                                      
         STH   RF,NCLT             SET N'ENTRIES TO BE PROCESSED                
         ST    R1,ANXTCLT          SET A(NEXT ONE TO BE PROCESSED)              
         OC    0(L'PCLTKCLT,R1),0(R1)                                           
         BZ    G_CPG08             NO CLT CODE, NOT DOING PRD GRP               
         DROP  RE                                                               
*                                                                               
         MVC   GRPGCLT,0(R1)                                                    
         MVI   GRPSTAT1,GRPPGRPQ                                                
         MVI   GRPGTYP,GRPGPGQ     PRD GROUP                                    
*                                                                               
G_CPG08  MVC   GRPGID,GROUP_ID     GROUP ID                                     
         TM    GRPGTYP,GRPGCGQ     CLT GROUP?                                   
         BZ    G_CPG10                                                          
         LA    RE,SPCGRTAB         TRANSLATE 1 CHAR CODE TO 2 CHARS             
         LHI   RF,(SPCGRTBX-SPCGRTAB)/3                                         
         CLC   GROUP_ID,0(RE)                                                   
         BE    *+16                                                             
         LA    RE,3(RE)                                                         
         BCT   RF,*-14                                                          
         B     G_CPG12             TRANSLATION FAILED                           
         MVC   GRPGID,2(RE)        GROUP ID                                     
*                                                                               
G_CPG10  OC    GROUPCOD,=C'0000'   ZERO PADDED, LEFT-JUSTIFIED                  
         PACK  DUB,GROUPCOD                                                     
         TP    DUB                 VALID NUMERIC?                               
         BNZ   G_CPG12                                                          
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    G_CPG12             MUST BE NON-ZERO                             
         STCM  R0,3,GRPGCODE       GROUP CODE, PWS                              
*                                                                               
         MVC   WORK2(L'GRPGKEY),GRPGKEY                                         
*                                                                               
G_CPG11  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR'                                
         BNE   G_CPG60                                                          
         CLC   IOKEY(GRPGVAL-GRPGKEY),IOKEYSAV                                  
         BNE   G_CPG60                                                          
         B     G_CPG16                                                          
*                                                                               
G_CPG12  OI    GRPSTAT1,GRPNOTFQ   GROUP NOT FOUND                              
         B     G_CPG_N                                                          
*                                                                               
G_CPG16  TM    GRPSTAT1,GRPCGRPQ   CLT GROUP?                                   
         BZ    G_CPG30                                                          
         MVC   TEMP2(L'IOKEY),IOKEY                                             
         LA    R2,TEMP2                                                         
         GOTOR (#GETCLT,AGETCLT),DMCB,GRPGAGY,GRPGMED,GRPGVAL                   
         LA    R2,IOKEY                                                         
         MVC   IOKEY,TEMP2                                                      
         BE    G_CPG20             APPLY CLT SECURITY                           
         MVC   GRPCLTC2,GRPGVAL    NO ACCESS TO THIS CLT CODE                   
         B     *+10                                                             
G_CPG20  MVC   GRPCLTCD,GRPGVAL                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOPRTDIR'                                
         BE    *+6                                                              
         DC    H'0'                RESTORE SEQUENCE                             
         B     G_CPG_Y                                                          
*                                                                               
G_CPG30  TM    GRPSTAT1,GRPPGRPQ   PRD GROUP?                                   
         BZ    *+10                                                             
         MVC   GRPPRDCD,GRPGVAL                                                 
         B     G_CPG_Y                                                          
*                                                                               
G_CPG40  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOPRTDIR'                                
         BNE   G_CPG60                                                          
         CLC   IOKEY(GRPGVAL-GRPGKEY),IOKEYSAV                                  
         BE    G_CPG16                                                          
*                                                                               
G_CPG60  TM    GRPSTAT1,GRPPGRPQ   PRD GROUP?                                   
         BZ    G_CPG70                                                          
         LH    RF,NCLT             BUMP TO NEXT ENTRY IN POOL                   
         SHI   RF,1                                                             
         BZ    G_CPG70                                                          
         STH   RF,NCLT                                                          
         L     R1,ANXTCLT                                                       
         AHI   R1,L'PCLTKCLT                                                    
         ST    R1,ANXTCLT                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPGKEY,WORK2       USE SAVED KEY FROM 1ST TIME                  
         MVC   GRPGCLT,0(R1)                                                    
         B     G_CPG11                                                          
*                                                                               
G_CPG70  L     RF,ALP                                                           
         CLC   (LP_VRSN1-LP_D)(4,RF),=AL1(03,05,00,06)                          
         BNH   G_CPG_X                                                          
*                                                                               
         LH    RF,NMED             BUMP TO NEXT ENTRY IN POOL                   
         SHI   RF,1                                                             
         BZ    G_CPG_X                                                          
         STH   RF,NMED                                                          
         L     R1,ANXTMED                                                       
         AHI   R1,L'PCLTKMED                                                    
         ST    R1,ANXTMED                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   GRPGMED,0(R1)                                                    
         B     G_CPG06                                                          
*                                                                               
G_CPG_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
*                                                                               
G_CPG_Y  J     EXITY                                                            
*                                                                               
G_CPG_N  J     EXITN                                                            
*                                                                               
SETEACTZ ST    RE,FULL1            SET ESTIMATE ACTUALIZE DATA                  
         LA    R2,PESTELEM-PESTREC(R2)                                          
         MVI   ELCODE,PEACTECQ                                                  
         BRAS  RE,NXTEL                                                         
         JNE   S_ACTZ_X                                                         
         USING PESTACTD,R2                                                      
         OC    PEACTDAT,PEACTDAT   HAVE ACTUALIZE BILL THRU DATE?               
         JZ    *+14                                                             
         MVC   EACTTHDT,PEACTDAT                                                
         MVI   EACTTHDT+2,1        SET TO FIRST DAY OF MONTH                    
         OC    PEACTCHG,PEACTCHG   HAVE LAST CHANGED DATE?                      
         JZ    *+10                                                             
         MVC   EACLCDAT,PEACTCHG                                                
         OC    PEACTPID,PEACTPID   HAVE LAST CHANGED PID?                       
         JZ    S_ACTZ_X                                                         
         OI    DL_FLAG1,GETPIDOQ                                                
         GOTOR GETWHO,DMCB,PEACTPID,(L'EACLCPID,EACLCPID)                       
S_ACTZ_X L     RE,FULL1                                                         
         BR    RE                                                               
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_PO#R NTR1  BASE=*,LABEL=*      GET PURCHASE ORDER RECORD                    
*                                                                               
         LA    RE,SAVED                                                         
         STCM  RE,15,LP_ADATA                                                   
*                                                                               
         XC    PO#_RDAT(PO#_RLNQ),PO#_RDAT                                      
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   G_PO#50                                                          
*                                                                               
         MVI   BYTE4,0             Init purchase order level                    
         LA    R2,IOKEY                                                         
         USING PCLTKEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   PCLTKAGY,AGY                                                     
         MVI   PCLTKRCD,PCLTRECQ   Client record                                
         SR    RE,RE                                                            
         ICM   RE,7,AMED                                                        
         JZ    G_PO#_N                                                          
         MVC   PCLTKMED,LW_DATA1-LW_D(RE)                                       
         ICM   RE,7,ACLT                                                        
         JZ    G_PO#_N                                                          
         MVC   PCLTKCLT,LW_DATA1-LW_D(RE)                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO1'                            
         JNE   G_PO#_N                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO1'                           
         JNE   G_PO#_N                                                          
         L     R2,IOADDR                                                        
         LA    R2,(PCLTELEM-PCLTREC)(R2)                                        
         USING PCLTPOEL,R2                                                      
G_PO#22  CLI   PCLTPOEL,0          End of record?                               
         JE    G_PO#24                                                          
         CLI   PCLTPOEL,PCLTPOEQ   Billed by purchase order elem?               
         JE    G_PO#22H                                                         
         LLC   RE,PCLTPOLN                                                      
         AR    R2,RE                                                            
         J     G_PO#22                                                          
G_PO#22H MVC   BYTE4,PCLTPOLV      Save purchase order level                    
         DROP  R2                                                               
*                                                                               
G_PO#24  LA    R2,IOKEY                                                         
         USING PPO#KEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   PPO#KAGY,AGY                                                     
         MVI   PPO#KRCD,PPO#KIDQ   PURCHASE ORDER RECORD CODE                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,AMED                                                        
         BZ    G_PO#_N                                                          
         MVC   PPO#KMED,LW_DATA1-LW_D(RE)                                       
*                                                                               
         ICM   RE,7,ACLT                                                        
         BZ    G_PO#_N                                                          
         MVC   PPO#KCLT,LW_DATA1-LW_D(RE)                                       
*                                                                               
         CLI   BYTE4,P_POLVCQ      Client level?                                
         JE    G_PO#26                                                          
         ICM   RE,7,APRD                                                        
         BZ    G_PO#_N                                                          
         MVC   PPO#KPRD,LW_DATA1-LW_D(RE)                                       
*                                                                               
         CLI   BYTE4,P_POLVPQ      Product level?                               
         JE    G_PO#26                                                          
         ICM   RE,7,AEST                                                        
         BZ    G_PO#_N                                                          
         MVC   PPO#KEST,LW_DATA1-LW_D(RE)                                       
         OC    PPO#KEST,PPO#KEST   Have estimate?                               
         JZ    G_PO#_N                                                          
*                                                                               
G_PO#26  GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO1'                            
         BNE   G_PO#_N                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO1'                           
         BNE   G_PO#_N                                                          
*                                                                               
         L     R2,IOADDR                                                        
         LA    R2,(PO#FIRST-PPO#REC)(R2)                                        
         USING PO#DELMD,R2                                                      
         ST    R2,FULL2            SAVE ADDRESS OF PO# ELEM                     
*                                                                               
G_PO#50  L     R2,FULL2            ADDRESS OF PO# ELEM TO PROCESS               
G_PO#54  CLI   PO#DELM,0           END OF PURCHASE ORDER RECORD?                
         BE    G_PO#_X                                                          
         CLI   PO#DELID,PO#DLIDQ   PURCHASE ORDER # ELEM?                       
         BE    G_PO#58                                                          
G_PO#56  SR    RE,RE                                                            
         IC    RE,PO#DLEN                                                       
         AR    R2,RE                                                            
         B     G_PO#54                                                          
*                                                                               
G_PO#58  OC    FLTPOSQ#,FLTPOSQ#   FILTERING ON PO SEQUENCE #?                  
         BZ    G_PO#62                                                          
         CLC   PO#DID,FLTPOSQ#     MATCH THAT OF FILTER?                        
         BNE   G_PO#56                                                          
         B     G_PO#70                                                          
*                                                                               
G_PO#62  OC    FLTPONUM,FLTPONUM   FILTERING ON PO#?                            
         BZ    G_PO#70                                                          
         SR    RE,RE                                                            
         IC    RE,PO#DLEN                                                       
         SHI   RE,PO#DHDLQ                                                      
         CHI   RE,1                PURCHASE ORDER NUMBER PRESENT?               
         BL    G_PO#56                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLTPONUM(0),PO#DPO# MATCH THAT OF FILTER?                        
         BNE   G_PO#56                                                          
*                                                                               
G_PO#70  SR    RE,RE                                                            
         IC    RE,PO#DLEN                                                       
         SHI   RE,PO#DHDLQ                                                      
         CHI   RE,1                PURCHASE ORDER NUMBER PRESENT?               
         BL    G_PO#56                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PO#NUMBR(0),PO#DPO#                                              
*                                                                               
         MVC   PO#SEQNO,PO#DID                                                  
         MVC   PO#STRDT,PO#DSTRT                                                
         MVC   PO#ENDDT,PO#DEND                                                 
*                                                                               
         LA    RE,PO#AMOUT+0                                                    
         CLI   PO#DGORN,C'N'       NET AMOUNT?                                  
         BNE   *+12                                                             
         MVI   PO#AMOUT,C'N'                                                    
         LA    RE,PO#AMOUT+1                                                    
         EDIT  PO#D$,(15,0(RE)),2,COMMAS=YES,ALIGN=LEFT                         
*                                                                               
         TM    PO#DACTV,PO#DINAQ   PURCHASE ORDER # IS INACTIVE?                
         BZ    *+8                                                              
         OI    PO#STATU,PO#SINAQ                                                
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PO#DLEN                                                       
         AR    R2,RE                                                            
         ST    R2,FULL2            FOR NEXT ROUND                               
         B     G_PO#_Y                                                          
*                                                                               
G_PO#_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
*                                                                               
G_PO#_Y  J     EXITY                                                            
*                                                                               
G_PO#_N  J     EXITN                                                            
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_ADCR NTR1  BASE=*,LABEL=*      PROCESS AD CODE RECORD DOWNLOAD              
*                                                                               
         L     R0,AIO5                                                          
         LHI   R1,ADC_LENQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT BLOCK                           
         L     R3,AIO5                                                          
         STCM  R3,15,LP_ADATA                                                   
         USING ADC_OUTD,R3                                                      
         L     R2,IOADDR                                                        
         MVI   DL_FLTR1,0          INIT DOWNLOAD FILTER SWITCH                  
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   G_ADC20                                                          
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         XC    PUBKCODE,PUBKCODE                                                
         MVC   IOADDR,AIO6                                                      
         OC    PUBCODE,PUBCODE                                                  
         BZ    G_ADC20                                                          
         CLC   PUBCODE,=C'ALL'     ALL PUB?                                     
         BNE   *+14                                                             
         MVC   PUBKCODE,=6X'FF'                                                 
         B     G_ADC20                                                          
         LA    R1,PUBCODE                                                       
         LA    R0,L'PUBCODE                                                     
         SR    RF,RF                                                            
         BASR  RE,0                                                             
         CLI   0(R1),C' '                                                       
         BNH   *+14                                                             
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
         LTR   R0,R0                                                            
         JZ    INVPUB                                                           
         GOTOR VPUBVAL,DMCB,((RF),PUBCODE),PUBKCODE                             
         CLI   0(R1),X'FF'                                                      
         JE    INVPUB                                                           
*                                                                               
G_ADC20  CLC   IOKEY(PJOBKCLT-PJOBKEY),IOKEYSAV                                 
         BNE   G_ADC_X                                                          
G_ADC20K GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ADCTAB),(6,0),SAVED,0             
         BNE   G_ADC_N                                                          
                                                                                
         L     R2,IOADDR                                                        
         USING PJOBREC,R2                                                       
                                                                                
         OC    ADCODE,ADCODE       FILTERING ON AD CODE?                        
         BZ    *+14                                                             
         CLC   PJOBKJOB,ADCODE     AD CODE MATCH?                               
         BNE   G_ADC20K                                                         
*                                                                               
         OC    AD_ID,AD_ID         FILTERING ON AD ID?                          
         JZ    G_ADC21                                                          
         CLI   AD_ID+(L'AD_ID-1),C' '    11 characters Ad-Id?                   
         JNE   *+8                                                              
         MVI   AD_ID+(L'AD_ID-1),0                                              
         CLC   PJOBADID,AD_ID      AD ID MATCH?                                 
         BNE   G_ADC20K                                                         
*                                                                               
G_ADC21  CLI   ADCRTYP,ABRTALLQ    DOWNLOAD ALL AD CODE DATA?                   
         BE    G_ADC30                                                          
         TM    ADCRTYP,ABRTADCQ    DOWNLOAD AD CODE RECORD?                     
         BNZ   G_ADC22                                                          
         TM    ADCRTYP,ABRTSUBQ    DOWNLOAD SUB AD CODE DATA?                   
         BZ    G_ADC22K                                                         
G_ADC22  OC    PJOBKPUB,PJOBKPUB   ANYTHING IN PUB (INSTRUCTION REC)?           
         BNZ   G_ADC20K                                                         
G_ADC22K TM    ADCRTYP,ABRTINSQ    DOWNLOADING INSTRUCTION RECORDS?             
         BZ    G_ADC30                                                          
         OC    PJOBKPUB,PJOBKPUB   ANYTHING IN PUB?                             
         BZ    G_ADC20K                                                         
         OC    PUBKCODE,PUBKCODE   FILTER ON PUB?                               
         BZ    G_ADC30                                                          
         CLC   PUBKCODE,PJOBKPUB   PUB MATCH THAT OF FILTER?                    
         BNE   G_ADC20K                                                         
                                                                                
G_ADC30  MVC   ADC_MEDC,PJOBKMED                                                
         MVC   ADC_CLTC,PJOBKCLT                                                
         MVC   ADC_PRDC,PJOBKPRD                                                
         MVC   ADC_ADCD,PJOBKJOB                                                
         CLI   ADC_ADCD,X'FF'                                                   
         BNE   *+10                                                             
         XC    ADC_ADCD,ADC_ADCD                                                
         MVC   ADC_ADID,PJOBADID                                                
         OC    PJOBKPUB,PJOBKPUB   ANYTHING IN PUB?                             
         BZ    G_ADC34                                                          
         XC    ADC_ADID,ADC_ADID   ADID REPLIED FOR AD CODE RECORD              
         CLC   PJOBKPUB,=6X'FF'    ALL PUB?                                     
         BNE   *+14                                                             
         MVC   ADC_PUBC,=C'ALL'                                                 
         B     G_ADC32                                                          
         GOTOR VPUBEDIT,DMCB,(X'08',PJOBKPUB),(C'S',ADC_PUBC)                   
         DROP  R2                                                               
                                                                                
G_ADC32  LA    R2,(PJOBELEM-PJOBREC)(R2)                                        
         MVI   ELCODE,X'66'                                                     
         LA    RF,ADC_COM1                                                      
         USING PJCOMEL,R2                                                       
         CLI   PJCOMEL,X'66'                                                    
         BE    G_ADC32M                                                         
G_ADC32K BRAS  RE,NXTEL                                                         
         BNE   G_ADC80                                                          
G_ADC32M SR    R1,R1                                                            
         IC    R1,PJCOMEL+1                                                     
         AHI   R1,-2-1             MINUS OVERHEAD AND EX                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),PJCOMDT                                                  
         MVI   DL_FLTR1,ADCINSRQ   SET TO SEND ALL INSTRUCTION LINES            
         LA    RF,(L'ADC_COM1)(RF)                                              
         LA    RE,ADC_COMG                                                      
         CR    RF,RE                                                            
         BH    G_ADC80                                                          
         B     G_ADC32K                                                         
         DROP  R2                                                               
                                                                                
G_ADC34  L     R2,IOADDR                                                        
         USING PJOBREC,R2                                                       
         CLI   (PJOBELEM-PJOBREC)(R2),X'15'                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ADCRTYP,ABRTALLQ    DOWNLOAD ALL AD CODE DATA?                   
         BE    *+12                                                             
         TM    ADCRTYP,ABRTSUBQ    DOWNLOAD SUB AD CODE DATA?                   
         BNZ   G_ADC40                                                          
         MVC   ADC_CAP1,PJOBCAP1                                                
         MVC   ADC_CAP2,PJOBCAP2                                                
         MVC   ADC_CPY#,PJOBCPY                                                 
         MVC   ADC_SPDS,PJOBSPC                                                 
                                                                                
         XC    WORK,WORK                                                        
         CP    PJOBUNTS,=P'0'                                                   
         BE    G_ADC36M                                                         
         CLI   PJOBUIND,X'89'                                                   
         BNE   G_ADC36B                                                         
         EDIT  PJOBUNTS,(6,WORK),2,ALIGN=LEFT                                   
         B     G_ADC36D                                                         
G_ADC36B EDIT  PJOBUNTS,(5,WORK),ALIGN=LEFT                                     
G_ADC36D LA    RE,WORK                                                          
         AR    RE,R0                                                            
         MVC   0(1,RE),PJOBUIND                                                 
         CLI   0(RE),C'I'                                                       
         BE    G_ADC36K                                                         
         CLI   0(RE),X'89'                                                      
         BNE   G_ADC36M                                                         
         OI    0(RE),X'40'                                                      
G_ADC36K MVI   1(RE),C'N'                                                       
G_ADC36M MVC   ADC_#LIN,WORK                                                    
                                                                                
         OC    PJOBCOLS,PJOBCOLS                                                
         BZ    G_ADC37                                                          
         CP    PJOBCOLS,=P'0'                                                   
         BE    G_ADC37                                                          
         EDIT  PJOBCOLS,(3,ADC_#COL),ALIGN=LEFT                                 
                                                                                
G_ADC37  XC    TEMP2,TEMP2                                                      
         OC    PJOBTUNS,PJOBTUNS                                                
         BZ    G_ADC37M                                                         
         CP    PJOBTUNS,=P'0'                                                   
         BE    G_ADC37M                                                         
         CLI   PJOBUIND,X'89'                                                   
         BNE   G_ADC37D                                                         
         EDIT  PJOBTUNS,(6,TEMP2),2,ALIGN=LEFT                                  
         B     G_ADC37K                                                         
G_ADC37D EDIT  PJOBTUNS,(5,TEMP2),ALIGN=LEFT                                    
G_ADC37K LA    RE,TEMP2                                                         
         AR    RE,R0                                                            
         MVC   0(1,RE),PJOBUIND                                                 
         CLI   0(RE),X'89'                                                      
         BNE   *+8                                                              
         OI    0(RE),X'40'                                                      
         CLI   0(RE),C'I'                                                       
         BNE   *+8                                                              
         MVI   1(RE),C'N'                                                       
G_ADC37M MVC   ADC_LNXC,TEMP2                                                   
                                                                                
         MVC   ADC_PREM,PJOBPRM                                                 
         MVC   ADC_STRD,PJOBSTA                                                 
         MVC   ADC_ENDD,PJOBEND                                                 
         MVC   ADC_PHOU,PJOBPROD                                                
         MVC   ADC_AGYS,PJOBSIG                                                 
         MVC   ADC_PJOB,PJOBPJOB                                                
         MVC   ADC_ALLO,PJOBALO                                                 
         MVC   ADC_BCON,PJOBBLCC                                                
         MVC   ADC_FILT,PJOBFILT                                                
         MVC   ADC_FSI_,PJOBFSI                                                 
         MVC   ADC_BREP,PJOBBREP                                                
         MVC   ADC_PUBL,PJOBPLIS                                                
         DROP  R2                                                               
                                                                                
         LA    R2,(PJOBELEM-PJOBREC)(R2)                                        
         MVI   ELCODE,PJSAIDQ                                                   
         BRAS  RE,NXTEL                                                         
         BNE   G_ADC40                                                          
         USING PJSAELM,R2                                                       
         MVC   ADC_STAT,PJSACH1    AD CODE STATUS                               
         DROP  R2                                                               
                                                                                
G_ADC40  CLI   ADCRTYP,ABRTALLQ    DOWNLOAD ALL AD CODE DATA?                   
         BE    *+12                                                             
         TM    ADCRTYP,ABRTADCQ    DOWNLOAD AD CODE DATA ONLY?                  
         BNZ   G_ADC80                                                          
         L     R2,IOADDR                                                        
         LA    R2,(PJOBELEM-PJOBREC)(R2)                                        
         USING PJSUBEL,R2                                                       
         MVI   ELCODE,PJSUBIDQ                                                  
         LA    RF,ADC_ADC1                                                      
         CLI   PJSUBCDE,PJSUBIDQ                                                
         BE    G_ADC42M                                                         
G_ADC42K BRAS  RE,NXTEL                                                         
         BNE   G_ADC80                                                          
G_ADC42M CLI   PJSUBCOD,X'FF'                                                   
         BE    *+14                                                             
         MVC   0(L'ADC_ADC1,RF),PJSUBCOD                                        
         B     *+10                                                             
         MVC   L'ADC_ADC1(L'ADC_AID1,RF),PJSUBAID                               
         LA    R1,(L'ADC_ADC1+L'ADC_AID1)(RF)                                   
         EDIT  PJSUBPCT,(L'ADC_SHR1,0(R1)),3,ALIGN=LEFT                         
         LA    RF,(L'ADC_ADC1+L'ADC_AID1+L'ADC_SHR1)(RF)                        
         LA    RE,ADC_ADCA                                                      
         CR    RF,RE                                                            
         BH    G_ADC80                                                          
         B     G_ADC42K                                                         
         DROP  R2                                                               
                                                                                
G_ADC80  DS    0H                                                               
         B     G_ADC_Y                                                          
*                                                                               
G_ADC_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
*                                                                               
G_ADC_Y  MVC   LP_ADATA,AIO5                                                    
         J     EXITY                                                            
*                                                                               
G_ADC_N  MVC   LP_ADATA,AIO5                                                    
         J     EXITN                                                            
*                                                                               
ADCTAB   LKKEY H,PJOBKEY,SAVED                                                  
         LKKEY SIN,PJOBKAGY,AGY                                                 
         LKKEY WMP,PJOBKMED,AMED                                                
         LKKEY LIT,PJOBKRCD,PJOBKRCQ                                            
         LKKEY WMP,PJOBKCLT,ACLT                                                
         LKKEY WMP,PJOBKPRD,APRD                                                
         LKKEY ALL,PJOBKJOB                                                     
         LKKEY ALL,PJOBKPUB                                                     
         LKKEY E                                                                
                                                                                
         DROP  RB,R3                                                            
                                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_CONR NTR1  BASE=*,LABEL=*      PROCESS CONTRACT RECORD DOWNLOAD             
*                                                                               
         L     R0,AIO5                                                          
         LHI   R1,CON_LENQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR OUTPUT BLOCK                           
         L     R3,AIO5                                                          
         STCM  R3,15,LP_ADATA                                                   
         USING CON_OUTD,R3                                                      
         L     R2,IOADDR                                                        
*                                                                               
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   G_CON20                                                          
         XC    IOKEY,IOKEY                                                      
         XC    IOKEYSAV,IOKEYSAV                                                
         XC    PUBKCODE,PUBKCODE                                                
         MVC   IOADDR,AIO6                                                      
         OC    PUBCODE,PUBCODE                                                  
         BZ    G_CON20                                                          
         LA    R1,PUBCODE                                                       
         LA    R0,L'PUBCODE                                                     
         SR    RF,RF                                                            
         BASR  RE,0                                                             
         CLI   0(R1),C' '                                                       
         BNH   *+14                                                             
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCTR  R0,RE                                                            
         LTR   R0,R0                                                            
         JZ    INVPUB                                                           
         GOTOR VPUBVAL,DMCB,((RF),PUBCODE),PUBKCODE                             
         CLI   0(R1),X'FF'                                                      
         JE    INVPUB                                                           
*                                                                               
G_CON20  CLC   IOKEY(PCONKCLT-PCONKEY),IOKEYSAV                                 
         BNE   G_CON_X                                                          
G_CON20K GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',CONTAB),(6,0),SAVED,0             
         BNE   G_CON_N                                                          
                                                                                
         L     R2,IOADDR                                                        
         USING PCONREC,R2                                                       
                                                                                
         OC    PUBKCODE,PUBKCODE                                                
         BZ    *+14                                                             
         CLC   PUBKCODE,PCONKPUB   MATCH PUB (INCLUDING ZONE/EDITION)?          
         BNE   G_CON20K                                                         
         OC    CONTNUM,CONTNUM                                                  
         BZ    *+14                                                             
         CLC   CONTNUM,PCONNUM     MATCH CONTRACT NUMBER?                       
         BNE   G_CON20K                                                         
                                                                                
         LA    R2,(PCONELEM-PCONREC)(R2)                                        
         USING PCONDESC,R2                                                      
                                                                                
         CLI   PCONDESC,X'10'      CONTRACT DESCRIPTION ELEM?                   
         BNE   G_CON20K                                                         
         TM    PCONLIND,X'80'      CONTRACT IS LOCKED?                          
         BNZ   G_CON20K                                                         
         CLC   BINSTRDT,PCONSDT    START DATE LESS THAN CONTRACT'S?             
         BH    G_CON20K                                                         
         CLC   BINENDDT,PCONEDT    END DATE HIGHER THAN CONTRACT'S?             
         BH    G_CON20K                                                         
         MVC   CON_STRD,PCONSDT                                                 
         MVC   CON_ENDD,PCONEDT                                                 
         MVC   CON_PRDC,PCONPRD                                                 
         DROP  R2                                                               
                                                                                
         L     R2,IOADDR                                                        
         USING PCONREC,R2                                                       
         MVC   CON_MEDC,PCONKMED                                                
         MVC   CON_CLTC,PCONKCLT                                                
         MVC   CON_CON#,PCONNUM                                                 
         GOTOR VPUBEDIT,DMCB,(X'08',PCONKPUB),(C'S',CON_PUBC)                   
         DROP  R2                                                               
                                                                                
         B     G_CON_Y                                                          
*                                                                               
G_CON_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
*                                                                               
G_CON_Y  MVC   LP_ADATA,AIO5                                                    
         J     EXITY                                                            
*                                                                               
G_CON_N  MVC   LP_ADATA,AIO5                                                    
         J     EXITN                                                            
*                                                                               
CONTAB   LKKEY H,PCONKEY,SAVED                                                  
         LKKEY SIN,PCONKAGY,AGY                                                 
         LKKEY WMP,PCONKMED,AMED                                                
         LKKEY LIT,PCONKRCD,PCONKRCQ                                            
         LKKEY WMP,PCONKCLT,ACLT                                                
         LKKEY ALL,PCONKPUB                                                     
         LKKEY ALL,PCONKZON                                                     
         LKKEY ALL,PCONKEDT                                                     
         LKKEY ALL,PCONNUM                                                      
         LKKEY E                                                                
                                                                                
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_FXRT NTR1  BASE=*,LABEL=*      GET FOREIGN EXCHANGE RECORD                  
*                                                                               
         LA    RE,SAVED                                                         
         STCM  RE,15,LP_ADATA                                                   
*                                                                               
         XC    TEMP,TEMP                                                        
         LA    R2,TEMP                                                          
         USING GEXCD,R2                                                         
         MVI   GEKREC,GEKRECQ      RECORD ID                                    
         MVC   GEKAGY,AGY          AGENCY                                       
         MVI   GEKSYS,X'04'        SET SYSTEM ID                                
         MVC   GEKCURF,FXCURFRO    FROM CURRENCY                                
         MVC   GEKCURT,FXCUR_TO    TO CURRENCY                                  
         MVI   GEKCTYP,GEKBOOQ     BOOKING RATE                                 
         MVI   GEKMED,X'FF'                                                     
         ICM   RF,7,ACLT           CLIENT                                       
         BZ    *+10                                                             
         MVC   GEKCLI,LW_DATA1-LW_D(RF)                                         
         MVI   GEKPRO,X'FF'                                                     
         MVI   GEKCAM,X'FF'                                                     
         MVC   GEKPEND,EFFS                                                     
         OC    BINENDDT,BINENDDT   HAVE END DATE?                               
         BZ    G_FXR20                                                          
         GOTOR VDATCON,DMCB,(3,BINENDDT),(2,GEKPEND)                            
*                                                                               
G_FXR20  XC    FX_RATE_,FX_RATE_   INIT OUTPUT DATA                             
         XC    BINSTRDT,BINSTRDT                                                
         XC    BINENDDT,BINENDDT                                                
         XC    ERRORMSG,ERRORMSG                                                
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         BNE   G_FXR_X                                                          
*                                                                               
         MVC   TEMP2,TEMP          SAVE STARTING KEY                            
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',GENDIR,TEMP,TEMP                        
         CLC   TEMP(GEKPEND-GEXCD),TEMP2                                        
         BNE   *+14                                                             
         CLC   GEKPSTA,GEKPEND-GEXCD+TEMP2    MATCH ON PERIOD?                  
         BNH   G_FXR50             RECORD FOUND                                 
*                                                                               
         MVC   TEMP,TEMP2          RESTORE STARTING KEY                         
         MVC   GEKCLI,EFFS         SET TO DEFAULT CLIENT                        
         MVC   TEMP2,TEMP          SAVE STARTING KEY                            
         GOTOR VDATAMGR,DMCB,=C'DMRDHI',GENDIR,TEMP,TEMP                        
         CLC   TEMP(GEKPEND-GEXCD),TEMP2                                        
         BNE   *+14                                                             
         CLC   GEKPSTA,GEKPEND-GEXCD+TEMP2    MATCH ON PERIOD?                  
         BNH   G_FXR50             RECORD FOUND                                 
*                                                                               
         MVC   TEMP,TEMP2          RESTORE STARTING KEY                         
         XC    GEKAGY,GEKAGY       CLEAR AGENCY                                 
         MVI   GEKSYS,X'FF'        FINANCIAL TIMES                              
         MVI   GEKCTYP,C'C'        FINANCIAL TIMES RATE                         
         MVC   TEMP2,TEMP          SAVE STARTING KEY                            
         GOTOR VDATAMGR,DMCB,=CL7'DMRDHI',=C'GENDIR',GEKEY,GEKEY                
         CLC   TEMP(GEKPEND-GEXCD),TEMP2                                        
         BNE   *+14                                                             
         CLC   GEKPSTA,GEKPEND-GEXCD+TEMP2    MATCH ON PERIOD?                  
         BNH   G_FXR50             RECORD FOUND                                 
*                                                                               
         B     G_FXR_E                                                          
*                                                                               
G_FXR50  GOTOR VDATAMGR,DMCB,=C'GETREC',GENFIL,GEDDA,AIO1,ELEM                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1                                                          
         OC    GEKPSTA,GEKPSTA                                                  
         BNZ   *+14                                                             
         MVC   BINSTRDT,TODAYB     NO START DATE, DEFAULT TO TODAY              
         B     G_FXR56                                                          
         GOTOR VDATCON,DMCB,(2,GEKPSTA),(3,BINSTRDT)                            
*                                                                               
G_FXR56  CLC   GEKPEND,EFFS        DATE IS UNTIL FURTHER NOTICE?                
         BE    G_FXR60                                                          
         GOTOR VDATCON,DMCB,(2,GEKPEND),(3,BINENDDT)                            
*                                                                               
G_FXR60  LA    R2,GEKEY+GEFIRST    POINT TO FIRST ELEMENT                       
         MVI   ELCODE,GEXELQ       LOOK FOR EXCHANGE ELEMENT                    
         DROP  R2                                                               
*                                                                               
         CLC   ELCODE,0(R2)        CHECK FIRST ELEMENT                          
         BE    *+12                                                             
         BRAS  RE,NXTEL                                                         
         BNE   G_FXR_E                                                          
*                                                                               
         USING GEXEL,R2                                                         
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+2(5),GEXRATE    GET RATE                                     
         SRP   DUB,64-1,5          SHIFT RIGHT ONE DIGIT                        
*                                                                               
         EDITR (P8,DUB),(8,FX_RATE_),5,FLOAT=-,ALIGN=LEFT,IZERO=Y               
         B     G_FXR_Y                                                          
*                                                                               
G_FXR_X  MVI   LP_RMODE,LP_RLAST   NO MORE                                      
*                                                                               
G_FXR_Y  J     EXITY                                                            
*                                                                               
G_FXR_N  J     EXITN                                                            
*                                                                               
G_FXR_E  LHI   RF,130              CANNOT FIND RATE                             
         BRAS  RE,GET_ETXT                                                      
         J     EXITY                                                            
*                                                                               
GET_ETXT LR    R0,RE                                                            
         GOTOR VGETTXT,DMCB+12,(RF),(60,ERRMSGST),(C'E',DMCB),0,0,0             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  RB,R2                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETPAYEE NTR1  BASE=*,LABEL=*      SET PAYEE INFORMATION                        
*                                                                               
         CLI   PUBSEND,YESQ        PUB PRESENT?                                 
         BNE   S_PYE_X             CANNOT CONTINUE W/O PUB RECORD               
         L     RE,LP_ABLK4                                                      
         LR    RF,RE                                                            
         LA    RE,(PCLTELEM-PCLTREC)(RE)                                        
         CLI   0(RE),X'02'                                                      
         BNE   S_PYE_X             NO CLT, NO NEED TO REPLY PAYEE               
*                                                                               
         USING PCLTKEY,RF                                                       
         USING PCLTELEM,RE                                                      
         USING GETADRPD,R1                                                      
         LA    R1,TEMP2                                                         
         XC    TEMP2,TEMP2                                                      
         MVI   BYTE2,C'P'          PAY ADDRESS                                  
         MVC   GADR_AGY,AGY                                                     
         MVC   GADR_MED,PCLTKMED                                                
         MVC   GADR_CLT,PCLTKCLT                                                
         MVC   GADR_OFC,PCLTOFF                                                 
         MVI   PAYEESW1,0          INIT PAYEE WORK SWITCH 1                     
         XC    SVPYALVL,SVPYALVL   INIT PAYEE ADDRESS LEVEL                     
*                                                                               
         LA    RE,PAYEE_VS                                                      
         LHI   RF,PAYEE_LQ                                                      
         XCEFL                                                                  
         L     RF,LP_ABLK7         PUB RECORD                                   
         GOTOR VPPGETAD,DMCB,(BYTE2,TEMP2),(RF),VDATAMGR                        
         CLI   0(R1),X'08'         PAY ADDRESS REC FOUND ?                      
         BNE   S_PYE60                                                          
         MVC   SVPYALVL,1(R1)      SAVE PAYEE ADDRESS LEVEL                     
         L     RE,4(R1)                                                         
         USING PGETADRD,RE                                                      
         MVC   PAYEENAM,PGADNAME                                                
         MVC   PAYEEAL1,PGADLIN1                                                
         MVC   PAYEEAL2,PGADLIN2                                                
         MVC   PAYEEATN,PGADATTN                                                
         MVC   PAYEETEL,PGADTEL                                                 
         MVC   PAYEEFAX,PGADFAX                                                 
         MVC   PAYEEEML,PGADEADD                                                
         OI    PAYEESW1,PAYEEFNQ   Payee address record found                   
*                                                                               
S_PYE60  LA    R1,TEMP2                                                         
         L     R2,LP_ABLK7                                                      
         LA    R2,(PUBNAMEL-PUBREC)(R2)                                         
         LR    RF,R2                                                            
         USING PUBREPEL,R2                                                      
         MVI   ELCODE,X'14'        CLIENT/OFFICE REPS ELEM CODE                 
S_PYE60H BRAS  RE,NXTEL                                                         
         BNE   S_PYE80                                                          
         CLC   PUBRPOFF,=X'FFFFFF' AGENCY SPECIFIC?                             
         BE    S_PYE64                                                          
         CLC   PUBRPOFF,GADR_CLT   CLIENT SPECIFIC?                             
         BE    S_PYE62                                                          
         CLI   PUBRPOFF,X'FF'      OFFICE SPECIFIC?                             
         BNE   S_PYE60H                                                         
         CLC   PUBRPOFF+1(L'GADR_OFC),GADR_OFC                                  
         BNE   S_PYE60H                                                         
*                                                                               
S_PYE62  OC    PUBPAREP(12),PUBPAREP                                            
         BZ    S_PYE60H                                                         
*                                                                               
S_PYE64  CLC   PUBPAREP,=4C'0'                                                  
         BE    S_PYE80                                                          
         OC    PUBPAREP,PUBPAREP                                                
         BZ    S_PYE80                                                          
*                                                                               
         MVC   PAYEECOD,PUBPAREP   REPLY PAYING REP CODE                        
*                                                                               
         OC    SVPYALVL,SVPYALVL   ANYTHING IN SAVED ADDRESS LEVEL?             
         BZ    *+14                                                             
         CLC   SVPYALVL,PUBRPOFF   ADDR IS MORE SPECIFIC THAN REP?              
         BL    S_PYE80                                                          
*                                                                               
         LA    RE,IOKEY            READ REP RECORD                              
         LA    R1,TEMP2                                                         
         USING PREPRECD,RE                                                      
         XC    PREPKEY,PREPKEY                                                  
         MVC   PREPKAGY,AGY                                                     
         MVC   PREPKMED,GADR_MED                                                
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,PAYEECOD                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOPRTDIR+IO7'                            
         BNE   S_PYE80                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOPRTFIL+IO7'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PAYEE_VS                                                      
         LHI   RF,PAYEE_LQ                                                      
         XCEFL                                                                  
         L     RE,IOADDR                                                        
         LA    RE,(PREPELEM-PREPREC)(RE)                                        
         CLI   0(RE),X'11'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID REP RECORD                           
         USING PREPELEM,RE                                                      
         MVC   PAYEENAM(L'PREPNAME),PREPNAME                                    
         MVC   PAYEEAL1(L'PREPLIN1),PREPLIN1                                    
         MVC   PAYEEAL2(L'PREPLIN2),PREPLIN2                                    
         MVC   PAYEEATN(L'PREPATTN),PREPATTN                                    
         MVC   PAYEETEL(L'PREPTEL),PREPTEL                                      
         MVC   PAYEEFAX(L'PREPFAX),PREPFAX                                      
         MVC   PAYEECOD,PUBPAREP   REPLY PAYING REP CODE                        
         OI    PAYEESW1,PBREPFNQ   PUB REP INFO SET                             
*                                                                               
S_PYE80  CLI   PAYEESW1,0          REPLY INFO SET?                              
         BNE   S_PYE_X                                                          
         LA    RE,PAYEE_VS                                                      
         LHI   RF,PAYEE_LQ                                                      
         XCEFL                                                                  
         L     R2,LP_ABLK7                                                      
         LA    R2,(PUBNAMEL-PUBREC)(R2)                                         
         LR    RF,R2                                                            
         CLI   0(R2),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID PUB RECORD                           
         USING PUBNAMEL,R2                                                      
         MVC   PAYEENAM(L'PUBNAME),PUBNAME                                      
         MVC   PAYEEAL1(L'PUBLINE1),PUBLINE1                                    
         MVC   PAYEEAL2(L'PUBLINE2),PUBLINE2                                    
*                                                                               
         MVI   ELCODE,X'11'        PUBLICATION SUPPL ADDRESS ELEM CODE          
         USING PUBSADEL,R2                                                      
S_PYE82H BRAS  RE,NXTEL                                                         
         BNE   S_PYE84                                                          
         CLC   PUBSAOFF,=X'FFFFFF' AGENCY SPECIFIC?                             
         BE    S_PYE82P                                                         
         CLC   PUBSAOFF,GADR_CLT   CLIENT SPECIFIC?                             
         BE    S_PYE82P                                                         
         CLI   PUBSAOFF,X'FF'      OFFICE SPECIFIC?                             
         BNE   S_PYE82H                                                         
         CLC   PUBSAOFF+1(L'GADR_OFC),GADR_OFC                                  
         BNE   S_PYE82H                                                         
*                                                                               
S_PYE82P MVC   PAYEEATN(L'PUBATTN),PUBATTN                                      
         MVC   PAYEETEL(L'PUBTEL),PUBTEL                                        
         MVC   PAYEEFAX(L'PUBSFAXN),PUBSFAXN                                    
*                                                                               
S_PYE84  LR    R2,RF                                                            
         MVI   ELCODE,X'14'        PUBLICATION REP ELEM CODE                    
         USING PUBREPEL,R2                                                      
S_PYE84H BRAS  RE,NXTEL                                                         
         BNE   S_PYE86                                                          
         CLC   PUBRPOFF,=X'FFFFFF' AGENCY SPECIFIC?                             
         BE    S_PYE84P                                                         
         CLC   PUBRPOFF,GADR_CLT   CLIENT SPECIFIC?                             
         BE    S_PYE84P                                                         
         CLI   PUBRPOFF,X'FF'      OFFICE SPECIFIC?                             
         BNE   S_PYE84H                                                         
         CLC   PUBRPOFF+1(L'GADR_OFC),GADR_OFC                                  
         BNE   S_PYE84H                                                         
*                                                                               
S_PYE84P MVC   PAYEECOD,PUBPAREP                                                
*                                                                               
S_PYE86  LR    R2,RF               POINT TO 1ST ELEM                            
         MVI   ELCODE,X'70'        PUBLICATION WEB ADDRESS ELEM CODE            
         BRAS  RE,NXTEL                                                         
         BNE   S_PYE_X                                                          
         USING PUBWEBEL,R2                                                      
         IC    R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PAYEEEML(0),PUBWEBS                                              
*                                                                               
S_PYE_X  J     EXIT                                                             
*                                                                               
NXTEL    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R2),0                                                          
         JNE   NXTEL                                                            
         LTR   R2,R2               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         DROP  RB,R2,RE,RF                                                      
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TRANSLATE PID TO A NAME                                             *         
*                                                                     *         
*        P1       A(PID)                                              *         
*        P2+0(1)  L'RETURN AREA                                       *         
*        P2+1(3)  A(RETURN AREA)                                      *         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
GETWHO   NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         IC    R2,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
         LA    R3,0(R3)                                                         
         L     R1,0(R1)            POINT TO PID                                 
         MVC   SAVEKEY,IOKEY       SAVE THE CURRENT RECORD KEY                  
         OC    0(L'CT0KNUM,R1),0(R1)                                            
         BZ    GETWHO08                                                         
                                                                                
         LA    RE,IOKEY                                                         
         USING CT0REC,RE           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KNUM,0(R1)       SET PID                                      
         L     R1,ALP                                                           
         L     R1,LP_ASECD                                                      
         MVC   CT0KAGY,SECAGY-SECD(R1)                                          
                                                                                
         LHI   R1,IORD+IOCTL+IO1                                                
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BNE   GETWHO08                                                         
                                                                                
         L     RE,AIO1             POINT TO FOUND RECORD                        
         LA    RF,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         USING SAPALEL,RF                                                       
GETWHO02 CLI   SAPALEL,0           CHECK FOR END OF RECORD                      
         BE    GETWHO08                                                         
         CLI   SAPALEL,SAPALELQ    MATCH ON ELEMENT CODE?                       
         BE    *+14                                                             
         IC    R0,SAPALLN          GET ELEMENT LENGTH                           
         AR    RF,R0               BUMP TO NEXT ELEMENT                         
         B     GETWHO02            GO FIND NEXT ELEMENT                         
                                                                                
         TM    DL_FLAG1,GETPIDOQ   GET PID ONLY?                                
         BZ    *+18                                                             
         MVC   WORK(L'SAPALPID),SAPALPID                                        
         LA    R1,8                TOTAL OUTPUT LENGTH                          
         B     GETWHO09                                                         
                                                                                
         LA    RE,IOKEY            READ PRESON RECORD                           
         USING SAPEREC,RE          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
         L     R1,ALP                                                           
         L     R1,LP_ASECD                                                      
         MVC   SAPEAGY,SECAGY-SECD(R1)                                          
         MVC   SAPEPID,SAPALPID    SET USERID FROM PREVIOUS RECORD              
                                                                                
         LHI   R1,IOHI+IOCTL+IO1                                                
         GOTOR (#IOEXEC,AIOEXEC)                                                
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,AIO1             POINT TO FOUND RECORD                        
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),IOKEYSAV                                
         BNE   GETWHO08                                                         
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
GETWHO04 CLI   SANAMEL,0           LOCATE NAME ELEMENT                          
         BE    GETWHO08                                                         
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    *+14                                                             
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     GETWHO04            GO PROCESS NEXT ELEMENT                      
                                                                                
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   GETWHO08            NO NAME IN ELEMENT                           
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         XC    WORK,WORK                                                        
         LA    R1,WORK             BUILD NAME IN WORK AREA                      
                                                                                
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
GETWHO06 IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   GETWHO10            END OF ELEMENT REACHED                       
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         MVI   0(R1),C' '                                                       
         AHI   R1,1                ADD IN A SPACING CHARACTER                   
         B     GETWHO06                                                         
                                                                                
GETWHO08 MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,7                TOTAL OUTPUT LENGTH                          
GETWHO09 SR    RF,RF                                                            
                                                                                
GETWHO10 BCTR  R1,0                                                             
         SR    R1,RF               R1=OUTPUT LENGTH-1                           
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      INIT OUTPUT AREA                             
         CR    R1,R2               NAME TOO LONG?                               
         BNH   *+6                                                              
         LR    R1,R2               USE MAX FOR RETURN AREA                      
         EX    R1,*+8                                                           
         B     GETWHOX                                                          
         MVC   0(0,R3),WORK        RETURN NAME IN OUTPUT AREA                   
                                                                                
GETWHOX  MVC   IOKEY,SAVEKEY       RESTORE SAVED KEY                            
         NI    DL_FLAG1,X'FF'-GETPIDOQ                                          
         J     EXIT                                                             
         DROP  RB,RE,RF                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
B#MED    EQU   3                   MEDIA RECORD                                 
B#CLT    EQU   4                   CLIENT RECORD                                
B#PRD    EQU   5                   PRODUCT RECORD                               
B#EST    EQU   6                   ESTIMATE RECORD                              
B#PUB    EQU   7                   PUBLICATION RECORD                           
B#REP    EQU   8                   PUBLISHER/REP RECORD                         
B#ADC    EQU   9                   AD CODE RECORD                               
B#AD2    EQU   5                   AD CODE RECORD (OUTPUT)                      
B#CON    EQU   5                   CONTRACT RECORD (OUTPUT)                     
B#COM    EQU   B#REP               Comment record                               
                                                                                
NOQ      EQU   C'N'                                                             
YESQ     EQU   C'Y'                                                             
EOR      EQU   0                   End of record element code                   
PCLTFELQ EQU   X'47'               CLIENT FREEZE STATUS ELEMENT                 
                                                                                
EFFS     DC    X'FFFFFFFFFFFF'                                                  
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
                                                                                
FILES    DS    0X                  ** SYSTEM/FILE LIST **                       
         DC    C'PRINT  '          SYSTEM NAME FOR OPEN                         
         DC    C'N'                                                             
PRTDIR   DC    C'PRTDIR '                                                       
         DC    C'N'                                                             
PRTFIL   DC    C'PRTFIL '                                                       
         DC    C'N'                                                             
PUBDIR   DC    C'PUBDIR '                                                       
         DC    C'N'                                                             
PUBFIL   DC    C'PUBFIL '                                                       
         DC    C'N'                                                             
CTFILE   DC    C'CTFILE '                                                       
         DC    C'N'                                                             
GENDIR   DC    C'GENDIR '                                                       
         DC    C'N'                                                             
GENFIL   DC    C'GENFIL '                                                       
         DC    C'X'                                                             
                                                                                
DMKEY    DC    C'DMKEY   '                                                      
                                                                                
       ++INCLUDE SPCGRTAB                                                       
*                                                                               
         DROP                                                                   
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
         LKARY T                                                                
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVE STORAGE **            
                                                                                
VPUBVAL  DS    V                   V(PUBVAL)                                    
VPUBEDIT DS    V                   V(PUBEDIT)                                   
VPPGETAD DS    V                   V(PPGETADR)                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
REQVALS  DS    0A                  ** REQUEST VALUES **                         
                                                                                
AGY      DS    CL(L'LP_AGY)        AGENCY ALPHA                                 
MAP#     DS    CL(L'LP_QMAPN)      MAP NUMBER                                   
                                                                                
MEDIND   DS    X                   MEDIA REQUEST VALUES                         
AMED     DS    AL3                                                              
                                                                                
CLTIND   DS    X                   CLIENT REQUEST VALUES                        
ACLT     DS    AL3                                                              
                                                                                
PRDIND   DS    X                   PRODUCT REQUEST VALUES                       
APRD     DS    AL3                                                              
                                                                                
ESTIND   DS    X                   ESTIMATE REQUEST VALUES                      
AEST     DS    AL3                                                              
                                                                                
REPIND   DS    X                   REP REQUEST VALUES                           
AREP     DS    AL3                                                              
                                                                                
BS#IND   DS    X                   Buy serial# keys                             
ABS#     DS    AL3                                                              
ANXTBS#  DS    A                   A(next Buy serial# key)                      
NUMBS#   DS    XL(L'LW_NUMN)       N'Buy serial# key to process                 
                                                                                
STRDATE  DS    CL(L'PESTST)        REQUEST START DATE                           
ENDDATE  DS    CL(L'PESTEND)       REQUEST END DATE                             
                                                                                
QCOMKEY  DS    CL(L'PCOMKMED+L'PCOMKNUM)                                        
QCOMOP1  DS    XL1                 Comment download option                      
                                                                                
BINSTRDT DS    XL3                 BINARY START DATE                            
BINENDDT DS    XL3                 BINARY END DATE                              
                                                                                
PUB_DFQ  EQU   X'80'               PUBDIR/PUBFIL for NXTREC                     
                                                                                
DL_FLAG1 DS    X                   DOWNLOAD FLAG                                
* free * EQU   X'80'                                                            
* free * EQU   X'40'                                                            
GETPIDOQ EQU   X'20'               GET PID ONLY                                 
                                                                                
DL_FLTR1 DS    X                   DOWNLOAD FILTER OPTION                       
ADCINSRQ EQU   001                 SEND INSTRUCTION COMMENTS                    
                                                                                
PUBCODE  DS    CL15                PUBLICATION CODE                             
PUBKCODE DS    XL(L'PJOBKPUB)      PUBLICATION KEY CODE                         
                                                                                
ADCODE   DS    CL(L'PJOBKJOB)      AD CODE                                      
AD_ID    DS    CL(L'PJOBADID)      AD ID                                        
ADCRTYP  DS    X                   AD CODE REOCRD TYPE                          
ABRTADCQ EQU   X'01'               AD CODE RECORD                               
ABRTINSQ EQU   X'02'               INSTRUCTION RECORD                           
ABRTSUBQ EQU   X'04'               SUB AD CODE INFORMATION                      
ABRTALLQ EQU   X'FF'               EVERYTHING                                   
                                                                                
QACCDOPT DS    CL1                 Accessible client download option            
*                                  Y = Yes, send additional data                
                                                                                
OFCBLK   DS    XL(OFCLENQ)         OFFICER block                                
                                                                                
SVCLTKEY DS    XL(L'IOKEY)         SAVED CLIENT KEY (PRODUCT READS)             
SVPRDKEY DS    XL(L'IOKEY)         SAVED PRODUCT KEY (ESTIMATE READS)           
                                                                                
QMEDCOD  DS    C                   Request media code                           
MEDNAME  DS    CL(L'PAGYNAME)      MEDIA NAME                                   
                                                                                
PRDSEND  DS    C                   YESQ TO SEND PRODUCT DETAILS                 
PUBSEND  DS    C                   YESQ TO SEND PUBLICATION DETAILS             
REPSEND  DS    C                   YESQ TO SEND PUBLISHER/REP DETAILS           
                                                                                
FXCURFRO DS    CL3                 FOREIGN EXCHANGE - "FROM" CURRENCY           
FXCUR_TO DS    CL3                 FOREIGN EXCHANGE - "TO" CURRENCY             
FX_RATE_ DS    CL8                 FOREIGN EXCHANGE RATE                        
                                                                                
ERRMSGST DS    XL8                 ERROR MSG START (DUMMY FLD HEADER)           
ERRORMSG DS    CL60                ERROR MSG                                    
                                                                                
BUYSERKY DS    CL13                Buy serial# key                              
BUYPAYSW DS    CL1                 Buy pay switch                               
ERRMCNUM DS    XL2                 Error map code number                        
                                                                                
CONTNUM  DS    XL(L'PCONNUM)       CONTRACT NUMBER                              
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ADCVALS  DS    0X                  ** EXTRACTED AD CODE VALUES **               
ADCSTRDT DS    XL(L'PJOBSTA)       ADCODE START DATE                            
ADCENDDT DS    XL(L'PJOBEND)       ADCODE END DATE                              
ADCSPDSC DS    CL(L'PJOBSPC)       ADCODE SPACE/DESCRIPTION                     
ADCVALSL EQU   *-ADCVALS                                                        
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
CLTVALS  DS    0X                  ** EXTRACTED CLIENT VALUES **                
CLTFRZTY DS    C                   CLIENT FROZEN TYPE:-                         
CLTFRZTA EQU   C'A'                ...ALL MONTHS FROZEN                         
CLTFRZTP EQU   C'P'                ...MONTH AND PRIOR FROZEN                    
CLTFRZTM EQU   C'M'                ...MONTH ONLY FROZEN                         
CLTFRZTS EQU   C'S'                ...MONTH AND SUBSEQUENT FROZEN               
CLTFRZMO DS    XL(L'PCLTFDTE)      FROZEN MONTH                                 
CLTVALSL EQU   *-CLTVALS                                                        
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ESTVALS  DS    0X                  ** EXTRACTED ESTIMATE VALUES **              
ESTSEND  DS    C                   YESQ TO SEND ESTIMATE VALUES                 
                                                                                
ESTTEST  DS    C                   ESTIMATE TEST STATUS:-                       
ESTTESTY EQU   C'T'                ...ESTIMATE IS TEST                          
ESTSTEWQ EQU   C'S'                ...ESTIMATE IS STEWARD                       
                                                                                
ESTLOCK  DS    C                   ESTIMATE LOCK STATUS:-                       
ESTLOCKY EQU   C'Y'                ...ESTIMATE IS LOCKED                        
                                                                                
EACTTHDT DS    XL3                 EST ACTUALIZ BILL THRU DATE                  
EACLCPID DS    CL(L'SAPALPID)      LAST CHANGE PID                              
EACLCDAT DS    XL3                 LAST CHANGE DATE                             
                                                                                
EST_TYPE DS    C                   Estimate Type                                
IDESKESQ EQU   C'K'                ...iDesk Estimate                            
                                                                                
ESTVALSL EQU   *-ESTVALS                                                        
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
GROUP_ID DS    CL(L'GRPKID*2)      GROUP ID                                     
GROUPCOD DS    CL(L'GRPKCODE*2)    GROUP CODE                                   
                                                                                
FLTPOSQ# DS    XL(L'PO#DID)        PURCHASE ORDER SEQUENCE # FILTER             
FLTPONUM DS    CL(PO#DMXLQ)        PURCHASE ORDER NUMBER FILTER                 
                                                                                
COS2FLAG DS    C                   COS2 Flag                                    
COS2DOLQ EQU   C'$'                                                             
COS2FACQ EQU   C'F'                                                             
                                                                                
REQVALSL EQU   *-REQVALS                                                        
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
ELCODE   DS    X                   ELEM CODE                                    
SAVEKEY  DS    XL(L'IOKEY)                                                      
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER CALLING MODE                   
                                                                                
ANXTMED  DS    A                   A(NEXT MED CODE IN POOL)                     
NMED     DS    H                   N'MED CODES LEFT TO PROCESS                  
                                                                                
ANXTCLT  DS    A                   A(NEXT CLT CODE IN POOL)                     
NCLT     DS    H                   N'CLT CODES LEFT TO PROCESS                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PYE_DATS DS    0X                  PAYEE DATA START                             
PAYEESW1 DS    X                   PAYEE WORK SWITCH 1                          
PAYEEFNQ EQU   X'80'               Payee found                                  
PBREPFNQ EQU   X'40'               PUB REP INFO FOUND                           
                                                                                
SVPYALVL DS    XL(L'PUBRPOFF)      SAVE PAYEE ADDRESS LEVEL                     
                                                                                
PAYEE_VS DS    0X                                                               
PAYEENAM DS    CL(L'PGADNAME)      PAYEE NAME                                   
PAYEEAL1 DS    CL(L'PGADLIN1)      PAYEE ADDRESS LINE 1                         
PAYEEAL2 DS    CL(L'PGADLIN2)      PAYEE ADDRESS LINE 2                         
PAYEEATN DS    CL(L'PGADATTN)      PAYEE ATTENTION                              
PAYEETEL DS    CL(L'PGADTEL)       PAYEE TELEPHONE                              
PAYEEFAX DS    CL(L'PGADFAX)       PAYEE FAX                                    
PAYEEEML DS    CL(L'PGADEADD)      PAYEE E-MAIL ADDRESS                         
PAYEECOD DS    CL(L'PUBPAREP)      PAYEE CODE (PAYING REP)                      
PAYEE_LQ EQU   *-PAYEE_VS                                                       
                                                                                
PYE_DLNQ EQU   *-PYE_DATS          PAYEE DATA LENGTH                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
GRP_DATS DS    0X                  GROUP DATA START                             
GRPSTAT1 DS    X                   GROUP STATUS BYTE                            
GRPCGRPQ EQU   X'80'               SEARCHING CLT GROUP                          
GRPPGRPQ EQU   X'40'               SEARCHING PRD GROUP                          
GRPNOTFQ EQU   X'20'               GROUP NOT FOUND                              
                                                                                
GRP_RDAT DS    0X                  GROUP REPLY DATA                             
GRPCLTCD DS    CL3                 CLT GROUP - CLT CODE                         
GRPCLTC2 DS    CL3                 CLT GROUP - CLT CODE (NO ACCESS)             
GRPPRDCD DS    CL3                 PRD GROUP - PRD CODE                         
GRP_RLNQ EQU   *-GRP_RDAT          GROUP REPLY DATA LENGTH                      
                                                                                
GRP_DLNQ EQU   *-GRP_DATS          GROUP DATA LENGTH                            
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
PO#_RDAT DS    0X                  PURCHASE ORDER REPLY DATA                    
PO#SEQNO DS    XL(L'PO#DID)        PURCHASE ORDER SEQUENCE NUMBER               
PO#NUMBR DS    CL(PO#DMXLQ)        PURCHASE ORDER NUMBER                        
PO#STRDT DS    XL(L'PO#DSTRT)      PURCHASE ORDER START DATE                    
PO#ENDDT DS    XL(L'PO#DEND)       PURCHASE ORDER END DATE                      
PO#AMOUT DS    CL16                PURCHASE ORDER AMOUNT                        
PO#STATU DS    X                   PURCHASE ORDER STATUS                        
PO#SINAQ EQU   X'01'               STATUS - INACTIVE                            
PO#_RLNQ EQU   *-PO#_RDAT          GROUP REPLY DATA LENGTH                      
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
ADC_OUTD DSECT                     AD CODE RECORD VALUES OUTPUT                 
ADC_MEDC DS    CL(L'PJOBKMED)                                                   
ADC_CLTC DS    CL(L'PJOBKCLT)                                                   
ADC_PRDC DS    CL(L'PJOBKPRD)                                                   
ADC_ADCD DS    CL(L'PJOBKJOB)                                                   
ADC_ADID DS    CL(L'PJOBADID)                                                   
ADC_PUBC DS    CL17                                                             
                                                                                
ADC_CAP1 DS    CL(L'PJOBCAP1)                                                   
ADC_CAP2 DS    CL(L'PJOBCAP2)                                                   
ADC_CPY# DS    CL(L'PJOBCPY)                                                    
ADC_SPDS DS    CL(L'PJOBSPC)                                                    
ADC_#LIN DS    CL7                                                              
ADC_#COL DS    CL3                                                              
ADC_LNXC DS    CL8                                                              
ADC_PREM DS    CL3                                                              
ADC_STRD DS    XL(L'PJOBSTA)                                                    
ADC_ENDD DS    XL(L'PJOBEND)                                                    
ADC_PHOU DS    CL(L'PJOBPROD)                                                   
ADC_AGYS DS    CL(L'PJOBSIG)                                                    
ADC_PJOB DS    CL(L'PJOBPJOB)                                                   
ADC_ALLO DS    CL(L'PJOBALO)                                                    
ADC_BCON DS    CL(L'PJOBBLCC)                                                   
ADC_FILT DS    CL(L'PJOBFILT)                                                   
ADC_FSI_ DS    CL(L'PJOBFSI)                                                    
ADC_BREP DS    CL(L'PJOBBREP)                                                   
ADC_PUBL DS    CL(L'PJOBPLIS)                                                   
ADC_STAT DS    XL(L'PJSACH1)                                                    
                                                                                
ADC_COM1 DS    CL65                                                             
ADC_COM2 DS    CL65                                                             
ADC_COM3 DS    CL65                                                             
ADC_COM4 DS    CL65                                                             
ADC_COM5 DS    CL65                                                             
ADC_COM6 DS    CL65                                                             
ADC_COM7 DS    CL65                                                             
ADC_COM8 DS    CL65                                                             
ADC_COM9 DS    CL65                                                             
ADC_COMA DS    CL65                                                             
ADC_COMB DS    CL65                                                             
ADC_COMC DS    CL65                                                             
ADC_COMD DS    CL65                                                             
ADC_COME DS    CL65                                                             
ADC_COMF DS    CL65                                                             
ADC_COMG DS    CL65                                                             
                                                                                
ADC_ADC1 DS    CL(L'PJOBKJOB)                                                   
ADC_AID1 DS    CL(L'PJOBADID)                                                   
ADC_SHR1 DS    CL7                                                              
ADC_ADC2 DS    CL(L'PJOBKJOB)                                                   
ADC_AID2 DS    CL(L'PJOBADID)                                                   
ADC_SHR2 DS    CL7                                                              
ADC_ADC3 DS    CL(L'PJOBKJOB)                                                   
ADC_AID3 DS    CL(L'PJOBADID)                                                   
ADC_SHR3 DS    CL7                                                              
ADC_ADC4 DS    CL(L'PJOBKJOB)                                                   
ADC_AID4 DS    CL(L'PJOBADID)                                                   
ADC_SHR4 DS    CL7                                                              
ADC_ADC5 DS    CL(L'PJOBKJOB)                                                   
ADC_AID5 DS    CL(L'PJOBADID)                                                   
ADC_SHR5 DS    CL7                                                              
ADC_ADC6 DS    CL(L'PJOBKJOB)                                                   
ADC_AID6 DS    CL(L'PJOBADID)                                                   
ADC_SHR6 DS    CL7                                                              
ADC_ADC7 DS    CL(L'PJOBKJOB)                                                   
ADC_AID7 DS    CL(L'PJOBADID)                                                   
ADC_SHR7 DS    CL7                                                              
ADC_ADC8 DS    CL(L'PJOBKJOB)                                                   
ADC_AID8 DS    CL(L'PJOBADID)                                                   
ADC_SHR8 DS    CL7                                                              
ADC_ADC9 DS    CL(L'PJOBKJOB)                                                   
ADC_AID9 DS    CL(L'PJOBADID)                                                   
ADC_SHR9 DS    CL7                                                              
ADC_ADCA DS    CL(L'PJOBKJOB)                                                   
ADC_AIDA DS    CL(L'PJOBADID)                                                   
ADC_SHRA DS    CL7                                                              
                                                                                
ADC_LENQ EQU   *-ADC_OUTD                                                       
                                                                                
                                                                                
CON_OUTD DSECT                     CONTRACT RECORD VALUES OUTPUT                
CON_MEDC DS    CL(L'PCONKMED)                                                   
CON_CLTC DS    CL(L'PCONKCLT)                                                   
CON_PUBC DS    CL17                                                             
CON_CON# DS    XL(L'PCONNUM)                                                    
CON_STRD DS    XL(L'PCONSDT)                                                    
CON_ENDD DS    XL(L'PCONEDT)                                                    
CON_PRDC DS    CL(L'PCONPRD)                                                    
CON_LENQ EQU   *-CON_OUTD                                                       
                                                                                
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PUBRECD  DSECT                     PUBLICATION RECORD                           
PUBKCODQ EQU   X'81'                                                            
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
       ++INCLUDE PUBSADEL                                                       
       ++INCLUDE PUBREPEL                                                       
       ++INCLUDE PUBWEBEL                                                       
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PREPRECD DSECT                     REP RECORD                                   
PREPKRCQ EQU   X'11'               PUBLISHER/REP RECORD CODE                    
       ++INCLUDE PREPREC                                                        
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PJOBRECD DSECT                     AD CODE RECORD                               
PJOBKRCQ EQU   X'15'                                                            
       ++INCLUDE PJOBREC                                                        
                                                                                
PCONRECD DSECT                     CONTRACT RECORD                              
PCONKRCQ EQU   X'10'                                                            
       ++INCLUDE PCONREC                                                        
                                                                                
PCOMRECD DSECT                     Comment RECORD                               
PCOMKRCQ EQU   X'40'                                                            
       ++INCLUDE PCOMREC                                                        
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PGENGRP           CLT/PRD/PUB GROUP RECORD                     
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPGENPO#          PURCHASE ORDER RECORD                        
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PSERRECD DSECT                                                                  
       ++INCLUDE PPSERPP           Buy serial#                                  
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
PBUYRECD DSECT                                                                  
       ++INCLUDE PBUYREC           Buy record                                   
PBUYKRCQ EQU   X'20'                                                            
PBUYFRST DS 0X                                                                  
       ++INCLUDE PBDELEM                                                        
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
GETADRPD DSECT                     PPGETADR PARAMETER BLOCK                     
GADR_AGY DS    CL(L'PCLTKAGY)                                                   
GADR_MED DS    CL(L'PCLTKMED)                                                   
GADR_CLT DS    CL(L'PCLTKCLT)                                                   
GADR_OFC DS    CL(L'PCLTOFF)                                                    
         PRINT ON                                                               
                                                                                
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE GEGENEXC                                                       
       ++INCLUDE DDOFFICED                                                      
                                                                                
PBYPAY_D DSECT                                                                  
       ++INCLUDE PPAYELEM                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028PPLNK10   05/10/16'                                      
         END                                                                    
