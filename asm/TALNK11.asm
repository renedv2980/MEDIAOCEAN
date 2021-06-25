*          DATA SET TALNK11    AT LEVEL 003 AS OF 05/29/15                      
*PHASE T70411E                                                                  
TALNK11  TITLE '- TALENT - GENERAL DOWNLOADS'                                   
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,CODE=CODE,REQUEST=*,SYSTEM=TALSYSQ,              *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                             
                                                                                
B#STFREC EQU   3                   I/O AREA FOR STAFF RECORDS                   
B#CLIREC EQU   B#STFREC            I/O AREA FOR CLIENT RECORDS                  
B#CMTREC EQU   B#STFREC            I/O AREA FOR COMMENT RECORDS                 
B#HCMREC EQU   B#STFREC            I/O AREA FOR HISTORY COMMENT RECORDS         
                                                                                
SSNTAB   EQU   (2000*L'TLW4SSN)+1                                               
COMTAB   EQU   (2000*L'TLCOBCOM)+1                                              
                                                                                
ERRTAB   EQU   7500                                                             
SVPTRBLK EQU   (520*L'TLDRREC)+1                                                
UPPTRBLK EQU   (520*L'TLDRREC)+1                                                
WORKLNQ  EQU   ERRTAB+SVPTRBLK+UPPTRBLK                                         
                                                                                
CODE     NMOD1 WORKLNQ,**TA11**,RR=RE                                           
         LR    RF,RC                                                            
         LR    R5,R1                                                            
         USING LP_D,R5             R5=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
                                                                                
         L     R9,LP_ABLK1         ROOT PASSES A(WORKD) IN BLOCK 1              
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2         ROOT PASSES A(SAVED) IN BLOCK 2              
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         MVC   RUNMODE,RUNPMODE    EXTRACT CALLING MODE                         
                                                                                
         LR    RE,RF                                                            
         ST    RE,ASSNTAB          SAVED A(SSN TABLE) FOR W4/CST SEARCH         
         ST    RE,AAGTTAB          SAVED A(AGENT TABLE) FOR AGT SEARCH          
         AHI   RE,SSNTAB                                                        
         ST    RE,ACOMTAB          SAVED A(COM TABLE) FOR CAST SEARCH           
                                                                                
         ST    RF,AERRTAB          SAVE A(ERROR TABLE) FOR RELEASE              
         AHI   RF,ERRTAB                                                        
         ST    RF,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RF,SVPTRBLK                                                      
         ST    RF,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE FOR RUNNING                                       *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST 'FIRST FOR RUN' MODE                    
         BNE   PRCWRK                                                           
                                                                                
         MVC   LP_BLKS+((B#STFREC-1)*L'LP_BLKS),AIO2                            
         MVC   WVALUES(WVALUEL),LVALUES                                         
                                                                                
         LA    R1,ADCONS           RELOCATE ADDRESS CONSTANTS                   
         LHI   R0,ADCONN                                                        
         L     RF,SRVRRELO                                                      
         BASR  RE,0                                                             
         L     R2,0(R1)                                                         
         AR    R2,RF                                                            
         ST    R2,0(R1)                                                         
         AHI   R1,L'ADCONS                                                      
         BCTR  R0,RE                                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        LVALUES MUST MATCH WVALUES                                   *         
***********************************************************************         
                                                                                
LVALUES  DS    0D                                                               
         DC    A(STFKEYT)          STAFF RECORD                                 
         DC    A(CLIKEYT)          CLIENT RECORD                                
         DC    A(CGRKEYT)          CLIENT GROUP PASSIVE                         
         DC    A(ACMKEYR)          ATS COMMENT RECORD                           
         DC    A(TCMKEYR)          TPC COMMENT RECORD                           
         DC    A(HCMKEYR)          HISTORY COMMENT RECORD                       
                                                                                
         DC    XL4'FFFFFFFF'                                                    
                                                                                
         DC    AL2(I#STVDLD)       STAFF VALIDATION DOWNLOAD                    
         DC    AL2(I#STFDLD)       STAFF DOWNLOAD                               
         DC    AL2(I#RELDLD)       RELEASE DOWNLOAD                             
         DC    AL2(I#W4SDLD)       W4 SEARCH DOWNLOAD                           
         DC    AL2(I#ANSDLD)       AGENT SEARCH DOWNLOAD                        
                                                                                
         DC    5X'00',X'01',(L'TAVACLI)X'FF'        CLIRANGE                    
         DC    (L'TLCMVER)X'0',(L'TLCMVER)X'1'      VERRANGE                    
         DC    X'00',X'FF'                          ALLRANG1                    
         DC    6X'00',6X'FF'                        ALLRANG6                    
                                                                                
USELST#  DC    AL2(USELSTN)        USELST                                       
USELSTS  DS    0CL3                                                             
USEADH   DC    C'ADH'                                                           
USEHLD   DC    C'HLD'                                                           
USESHL   DC    C'SHL'                                                           
USELSTN  EQU   (*-USELSTS)/L'USELSTS                                            
USELSTL  EQU   *-USELST#                                                        
         EJECT                                                                  
***********************************************************************         
*        FIRST FOR NEW WORK                                           *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         LA    R0,REQVALS                                                       
         LHI   R1,REQVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        RUN A DOWNLOAD REQUEST                                       *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   YES                                                              
                                                                                
         LA    R0,OUTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,OUTVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   COMPANY,LP_AGYB     SET COMPANY CODE                             
         MVC   CPYALPH,LP_AGY      SET COMPANY ID                               
         MVC   MAP,LP_QMAPN        SET PROCESSING MAP NUMBER                    
                                                                                
         CLC   MAP,STVDLD          ONLY FOR STAFF VALIDATION DOWNLOAD           
         JE    RREQ10                                                           
         CLC   MAP,STFDLD          AND STAFF DOWNLOAD                           
         JNE   RREQ20                                                           
RREQ10   OC    REQVALS(RQSTFLNQ),SPACES  SET AS UPPERCASE                       
                                                                                
RREQ20   GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS STAFF NAME RECORD                                    *         
***********************************************************************         
                                                                                
NXTNAM   J     *+12                                                             
         DC    C'*NXTNAM*'                                                      
         LR    RB,RF                                                            
         USING NXTNAM,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NOMORE                                                           
         XC    NAMVALS(NAMVALL),NAMVALS                                         
         XC    STAFFKEY,STAFFKEY                                                
                                                                                
         USING TLSTD,R1                                                         
         LA    R1,IOKEY                                                         
         XC    TLSTKEY,TLSTKEY                                                  
         MVI   TLSTCD,TLSTCDQ                                                   
         MVC   TLSTUSER,LP_USRID                                                
         MVC   TLSTSTAF,RQI2STF                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NNAMISTF                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         L     R1,IOADDR           R1=A(STAFF RECORD)                           
         LA    R2,TLSTELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TASTD,R2                                                         
NNAM10   CLI   0(R2),0             BUMP TO STAFF ELEMENT                        
         JE    NNAMIPSW                                                         
         CLI   0(R2),TASTELQ                                                    
         JE    NNAM20                                                           
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NNAM10                                                           
                                                                                
NNAM20   OC    RQI2PSW,SPACES                                                   
         OC    TASTPWD,SPACES                                                   
         CLC   RQI2PSW,TASTPWD     CHECK PASSWORD                               
         BNE   NNAMIPSW                                                         
                                                                                
         MVC   NAMNAME(L'TASTFST),TASTFST  RETURN FIRST NAME                    
                                                                                
         LA    RE,NAMNAME+L'TASTFST                                             
NNAM30   CLI   0(RE),C' '                                                       
         JH    NNAM40                                                           
         SHI   RE,1                                                             
         J     NNAM30                                                           
                                                                                
NNAM40   LA    RE,2(RE)                                                         
         MVC   0(L'TASTLST,RE),TASTLST     RETURN LAST NAME                     
         DROP  R2                                                               
                                                                                
         MVC   STAFFKEY,IOKEY              SAVE STAFF KEY                       
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,NAMVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
NNAMISTF MVC   LP_ERROR,=AL2(353)                                               
         J     NNAMEXIT                                                         
                                                                                
NNAMIPSW MVC   LP_ERROR,=AL2(141)                                               
         J     NNAMEXIT                                                         
                                                                                
NNAMEXIT MVI   LP_EMSYS,70                                                      
         MVI   LP_RMODE,LP_RERRR                                                
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS COMMERCIAL TYPE RECORD                               *         
***********************************************************************         
                                                                                
NXTCOT   J     *+12                                                             
         DC    C'*NXTCOT*'                                                      
         LR    RB,RF                                                            
         USING NXTCOT,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NCOT10                                                           
         LA    RE,COTTAB                                                        
         ST    RE,ANXTCOT                                                       
                                                                                
         USING COTTABD,R4                                                       
NCOT10   L     R4,ANXTCOT          R4=A(CURRENT COTYPE TABLE ENTRY)             
                                                                                
         CLI   0(R4),X'FF'         EXIT IF ALL COMMERCIAL TYPES HAVE            
         JE    NOMORE              BEEN PROCESSED                               
                                                                                
         MVC   COTCODE,COTCD       RETURN COMMERCIAL TYPE CODE                  
         MVC   COTDESC,COTDS       AND DESCRIPTION                              
                                                                                
         LA    R4,COTLNQ(R4)       SAVE ADDRESS OF NEXT COMMERCIAL              
         ST    R4,ANXTCOT          TYPE TABLE ENTRY                             
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,COTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
COTTAB   DC    C'A',CL12'ADDENDUM'                                              
         DC    C'B',CL12'PUBLIC SERVICE'                                        
         DC    C'E',CL12'SEASONAL'                                              
         DC    C'F',CL12'FOREIGN'                                               
         DC    C'N',CL12'ANIMATICS'                                             
         DC    C'R',CL12'PROMO'                                                 
         DC    C'S',CL12'SPANISH'                                               
         DC    C'T',CL12'SHORT TERM'                                            
         DC    C'X',CL12'ASIAN'                                                 
         DC    C'7',CL12'COMM.TYPE NAME'                                        
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS LIMITED AGENCY RECORD                                *         
***********************************************************************         
                                                                                
NXTALM   J     *+12                                                             
         DC    C'*NXTALM*'                                                      
         LR    RB,RF                                                            
         USING NXTALM,RB                                                        
                                                                                
         L     R2,ACURTAVA         DEFAULT TO LAST ELEMENT ADDRESS              
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NALM40                                                           
         XC    ALMVALS(ALMVALL),ALMVALS                                         
         B     NALM20                                                           
                                                                                
NALM10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLC   IOKEY(TLSTSSEQ-TLSTD),STAFFKEY                                   
         JNE   NOMORE                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLSTD,R1                                                         
NALM20   L     R1,IOADDR           R1=A(STAFF RECORD)                           
         LA    R2,TLSTELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TAVAD,R2                                                         
NALM30   CLI   0(R2),0             BUMP TO AGENCY/CLIENT LIMIT                  
         JE    NALM10                                                           
         CLI   0(R2),TAVAELQ                                                    
         JNE   NALM40                                                           
         CLC   TAVAAGY,ALMAGY                                                   
         JNE   NALM50                                                           
NALM40   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NALM30                                                           
                                                                                
NALM50   ST    R2,ACURTAVA         SAVE A(CURRENT AGY/CLI LIMIT ELEM)           
                                                                                
         MVC   ALMAGY,TAVAAGY      RETURN LIMITED AGENCY                        
         DROP  R2                                                               
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,ALMVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS LIMITED CLIENT RECORD                                *         
***********************************************************************         
                                                                                
NXTCLM   J     *+12                                                             
         DC    C'*NXTCLM*'                                                      
         LR    RB,RF                                                            
         USING NXTCLM,RB                                                        
                                                                                
         XC    CLMCLI,CLMCLI                                                    
         MVC   CURRCLI,=6X'FF'                                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NCLM10                                                           
         XC    CLMVALS(CLMVALL),CLMVALS                                         
         XC    LASTCLI,LASTCLI                                                  
                                                                                
         USING TLSTD,R1                                                         
NCLM10   L     R1,IOADDR                                                        
         CLC   TLSTKEY,STAFFKEY    GET PRIMARY STAFF RECORD INTO                
         JE    NCLM40              IO AREA                                      
                                                                                
         LA    R1,IOKEY                                                         
         MVC   TLSTKEY,STAFFKEY                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    NCLM30                                                           
         DC    H'00'                                                            
                                                                                
NCLM20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLC   IOKEY(TLSTSSEQ-TLSTD),STAFFKEY                                   
         JNE   NCLM110                                                          
                                                                                
NCLM30   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         L     R1,IOADDR           R1=A(STAFF RECORD)                           
NCLM40   LA    R2,TLSTELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TAVAD,R2                                                         
NCLM50   CLI   0(R2),0             BUMP TO AGENCY/CLIENT LIMIT                  
         JE    NCLM20                                                           
         CLI   0(R2),TAVAELQ                                                    
         JNE   NCLM60                                                           
         CLI   TAVALEN,TAVALNQ     ELEMENT MUST CONTAIN AT LEAST                
         JNE   NCLM70              ONE CLIENT                                   
NCLM60   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NCLM50                                                           
                                                                                
NCLM70   LA    RE,TAVACLI          RE=A(CURRENT CLIENT IN ELEMENT)              
         ZIC   RF,TAVALEN                                                       
         SHI   RF,TAVALNQ          RF=L'CLIENTS IN ELEMENT                      
NCLM80   CLC   0(L'TAVACLI,RE),LASTCLI                                          
         JNH   NCLM90                                                           
         CLC   0(L'TAVACLI,RE),CURRCLI                                          
         JNH   NCLM100                                                          
NCLM90   LA    RE,L'TAVACLI(RE)                                                 
         SHI   RF,L'TAVACLI                                                     
         LTR   RF,RF                                                            
         JNZ   NCLM80                                                           
         J     NCLM60                                                           
                                                                                
NCLM100  MVC   CURRCLI,0(RE)       RETURN LIMITED CLIENT                        
         J     NCLM60                                                           
         DROP  R2                                                               
                                                                                
NCLM110  CLC   CURRCLI,=6X'FF'     IF THERE IS A CLIENT CODE TO RETURN          
         JE    NOMORE                                                           
         MVC   CLMCLI,CURRCLI      RETURN IT NOW                                
         MVC   LASTCLI,CURRCLI                                                  
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CLMVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT STAFF SEARCH STATUS RECORD                            *         
***********************************************************************         
                                                                                
NXTSTS   J     *+12                                                             
         DC    C'*NXTSTS*'                                                      
         LR    RB,RF                                                            
         USING NXTSTS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         MVI   STSSTAT,STSSINV                                                  
         BAS   RE,STVALREQ         VALIDATE REQUEST                             
         JNE   NSTSX                                                            
                                                                                
         XC    STAFFKEY,STAFFKEY                                                
         MVI   STSSTAT,0                                                        
                                                                                
         USING CTIREC,R2                                                        
         LA    R2,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(L'RQUSRID),RQUSRID                                        
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO3'                            
         JNE   NSTSX                                                            
         DROP  R2                                                               
                                                                                
         USING CTIREC,R2                                                        
         L     R2,AIO3                                                          
                                                                                
         USING CTDSCD,R3                                                        
         LA    R3,CTIDATA                                                       
NSTS10   CLI   0(R3),0             PROCESS RECORD                               
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),CTDSCELQ      DESCRIPTION ELEMENT (X'02')                  
         JNE   *+14                                                             
         MVC   USERID,CTDSC        SAVE HEX USERID                              
         J     NSTS20                                                           
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         J     NSTS10                                                           
         DROP  R2,R3                                                            
                                                                                
NSTS20   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ASTFKEYT),('B#STFREC',0),*        
               SAVED,0,0                                                        
         JNE   NSTSX                                                            
                                                                                
         USING TASTD,R4                                                         
         CLC   MAP,STVDLD          IF STAFF VALIDATION DOWNLOAD                 
         JNE   NSTS30                                                           
         L     R4,IOADDR                                                        
         MVI   ELCODE,TASTELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   NSTSX                                                            
         CLC   RQSTFPW,TASTPWD     PASSWORD MUST MATCH TO CONTINUE              
         BNE   NSTSX                                                            
         DROP  R4                                                               
                                                                                
NSTS30   MVC   STAFFKEY,IOKEY                                                   
                                                                                
NSTSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,STSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
                                                                                
         CLI   RQSTRST,C'Y'                                                     
         JNE   NSTSX10                                                          
         CLI   STSSTAT,STSSINV                                                  
         JE    NO                                                               
         MVI   STSSTAT,STSSSUC                                                  
         OC    STAFFKEY,STAFFKEY                                                
         JNZ   YES                                                              
         MVI   STSSTAT,STSSUNS                                                  
         J     NO                                                               
                                                                                
NSTSX10  MVI   STSSTAT,0                                                        
         OC    STAFFKEY,STAFFKEY                                                
         JNZ   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
STVALREQ NTR1                                                                   
         CLC   RQUSRID,SPACES      ASSERT THAT USER ID IS PROVIDED              
         JE    NO                                                               
         OC    RQSTFCD,SPACES      ASSERT THAT STAFF ID IS PROVIDED             
         JE    NO                                                               
                                                                                
         CLC   MAP,STVDLD          IF STAFF VALIDATION DOWNLOAD                 
         JNE   SVR10                                                            
         CLC   RQSTFPW,SPACES      ASSERT THAT PASSWORD IS PROVIDED             
         JNE   SVR20                                                            
         J     NO                                                               
                                                                                
SVR10    CLC   RQSTFPW,SPACES      IF STAFF SEARCH DOWNLOAD                     
         JNE   NO                  ASSERT THAT PASSWORD IS NOT PROVIDED         
                                                                                
SVR20    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQSTRST)                        
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        READ STAFF RECORD                                            *         
***********************************************************************         
                                                                                
NXTSTF   J     *+12                                                             
         DC    C'*NXTSTF*'                                                      
         LR    RB,RF                                                            
         USING NXTSTF,RB                                                        
         CLI   LP_RMODE,LP_RFRST   TEST FIRST TIME                              
         JNE   NOMORE                                                           
                                                                                
         OC    STAFFKEY,STAFFKEY                                                
         JZ    NO                                                               
                                                                                
         XC    STFVALS(STFVALL),STFVALS                                         
                                                                                
         USING TLSTD,R2            R2=A(STAFF RECORD)                           
         L     R2,IOADDR                                                        
                                                                                
         MVC   STAFFUID,RQUSRID    USER ID                                      
         MVC   STAFFCD,TLSTSTAF    STAFF CODE                                   
                                                                                
         XC    SVCLIGRP,SVCLIGRP   CLEAR AREA FOR ANY CLIENT GROUPS             
                                                                                
         USING TASTD,R3                                                         
         LA    R3,TLSTELEM                                                      
NXTSTF30 CLI   0(R3),0             1ST CHECK FOR A CLIENT GROUP                 
         JE    NXTSTF40                                                         
         CLI   0(R3),TASTELQ       STAFF ELEMENT (X'12')                        
         BNE   NXTSTF35                                                         
         MVC   SVCLIGRP,TASTCLG                                                 
         CLC   MAP,STFDLD                                                       
         BNE   NXTSTF40                                                         
         MVC   STAFFPHO,TASTTEL                                                 
         B     NXTSTF40                                                         
NXTSTF35 SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTSTF30                                                         
         DROP  R3                                                               
                                                                                
NXTSTF40 LA    R3,TLSTELEM                                                      
NXTSTF50 CLI   0(R3),0             PROCESS RECORD                               
         JE    NXTSTFX                                                          
         CLI   0(R3),TASTELQ       STAFF ELEMENT (X'12')                        
         BE    NXTSTF70                                                         
         CLI   0(R3),TACMELQ       VARIABLE LENGTH COMMENT ELM (X'8D')          
         BE    NXTSTF90                                                         
NXTSTF60 SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     NXTSTF50                                                         
         DROP  R2                                                               
                                                                                
         USING TASTD,R3                                                         
NXTSTF70 MVC   STAFFFN,TASTFST     FIRST NAME                                   
         MVC   STAFFLN,TASTLST     LAST NAME                                    
         MVC   STAFFTP,TASTTYPE    TYPE                                         
                                                                                
         MVI   STAFFST1,C'Y'       SEND CIHR STATUS                             
         MVI   STAFFST2,C'Y'       SEND ACTION FORMS STATUS                     
         MVI   STAFFST3,C'N'                                                    
         MVI   STAFFST4,C'N'                                                    
         MVI   STAFFST5,C'N'                                                    
         MVI   STAFFST6,C'N'                                                    
         MVI   STAFFST7,C'N'                                                    
         MVI   STAFFST8,C'N'                                                    
         B     NXTSTF60                                                         
         DROP  R3                                                               
                                                                                
         USING TACMD,R3                                                         
NXTSTF90 CLI   TACMTYPE,TACMTYPI   EMAIL?                                       
         BNE   NXTSTF60                                                         
         SR    RF,RF               COMMENT ELEMENT                              
         IC    RF,TACMLEN                                                       
         SHI   RF,4                                                             
         BM    NXTSTF60                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   STAFFEML(0),TACMCOMM                                             
         B     NXTSTF60                                                         
         DROP  R3                                                               
                                                                                
NXTSTFX  LA    R0,STFVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
*        READ WORKGROUP RECORD                                        *         
***********************************************************************         
                                                                                
NXTLIM   J     *+12                                                             
         DC    C'*NXTLIM*'                                                      
         LR    RB,RF                                                            
         USING NXTLIM,RB                                                        
                                                                                
         OC    STAFFKEY,STAFFKEY   DO NOT RUN IF STAFF RECORD                   
         JZ    NOMORE              DOES NOT EXIST                               
                                                                                
         L     R2,ACURTAVA         DEFAULT TO LAST ELEMENT ADDRESS              
         CLI   LP_RMODE,LP_RFRST   AND SKIP AHEAD                               
         JNE   NLIM40                                                           
         J     NLIM20                                                           
                                                                                
NLIM10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLC   IOKEY(TLSTSSEQ-TLSTD),STAFFKEY                                   
         JNE   NOMORE                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
NLIM20   XC    ACURTAVA,ACURTAVA   INITIALIZE VARIABLES FOR NEW RECORD          
                                                                                
         USING TLSTD,R1                                                         
         L     R1,IOADDR           R1=A(STAFF RECORD)                           
         LA    R2,TLSTELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TAVAD,R2                                                         
NLIM30   CLI   0(R2),0             BUMP TO AGENCY/CLIENT LIMIT                  
         JE    NLIM10                                                           
         CLI   0(R2),TAVAELQ                                                    
         JNE   NLIM40                                                           
         CLC   TAVAAGY,ALMAGY                                                   
         JNE   NLIM50                                                           
NLIM40   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NLIM30                                                           
                                                                                
NLIM50   ST    R2,ACURTAVA         SAVE A(CURRENT AGY/CLI LIMIT ELEM)           
         XC    LIMVALS(LIMVALL),LIMVALS                                         
                                                                                
         OC    TAVAAGY,TAVAAGY                                                  
         BZ    NLIM40                                                           
         MVC   LIMAGY,TAVAAGY      PASS DOWN AGENCY CODE                        
                                                                                
         ZIC   RF,TAVALEN                                                       
         SHI   RF,TAVALNQ                                                       
         LTR   RF,RF               IF NO CLIENTS, DONE BUILDING THIS            
         JZ    NLIMX               ELEMENT                                      
                                                                                
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   LIMENT(0),TAVACLI   ELSE PASS DOWN CLIENTS                       
                                                                                
         XR    RE,RE                                                            
         D     RE,=A(L'TAVACLI)                                                 
         STH   RF,LIMNUM                                                        
         DROP  R2                                                               
                                                                                
NLIMX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,LIMVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS RELEASE STATUS RECORD                                *         
***********************************************************************         
                                                                                
NXTREL   J     *+12                                                             
         DC    C'*NXTREL*'                                                      
         LR    RB,RF                                                            
         USING NXTREL,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NREL10                                                           
         XC    RELVALS(RELVALL),RELVALS                                         
         MVI   RELFLAG,0                                                        
                                                                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQRLCIN                                 
                                                                                
         ZICM  RF,ARQRLEO,3        REFORMAT ERROR OVERRIDE ARRAY                
         MVC   IEROVER,9(RF)                                                    
         LA    RF,10(RF)                                                        
         STCM  RF,7,AEROVER                                                     
                                                                                
         XC    IXEROVER(4),IXEROVER                                             
                                                                                
         L     RF,AERRTAB                                                       
         MVI   0(RF),X'FF'         INITIALIZE ERROR TABLE                       
                                                                                
NREL10   TM    RELFLAG,EXECUTED    IF ROUTINE ALREADY EXECUTED                  
         JO    NOMORE              DO NOT PERFORM AGAIN                         
                                                                                
         LA    R1,IOKEY            R1=A(KEY)                                    
                                                                                
         BAS   RE,VALAGY           VALIDATE AGENCY                              
         JNE   NREL20                                                           
                                                                                
         BAS   RE,VALCLI           VALIDATE CLIENT                              
         JNE   NREL20                                                           
                                                                                
         BAS   RE,VALCST           VALIDATE CAST                                
         JNE   NREL20                                                           
                                                                                
         BAS   RE,VALCOM           VALIDATE COMMERCIAL                          
                                                                                
NREL20   MVI   RELSTAT,RELSTER     IF AN ERROR HAS BEEN ENCOUNTERED             
         L     RF,AERRTAB                                                       
         CLI   0(RF),X'FF'         RETURN "NOT OK" STATUS                       
         JNE   NRELX                                                            
         MVI   RELSTAT,RELSTOK1    ELSE RETURN "OK" STATUS                      
         TM    CASTSTAT,SPLITCST                                                
         BZ    NREL30                                                           
         MVI   RELSTAT,RELSTOK2                                                 
                                                                                
NREL30   CLI   RQRLMOD,RQRLEXE     IF MODE IS EXECUTE                           
         JNE   NRELX                                                            
         TM    CASTSTAT,SPLITCST   AND CAST IS NOT SPLIT                        
         JO    NREL40                                                           
         L     R3,ATACOEL                                                       
         BAS   RE,UPDCOMML         RELEASE/UPDATE COMMERCIAL                    
         J     NREL50                                                           
                                                                                
NREL40   BAS   RE,UPDCAST          IF CST IS SPLIT, LAST SERV THE CAST          
                                                                                
NREL50   BAS   RE,ADDWTR           ADD WEB TRANSACTION RECORD                   
                                                                                
         MVC   RELSTAF,RQRLSTF     RETURN STAFF ID AND DATE                     
         GOTO1 VDATCON,DMCB,(5,0),(8,RELDATE)                                   
         J     NRELX                                                            
                                                                                
***********************************************************************         
*        VALIDATE AGENCY                                              *         
*        ON ENTRY ... R1=A(IOKEY)                                               
***********************************************************************         
                                                                                
VALAGY   NTR1                                                                   
         USING TLAYD,R1                                                         
         XC    TLAYKEY,TLAYKEY     READ FOR AGENCY KEY                          
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,RQRLAGY                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VAGY10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERNLUAGY                                  
         J     NO                                                               
         DROP  R1                                                               
                                                                                
VAGY10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLAYD,R1                                                         
         L     R1,IOADDR           R1=A(AGENCY RECORD)                          
         LA    R2,TLAYELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TAAYD,R2                                                         
VAGY20   CLI   0(R2),0             BUMP TO AGENCY ELEMENT                       
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TAAYELQ                                                    
         JE    VAGY30                                                           
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VAGY20                                                           
                                                                                
VAGY30   TM    TAAYSTA3,TAAYSLCK   AGENCY CANNOT BE LOCKED                      
         JZ    VAGY40                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERAGYLCK                                  
         J     NO                                                               
                                                                                
VAGY40   MVC   SVAYSTA5,TAAYSTA5   SAVE 5TH STATUS                              
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        VALIDATE CLIENT                                              *         
*        ON ENTRY ... R1=A(IOKEY)                                               
***********************************************************************         
                                                                                
VALCLI   NTR1                                                                   
         USING TLCLD,R1                                                         
         TM    SVAYSTA5,TAAYCIHR   IF AGENCY IS NOT ENABLED FOR CIHR            
         JO    YES                                                              
         XC    TLCLKEY,TLCLKEY     READ FOR CLIENT KEY                          
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,RQRLAGY                                                  
         MVC   TLCLCLI,RQRLCLI                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   VCLI30                                                           
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLCLD,R1                                                         
         L     R1,IOADDR           R1=A(CLIENT RECORD)                          
         LA    R2,TLCLELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TACID,R2                                                         
VCLI10   CLI   0(R2),0             BUMP TO CLIENT INFORMATION ELEMENT           
         JE    VCLI30                                                           
         CLI   0(R2),TACIELQ                                                    
         JE    VCLI20                                                           
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VCLI10                                                           
                                                                                
VCLI20   TM    TACISTA2,TACICIHR   AGENCY/CLIENT MUST BE ELIGIBLE               
         JO    YES                                                              
VCLI30   GOTOR (#ADDERR,AADDERR),DMCB,ERACNELI                                  
         J     NO                                                               
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        VALIDATE CAST                                                *         
*        ON ENTRY ... R1=A(IOKEY)                                               
***********************************************************************         
                                                                                
VALCST   NTR1                                                                   
         USING TLCAPD,R1                                                        
         XC    TLCAPKEY,TLCAPKEY   READ FOR CAST HOLDING FEE KEY                
         MVI   TLCAPCD,TLCAHCDQ                                                 
         MVC   TLCAHCOM,RQRLCOM                                                 
         MVI   TLCAHSRT+5,1                                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VCST20                                                           
                                                                                
VCST10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
                                                                                
VCST20   LA    R1,IOKEY            R1=A(CAST HOLDING FEE KEY)                   
         CLI   TLCAPCD,TLCAHCDQ                                                 
         JNE   VCST230                                                          
         CLC   TLCAHCOM,RQRLCOM                                                 
         JNE   VCST230                                                          
         MVC   SVCAHKEY,TLCAPKEY   SAVE CAST HOLDING FEE KEY                    
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLCAD,R1                                                         
         L     R1,IOADDR           R1=A(CAST RECORD)                            
         LA    R2,TLCAELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TACAD,R2                                                         
VCST30   CLI   0(R2),0             BUMP TO CAST DETAILS ELEMENT                 
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TACAELQ                                                    
         JE    VCST40                                                           
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VCST30                                                           
                                                                                
VCST40   OC    TACALAST,TACALAST   IF CAST HAS LAST SERVICES DATE               
         JNZ   VCST10              DO NOT CONSIDER IT                           
                                                                                
VCST50   ZIC   RE,RQRLCIN          RE=CAST COUNTER                              
         LTR   RE,RE                                                            
         JNZ   *+6                                                              
         DC    H'00'                                                            
                                                                                
         ZICM  RF,ARQRLCS,3        RF=A(CAST LIST)                              
                                                                                
VCST60   CLC   TLCASEQ,0(RF)       IF ALL CAST ARE NOT ON THE                   
         JE    VCST70              NOTICE                                       
         LA    RF,2(RF)                                                         
         BCT   RE,VCST60                                                        
         OI    CASTSTAT,SPLITCST   SET SPLIT CYCLE INDICATOR                    
         J     VCST10                                                           
                                                                                
VCST70   OI    CASTSTAT,ELIGBCST   SET ELIGIBLE CAST FOUND                      
         MVC   SVGUA,TACAGUA       AND SAVE GUARANTEE CODE                      
         DROP  R2                                                               
                                                                                
         L     R1,IOADDR           R1=A(CAST RECORD)                            
         LA    R2,TLCAELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TACRD,R2                                                         
VCST80   CLI   0(R2),0             BUMP TO APPLIED CREDIT ELEMENT               
         JE    VCST120                                                          
         CLI   0(R2),TACRELQ                                                    
         JE    VCST100                                                          
VCST90   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VCST80                                                           
                                                                                
VCST100  CLC   TACRUSE,=C'HLD'                                                  
         JE    VCST110                                                          
         CLC   TACRUSE,=C'SHL'                                                  
         JE    VCST110                                                          
         CLC   TACRUSE,=C'ADH'                                                  
         JNE   VCST90                                                           
VCST110  CLC   TACREND,RQRLCYS                                                  
         JL    VCST90                                                           
         CLC   TACRSTRT,RQRLCYE                                                 
         JH    VCST90                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERALPAID                                  
         J     NO                                                               
         DROP  R1,R2                                                            
                                                                                
VCST120  OC    SVGUA,SVGUA         IF CAST HAS A GUARANTEE CODE                 
         JZ    VCST10              DEFINED                                      
                                                                                
         USING TLGUD,R1                                                         
         LA    R1,IOKEY                                                         
         XC    TLGUKEY,TLGUKEY     READ GUARANTEE KEY                           
         MVI   TLGUCD,TLGUCDQ                                                   
         MVC   TLGUSSN,SVCAHKEY+TLCAHSSN-TLCAPD                                 
         MVC   TLGUGUA,SVGUA                                                    
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLGUD,R1                                                         
         L     R1,IOADDR           R1=A(GUARANTEE RECORD)                       
         LA    R2,TLGUELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TAGUD,R2                                                         
VCST130  CLI   0(R2),0             BUMP TO GUARANTEE DETAILS ELEMENT            
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TAGUELQ                                                    
         JE    VCST140                                                          
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VCST130                                                          
                                                                                
VCST140  OC    TAGUCOM,TAGUCOM     IF GUARANTEE IS PER CYCLE                    
         JZ    VCST220                                                          
         CLC   RQRLCOM,TAGUCOM     AND COMMERCIAL BEING RELEASED                
         JNE   VCST220             IS GUARANTEE'S MAIN COMMERCIAL ...           
         DROP  R2                                                               
                                                                                
         USING TLCAPD,R1                                                        
         LA    R1,IOKEY                                                         
         XC    TLCAPKEY,TLCAPKEY   READ CAST GUARANTEE PASSIVE KEYS             
         MVI   TLCAPCD,TLCAGCDQ                                                 
         MVC   TLCAGSSN,SVCAHKEY+TLCAHSSN-TLCAPD                                
         MVC   TLCAGGUA,SVGUA                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VCST160                                                          
                                                                                
VCST150  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
                                                                                
VCST160  LA    R1,IOKEY                                                         
         CLI   TLCAPCD,TLCAGCDQ                                                 
         JNE   VCST220                                                          
         CLC   TLCAGSSN,SVCAHKEY+TLCAHSSN-TLCAPD                                
         JNE   VCST220                                                          
         CLC   TLCAGGUA,SVGUA                                                   
         JNE   VCST220                                                          
         CLC   TLCAGCOM,RQRLCOM    CANNOT BE ACTIVE COMMERCIALS                 
         JE    VCST150             ATTACHED TO PER CYCLE GUARANTEE              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLCAD,R1                                                         
         L     R1,IOADDR           R1=A(CAST RECORD)                            
         LA    R2,TLCAELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TACAD,R2                                                         
VCST170  CLI   0(R2),0             BUMP TO CAST DETAILS ELEMENT                 
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TACAELQ                                                    
         JE    VCST180                                                          
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VCST170                                                          
VCST180  OC    TACALAST,TACALAST   DO NOT CONSIDER IF LAST SERVICED             
         JNZ   VCST150                                                          
         DROP  R2                                                               
                                                                                
         MVC   SVIOKEY,IOKEY       SAVE CURRENT CAST KEY                        
                                                                                
         USING TLCOPD,R1                                                        
         LA    R1,IOKEY                                                         
         XC    TLCOPKEY,TLCOPKEY    READ FOR COMMERCIAL KEY                     
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVIOKEY+TLCAGCOM-TLCAPD                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JNE   VCST210                                                          
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLCOD,R1                                                         
         L     R1,IOADDR           R1=A(COMMERCIAL RECORD)                      
         LA    R2,TLCOELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TACOD,R2                                                         
VCST190  CLI   0(R2),0             BUMP TO COMM'L DETAILS ELEMENT               
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TACOELQ                                                    
         JE    VCST200                                                          
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VCST190                                                          
VCST200  TM    TACOSTAT,TACOSTRL    DO NOT CONSIDER IF RELEASED                 
         JO    VCST210                                                          
         GOTOR (#ADDERR,AADDERR),DMCB,ERPRIGRT                                  
         J     VCST10                                                           
         DROP  R2                                                               
                                                                                
VCST210  MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     VCST150                                                          
                                                                                
VCST220  MVC   IOKEY(L'SVCAHKEY),SVCAHKEY                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    VCST10                                                           
         DC    H'00'                                                            
                                                                                
VCST230  TM    CASTSTAT,ELIGBCST   AT LEAST 1 ELIGIBLE CAST                     
         JO    YES                 MEMBER MUST HAVE BEEN FOUND                  
         GOTOR (#ADDERR,AADDERR),DMCB,ERCSTDEL                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        VALIDATE COMMERCIAL                                          *         
*        ON ENTRY ... R1=A(IOKEY)                                               
***********************************************************************         
                                                                                
VALCOM   NTR1                                                                   
         USING TLCOPD,R1                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR COMMERCIAL KEY                      
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,RQRLCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    VCOM10                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCIDNFD                                  
         J     NO                                                               
         DROP  R1                                                               
                                                                                
VCOM10   GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCOD,R1                                                         
         L     R1,IOADDR           R1=A(COMMERCIAL RECORD)                      
         LA    R2,TLCOELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         CLC   TLCOAGY,RQRLAGY     COMMERCIAL MUST STILL BE UNDER               
         JE    VCOM20              SAME AGENCY                                  
         GOTOR (#ADDERR,AADDERR),DMCB,ERNLUAGY                                  
         J     NO                                                               
                                                                                
VCOM20   CLC   TLCOCLI,RQRLCLI     COMMERCIAL MUST STILL BE UNDER               
         JE    VCOM30              SAME CLIENT                                  
         GOTOR (#ADDERR,AADDERR),DMCB,ERNLUCLI                                  
         J     NO                                                               
                                                                                
VCOM30   OC    TLCOPRD,TLCOPRD     IF COMMERCIAL HAS PRODUCT DEFINED            
         JZ    VCOM40                                                           
         OC    RQRLPRD,SPACES      PAD REQUEST PRODUCT WITH SPACES              
                                                                                
VCOM40   CLC   TLCOPRD,RQRLPRD     COMMERCIAL MUST STILL BE UNDER               
         JE    VCOM50              SAME PRODUCT                                 
         GOTOR (#ADDERR,AADDERR),DMCB,ERNLUPRD                                  
         J     NO                                                               
         DROP  R1                                                               
                                                                                
         USING TACOD,R2                                                         
VCOM50   CLI   0(R2),0             BUMP TO COMM'L DETAILS ELEMENT               
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TACOELQ                                                    
         JE    VCOM60                                                           
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     VCOM50                                                           
                                                                                
VCOM60   ST    R2,ATACOEL          SAVE A(COMMERCIAL DETAILS ELEMENT)           
                                                                                
         TM    TACOSTAT,TACOSTLO   COMMERCIAL CANNOT BE LOCKED                  
         JZ    VCOM70                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCIDLCK                                  
                                                                                
VCOM70   TM    TACOSTAT,TACOSTRL   COMMERCIAL CANNOT BE ALREADY                 
         JZ    VCOM80              RELEASED                                     
         GOTOR (#ADDERR,AADDERR),DMCB,ERCIDREL                                  
         J     NO                                                               
                                                                                
VCOM80   CLC   TACOCID,RQRLCID     COMMERCIAL ID MUST BE THE SAME               
         JE    VCOM90                                                           
         GOTOR (#ADDERR,AADDERR),DMCB,ERCIDCHG                                  
         J     NO                                                               
                                                                                
VCOM90   TM    TACOSTA2,TACOCHHF   COMMERCIAL CANNOT HAVE A REISSUE             
         JZ    NO                  PENDING                                      
         GOTOR (#ADDERR,AADDERR),DMCB,ERRISPND                                  
         J     NO                                                               
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        RELEASE/UPDATE COMMERCIAL RECORD                             *         
*        ON ENTRY ... R3=A(COMMERCIAL DETAILS ELEMENT)                          
***********************************************************************         
                                                                                
UPDCOMML NTR1                                                                   
         GOTOR (#SAVPTRS,ASAVPTRS)                                              
                                                                                
         USING TACOD,R3                                                         
         OI    TACOSTAT,TACOSTRL   RELEASE THE COMMERCIAL                       
         DROP  R3                                                               
                                                                                
         USING TLCOD,R1                                                         
         L     R1,IOADDR           R1=A(COMMERCIAL RECORD)                      
         LA    R2,TLCOELEM         R2=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TAACD,R2                                                         
UCOM10   CLI   0(R2),0             BUMP TO ACTIVITY ELEMENT                     
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TAACELQ                                                    
         JNE   UCOM20                                                           
         CLI   TAACSCR,0                                                        
         JE    UCOM30                                                           
UCOM20   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     UCOM10                                                           
                                                                                
UCOM30   MVC   TAACID,LP_USRID     UPDATE WITH ORIGIN ID,                       
         MVC   TAACSTAF,RQRLSTF    STAFF, DATE AND TIME                         
         GOTO1 VDATCON,DMCB,(5,0),(1,TAACCDTE)                                  
         TIME  DEC                                                              
         STCM  R0,14,TAACCTIM                                                   
         STCM  R0,14,SVTIME                                                     
         DROP  R2                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         GOTOR (#UPDPTRS,AUPDPTRS)                                              
         J     YES                                                              
                                                                                
***********************************************************************         
*        LAST SERVICE CAST RECORDS                                    *         
***********************************************************************         
                                                                                
UPDCAST  NTR1                                                                   
         USING TACOD,R2                                                         
         L     R2,ATACOEL                                                       
         OC    TACOVDTE,TACOVDTE   IF COMMERCIAL IS VERIFIED                    
         JZ    UCST10                                                           
         XC    TACOVDTE,TACOVDTE   CLEAR DATE OF VERIFICATION                   
         XC    TACOVTIM,TACOVTIM         TIME OF VERIFICATION                   
         XC    TACOVSTU,TACOVSTU         USER ID                                
         XC    TACOVST,TACOVST           STAFF ID                               
         OI    TACOUVST,TACOUVNM   SET UNVERIFIED BY NON-MUSICIAN               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
         DROP  R2                                                               
                                                                                
UCST10   GOTO1 VDATCON,DMCB,(1,RQRLCYS),(0,WORK)                                
         GOTO1 VADDAY,DMCB,WORK,WORK,-1                                         
         GOTO1 VDATCON,DMCB,(0,WORK),(1,WORK)                                   
                                                                                
         USING TLCAPD,R1                                                        
         XC    TLCAPKEY,TLCAPKEY   INITIALIZE CAST HOLDING FEE KEY              
         MVI   TLCAPCD,TLCAHCDQ                                                 
         MVC   TLCAHCOM,RQRLCOM                                                 
         MVI   TLCAHSRT+5,1                                                     
         MVC   SVCAHKEY,TLCAPKEY                                                
         DROP  R1                                                               
                                                                                
         ZIC   R3,RQRLCIN          R3=CAST COUNTER                              
         ZICM  R4,ARQRLCS,3        R4=A(CAST LIST)                              
                                                                                
UCST20   MVC   IOKEY(L'TLCAPKEY),SVCAHKEY                                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     UCST40                                                           
                                                                                
UCST30   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
                                                                                
         USING TLCAPD,R1                                                        
UCST40   LA    R1,IOKEY                                                         
         CLI   TLCAPCD,TLCAHCDQ                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TLCAHCOM,RQRLCOM                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TLCAHSRT+4(2),0(R4)                                              
         JNE   UCST30                                                           
         DROP  R1                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO3'                           
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCAD,R1                                                         
         L     R1,IOADDR           R1=A(CAST RECORD)                            
         LA    R2,TLCAELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TACAD,R2                                                         
UCST50   CLI   0(R2),0             BUMP TO CAST DETAILS ELEMENT                 
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TACAELQ                                                    
         JE    UCST60                                                           
         ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     UCST50                                                           
                                                                                
UCST60   MVC   TACALAST,WORK       SAVE LAST SERVICES DATE                      
         DROP  R2                                                               
                                                                                
         USING TAACD,R2                                                         
UCST70   CLI   0(R2),0             BUMP TO ACTIVITY ELEMENT                     
         JNE   *+6                                                              
         DC    H'00'               (MUST FIND IT)                               
         CLI   0(R2),TAACELQ                                                    
         JNE   UCST80                                                           
         CLI   TAACSCR,0                                                        
         JE    UCST90                                                           
UCST80   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     UCST70                                                           
                                                                                
UCST90   DS    0H                                                               
                                                                                
UCST100  MVC   TAACID,LP_USRID     UPDATE WITH ORIGIN ID,                       
         MVC   TAACSTAF,RQRLSTF    STAFF, DATE AND TIME                         
         GOTO1 VDATCON,DMCB,(5,0),(1,TAACCDTE)                                  
         TIME  DEC                                                              
         STCM  R0,14,TAACCTIM                                                   
         STCM  R0,14,SVTIME                                                     
         DROP  R2                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO3'                           
                                                                                
         LA    R4,2(R4)                                                         
         BCT   R3,UCST20                                                        
         J     YES                                                              
                                                                                
***********************************************************************         
*        ADD WEB TRANSACTION RECORD                                   *         
***********************************************************************         
                                                                                
ADDWTR   NTR1                                                                   
         USING TLWTD,R2                                                         
         L     R2,IOADDR                                                        
         XC    0(255,R2),0(R2)                                                  
         MVI   TLWTCD,TLWTCDQ                                                   
         MVI   TLWTWBAP,TLWTWACO                                                
         GOTO1 VDATCON,DMCB,(5,0),(1,TLWTDATE)                                  
         MVC   TLWTTIME,SVTIME                                                  
                                                                                
         MVI   TLWTACTN,TLWTARCO   INDICATE IF FULL CAST                        
         TM    CASTSTAT,SPLITCST                                                
         JZ    AWTR10                                                           
         MVI   TLWTACTN,TLWTARCA   OR SPLIT CAST IS RELEASED                    
                                                                                
AWTR10   GOTO1 VHEXOUT,DMCB,RQRLCOM,TLWTWBID,L'RQRLCOM,0                        
         GOTO1 VDATCON,DMCB,(1,RQRLCYS),(20,TLWTWBID+8)                         
                                                                                
         MVI   TLWTLEN+1,41        INITIALIZE RECORD LENGTH                     
         DROP  R2                                                               
                                                                                
         USING TAWTD,R2                                                         
         LA    R2,ELEM             BUILD WEB TRANSACTION ELEMENT                
         XC    ELEM,ELEM                                                        
         MVI   TAWTEL,TAWTELQ                                                   
         MVI   TAWTLEN,TAWT2LNQ                                                 
         MVC   TAWTSTAF,RQRLSTF                                                 
                                                                                
         TM    CASTSTAT,SPLITCST   IF COMMERCIAL IS RELEASED                    
         JO    AWTR20              ADD ELEMENT NOW                              
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,(R2),0                        
         J     AWTR40                                                           
                                                                                
AWTR20   ZIC   R3,RQRLCIN          R3=CAST COUNTER                              
         ZICM  R4,ARQRLCS,3        R4=A(CAST LIST)                              
AWTR30   GOTO1 VHEXOUT,DMCB,0(R4),TAWTRCSQ,2,0                                  
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,(R2),0                        
         LA    R4,2(R4)            ADD ELEMENT FOR EACH LAST                    
         BCT   R3,AWTR30           SERVICED PERFORMER                           
                                                                                
AWTR40   GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOFIL+IO3'                           
         J     YES                                                              
                                                                                
***********************************************************************         
*        EXITS FOR NXTREL ROUTINES                                    *         
***********************************************************************         
                                                                                
NRELX    LA    R0,RELVALS          RESTORE ADDRESS OF RELEASE                   
         STCM  R0,15,LP_ADATA      OUTPUT VALUES                                
         OI    RELFLAG,EXECUTED    SET ROUTINE HAS BEEN EXECUTED                
         MVI   LP_RMODE,LP_RNEXT                                                
         J     YES                                                              
                                                                                
***********************************************************************         
*        ERROR ENTRIES FOR RELEASE                              *               
*        (LOOK AT ERRENTD FOR DSECT)                                            
***********************************************************************         
                                                                                
ERNLUAGY DC    AL1(ENLUAGYX-*),AL2(1),AL1(ERRCATY3),AL1(D#RLAGY)                
         DC    C'Commercial no longer under this Agency'                        
ENLUAGYX EQU   *                                                                
                                                                                
ERACNELI DC    AL1(EACNELIX-*),AL2(13),AL1(ERRCATY1),AL1(D#RLCLI)               
         DC    C'Direct release not enabled for this Agency/Client'             
EACNELIX EQU   *                                                                
                                                                                
ERAGYLCK DC    AL1(EAGYLCKX-*),AL2(2),AL1(ERRCATY3),AL1(D#RLAGY)                
         DC    C'Agency is locked'                                              
EAGYLCKX EQU   *                                                                
                                                                                
ERALPAID DC    AL1(EALPAIDX-*),AL2(4),AL1(ERRCATY3),AL1(D#RLCST)                
         DC    C'Cast already paid for this cycle'                              
EALPAIDX EQU   *                                                                
                                                                                
ERPRIGRT DC    AL1(EPRIGRTX-*),AL2(5),AL1(ERRCATY1),AL1(D#RLCID)                
         DC    C'Primary Commercial for a Per Cycle Guarantee'                  
EPRIGRTX EQU   *                                                                
                                                                                
ERCSTDEL DC    AL1(ECSTDELX-*),AL2(1),AL1(ERRCATY3),AL1(D#RLCST)                
         DC    C'All performers on notice have been deleted'                    
ECSTDELX EQU   *                                                                
                                                                                
ERCIDNFD DC    AL1(ECIDNFDX-*),AL2(6),AL1(ERRCATY3),AL1(D#RLCID)                
         DC    C'Commercial no longer exists'                                   
ECIDNFDX EQU   *                                                                
                                                                                
ERNLUCLI DC    AL1(ENLUCLIX-*),AL2(7),AL1(ERRCATY3),AL1(D#RLCLI)                
         DC    C'Client has changed'                                            
ENLUCLIX EQU   *                                                                
                                                                                
ERNLUPRD DC    AL1(ENLUPRDX-*),AL2(8),AL1(ERRCATY3),AL1(D#RLPRD)                
         DC    C'Product has changed'                                           
ENLUPRDX EQU   *                                                                
                                                                                
ERCIDLCK DC    AL1(ECIDLCKX-*),AL2(9),AL1(ERRCATY1),AL1(D#RLCID)                
         DC    C'Commercial is locked'                                          
ECIDLCKX EQU   *                                                                
                                                                                
ERCIDREL DC    AL1(ECIDRELX-*),AL2(10),AL1(ERRCATY3),AL1(D#RLCID)               
         DC    C'Commercial already released'                                   
ECIDRELX EQU   *                                                                
                                                                                
ERCIDCHG DC    AL1(ECIDCHGX-*),AL2(11),AL1(ERRCATY3),AL1(D#RLCID)               
         DC    C'Commercial ID has changed'                                     
ECIDCHGX EQU   *                                                                
                                                                                
ERRISPND DC    AL1(ERISPNDX-*),AL2(12),AL1(ERRCATY3),AL1(D#RLCID)               
         DC    C'Notice has a reissue pending'                                  
ERISPNDX EQU   *                                                                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS RELEASE ERRRORS                                      *         
***********************************************************************         
                                                                                
NXTERR   J     *+12                                                             
         DC    C'*NXTERR*'                                                      
         LR    RB,RF                                                            
         USING NXTERR,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NERR10                                                           
         L     RE,AERRTAB                                                       
         ST    RE,ANXTERR                                                       
         XC    ERRVALS(ERRVALL),ERRVALS                                         
                                                                                
         USING ERRENTD,R4                                                       
NERR10   L     R4,ANXTERR          R4=A(CURRENT ERROR TABLE ENTRY)              
                                                                                
         CLI   0(R4),X'FF'         EXIT IF ALL ERRORS HAVE BEEN                 
         JE    NOMORE              PROCESSED                                    
                                                                                
         MVC   ERRNUMB,EENUMB                                                   
         MVC   ERRCATY,EECATY                                                   
         MVC   ERRFILD,EEFIELD                                                  
                                                                                
         ZIC   R0,EELEN                                                         
                                                                                
         MVC   ERREMSG,SPACES                                                   
         LR    RE,R0                                                            
         SHI   RE,6                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ERREMSG(0),EEMSG                                                 
                                                                                
         AR    R4,R0               SAVE ADDRESS OF NEXT ERROR TABLE             
         ST    R4,ANXTERR          ENTRY                                        
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,ERRVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
         DROP  R4,RB                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS COMMENT RECORDS                                      *         
***********************************************************************         
                                                                                
NXTCMT   J     *+12                                                             
         DC    C'*NXTCMT*'                                                      
         LR    RB,RF                                                            
         USING NXTCMT,RB                                                        
                                                                                
         USING ERRENTD,RF                                                       
         CLI   RQRLMOD,RQRLRTV     ONLY ENTER ROUTINE IF MODE IS                
         JNE   NOMORE              RETRIEVE                                     
         L     RF,AERRTAB                                                       
         CLI   EECATY,ERRCATY3                                                  
         JE    NOMORE                                                           
         DROP  RF                                                               
                                                                                
         XC    CMTVALS(CMTVALL),CMTVALS                                         
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NCMT10                                                           
         MVI   CMTFLAG,0                                                        
         MVI   LASTSEQ,0                                                        
                                                                                
NCMT10   TM    CMTFLAG,COGDONE     SKIP AHEAD IF COMMERCIAL RECORD              
         JO    NCMT50              GENERAL COMMENT ALREADY PROCESSED            
         OI    CMTFLAG,COGDONE                                                  
                                                                                
         USING TLCOD,R1                                                         
         L     R1,IOADDR           R1=A(COMMERCIAL RECORD)                      
         LA    R2,TLCOELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TACMD,R2                                                         
NCMT20   CLI   0(R2),0             BUMP TO GENERAL COMMENT ELEMENT              
         JE    NCMT50                                                           
         CLI   0(R2),TACMELQ                                                    
         JNE   NCMT30                                                           
         CLI   TACMTYPE,TACMTYPG                                                
         JE    NCMT40                                                           
NCMT30   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NCMT20                                                           
                                                                                
NCMT40   MVI   CMTTYPE,CMTTYCOG     SEND OFF GENERAL COMMERCIAL                 
         ZIC   RE,TACMLEN           COMMENT                                     
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CMTCOMT(0),TACMCOMM                                              
         J     NCMTX                                                            
         DROP  R1,R2                                                            
                                                                                
NCMT50   TM    CMTFLAG,COADONE     SKIP AHEAD IF ATS COMMERCIAL                 
         JO    NCMT130             COMMENT RECORD ALREADY PROCESSED             
                                                                                
         CLI   LASTSEQ,0           SKIP AHEAD IF RECORD IS ALREADY              
         JNE   NCMT70              HERE                                         
                                                                                
NCMT60   GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',AACMKEYR),('B#CMTREC',0),*        
               SAVED,0,0                                                        
         JNE   NCMT120                                                          
                                                                                
         USING TLCMD,R1                                                         
NCMT70   L     R1,IOADDR           R1=A(COMMENT RECORD)                         
         LA    R2,TLCMELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TAXCD,R2                                                         
NCMT80   CLI   0(R2),0             BUMP TO NEXT COMMENT ELEMENT                 
         JE    NCMT110                                                          
         CLI   0(R2),TAXCELQ                                                    
         JNE   NCMT90                                                           
         CLC   TAXCSEQ,LASTSEQ                                                  
         JH    NCMT100                                                          
NCMT90   ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NCMT80                                                           
                                                                                
NCMT100  MVI   CMTTYPE,CMTTYCOA     SEND OFF ATS COMMERCIAL COMMENT             
         ZIC   RE,TAXCLEN                                                       
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CMTCOMT(0),TAXCCMNT                                              
                                                                                
         MVC   LASTSEQ,TAXCSEQ      AND SAVE THE COMMENT'S SEQUENCE             
         J     NCMTX                NUMBER                                      
                                                                                
NCMT110  MVI   LASTSEQ,0            GET NEXT ATS COMMERCIAL COMMENT             
         J     NCMT60                                                           
                                                                                
NCMT120  OI    CMTFLAG,COADONE      SET ATS COMMERCIAL COMMENTS                 
         MVI   LASTSEQ,0            PROCESSED                                   
         MVI   LP_RMODE,LP_RFRST                                                
         DROP  R1,R2                                                            
                                                                                
NCMT130  TM    CMTFLAG,COTDONE      SKIP AHEAD IF TPC COMMERCIAL                
         JO    NCMT210              COMMENT RECORD ALREADY PROCESSED            
                                                                                
         CLI   LASTSEQ,0            SKIP AHEAD IF RECORD IS ALREADY             
         JNE   NCMT150              HERE                                        
                                                                                
NCMT140  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',ATCMKEYR),('B#CMTREC',0),*        
               SAVED,0,0                                                        
         JNE   NCMT200                                                          
                                                                                
         USING TLCMD,R1                                                         
NCMT150  L     R1,IOADDR           R1=A(COMMENT RECORD)                         
         LA    R2,TLCMELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TAXCD,R2                                                         
NCMT160  CLI   0(R2),0             BUMP TO NEXT COMMENT ELEMENT                 
         JE    NCMT190                                                          
         CLI   0(R2),TAXCELQ                                                    
         JNE   NCMT170                                                          
         CLC   TAXCSEQ,LASTSEQ                                                  
         JH    NCMT180                                                          
NCMT170  ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NCMT160                                                          
                                                                                
NCMT180  MVI   CMTTYPE,CMTTYCOT    SEND OFF GENERAL COMMERCIAL                  
         ZIC   RE,TAXCLEN          COMMENT                                      
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CMTCOMT(0),TAXCCMNT                                              
                                                                                
         MVC   LASTSEQ,TAXCSEQ     AND SAVE THE COMMENT'S SEQUENCE              
         J     NCMTX               NUMBER                                       
                                                                                
NCMT190  MVI   LASTSEQ,0           GET NEXT TPC COMMERCIAL COMMENT              
         J     NCMT140                                                          
                                                                                
NCMT200  OI    CMTFLAG,COTDONE     SET TPC COMMERCIAL COMMENTS                  
         MVI   LP_RMODE,LP_RFRST   PROCESSED                                    
         DROP  R1,R2                                                            
                                                                                
NCMT210  GOTOR (#NXTREC,ANXTREC),DMCB,('YESQ',AHCMKEYR),('B#CMTREC',0),*        
               SAVED,0,0                                                        
         JNE   NOMORE              PROCESS HISTORY COMMENT RECORD               
                                                                                
         USING TLHCD,R1                                                         
NCMT220  L     R1,IOADDR           R1=A(COMMENT RECORD)                         
         LA    R2,TLHCELEM         R2=A(FIRST ELEMENT)                          
                                                                                
         USING TACMD,R2                                                         
NCMT230  CLI   0(R2),0             BUMP TO COMMENT ELEMENT                      
         JE    NOMORE                                                           
         CLI   0(R2),TACMELQ                                                    
         JNE   NCMT240                                                          
         CLI   TACMCOMM,X'E1'                                                   
         JNE   NCMT250                                                          
NCMT240  ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         J     NCMT230                                                          
                                                                                
NCMT250  MVI   CMTTYPE,CMTTYHST     SEND OFF GENERAL COMMERCIAL                 
         ZIC   RE,TACMLEN           COMMENT                                     
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CMTCOMT(0),TACMCOMM                                              
         DROP  R1,R2                                                            
                                                                                
NCMTX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CMTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT W4 SEARCH STATUS RECORD                               *         
***********************************************************************         
                                                                                
NXTW4S   J     *+12                                                             
         DC    C'*NXTW4S*'                                                      
         LR    RB,RF                                                            
         USING NXTW4S,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         ZICM  RE,ARQW4SSF,3       TEMPORARY CODE TO HANDLE BUG                 
         CLI   3(RE),X'0C'         WHERE TALNK15'S EMBEDDED                     
         JNE   NW4S00              DOWNLOAD CALLS BLOW AWAY                     
         CLI   7(RE),1             THE ERROR STATUS                             
         JNE   NW4S00                                                           
         OI    ERRSTAT,ESREVIW                                                  
                                                                                
NW4S00   GOTOR (#FMTLIST,AFMTLIST),DMCB,RQW4SSIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQW4STIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQW4SCIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQW4SKIN                                
                                                                                
         MVI   W4SSTAT,W4SSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,W4VALREQ         VALIDATE REQUEST                             
         JNE   NW4SX                                                            
                                                                                
         MVI   W4SSTAT,W4SSUNS     REINITIALIZE STATUS TO UNSUCCESSFUL          
         MVC   RECCOUNT,RQW4SLM    AND INITIALIZE VARIABLES                     
         XC    RQW4SSSN,RQW4SSSN                                                
         XC    SVIOKEY,SVIOKEY                                                  
         XC    SVCFID,SVCFID                                                    
         XC    SVTSSN,SVTSSN                                                    
                                                                                
         MVI   BYTE1,0                                                          
         CLI   RQW4SSKA,C'Y'       UNLESS SKIPPING AGENCY/CLIENT                
         JE    NW4S10              ACCESS CHECK                                 
         TM    ERRSTAT,ESREVIW     OR REVIEWING AN ERROR                        
         JO    NW4S10              SAVE AGENCY/CLIENT LIMITATIONS               
         MVI   BYTE1,X'80'                                                      
NW4S10   GOTOR (#VALSTF,AVALSTF),DMCB,(BYTE1,RQW4SSTF)                          
         JNE   NW4SX                                                            
                                                                                
         LA    R3,IOKEY                                                         
                                                                                
         L     R2,ASSNTAB                                                       
         MVI   0(R2),X'FF'         BUILD SOCIAL SECURITY NUMBER                 
         BAS   RE,BLDW4SSN         FILTER LIST                                  
         BAS   RE,BLDCOSSN                                                      
         JNE   NW4SX                                                            
                                                                                
         GOTOR SVLENGTH,DMCB,(L'RQW4SLNM,RQW4SLNM),LNAMELEN,0                   
         GOTOR SVLENGTH,DMCB,(L'RQW4SFNM,RQW4SFNM),FNAMELEN,0                   
                                                                                
         XC    0(L'TLDRKEY,R3),0(R3)                                            
                                                                                
         USING TLW4D,R3                                                         
         OC    RQW4SSSN,RQW4SSSN   IF SOCIAL SECURITY NUMBER ENTERED            
         JZ    NW4S20              READ FOR IT                                  
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,RQW4SSSN                                                 
         LHI   R0,L'TLW4KEY-1                                                   
         J     NW4S30                                                           
         DROP  R3                                                               
                                                                                
         USING TLW4PD,R3                                                        
NW4S20   MVI   TLW4PCD,TLW4NCDQ    IF NAMES WERE ENTERED, READ FOR THEM         
         MVC   TLW4NLST,RQW4SLNM                                                
         LHI   R0,TLW4NLST-TLW4PD-1                                             
         ZIC   RE,LNAMELEN                                                      
         AR    R0,RE                                                            
         DROP  R3                                                               
                                                                                
NW4S30   STC   R0,LKEYCOMP         SAVE L' KEY COMPARE                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTW4            IF W4 RECORD MATCHES ALL FILTERS             
         JNE   NW4SX               RETURN SEARCH SUCCESSFUL STATUS              
         MVI   W4SSTAT,W4SSSUC                                                  
         TM    ERRSTAT,ESREVIW                                                  
         JZ    NW4SX                                                            
         MVI   W4SSTAT,WSSTEMB                                                  
                                                                                
NW4SX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4SVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
W4VALREQ NTR1                                                                   
         OC    RQW4SSTF,RQW4SSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
                                                                                
         XR    R0,R0                                                            
         CLI   RQW4SSIN,0          ENSURE THAT 1 OF THE FOLLOWING               
         JE    *+8                 FIELDS AND ONLY 1 OF THE FOLLOWING           
         AHI   R0,1                FIELDS ARE PROVIDED:                         
         OC    RQW4SLNM,RQW4SLNM   SOCIAL SECURITY NUMBERS                      
         JZ    *+8                 LAST NAME                                    
         AHI   R0,1                OR INTERNAL COMMERCIAL NUMBERS               
         CLI   RQW4SCIN,0                                                       
         JE    *+8                                                              
         AHI   R0,1                                                             
         CHI   R0,1                                                             
         JNE   NO                                                               
                                                                                
         OC    RQW4SFNM,RQW4SFNM   IF FIRST NAME IS PROVIDED                    
         JZ    WVR10                                                            
         OC    RQW4SLNM,RQW4SLNM   ASSERT THAT LAST NAME IS PROVIDED            
         JZ    NO                                                               
                                                                                
WVR10    CLI   RQW4STIN,0          ENFORCE VALID VALUES FOR                     
         JE    WVR30               TYPE FILTERS                                 
         ZIC   R2,RQW4STIN                                                      
         ZICM  R3,ARQW4STF,3                                                    
WVR20    GOTOR (#VALFLD,AVALFLD),DMCB,('VFW4TY',0(R3))                          
         JNE   NO                                                               
         LA    R3,1(R3)                                                         
         BCT   R2,WVR20                                                         
                                                                                
WVR30    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4SSKA)                       
         JNE   NO                                                               
                                                                                
         CLI   RQW4SKIN,0          IF TRACKS ARE PROVIDED                       
         JE    WVR40               ASSERT THAT NUMBER OF TRACKS                 
         CLC   RQW4SKIN,RQW4SCIN   EQUALS NUMBER OF INTERNAL                    
         JNE   NO                  COMMERCIAL NUMBERS                           
                                                                                
WVR40    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQW4SES9)                       
         JNE   NO                                                               
                                                                                
         OC    RQW4SLM,RQW4SLM     IF LIMIT RESULTS TO IS PROVIDED              
         JZ    YES                                                              
         OC    RQW4SLNM,RQW4SLNM   ASSERT THAT LAST NAME IS PROVIDED            
         JNZ   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE REFORMATS W4 SEARCH'S SOCIAL SECURITY NUMBER         *         
*        FILTER LIST                                                  *         
*        ON ENTRY ... R2=A(SSN TAB)                                   *         
*                     R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
BLDW4SSN NTR1                                                                   
         ZIC   R0,RQW4SSIN         IF ANY SOCIAL SECURITY NUMBERS               
         LTR   R0,R0               ARE PROVIDED ...                             
         JZ    NO                                                               
         ZICM  R1,ARQW4SSF,3                                                    
                                                                                
BW4SSN10 MVC   0(L'TLW4SSN,R2),0(R1)                                            
         LA    R1,L'TLW4SSN(R1)                                                 
         LA    R2,L'TLW4SSN(R2)                                                 
         BCT   R0,BW4SSN10         COPY SOCIAL SECURITY NUMBER ARRAY            
         MVI   0(R2),X'FF'         INTO LOCAL STORAGE                           
                                                                                
         L     R2,ASSNTAB                                                       
         MVC   RQW4SSSN,0(R2)                                                   
         XC    0(L'TLW4SSN,R2),0(R2)                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE BUILDS W4 SEARCH'S COMMERCIAL SOCIAL SECURITY        *         
*        NUMBER FILTER LIST                                           *         
*        ON ENTRY ... R2=A(SSN TAB)                                   *         
*                     R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
BLDCOSSN NTR1                                                                   
         ZIC   R0,RQW4SCIN         IF ANY INTERNAL COMMERCIAL NUMBERS           
         LTR   R0,R0               ARE PROVIDED ...                             
         JZ    YES                                                              
         ZICM  R6,ARQW4SCF,3                                                    
                                                                                
BCOSSN10 GOTOR SETCURTF,DMCB,RQW4SKIN                                           
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY     READ FOR ALL CAST KEYS                       
         MVI   TLCACD,TLCACDQ      FOR COMMERCIAL                               
         MVC   TLCACOM,0(R6)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     BCOSSN30                                                         
BCOSSN20 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
BCOSSN30 CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   BCOSSN70                                                         
                                                                                
         CLI   RQW4SKIN,0                                                       
         JE    BCOSSN40                                                         
         OC    CURTRKFL,CURTRKFL                                                
         JZ    BCOSSN40                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         L     R4,AIO3                                                          
         BRAS  RE,FLTTRK                                                        
         JNE   BCOSSN20                                                         
                                                                                
BCOSSN40 L     R2,ASSNTAB                                                       
BCOSSN50 CLI   0(R2),X'FF'         IF SOCIAL SECRURITY NUMBER HAS               
         JE    BCOSSN60            NOT ALREADY BEEN ENCOUNTERED                 
         CLC   TLCASSN,0(R2)       FIND NEXT EMPTY SLOT IN TABLE ...            
         JE    BCOSSN20                                                         
         LA    R2,L'TLCASSN(R2)                                                 
         J     BCOSSN50                                                         
                                                                                
BCOSSN60 MVC   0(L'TLCASSN,R2),TLCASSN                                          
         MVI   L'TLCASSN(R2),X'FF' . . AND ADD THIS SOCIAL SECURITY             
         J     BCOSSN20            NUMBER TO IT                                 
         DROP  R3                                                               
                                                                                
BCOSSN70 LA    R6,L'TLCACOM(R6)    WHEN ALL CAST PROCESSED, BUMP                
         BCT   R0,BCOSSN10         TO NEXT COMMERCIAL                           
                                                                                
***********************************************************************         
                                                                                
         L     R2,ASSNTAB          IF ANY COMMERCIAL HAD ANY CAST               
         CLI   0(R2),X'FF'                                                      
         JE    NO                                                               
         MVC   RQW4SSSN,0(R2)      SET TO READ FOR THE FIRST ONE                
         XC    0(L'TLW4SSN,R2),0(R2)                                            
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT W4 SEARCH COMPRESSED DETAILS RECORDS                  *         
***********************************************************************         
                                                                                
NXTW4D   J     *+12                                                             
         DC    C'*NXTW4D*'                                                      
         LR    RB,RF                                                            
         USING NXTW4D,RB                                                        
                                                                                
         OC    RQW4SSSN,RQW4SSSN   EXIT IF SEARCHING BY SS#                     
         JNZ   NOMORE                                                           
         CLI   W4SSTAT,W4SSSUC     OR IF INITIAL SEARCH WAS                     
         JNE   NOMORE              NOT SUCCESSFUL                               
                                                                                
         USING TLW4PD,R3                                                        
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NW4D20              READ NEXT W4 RECORD                          
         LA    R3,IOKEY                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTW4                                                         
         JNE   NOMORE                                                           
         DROP  R3                                                               
                                                                                
         OC    RQW4SLM,RQW4SLM     DONE IF RECORD LIMIT WAS PROVIDED            
         JZ    NW4D10                                                           
         OC    RECCOUNT,RECCOUNT   AND HAS BEEN REACHED                         
         JNZ   NW4D10                                                           
         MVI   RQW4SLM,X'FF'                                                    
         J     NOMORE                                                           
                                                                                
NW4D10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
NW4D20   BRAS  RE,BLDW4X           BUILD EXPANDED W4 RECORD                     
                                                                                
         OC    RQW4SLM,RQW4SLM     IF RECORD LIMIT WAS PROVIDED                 
         JZ    NW4DX                                                            
         LH    R0,RECCOUNT         DECREMENT IT NOW                             
         SHI   R0,1                                                             
         STH   R0,RECCOUNT                                                      
                                                                                
NW4DX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4XVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT W4 SEARCH EXPANDED DETAILS RECORDS                    *         
***********************************************************************         
                                                                                
NXTW4X   J     *+12                                                             
         DC    C'*NXTW4X*'                                                      
         LR    RB,RF                                                            
         USING NXTW4X,RB                                                        
                                                                                
         CLI   W4SSTAT,W4SSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              UNSUCCESSFUL                                 
         OC    RQW4SSSN,RQW4SSSN   OR IF SEARCH REQUEST DOES NOT                
         JZ    NOMORE              CONTAIN SOCIAL SECURITY NUMBER               
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JE    NW4X10                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTW4                                                         
         JNE   NOMORE                                                           
                                                                                
NW4X10   BRAS  RE,BLDW4X           BUILD EXPANDED W4 RECORD                     
                                                                                
         MVC   SVTSSN,W4XTRS       SAVE TRUSTEE SS# AND                         
         LA    R2,SVCFID           PREPARE TO SAVE ATTACHED CORPS               
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TATIELQ      READ THROUGH ALL TAX ID ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
NW4X20   BRAS  RE,NEXTEL                                                        
         JNE   NW4XX                                                            
         CLI   TATITYPE,TATITYCO   IF TYPE IS CORPORATION                       
         JNE   NW4X20              SAVE IT INTO ATTACHED CORP ARRAY             
         MVC   0(L'TLW4FID,R2),TATIID                                           
         LA    R2,L'TLW4FID(R2)                                                 
         J     NW4X20                                                           
         DROP  R4                                                               
                                                                                
NW4XX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4XVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT W4 SEARCH ATTACHED CORPORATION DETAILS RECORDS        *         
***********************************************************************         
                                                                                
NXTW4C   J     *+12                                                             
         DC    C'*NXTW4C*'                                                      
         LR    RB,RF                                                            
         USING NXTW4C,RB                                                        
                                                                                
         OC    RQW4SSSN,RQW4SSSN   EXIT IF SEARCH REQUEST DOES NOT              
         JZ    NOMORE              CONTAIN SOCIAL SECURITY NUMBER               
         OC    SVCFID,SVCFID       OR IF W4 DOES NOT HAVE AN ATTACHED           
         JZ    NOMORE              CORPORATION                                  
                                                                                
         LA    R2,SVCFID           FIND NEXT ATTACHED CORP TO RETURN            
NW4C10   OC    0(L'TLW4FID,R2),0(R2)                                            
         JNZ   NW4C20                                                           
         LA    R2,L'TLW4FID(R2)                                                 
         J     NW4C10                                                           
                                                                                
         USING TLW4D,R3                                                         
NW4C20   LA    R3,IOKEY                                                         
         XC    TLW4KEY,TLW4KEY     READ W4 RECORD FOR THE CORPORATION           
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4FID,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         BRAS  RE,BLDW4X           BUILD EXPANDED W4 RECORD                     
         XC    0(9,R2),0(R2)       AND CLEAR FEDERAL ID FROM ARRAY              
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4XVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUPUT W4 SEARCH ATTACHED TRUSTEE DETAILS RECORD              *         
***********************************************************************         
                                                                                
NXTW4T   J     *+12                                                             
         DC    C'*NXTW4T*'                                                      
         LR    RB,RF                                                            
         USING NXTW4T,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   EXIT IF NOT THE FIRST TIME IN                
         JNE   NOMORE                                                           
         OC    RQW4SSSN,RQW4SSSN   OR IF SEARCH REQUEST DOES NOT                
         JZ    NOMORE              CONTAIN SOCIAL SECURITY NUMBER               
         OC    SVTSSN,SVTSSN       OR IF W4 DOES NOT HAVE A TRUSTEE             
         JZ    NOMORE                                                           
         TM    ERRSTAT,ESREVIW     OR IF DOWNLOADING BECAUSE OF AN              
         JO    NOMORE              ERROR ELSEWHERE                              
                                                                                
         USING TLW4D,R3                                                         
         LA    R3,IOKEY                                                         
         XC    TLW4KEY,TLW4KEY     READ W4 RECORD FOR THE CORPORATION           
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,SVTSSN                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         BRAS  RE,BLDW4X           BUILD EXPANDED W4 RECORD                     
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4XVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT W4 SEARCH LIMIT STATUS RECORD                         *         
***********************************************************************         
                                                                                
NXTW4L   J     *+12                                                             
         DC    C'*NXTW4L*'                                                      
         LR    RB,RF                                                            
         USING NXTW4L,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         OC    RQW4SLM,RQW4SLM                                                  
         JZ    NOMORE                                                           
         MVI   W4LSTAT,C'N'                                                     
         CLI   RQW4SLM,X'FF'                                                    
         JNE   NW4LX                                                            
         MVI   W4LSTAT,C'Y'                                                     
                                                                                
NW4LX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4LVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD EXPANDED W4 DETAILS RECORD                             *         
*        ON ENTRY ... IOADDR=A(W4 RECORD)                             *         
***********************************************************************         
                                                                                
BLDW4X   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,W4XVALS          CLEAR EXPANDED W4 DETAILS                    
         LHI   R1,W4XVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING TLW4D,R2                                                         
         L     R2,IOADDR           R2=A(W4 RECORD)                              
         MVC   W4XSSN,TLW4SSN      PASS BACK SS#/FID                            
         LA    R2,TLW4ELEM         R2=A(FIRST ELEMENT)                          
         DROP  R2                                                               
                                                                                
BW4X10   CLI   0(R2),0             READ THROUGH ALL ELEMENTS                    
         JE    NW4XX                                                            
                                                                                
         USING TAA2D,R2                                                         
         CLI   0(R2),TAA2ELQ       IF ADDRESS ELEMENT                           
         JNE   BW4X20                                                           
         MVC   W4XAD1,TAA2ADD1     PASS BACK ADDRESS LINE 1                     
         MVC   W4XAD2,TAA2ADD2     ADDRESS LINE 2                               
         MVC   W4XAD3,TAA2ADD3     ADDRESS LINE 3                               
         MVC   W4XCTY,TAA2CITY     CITY                                         
         MVC   W4XSTA,TAA2ST       STATE                                        
         MVC   W4XZIP,TAA2ZIP      ZIP                                          
         CLI   TAA2LEN,TAA2LNQ                                                  
         JL    BW4X160                                                          
         MVC   W4XCRY,TAA2CTRY     AND COUNTRY                                  
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TANUD,R2                                                         
BW4X20   CLI   0(R2),TANUELQ       IF FREE FORM NUMBER ELEMENT                  
         JNE   BW4X30                                                           
         CLI   TANUTYPE,TANUPIDN   NUMBER TYPE PID NUMBER                       
         JNE   BW4X20A                                                          
         LA    R1,W4XPID           PASS BACK PID NUMBER                         
         J     BW4X20D                                                          
BW4X20A  CLI   TANUTYPE,TANUTPHN   NUMBER TYPE PHONE NUMBER                     
         JNE   BW4X20B                                                          
         LA    R1,W4XPHO           PASS BACK PHONE NUMBER                       
         J     BW4X20D                                                          
BW4X20B  CLI   TANUTYPE,TANUTMEM   NUMBER TYPE ACTRA MEMBERSHIP                 
         JNE   BW4X20C                                                          
         LA    R1,W4XMNU           PASS BACK ACTRA MEMBERSHIP NUMBER            
         J     BW4X20D                                                          
BW4X20C  CLI   TANUTYPE,TANUTGST   NUMBER TYPE GST NUMBER                       
         JNE   BW4X160                                                          
         LA    R1,W4XGST           PASS BACK GST NUMBER                         
BW4X20D  ZIC   RE,TANULEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R1),TANUMBER                                                 
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAFND,R2                                                         
BW4X30   CLI   0(R2),TAFNELQ       IF FREE FORM NAME ELEMENT                    
         JNE   BW4X40                                                           
         CLI   TAFNTYPE,TAFNTWEB   NAME TYPE WEB APPLICATION ID                 
         JNE   BW4X160                                                          
         ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   W4XWID,TAFNNAME     PASS BACK WEB APPLICATION ID                 
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAFLD,R2                                                         
BW4X40   CLI   0(R2),TAFLELQ       IF FILTERS ELEMENT                           
         JNE   BW4X50                                                           
         MVC   W4XFIL,TAFLFLT1     PASS BACK FILTERS                            
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TATID,R2                                                         
BW4X50   CLI   0(R2),TATIELQ       IF TAX ID ELEMENT                            
         JNE   BW4X60                                                           
         CLI   TATITYPE,TATITYCO   ID TYPE CORPORATION                          
         JNE   BW4X160                                                          
         LA    RE,W4XCP1                                                        
         NI    TATICRPN,X'0F'                                                   
         ZIC   RF,TATICRPN                                                      
         BCTR  RF,0                                                             
         MHI   RF,9                                                             
         AR    RE,RF                                                            
         MVC   0(9,RE),TATIID      AND PASS BACK FEDERAL ID NUMBER              
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAW4D,R2                                                         
BW4X60   CLI   0(R2),TAW4ELQ       IF EMPLOYEE W4 DETAILS ELEMENT               
         JNE   BW4X70                                                           
         MVC   W4XTYP,TAW4TYPE     PASS BACK TYPE                               
         MVC   W4XSXC,TAW4SEX      SEX CODE                                     
         MVC   W4XETH,TAW4RACE     ETHNICITY                                    
         MVC   W4XTFQ,TAW4FREQ     FREQUENCY CODE                               
         MVC   W4XAFM,TAW4LOCL     AFM LOCAL                                    
         MVC   W4XRST,TAW4RECP     RECIPROCAL STATE                             
         MVC   W4XTCP,TAW4CP       AND TAXABLE CANADIAN PROVINCE                
                                                                                
         CLI   TAW4TYPE,TAW4TYIN   IF INDIVIDUAL                                
         JE    BW4X60A                                                          
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         JE    BW4X60A                                                          
         CLI   TAW4TYPE,TAW4TYFO   OR FOREIGNER                                 
         JNE   BW4X60B                                                          
BW4X60A  MVC   W4XLNM,TAW4NAM2     PASS BACK LAST NAME                          
         MVC   W4XFNM,TAW4NAM1     AND FIRST NAME                               
         J     BW4X60C                                                          
                                                                                
BW4X60B  MVC   W4XCPN,TAW4CRPN     ELSE,PASS BACK CORPORATION NAME              
         OC    TAW4INDT,TAW4INDT   AND INDEMNIFICATION DATE                     
         JZ    BW4X60C                                                          
         GOTO1 VDATCON,DMCB,(1,TAW4INDT),(8,W4XIDT)                             
                                                                                
BW4X60C  MVI   W4XYTD,C'Y'                                                      
         TM    TAW4STAT,TAW4STNY   PASS BACK "PRINT YTD ON CHECKS?"             
         JZ    *+8                 STATUS                                       
         MVI   W4XYTD,C'N'                                                      
                                                                                
         MVI   W4XCKF,C'N'                                                      
         TM    TAW4STAT,TAW4SCKF   PASS BACK "SORT CHECKS FIRST?"               
         JZ    *+8                 STATUS                                       
         MVI   W4XCKF,C'Y'                                                      
                                                                                
         MVI   W4XILF,C'N'                                                      
         TM    TAW4STA2,TAW4SCKF   PASS BACK "IRS LOCK ON FEDERAL               
         JZ    *+8                 WITHHOLDING?" STATUS                         
         MVI   W4XILF,C'Y'                                                      
                                                                                
         MVI   W4XLCK,C'N'                                                      
         TM    TAW4STA2,TAW4SLCK   PASS BACK "W4 LOCKED?" STATUS                
         JZ    *+8                                                              
         MVI   W4XLCK,C'Y'                                                      
                                                                                
         MVI   W4XFIW,C'Y'                                                      
         TM    TAW4STA2,TAW4SNFI   PASS BACK "FICA WITHHOLDING?"                
         JZ    *+8                 STATUS                                       
         MVI   W4XFIW,C'N'                                                      
                                                                                
         MVI   W4XDCW,C'Y'                                                      
         TM    TAW4STA2,TAW4SNDU   PASS BACK "DUE COMPANY WITHHOLDING?"         
         JZ    *+8                 STATUS                                       
         MVI   W4XDCW,C'N'                                                      
                                                                                
         MVI   W4XPEN,C'Y'                                                      
         TM    TAW4STA2,TAW4SNPE   PASS BACK "PENSION?" STATUS                  
         JZ    *+8                                                              
         MVI   W4XPEN,C'N'                                                      
                                                                                
         MVI   W4XFAD,C'N'                                                      
         TM    TAW4STA2,TAW4SFGN   PASS BACK "FOREIGN ADDRESS?" STATUS          
         JZ    *+8                                                              
         MVI   W4XFAD,C'Y'                                                      
                                                                                
         MVI   W4XDIR,C'N'                                                      
         TM    TAW4STA2,TAW4SDD    PASS BACK "DIRECT DEPOSIT?" STATUS           
         JZ    *+8                                                              
         MVI   W4XDIR,C'Y'                                                      
                                                                                
         MVI   W4XWIR,C'N'                                                      
         TM    TAW4STA2,TAW4SWIR   PASS BACK "WIRE TRANSFER CHECKS?"            
         JZ    *+8                 STATUS                                       
         MVI   W4XWIR,C'Y'                                                      
                                                                                
         MVI   W4XSPL,C'N'                                                      
         TM    TAW4STA3,TAW4SSPL   PASS BACK "SPECIAL LETTER ON FILE?"          
         JZ    *+8                 STATUS                                       
         MVI   W4XSPL,C'Y'                                                      
                                                                                
         MVI   W4XEFT,C'N'                                                      
         TM    TAW4STA3,TAW4SEFT   PASS BACK "EFT?" STATUS                      
         JZ    *+8                                                              
         MVI   W4XEFT,C'Y'                                                      
                                                                                
         MVI   W4XREG,C'N'                                                      
         TM    TAW4STA3,TAW4SREG   PASS BACK "REGRESS TESTING?" STATUS          
         JZ    *+8                                                              
         MVI   W4XREG,C'Y'                                                      
                                                                                
         CLI   TAW4TYPE,TAW4TYFO   IF W4 TYPE IS FOREIGNER                      
         JE    BW4X60D                                                          
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         JNE   BW4X60E                                                          
BW4X60D  MVI   W4XFTX,C'Y'         PASS BACK "TAX TAXES FOR FOREIGNER?"         
         TM    TAW4STA3,TAW4SNTX   STATUS                                       
         JZ    BW4X60E                                                          
         MVI   W4XFTX,C'N'                                                      
                                                                                
BW4X60E  CLI   TAW4TYPE,TAW4TYIN   IF W4 TYPE IS INDIVIDUAL                     
         JNE   BW4X60F                                                          
         MVI   W4XNHA,C'N'         PASS BACK "ELIGIBLE FOR NEW HIRE             
         TM    TAW4STA3,TAW4SNHA   ACT?" STATUS                                 
         JZ    *+8                                                              
         MVI   W4XNHA,C'Y'                                                      
         MVI   W4XNHP,C'N'         PASS BACK "NEW HIRE ACT ELIGIBILITY          
         TM    TAW4STA3,TAW4SNHP   PENDING?" STATUS                             
         JZ    *+8                                                              
         MVI   W4XNHP,C'Y'                                                      
         OC    TAW4NHAD,TAW4NHAD   PASS BACK NEW HIRE DATE                      
         JZ    BW4X60F                                                          
         GOTO1 VDATCON,DMCB,(1,TAW4NHAD),(8,W4XNHD)                             
                                                                                
BW4X60F  CLI   TAW4LEN,TAW4LN2Q    IF EXTENDED EMPLOYEE W4 DETAILS              
         JL    BW4X160                                                          
         MVC   W4XMNM,TAW4MIDN     PASS BACK MIDDLE NAME                        
         MVC   W4XSUF,TAW4SUFF     AND SUFFIX                                   
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAAKD,R2                                                         
BW4X70   CLI   0(R2),TAAKELQ       IF EMPLOYEE AKA NAME ELEMENT                 
         JNE   BW4X80                                                           
         MVC   W4XAKF,TAAKNAM1     PASS BACK FIRST NAME                         
         MVC   W4XAKL,TAAKNAM2     AND LAST NAME                                
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAWHD,R2                                                         
BW4X80   CLI   0(R2),TAWHELQ       IF EMPLOYEE WITHHOLDING DETAILS              
         JNE   BW4X90              ELEMENT                                      
                                                                                
         CLC   =C'FD',TAWHUNIT     IF FEDERAL TAX UNIT                          
         JNE   BW4X80A                                                          
         MVC   W4XFMS,TAWHSTAT     PASS BACK MARRIED/SINGLE STATUS              
         MVC   W4XFFX,TAWHFLAT     EXEMPTIONS AND FLAT TAX RATE                 
         EDIT  TAWHEXS,W4XFEX,ALIGN=LEFT,ZERO=NOBLANK                           
         J     BW4X160                                                          
                                                                                
BW4X80A  CLI   TAWHUNIT+2,C' '     IF STATE TAX UNIT                            
         JH    BW4X80B                                                          
         MVC   W4XSAR,TAWHUNIT     PASS BACK STATE CODE                         
         MVC   W4XSMS,TAWHSTAT     BACK MARRIED/SINGLE STATUS                   
         MVC   W4XSFX,TAWHFLAT     EXEMPTIONS AND FLAT TAX RATE                 
         EDIT  TAWHEXS,W4XSEX,ALIGN=LEFT,ZERO=NOBLANK                           
         J     BW4X160                                                          
*                                  IF CITY TAX UNIT                             
BW4X80B  MVC   W4XCAR,TAWHUNIT     PASS BACK CITY CODE                          
         MVC   W4XCMS,TAWHSTAT     MARRIED/SINGLE STATUS                        
         MVC   W4XCFX,TAWHFLAT     EXEMPTIONS AND FLAT TAX RATE                 
         EDIT  TAWHEXS,W4XCEX,ALIGN=LEFT,ZERO=NOBLANK                           
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAPED,R2                                                         
BW4X90   CLI   0(R2),TAPEELQ       IF PAYEE ELEMENT                             
         JNE   BW4X100                                                          
         MVC   W4XPNM,TAPENAME     PASS BACK PAYEE NAME                         
         MVC   W4XPA1,TAPEADD1     ADDRESS LINE 1                               
         MVC   W4XPA2,TAPEADD2     ADDRESS LINE 2                               
         MVC   W4XPA3,TAPEADD3     ADDRESS LINE 3                               
         MVC   W4XPA4,TAPEADD4     ADDRESS LINE 4, INACTIVE DATE                
         GOTO1 VDATCON,DMCB,(1,TAPEEXP),(8,W4XPEX)                              
         CLI   TAPELEN,TAPELNQ                                                  
         JL    BW4X160                                                          
         MVC   W4XPCY,TAPECITY     CITY                                         
         MVC   W4XPST,TAPEST       STATE                                        
         MVC   W4XPZP,TAPEZIP      ZIP                                          
         MVC   W4XPCT,TAPECTRY     COUNTRY AND ACTIVE DATE                      
         GOTO1 VDATCON,DMCB,(1,TAPEACT),(8,W4XPAC)                              
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TACMD,R2                                                         
BW4X100  CLI   0(R2),TACMELQ       IF VARIABLE LENGTH COMMENT ELEMENT           
         JNE   BW4X110                                                          
         LA    RF,W4XCMT                                                        
         CLI   TACMTYPE,TACMTYPG   TYPE GENERAL COMMENT                         
         JE    BW4X100A                                                         
         LA    RF,W4XEML                                                        
         CLI   TACMTYPE,TACMTYPI   OR TYPE EMAIL ADDRESS                        
         JNE   BW4X160                                                          
BW4X100A ZIC   RE,TACMLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RF),TACMCOMM    PASS IT BACK                                 
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAOWD,R2                                                         
BW4X110  CLI   0(R2),TAOWELQ       IF W4 EXTRA DETAILS ELEMENT                  
         JNE   BW4X120                                                          
         CLI   TAOWTYPE,TAOWTMPF   OTHER TYPE MOTION PICTURE FUND               
         JNE   BW4X110A            PASS BACK MPR FUND PERCENTAGE                
         MVC   W4XMPR,TAOWFLAT                                                  
         J     BW4X160                                                          
BW4X110A CLI   TAOWTYPE,TAOWTCHA   OTHER TYPE PERMANENT CHARITY                 
         JNE   BW4X160             PASS BACK PERMANENT CHARITY %                
         MVC   W4XPCH,TAOWFLAT                                                  
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAWXD,R2                                                         
BW4X120  CLI   0(R2),TAWXELQ       IF OTHER WITHHOLDING ELEMENT                 
         JNE   BW4X130             PASS BACK INFORMATION                        
         MVC   W4XERN,TAWXMERN                                                  
         GOTO1 VDATCON,DMCB,(1,TAWXDOB),(8,W4XDOB)                              
         MVC   W4XDED,TAWXPCT                                                   
         MVC   W4XTRS,TAWXTSSN                                                  
         J     BW4X160                                                          
         DROP  R2                                                               
                                                                                
         USING TAACD,R2                                                         
BW4X130  CLI   0(R2),TAACELQ       IF ACTIVITY ELEMENT                          
         JNE   BW4X140             PASS BACK LAST CHANGED DATE                  
         CLI   TAACSCR,0                                                        
         JNE   BW4X160                                                          
         GOTO1 VDATCON,DMCB,(1,TAACCDTE),(8,W4XACD)                             
         DROP  R2                                                               
                                                                                
         USING TAD1D,R2                                                         
BW4X140  CLI   0(R2),TAD1ELQ       IF TD1 ELEMENT                               
         JNE   BW4X160                                                          
                                                                                
         TM    TAD1STAT,TAD1SCAN   PASS BACK FEDERAL DETAILS                    
         JZ    BW4X150                                                          
         MVC   W4XFNC,TAD1NCL1                                                  
         MVI   W4XFD1,C'N'                                                      
         TM    TAD1STAT,TAD1SCC1                                                
         JZ    *+8                                                              
         MVI   W4XFD1,C'Y'                                                      
         MVI   W4XFET,C'N'                                                      
         TM    TAD1STAT,TAD1SEXM                                                
         JZ    *+8                                                              
         MVI   W4XFET,C'Y'                                                      
         MVC   W4XFPZ,TAD1PZON                                                  
         MVI   W4XFEC,C'N'                                                      
         TM    TAD1STAT,TAD1SECP                                                
         JZ    BW4X160                                                          
         MVI   W4XFEC,C'Y'                                                      
         J     BW4X160                                                          
                                                                                
BW4X150  MVC   W4XPNC,TAD1NCL1     OR PROVINCIAL DETAILS                        
         MVI   W4XPD1,C'N'                                                      
         TM    TAD1STAT,TAD1SCC1                                                
         JZ    *+8                                                              
         MVI   W4XPD1,C'Y'                                                      
         MVI   W4XPET,C'N'                                                      
         TM    TAD1STAT,TAD1SEXM                                                
         JZ    *+8                                                              
         MVI   W4XPET,C'Y'                                                      
         MVC   W4XPPZ,TAD1PZON                                                  
         MVC   W4XPHD,TAD1HOUS                                                  
         MVC   W4XPSP,TAD1SPRT                                                  
         MVI   W4XPHC,C'N'                                                      
         TM    TAD1STAT,TAD1SEHC                                                
         JZ    *+8                                                              
         MVI   W4XPHC,C'Y'                                                      
         DROP  R2                                                               
                                                                                
BW4X160  ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         J     BW4X10                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT AGENT SEARCH STATUS RECORD                            *         
***********************************************************************         
                                                                                
NXTANS   J     *+12                                                             
         DC    C'*NXTANS*'                                                      
         LR    RB,RF                                                            
         USING NXTANS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         L     R2,AAGTTAB                                                       
         MVI   0(R2),X'FF'                                                      
         BAS   RE,BLDANAGT         BUILD AGENT FILTER LIST                      
                                                                                
         MVI   ANSSTAT,ANSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,ANVALREQ         VALIDATE REQUEST                             
         JNE   NANSX                                                            
         MVI   ANSSTAT,ANSSUNS     REINITILIAZE STATUS TO UNSUCCESSFUL          
         MVC   RECCOUNT,RQANSLM    AND INITIALIZE RECORD LIMIT                  
                                                                                
         LA    R3,IOKEY                                                         
         XC    0(L'TLDRKEY,R3),0(R3)                                            
                                                                                
         USING TLAND,R3                                                         
         OC    RQANSAGT,RQANSAGT   IF AGENT CODE ENTERED                        
         JZ    NAN10               READ FOR IT                                  
         MVI   TLANCD,TLANCDQ                                                   
         MVC   TLANAGT,RQANSAGT                                                 
         LHI   R0,L'TLANKEY-1                                                   
         J     NAN30                                                            
         DROP  R3                                                               
                                                                                
NAN10    MVI   FNAMELEN,0                                                       
                                                                                
         USING TLANPD,R3                                                        
         CLI   RQANSSUP,C'Y'       IF NAME WAS ENTERED WITH SUPER               
         JNE   NAN20               SEARCH OPTION                                
         GOTOR SVLENGTH,DMCB,(L'TLANBNAM,RQANSNAM),LNAMELEN,0                   
         L     RE,ASVPTRS                                                       
         MVI   0(RE),X'FF'         INITIALIZE AGENT CODE TABLE                  
         MVI   TLANPCD,TLANBCDQ    AND READ FOR IT                              
         MVC   TLANBNAM(L'RQANSNAM),RQANSNAM                                    
         ZIC   R0,LNAMELEN                                                      
         J     NAN30                                                            
                                                                                
NAN20    GOTOR SVLENGTH,DMCB,(L'TLANNAME,RQANSNAM),LNAMELEN,0                   
         MVI   TLANPCD,TLANNCDQ    ELSE, IF NAME WAS ENTERED WITHOUT            
         MVC   TLANNAME,RQANSNAM   SUPER SEARCH OPTION, READ FOR IT             
         LHI   R0,TLANNAME-TLANPD-1                                             
         ZIC   RE,LNAMELEN                                                      
         AR    R0,RE                                                            
         DROP  R3                                                               
                                                                                
NAN30    STC   R0,LKEYCOMP         SAVE L' KEY COMPARE                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTAGT           IF AGENT RECORD MATCHES ALL FILTERS          
         JNE   NANSX               RETURN SEARCH SUCCESSFUL STATUS              
         MVI   ANSSTAT,ANSSSUC                                                  
                                                                                
NANSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,ANSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE REFORMATS AGENT SEARCH'S AGENCT CODE FILTER LIST     *         
*        ON ENTRY ... R2=A(AGENT CODE TABLE)                          *         
*                     R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
BLDANAGT NTR1                                                                   
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQANSAIN                                
                                                                                
         ZIC   R0,RQANSAIN         IF ANY AENT CODES ARE PROVIDED ...           
         LTR   R0,R0                                                            
         JZ    NO                                                               
         ZICM  R1,ARQANSAF,3                                                    
                                                                                
BANAGT10 MVC   0(L'TLANAGT,R2),0(R1)                                            
         LA    R1,L'TLANAGT(R1)                                                 
         LA    R2,L'TLANAGT(R2)                                                 
         BCT   R0,BANAGT10         COPY AGENT CODE ARRAY INTO                   
         MVI   0(R2),X'FF'         LOCAL STORAGE                                
                                                                                
         L     R2,AAGTTAB                                                       
         MVC   RQANSAGT,0(R2)                                                   
         XC    0(L'TLANAGT,R2),0(R2)                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
ANVALREQ NTR1                                                                   
******** OC    RQANSSTF,RQANSSTF   ASSERT THAT STAFF ID IS PROVIDED             
******** JZ    NO                                                               
                                                                                
         OC    RQANSAGT,RQANSAGT   IF AGENT CODE IS PROVIDED                    
         JZ    AVR10                                                            
         OC    RQANSNAM,RQANSNAM   ASSERT NAME IS NOT PROVIDED                  
         JNZ   NO                                                               
         OC    RQANSLM,RQANSLM     ASSERT LIMIT RESULTS TO IS NOT               
         JZ    AVR20               PROVIDED                                     
         J     NO                                                               
                                                                                
AVR10    OC    RQANSNAM,RQANSNAM   ASSERT THAT AGENT CODE OR AGENT              
         JZ    NO                  NAME HAS BEEN PROVIDED                       
                                                                                
AVR20    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQANSAOV)                       
         JNE   NO                                                               
                                                                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQANSSUP)                       
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT AGENT SEARCH EXPANDED DETAILS RECORDS                 *         
***********************************************************************         
                                                                                
NXTAND   J     *+12                                                             
         DC    C'*NXTAND*'                                                      
         LR    RB,RF                                                            
         USING NXTAND,RB                                                        
                                                                                
         CLI   ANSSTAT,ANSSSUC     EXIT IF INITIAL SEARCH WAS                   
         JNE   NOMORE              NOT SUCCESSFUL                               
                                                                                
         LA    R3,IOKEY            INITIALIZE VARIABLES                         
         LA    R0,ANDVALS                                                       
         LHI   R1,ANDVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING TLANPD,R3                                                        
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NAND20              READ NEXT AGENT RECORD                       
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTAGT                                                        
         JNE   NOMORE                                                           
         DROP  R3                                                               
                                                                                
         OC    RQANSLM,RQANSLM     DONE IF RECORD LIMIT WAS PROVIDED            
         JZ    NAND10                                                           
         OC    RECCOUNT,RECCOUNT   AND HAS BEEN REACHED                         
         JNZ   NAND10                                                           
         MVI   RQANSLM,X'FF'                                                    
         J     NOMORE                                                           
                                                                                
NAND10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLAND,R1                                                         
NAND20   L     R1,IOADDR           R1=A(AGENT RECORD)                           
         MVC   ANDCOD,TLANAGT      PASS BACK AGENT CODE                         
         LA    R1,TLANELEM         R1=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
NAND30   CLI   0(R1),0             READ THROUGH ALL ELEMENTS                    
         JE    NAND110                                                          
                                                                                
         USING TANAD,R1                                                         
         CLI   0(R1),TANAELQ       IF NAME ELEMENT                              
         JNE   NAND40                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ANDNAME(0),TANANAME PASS BACK NAME                               
         J     NAND100                                                          
         DROP  R1                                                               
                                                                                
         USING TASND,R1                                                         
NAND40   CLI   0(R1),TASNELQ       IF SHORT NAME ELEMENT                        
         JNE   NAND50                                                           
         MVC   ANDSNAM,TASNAME     PASS BACK SHORT NAME                         
         J     NAND100                                                          
         DROP  R1                                                               
                                                                                
         USING TAADD,R1                                                         
NAND50   CLI   0(R1),TAADELQ       IF ADDRESS ELEMENT                           
         JNE   NAND60                                                           
         MVC   ANDADD1,TAADADD     PASS BACK ADDRESS LINES                      
         CLI   TAADLNES,1                                                       
         JE    NAND100                                                          
         MVC   ANDADD2,TAADADD+(1*L'TAADADD)                                    
         CLI   TAADLNES,2                                                       
         JE    NAND100                                                          
         MVC   ANDADD3,TAADADD+(2*L'TAADADD)                                    
         CLI   TAADLNES,3                                                       
         JE    NAND100                                                          
         MVC   ANDADD4,TAADADD+(3*L'TAADADD)                                    
         J     NAND100                                                          
         DROP  R1                                                               
                                                                                
         USING TANUD,R1                                                         
NAND60   CLI   0(R1),TANUELQ       IF FREE FORM NUMER ELEMENT                   
         JNE   NAND70                                                           
         CLI   TANUTYPE,TANUTFAX   TYPE FAX                                     
         JNE   NAND100                                                          
         ZIC   RE,TANULEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ANDFAXN(0),TANUMBER PASS BACK FAX NUMBER                         
         J     NAND100                                                          
         DROP  R1                                                               
                                                                                
         USING TAFND,R1                                                         
NAND70   CLI   0(R1),TAFNELQ       IF FREE FORM NAME ELEMENT                    
         JNE   NAND80                                                           
         CLI   TAFNTYPE,TAFNTATT   TYPE ATTENTION NAME                          
         JNE   NAND100                                                          
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ANDATTN(0),TAFNNAME PASS BACK ATTENTION NAME                     
         J     NAND100                                                          
         DROP  R1                                                               
                                                                                
         USING TAAND,R1                                                         
NAND80   CLI   0(R1),TAANELQ       IF AGENT ELEMENT                             
         JNE   NAND90                                                           
         MVC   ANDPHON,TAANTEL     PASS BACK PHONE NUMBER                       
         MVC   ANDOTRC,TAANTNR     OLD T&R CODE                                 
         MVC   ANDSSN,TAANSSN      AND SOCIAL SECURITY NUMBER                   
                                                                                
         MVI   ANDISAG,C'N'        PASS BACK "IGNORE ON SAG CHECKS?"            
         TM    TAANSTAT,TAANSNCK   STATUS                                       
         JZ    NAND80A                                                          
         MVI   ANDISAG,C'Y'                                                     
                                                                                
NAND80A  MVI   ANDAOVR,C'N'        PASS BACK "ALLOWABLE OVERRIDE?"              
         TM    TAANSTAT,TAANSOVR   STATUS                                       
         JZ    NAND100                                                          
         MVI   ANDAOVR,C'Y'                                                     
         J     NAND100                                                          
         DROP  R1                                                               
                                                                                
         USING TACMD,R1                                                         
NAND90   CLI   0(R1),TACMELQ       IF VARIABLE LENGTH COMMENT ELEMENT           
         JNE   NAND100                                                          
         CLI   TACMTYPE,TACMTYPI   TYPE EMAIL ADDRESS                           
         JNE   NAND100                                                          
         ZIC   RE,TACMLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   ANDEMAL(0),TACMCOMM PASS BACK EMAIL ADDRESS                      
         J     NAND100                                                          
         DROP  R1                                                               
                                                                                
NAND100  ZIC   RF,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         J     NAND30                                                           
                                                                                
NAND110  OC    RQANSLM,RQANSLM     IF RECORD LIMIT WAS PROVIDED                 
         JZ    NANDX                                                            
         LH    R0,RECCOUNT         DECREMENT IT NOW                             
         SHI   R0,1                                                             
         STH   R0,RECCOUNT                                                      
                                                                                
NANDX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,ANDVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT AGENT SEARCH LIMIT STATUS RECORD                      *         
***********************************************************************         
                                                                                
NXTANL   J     *+12                                                             
         DC    C'*NXTANL*'                                                      
         LR    RB,RF                                                            
         USING NXTANL,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         OC    RQANSLM,RQANSLM                                                  
         JZ    NOMORE                                                           
         MVI   ANLSTAT,C'N'                                                     
         CLI   RQANSLM,X'FF'                                                    
         JNE   NANLX                                                            
         MVI   ANLSTAT,C'Y'                                                     
                                                                                
NANLX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,ANLVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT CAST SEARCH STATUS RECORD                             *         
***********************************************************************         
                                                                                
NXTCAS   J     *+12                                                             
         DC    C'*NXTCAS*'                                                      
         LR    RB,RF                                                            
         USING NXTCAS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         ZICM  RE,ARQCACOM,3       TEMPORARY CODE TO HANDLE BUG                 
         CLI   3(RE),X'0C'         WHERE TALNK17'S EMBEDDED                     
         JNE   NCAS00              DOWNLOAD CALLS BLOW AWAY                     
         CLI   7(RE),1             THE ERROR STATUS                             
         JNE   NCAS00                                                           
         OI    ERRSTAT,ESREVIW                                                  
                                                                                
NCAS00   GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCASCIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCASUIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCASWIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCASTIN                                
                                                                                
         MVI   CASSTAT,CASSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,CAVALREQ         VALIDATE REQUEST                             
         JNE   NCASX                                                            
                                                                                
         MVI   CASSTAT,CASSUNS     INITIALIZE STATUS TO UNSUCCESSFUL            
         MVC   ABOVELIM,RQCASLIM   AND RECORDS ABOVE LIMIT VARIABLES            
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,(X'80',RQCASSTF)                          
         JNE   NCASX               SAVE AGENCY/CLIENT LIMITS                    
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
                                                                                
         BAS   RE,BLDCOMFL         BUILD COMMERCIAL FILTER LIST                 
         JNE   NCASX                                                            
                                                                                
         GOTOR SETCURTF,DMCB,RQCASTIN                                           
         BAS   RE,SETTST           SET TRACK STATUS                             
                                                                                
         GOTOR SVLENGTH,DMCB,(L'RQCASLNM,RQCASLNM),LNAMELEN,RQCASLNI            
         GOTOR SVLENGTH,DMCB,(L'RQCASFNM,RQCASFNM),FNAMELEN,RQCASFNI            
                                                                                
         XC    LASTCOM,LASTCOM                                                  
                                                                                
         BAS   RE,BLDCASSN         BUILD SSN FILTER LIST                        
         JNE   NCASX                                                            
                                                                                
         OC    RQCASGUA,RQCASGUA   IF GUARANTEE CODE IS PROVIDED                
         JZ    *+8                                                              
         BAS   RE,STPCVARS         SET PER CYCLE VARIABLES                      
                                                                                
         OC    RQCASAGT,RQCASAGT   IF AGENT CODE IS PROVIDED                    
         JZ    NCAS10              TRANSLATE AGENT CODE                         
         GOTOR (#TRNSAGT,ATRNSAGT),DMCB,(X'80',RQCASAGT),AGTFILT                
                                                                                
         USING TLCOPD,R3                                                        
NCAS10   OC    RQCASCOM,RQCASCOM   IF INTERNAL COMMERCIAL NUMBER                
         JNZ   NCAS20              IS NOT PROVIDED                              
         OC    RQCASCID,RQCASCID   BUT COMMERCIAL ID IS                         
         JZ    NCAS30              READ FOR COMMERCIAL ID                       
         XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLCOPCD,TLCOICDQ                                                 
         MVC   TLCOIAGY,RQCASAGY                                                
         MVC   TLCOICID,RQCASCID                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLCOICOM-TLCOPD),IOKEYSAV                                  
         JNE   NCASX               IF FOUND, SET INTERNAL COMMERCIAL            
         MVC   RQCASCOM,TLCOICOM   NUMBER AND RECLEAR KEY                       
         DROP  R3                                                               
                                                                                
         USING TLCAD,R3                                                         
NCAS20   XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLCACD,TLCACDQ      IF INTERNAL COMMERCIAL NUMBER IS             
         MVC   TLCACOM,RQCASCOM    PROVIDED, BUILD KEY                          
         LHI   R0,TLCASORT-TLCAD-1 AND SET R0 WITH LENGTH FOR COMPARE           
         J     NCAS70                                                           
         DROP  R3                                                               
                                                                                
         USING TLCAPD,R3                                                        
NCAS30   XC    0(L'TLDRKEY,R3),0(R3)                                            
         OC    RQCASAGT,RQCASAGT   IF AGENT CODE IS PROVIDED                    
         JZ    NCAS40              BUILD KEY                                    
         MVI   TLCAPCD,TLCAACDQ    AND SET R0 WITH LENGTH FOR COMPARE           
         MVC   TLCAAAGT,RQCASAGT                                                
         LHI   R0,TLCAASSN-TLCAPD-1                                             
         OC    RQCASSSN,RQCASSSN   IF SOCIAL SECURITY NUMBER IS                 
         JZ    NCAS70              ALSO PROVIDED, ADD IT TO KEY                 
         MVC   TLCAASSN,RQCASSSN   AND SET R0 WITH LENGTH FOR COMPARE           
         LHI   R0,TLCAACOM-TLCAPD-1                                             
         J     NCAS70                                                           
                                                                                
NCAS40   OC    RQCASGUA,RQCASGUA   IF GUARANTEE CODE IS PROVIDED                
         JZ    NCAS50              BUILD KEY                                    
         MVI   TLCAPCD,TLCAGCDQ    AND SET R0 WITH LENGTH FOR COMPARE           
         MVC   TLCAGSSN,RQCASSSN                                                
         MVC   TLCAGGUA,RQCASGUA                                                
         LHI   R0,TLCAGCOM-TLCAPD-1                                             
         J     NCAS70                                                           
                                                                                
NCAS50   OC    RQCASSSN,RQCASSSN   IF SOCIAL SECURITY NUMBER IS                 
         JZ    NCAS60              PROVIDED, BUILD KEY                          
         MVI   TLCAPCD,TLCACCDQ    AND SET R0 WITH LENGTH FOR COMPARE           
         MVC   TLCACSSN,RQCASSSN                                                
         LHI   R0,TLCACCOM-TLCAPD-1                                             
         J     NCAS70                                                           
         DROP  R3                                                               
                                                                                
NCAS60   DC    H'00'                                                            
                                                                                
NCAS70   STC   R0,LKEYCOMP         SAVE L' KEY COMPARE                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTCAST          IF CAST RECORD MATCHES ALL FILTERS           
         JNE   NW4SX               RETURN SEARCH SUCCESSFUL STATUS              
         MVI   CASSTAT,CASSSUC                                                  
         TM    ERRSTAT,ESREVIW                                                  
         JZ    NCAS80                                                           
         MVI   CASSTAT,CASSEMB                                                  
         J     NCASX                                                            
                                                                                
NCAS80   GOTO1 VDATCON,DMCB,(5,0),(8,CASSDAT)                                   
         TIME  DEC                                                              
         STCM  R0,14,FULL1                                                      
         GOTOR (#OUTTIME,AOUTTIME),DMCB,FULL1,CASSTIM                           
                                                                                
NCASX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CASVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
CAVALREQ NTR1                                                                   
         TM    ERRSTAT,ESREVIW                                                  
         JO    YES                                                              
                                                                                
         OC    RQCASSTF,RQCASSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
                                                                                
         CLI   RQCASCIN,0          ASSERT THAT INTERNAL COMMERCIAL              
         JH    CVR10               NUMBER                                       
         OC    RQCASCID,RQCASCID   OR COMMERCIAL ID                             
         JNZ   CVR10                                                            
         OC    RQCASAGT,RQCASAGT   OR AGENT                                     
         JNZ   CVR10                                                            
         OC    RQCASSSN,RQCASSSN   OR SOCIAL SECURITY NUMBER                    
         JNZ   CVR10                                                            
         OC    RQCASAGY,RQCASAGY   OR AGENCY AND LAST NAME ARE PROVIDED         
         JZ    NO                                                               
         OC    RQCASLNM,RQCASLNM                                                
         JNZ   CVR10                                                            
         OC    RQCASCOT,RQCASCOT   OR AGENCY AND COMMERCIAL TITLE               
         JZ    NO                  ARE PROVIDED                                 
                                                                                
CVR10    OC    RQCASSEQ,RQCASSEQ   IF CAST SEQUENCE NUMBER IS PROVIDED          
         JZ    CVR20                                                            
         CLI   RQCASCIN,1          ASSERT THAT 1 INTERNAL COMMERCIAL            
         JNE   NO                  NUMBER IS PROVIDED                           
                                                                                
CVR20    GOTOR (#VALFLD,AVALFLD),DMCB,('VFCAM',RQCASONO)                        
         JNE   NO                                                               
                                                                                
         CLI   RQCASUIN,0          ENFORCE VALID VALUES FOR                     
         JE    CVR40               UNION FILTERS                                
         ZIC   R2,RQCASUIN                                                      
         ZICM  R3,ARQCAUNI,3                                                    
CVR30    GOTOR (#VALFLD,AVALFLD),DMCB,('VFUNI',0(R3))                           
         JNE   NO                                                               
         LA    R3,3(R3)                                                         
         BCT   R2,CVR30                                                         
                                                                                
CVR40    OC    RQCASGUA,RQCASGUA   IF GUARANTEE CODE IS PROVIDED                
         JZ    CVR50                                                            
         OC    RQCASSSN,RQCASSSN   ASSERT THAT SOCIAL SECURITY NUMBER           
         JZ    NO                  IS PROVIDED                                  
                                                                                
CVR50    CLI   RQCASTIN,0          IF TRACKS ARE PROVIDED                       
         JE    CVR60                                                            
         CLC   RQCASTIN,RQCASCIN   ENSURE THE NUMBER OF TRACKS MATCHES          
         JNE   NO                  THE NUMBER OF INTERNAL                       
                                                                                
CVR60    CLI   ARQCACOM,0          IF INTERNAL COMMERCIAL NUMBERS               
         JNE   CVR70               ARE NOT PROVIDED                             
         CLI   RQCASMST,0          ASSERT THAT ON MASTER COMMERCIAL?            
         JNE   NO                  IS NOT PROVIDED                              
         CLI   RQCASLFT,0          ASSERT THAT ON LIFT VERSION?                 
         JNE   NO                  IS NOT PORVIDED                              
         CLI   RQCASVER,0          ASSERT THAT ON VERSION X?                    
         JE    CVR80               IS NOT PROVIDED                              
                                                                                
CVR70    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASMST)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASLFT)                       
         JNE   NO                                                               
                                                                                
CVR80    GOTOR (#VALFLD,AVALFLD),DMCB,('VFOORN',RQCASREL)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASORE)                       
         JNE   NO                                                               
                                                                                
         CLI   RQCASWIN,0          ENFORCE VALID VALUES FOR                     
         JE    CVR100              W4 TYPE FILTERS                              
         ZIC   R2,RQCASWIN                                                      
         ZICM  R3,ARQCAW4T,3                                                    
CVR90    GOTOR (#VALFLD,AVALFLD),DMCB,('VFW4TY',0(R3))                          
         JNE   NO                                                               
         LA    R3,1(R3)                                                         
         BCT   R2,CVR90                                                         
                                                                                
CVR100   OC    RQCASLNM,RQCASLNM   IF LAST NAME IS PROVIDED                     
         JZ    CVR110                                                           
         CLI   RQCASLNI,0          ASSERT THAT LAST NAME SEARCH                 
         JE    CVR120              INSTRUCTIONS ARE BLANK                       
         CLI   RQCASLNI,C'E'       OR E                                         
         JE    CVR120                                                           
         J     NO                                                               
                                                                                
CVR110   CLI   RQCASLNI,0          IF LAST NAME IS NOT PROVIDED                 
         JNE   NO                  ASSERT INSTRUCTIONS ARE BLANK                
                                                                                
CVR120   OC    RQCASFNM,RQCASFNM   IF FIRST NAME IS PROVIDED                    
         JZ    CVR130                                                           
         CLI   RQCASFNI,0          ASSERT THAT FIRST NAME SEARCH                
         JE    CVR140              INSTRUCTIONS ARE BLANK                       
         CLI   RQCASFNI,C'E'       OR E                                         
         JE    CVR140                                                           
         J     NO                                                               
                                                                                
CVR130   CLI   RQCASFNI,0          IF FIRST NAME IS NOT PROVIDED                
         JNE   NO                  ASSERT INSTRUCTIONS ARE BLANK                
                                                                                
CVR140   CLI   RQCASCGR,0          ASSERT THAT CATEGORY GROUPING                
         JE    CVR150              IS BLANK                                     
         CLI   RQCASCGR,C'P'       OR P                                         
         JE    CVR150                                                           
         CLI   RQCASCGR,C'E'       OR E                                         
         JNE   NO                                                               
                                                                                
CVR150   GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASRCT)                       
         JNE   NO                                                               
                                                                                
         OC    RQCASCGC,RQCASCGC   IF CHECK GRT VALID FOR CLIENT ...            
         JZ    CVR160              IS PROVIDED                                  
         OC    RQCASCGA,RQCASCGA   ASSERT THAT CHECK GRT VALID FOR              
         JZ    NO                  AGENCY ... IS PROVIDED                       
                                                                                
CVR160   GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASEP6)                       
         JNE   NO                                                               
                                                                                
         CLI   RQCASEMU,C'C'                                                    
         JE    CVR170                                                           
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASEMU)                       
         JNE   NO                                                               
                                                                                
CVR170   GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASPAD)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASES9)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCASELA)                       
         JNE   NO                                                               
                                                                                
         CLI   ARQCACOM,0          IF INTERNAL COMMERCIAL NUMBERS               
         JE    YES                 ARE PROVIDED                                 
         OC    RQCASLIM,RQCASLIM   ASSERT LIMIT RESULTS TO IS                   
         JZ    YES                 NOT PROVIDED                                 
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS INTERNAL COMMERCIAL NUMBER FILTER LIST        *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
BLDCOMFL NTR1                                                                   
         L     R2,ACOMTAB          INITIALIZE INTERNAL COMMERCIAL               
         MVI   0(R2),X'FF'         NUMBER TABLE                                 
                                                                                
***********************************************************************         
                                                                                
         CLI   RQCASCIN,0          IF ANY INTERNAL COMMERCIAL NUMBERS           
         JE    BCFL20              PROVIDED                                     
         ZIC   RE,RQCASCIN         SAVE THEM INTO INTERNAL COMMERCIAL           
         ZICM  RF,ARQCACOM,3       NUMBER TABLE                                 
BCFL10   MVC   0(L'RQCASCOM,R2),0(RF)                                           
         LA    R2,L'RQCASCOM(R2)                                                
         MVI   0(R2),X'FF'                                                      
         LA    RF,L'RQCASCOM(RF)                                                
         BCT   RE,BCFL10                                                        
         J     BCFL100                                                          
                                                                                
***********************************************************************         
                                                                                
BCFL20   OC    RQCASCOT,RQCASCOT   IF COMMERCIAL TITLE IS PROVIDED              
         JZ    BCFL70                                                           
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR ALL COMMERCIAL KEY/RECORDS          
         MVI   TLCOPCD,TLCOBCDQ    THAT MATCH COMMERCIAL TITLE FILTER           
         MVC   TLCOBNAM,RQCASCOT                                                
                                                                                
         GOTOR SVLENGTH,DMCB,(L'TLCOBNAM,RQCASCOT),LKEYCOMP,0                   
         ZIC   R4,LKEYCOMP                                                      
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     BCFL40                                                           
BCFL30   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
BCFL40   EX    R4,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JNE   BCFL100                                                          
         TM    TLCOBSTA,TLCOBSID   EXCLUDE COMMERCIAL ID KEYS                   
         JO    BCFL30                                                           
                                                                                
         LR    R1,R2                                                            
BCFL50   CLI   0(R1),X'FF'                                                      
         JE    BCFL60                                                           
         CLC   TLCOBCOM,0(R1)      EXCLUDE IF INTERNAL COMMERCIAL               
         JE    BCFL30              NUMBER ALREADY SAVED                         
         LA    R1,L'TLCOBCOM(R1)                                                
         J     BCFL50                                                           
                                                                                
BCFL60   MVC   0(L'TLCOBCOM,R1),TLCOBCOM                                        
         MVI   L'TLCOBCOM(R1),X'FF'                                             
         J     BCFL30                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
BCFL70   OC    RQCASAGY,RQCASAGY   IF AGENCY IS NOT PROVIDED                    
         JNZ   YES                                                              
         OC    RQCASCID,RQCASCID   AND SEARCHING FOR COMMERCIAL ID              
         JZ    YES                                                              
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   READ FOR ALL COMMERCIAL KEY/RECORDS          
         MVI   TLCOPCD,TLCOBCDQ    THAT MATCH COMMERCIAL ID FILTER              
         MVC   TLCOBNAM(L'RQCASCID),RQCASCID                                    
         OC    TLCOBNAM,SPACES                                                  
         MVI   TLCOBSTA,TLCOBSID                                                
                                                                                
         XC    RQCASCID,RQCASCID                                                
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     BCFL90                                                           
BCFL80   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
BCFL90   CLC   IOKEY(TLCOBCOM-TLCOPD),IOKEYSAV                                  
         JNE   BCFL100                                                          
                                                                                
         MVC   0(L'TLCOBCOM,R2),TLCOBCOM                                        
         LA    R2,L'TLCOBCOM(R2)   ADD COMMERCIAL TO FILTER LIST                
         MVI   0(R2),X'FF'                                                      
         J     BCFL80                                                           
         DROP  R3                                                               
                                                                                
***********************************************************************         
                                                                                
BCFL100  L     R2,ACOMTAB          IF SEARCHING FOR ANY INTERNAL                
         CLI   0(R2),X'FF'         COMMERCIAL NUMBERS                           
         JE    NO                                                               
         MVC   RQCASCOM,0(R2)      SET TO READ FOR THE FIRST ONE                
         XC    0(L'RQCASCOM,R2),0(R2)                                           
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE SETS CAST SEARCH'S TRACK STATUS                                
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
SETTST   NTR1                                                                   
         CLI   RQCASTIN,1          IF FILTERING ON JUST ONE TRACK               
         JNE   XIT                                                              
         CLI   CURTRKFL+1,C' '                                                  
         JNE   XIT                                                              
         CLI   CURTRKFL,0                                                       
         JE    XIT                                                              
                                                                                
         MVI   CASSTST,CASSTS2      INITIALIZE TO NOT FOUND                     
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY    READ FOR AFM CONTRACT                       
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,RQCASCOM                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAMCELQ       READ THROUGH ALL TRACKS                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
STST10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         CLC   TAMCTRK,CURTRKFL     IF TRACK IS FOUND                           
         JNE   STST10               SET TO AFM# DOES NOT MATCH CID              
         MVI   CASSTST,CASSTS3                                                  
                                                                                
         OC    TAMCCON,TAMCCON      IF AFM# MATCHES COMMERCIAL ID               
         JNZ   XIT                  SET TO SUCCESSFUL                           
         MVI   CASSTST,CASSTS1                                                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS CAST SEARCH'S SOCIAL SECURITY NUMBER          *         
*        FILTER LIST                                                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
BLDCASSN NTR1                                                                   
         L     R2,ASSNTAB                                                       
         MVI   0(R2),X'FF'                                                      
                                                                                
         OC    RQCASLNM,RQCASLNM   IF LAST NAME IS PROVIDED                     
         JZ    YES                                                              
         OC    RQCASCOM,RQCASCOM   AND NOT SEARCHING FOR ONE                    
         JNZ   YES                 PARTICULAR COMMERCIAL ID                     
         OC    RQCASCID,RQCASCID                                                
         JNZ   YES                                                              
                                                                                
         USING TLW4PD,R3                                                        
         XC    TLW4PKEY,TLW4PKEY   READ FOR ALL W4 KEY/RECORDS                  
         MVI   TLW4PCD,TLW4NCDQ    THAT MATCH LAST NAME FILTER                  
         MVC   TLW4NLST,RQCASLNM                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     BCASSN20                                                         
BCASSN10 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
BCASSN20 ZIC   RE,LNAMELEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   TLW4NLST(0),RQCASLNM                                             
         JNE   BCASSN40                                                         
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLW4D,R4                                                         
         L     R4,AIO3             R1=A(W4 RECORD)                              
         MVC   SVSSN,TLW4SSN       SAVE SS#                                     
         DROP  R4                                                               
                                                                                
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         BRAS  RE,FLTW4FN          ENSURE FIRST NAME MATCHES FILTER             
         JNE   BCASSN10                                                         
         BRAS  RE,FLTW4TY          ENSURE W4 TYPE IS NOT EXCLUDED               
         JNE   BCASSN10                                                         
         DROP  R4                                                               
                                                                                
BCASSN30 GOTOR CKW4ACC,DMCB,SVSSN  ENSURE STAFF HAS ACCESS                      
         JNE   BCASSN10                                                         
                                                                                
         MVC   0(L'SVSSN,R2),SVSSN ADD SSN TO FILTYER LIST                      
         LA    R2,L'SVSSN(R2)                                                   
         MVI   0(R2),X'FF'                                                      
         J     BCASSN10                                                         
                                                                                
***********************************************************************         
                                                                                
BCASSN40 L     R2,ASSNTAB          IF ANY W4S MACTHED LAST NAME                 
         CLI   0(R2),X'FF'         FILTER                                       
         JE    NO                                                               
         MVC   RQCASSSN,0(R2)      SET TO READ FOR THE FIRST ONE                
         XC    0(L'SVSSN,R2),0(R2)                                              
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE SAVES PER CYCLE GUARANTEE VARIABLES                  *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLGUD,R3                                                         
STPCVARS NTR1                                                                   
         XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ      READ GUARANTEE KEY/RECORD                    
         MVC   TLGUSSN,RQCASSSN                                                 
         MVC   TLGUGUA,RQCASGUA                                                 
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO3             R4=A(GUARANTEE RECORD)                       
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAGUCOM,TAGUCOM     IF GUARANTEE IS PER CYCLE                    
         JZ    XIT                 SAVE PRIMARY COMMERCIAL                      
         MVC   PRICOM,TAGUCOM                                                   
         DROP  R4                                                               
                                                                                
***********************************************************************         
                                                                                
         XC    FPCYSTRT,FPCYSTRT                                                
         XC    LPCYSTRT,LPCYSTRT                                                
                                                                                
         USING TLCKPD,R3                                                        
         XC    TLCKPKEY,TLCKPKEY   READ ALL CHECK RECORDS FOR THIS              
         MVI   TLCKPCD,TLCKHCDQ    PERFORMER ON PRIMARY COMMERCIAL              
         MVC   TLCKHCOM,PRICOM                                                  
         MVC   TLCKHSSN,RQCASSSN                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     SPCV20                                                           
SPCV10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
SPCV20   CLC   IOKEY(TLCKHCAT-TLCKPCD),IOKEYSAV                                 
         JNE   SPCV60                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO4'                           
         DROP  R3                                                               
                                                                                
         USING TACDD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BRAS  RE,GETEL                                                         
         JNE   SPCV10                                                           
         OC    TACDDTE,TACDDTE     ONLY CONSIDER CHECKS THAT HAVE               
         JNZ   SPCV10              NOT PROCESSED YET                            
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TACAELQ      GET CAST DETAIL ELEMENT                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TACAGUA,RQCASGUA    ONLY CONSIDER CHECKS WITH GUARANTEE          
         JNE   SPCV10              CODE THAT MATCHES CURRENT PAYMENT            
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGAUSES          RF=DISPLACEMENT OF USES TABLE                
         DROP  RE                                                               
                                                                                
         USING USETABD,RF                                                       
         AR    RF,RE               RF=A(USE TABLE)                              
                                                                                
SPCV30   CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TAPDUSE,USECDE      FIND USE EQUATE IN USE TABLE                 
         JE    SPCV40                                                           
         LH    RE,USELEN                                                        
         AR    RF,RE                                                            
         J     SPCV30                                                           
                                                                                
SPCV40   TM    USESTAT2,APPREUSE   IF CHECK IS FOR A PER CYCLE PAYMENT          
         JZ    SPCV10                                                           
         MVC   SVIOKEY,IOKEY       SAVE CHECK KEY                               
         DROP  R4,RF                                                            
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO4             R4=A(CHECK RECORD)                           
                                                                                
         USING TLINPD,R3                                                        
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINBCDQ    CHECK IF INVOICE HAS ALREADY BEEN            
         MVC   TLINBAGY,TLCKAGY    BILLED                                       
         MVC   TLINBINV,TLCKINV                                                 
         XC    TLINBINV,=6X'FF'                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   SPCV50                                                           
         TM    TLINBST2,TAINSBIL   IF NOT, LEAVE CYCLE START DATE               
         JZ    XIT                 UNSET AND EXIT                               
         DROP  R3,R4                                                            
                                                                                
SPCV50   MVC   IOKEY,SVIOKEY       RESTORE CHECK READ SEQUENCE                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     SPCV10                                                           
                                                                                
***********************************************************************         
                                                                                
         USING TAGCD,R4                                                         
SPCV60   L     R4,AIO3             IF ALL PER CYCLE PAYMENTS HAVE               
         MVI   ELCODE,TAGCELQ      BEEN PROCESSED, SAVE CYCLE START             
         BRAS  RE,GETEL            DATE OF THE LATEST ONE                       
         J     *+8                                                              
SPCV70   BRAS  RE,NEXTEL                                                        
         JNE   SPCV80                                                           
         MVC   LPCYSTRT,TAGCSTRT                                                
         J     SPCV70                                                           
         DROP  R4                                                               
                                                                                
         USING TLCAPD,R3                                                        
SPCV80   XC    TLCAPKEY,TLCAPKEY   READ CAST RECORD FOR PRIMARY                 
         MVI   TLCAPCD,TLCAGCDQ    COMMERCIAL                                   
         MVC   TLCAGSSN,RQCASSSN                                                
         MVC   TLCAGGUA,RQCASGUA                                                
         MVC   TLCAGCOM,PRICOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLCAGCAT-TLCAPCD),IOKEYSAV                                 
         JNE   SPCV90                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL            IF FIRST FIXED CYCLE AT CAST LEVEL           
         JE    *+6                 SAVE AS FIRST PER CYCLE PAYMENT              
         DC    H'00'               START DATE                                   
         OC    TACAFCYC,TACAFCYC                                                
         JZ    SPCV90                                                           
         MVC   FPCYSTRT,TACAFCYC                                                
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         USING TLCOPD,R3                                                        
SPCV90   XC    TLCOPKEY,TLCOPKEY   IF FFC NOT AT CAST LEVEL                     
         MVI   TLCOPCD,TLCOCCDQ    READ PRIMARY COMMERCIAL RECORD               
         MVC   TLCOCCOM,PRICOM                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                 SAVE COMMERCIAL FIRST FIXED CYCLE            
         DC    H'00'               AS FIRST PER CYCLE PAYMENT START             
         MVC   FPCYSTRT,TACOFCYC   DATE                                         
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT CAST SEARCH EXPANDED DETAILS RECORDS                  *         
***********************************************************************         
                                                                                
NXTCAX   J     *+12                                                             
         DC    C'*NXTCAX*'                                                      
         LR    RB,RF                                                            
         USING NXTCAX,RB                                                        
                                                                                
         CLI   CASSTAT,CASSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NCAX10              READ NEXT CAST RECORD                        
         OC    RQCASSEQ,RQCASSEQ                                                
         JNZ   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTCAST                                                       
         JNE   NOMORE                                                           
                                                                                
NCAX10   OC    RQCASLIM,RQCASLIM   DONE IF RECORD LIMIT WAS PROVIDED            
         JZ    NCAX20                                                           
         OC    ABOVELIM,ABOVELIM   AND HAS BEEN REACHED                         
         JNZ   NCAX20                                                           
         MVI   RQCASLIM,X'FF'                                                   
         J     NOMORE                                                           
                                                                                
NCAX20   MVI   CAXFCE,C'N'                                                      
                                                                                
         USING TLCAD,R4                                                         
         L     R4,IOADDR           R4=A(CAST RECORD)                            
         GOTO1 VHEXOUT,DMCB,TLCACOM,CAXCOM,L'TLCACOM,0                          
         GOTO1 (RF),(R1),TLCASEQ,CAXSEQ,L'TLCASEQ,0                             
         OC    TLCASRVS,TLCASRVS                                                
         JZ    NCAX25                                                           
         GOTO1 (RF),(R1),TLCASRVS,CAXVSQ,L'TLCASRVS,0                           
         DROP  R4                                                               
                                                                                
NCAX25   MVI   CAXALL,C'N'                                                      
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
NCAX30   BRAS  RE,NEXTEL                                                        
         JNE   NCAX130                                                          
                                                                                
         USING TACMD,R4                                                         
         CLI   0(R4),TACMELQ       PROCESS COMMENT ELEMENTS                     
         JNE   NCAX50                                                           
         LA    RE,CAXCMT                                                        
         CLI   TACMTYPE,TACMTYPG   PASS BACK GENERAL COMMENT                    
         JE    NCAX40                                                           
         LA    RE,CAXRDE                                                        
         CLI   TACMTYPE,TACMTYPD   OR ROLE DESCRIPTION                          
         JNE   NCAX30                                                           
NCAX40   ZIC   RF,TACMLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),TACMCOMM                                                 
         J     NCAX30                                                           
         DROP  R4                                                               
                                                                                
         USING TARLD,R4                                                         
NCAX50   CLI   0(R4),TARLELQ       PROCESS CAST RELEASE STATUS ELEMENT          
         JNE   NCAX60                                                           
         MVC   CAXRLL,TARLSTAT     PASS BACK ...   RELEASE STATUS               
         OC    TARLDATE,TARLDATE                                                
         JZ    NCAX50A                                                          
         GOTO1 VDATCON,DMCB,(1,TARLDATE),(8,CAXRLD) RELEASE DATE                
NCAX50A  OC    TARLEFDT,TARLEFDT                                                
         JZ    NCAX50B                                                          
         GOTO1 VDATCON,DMCB,(1,TARLEFDT),(8,CAXEFF) EFFECTIVE DATE              
NCAX50B  MVC   CAXPRL,TARLSTAP                      PREV. REL STATUS            
         OC    TARLDATP,TARLDATP                                                
         JZ    NCAX30                                                           
         GOTO1 VDATCON,DMCB,(1,TARLDATP),(8,CAXPLD) PREV. REL DATE              
         J     NCAX30                                                           
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
NCAX60   CLI   0(R4),TACAELQ       PROCESS CAST DETAILS ELEMENT                 
         JNE   NCAX70                                                           
         MVC   CAXONO,TACAONOF     PASS BACK ...    ON/OFF CAMERA               
         MVC   CAXTAX,TACAUNIT                      TAX UNIT                    
         MVC   CAXUNI,TACAUN                        UNION                       
         MVC   CAXLCL,TACALOCL                      LOCAL                       
         MVC   CAXCYR,TACAYEAR                      CONTRACT YEAR               
         GOTO1 VDATCON,DMCB,(1,TACAFCYC),(8,CAXFFC) FIRST FIXED CYCLE           
         GOTO1 (RF),(R1),(1,TACAFRST),(8,CAXFSV)    FIRST SERVICES              
         GOTO1 (RF),(R1),(1,TACALAST),(8,CAXLSV)    LAST SERVICES               
         GOTO1 (RF),(R1),(1,TACAEXP),(8,CAXEXP)     EXPIRATION DATE             
         MVC   CAXDBL,TACADBL                       DOUBLES                     
         MVC   CAX2OP,TACAOV2                       2ND OVERSCALE %             
                                                                                
         MVI   CAXMST,C'Y'         PASS BACK "ON MASTER COMMERCIAL?"            
         TM    TACASTAT,TACASTLO                                                
         JZ    *+8                                                              
         MVI   CAXMST,C'N'                                                      
                                                                                
         MVI   CAXLFT,C'N'         PASS BACK "ON LIFT VERSION?"                 
         TM    TACASTAT,TACASTLF                                                
         JZ    *+8                                                              
         MVI   CAXLFT,C'Y'                                                      
                                                                                
         MVI   CAXCPA,C'N'         PASS BACK "CHECKS PAYABLE TO AGENT?"         
         TM    TACASTAT,TACASTAG                                                
         JZ    *+8                                                              
         MVI   CAXCPA,C'Y'                                                      
                                                                                
         MVI   CAXPCR,C'N'         PASS BACK "PAY CANADIAN RATES                
         TM    TACASTA2,TACASTCR   ON US COMMERCIAL?"                           
         JZ    *+8                                                              
         MVI   CAXPCR,C'Y'                                                      
                                                                                
         MVI   CAXPUR,C'N'         PASS BACK "PAY US RATES ON                   
         TM    TACASTA2,TACASTDP   CANADIAN COMMERCIAL?"                        
         JZ    *+8                                                              
         MVI   CAXPUR,C'Y'                                                      
                                                                                
         MVI   CAXFGN,C'Y'         PASS BACK "FOREIGN USE?                      
         TM    TACASTA2,TACASFGR                                                
         JZ    *+8                                                              
         MVI   CAXFGN,C'N'                                                      
                                                                                
         MVI   CAXINA,C'Y'         PASS BACK "INTERACTIVE USE?"                 
         TM    TACASTA2,TACASINA                                                
         JZ    *+8                                                              
         MVI   CAXINA,C'N'                                                      
                                                                                
         MVI   CAXINR,C'Y'         PASS BACK "INDUSTRIAL USE?"                  
         TM    TACASTA2,TACASINR                                                
         JZ    *+8                                                              
         MVI   CAXINR,C'N'                                                      
                                                                                
         MVI   CAXACP,C'Y'         PASS BACK "APPLY CBL TO PER CYCLE?"          
         TM    TACASTAT,TACASXAC                                                
         JZ    *+8                                                              
         MVI   CAXACP,C'N'                                                      
                                                                                
         MVI   CAXAPP,C'Y'         PASS BACK "APPLY PAX TO PER CYCLE?"          
         TM    TACASTAT,TACASXAP                                                
         JZ    *+8                                                              
         MVI   CAXAPP,C'N'                                                      
                                                                                
         MVI   CAXPDT,C'Y'         PASS BACK "PRINT DAILY FTRACKS?"             
         TM    TACASTAT,TACASTNF                                                
         JZ    *+8                                                              
         MVI   CAXPDT,C'N'                                                      
                                                                                
         MVI   CAXCLB,C'N'         PASS BACK "CELEBRITY?"                       
         TM    TACASTAT,TACASCLB                                                
         JZ    *+8                                                              
         MVI   CAXCLB,C'N'                                                      
                                                                                
         MVI   CAXGRR,C'N'         PASS BACK "GRR COVERS ALL USE?"              
         TM    TACASTA2,TACASPUS                                                
         JZ    *+8                                                              
         MVI   CAXGRR,C'Y'                                                      
                                                                                
         MVI   CAXEUR,C'N'         PASS BACK "PAID IN EUROS?"                   
         TM    TACASTA2,TACASEUR                                                
         JZ    *+8                                                              
         MVI   CAXEUR,C'Y'                                                      
                                                                                
         MVI   CAXAFT,C'Y'         PASS BACK "APPLY USE TO FTRACKS?"            
         TM    TACASTA3,TACASXFT                                                
         JZ    *+8                                                              
         MVI   CAXAFT,C'N'                                                      
                                                                                
         MVI   CAXEQY,C'Y'         PASS BACK "EQUITY?"                          
         TM    TACASTA3,TACASEQY                                                
         JZ    *+8                                                              
         MVI   CAXEQY,C'N'                                                      
                                                                                
         MVC   SVYEAR,TACAYEAR     SAVED CONTRACT YEAR                          
         MVC   LFTRSTRT,TACAFCYC   AND FIRST FIXED CYCLE                        
                                                                                
         GOTO1 VDATCON,DMCB,(1,TACARERC),(8,CAXRER)    RERECORD DATE            
         OC    TACATSEQ,TACATSEQ                                                
         JZ    NCAX30                                                           
         GOTO1 VHEXOUT,DMCB,TACATSEQ,CAXTSQ,L'TACATSEQ,0                        
         J     NCAX30                                                           
         DROP  R4                                                               
                                                                                
NCAX70   CLI   0(R4),TACRELQ       PROCESS APPLIED CREDIT HISTORY               
         JNE   NCAX80              ELEMENT                                      
         MVI   CAXFCE,C'Y'         PASS BACK "FIXED CYCLE EXISTS?"              
         J     NCAX30                                                           
                                                                                
         USING TAO2D,R4                                                         
NCAX80   CLI   0(R4),TAO2ELQ       PROCESS SECOND OVERSCALE                     
         JNE   NCAX90              PERCENTAGE ELEMENT                           
                                                                                
         ZIC   R1,TAO2NUM          R1=# OF OVERSCALE PERCENTAGES                
         LA    R2,CAX2UP1          R2=A(2ND OVERSCALE % OUTPUT FIELD)           
         LA    R6,TAO2SBEL         R6=A(1ST 2ND OVERSCALE % SUB-ELEM)           
                                                                                
         USING TAO2SBEL,R6                                                      
NCAX80A  MVC   0(L'CAX2UP1,R2),TAO2USE                                          
         CLC   TAO2USE,SPACES                                                   
         JNE   *+10                                                             
         MVC   0(3,R2),=C'ALL'                                                  
         MVC   L'CAX2UP1(L'CAX2OP1,R2),TAO2PCT                                  
         DROP  R6                                                               
                                                                                
         LA    R2,CAX2OLNQ(R2)     BUMP TO NEXT OUTPUT FIELD                    
         LA    R6,L'TAO2SBEL(R6)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R1,NCAX80A                                                       
         J     NCAX30                                                           
         DROP  R4                                                               
                                                                                
         USING TAOAD,R4                                                         
NCAX90   CLI   0(R4),TAOAELQ       PROCESS OVERSCALE AMOUNT ELEMENT             
         JNE   NCAX100                                                          
                                                                                
         ZIC   R1,TAOANUM          R1=# OF OVERSCALE AMOUNTS                    
         LA    R2,CAXUA1           R2=A(OVERSCALE AMOUNT OUTPUT FIELD)          
         LA    R6,TAOASBEL         R6=A(1ST OVERSCALE AMOUNT SUB-ELEM)          
                                                                                
         USING TAOASBEL,R6                                                      
NCAX90A  MVC   0(L'CAXUA1,R2),TAOAUSE                                           
         CLC   TAOAUSE,SPACES                                                   
         JNE   *+10                                                             
         MVC   0(3,R2),=C'ALL'                                                  
         MVC   L'CAXUA1(L'CAXOA1,R2),TAOAAMT                                    
         DROP  R6                                                               
                                                                                
         LA    R2,CAXOALNQ(R2)     BUMP TO NEXT OUTPUT FIELD                    
         LA    R6,L'TAOASBEL(R6)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R1,NCAX90A                                                       
         J     NCAX30                                                           
         DROP  R4                                                               
                                                                                
         USING TAOPD,R4                                                         
NCAX100  CLI   0(R4),TAOPELQ       PROCESS OVERSCALE PERCENTAGE ELEMENT         
         JNE   NCAX110                                                          
                                                                                
         ZIC   R1,TAOPNUM          R1=# OF OVERSCALE PERCENTAGES                
         LA    R2,CAXUP1           R2=A(OVERSCALE % OUTPUT FIELD)               
         LA    R6,TAOPSBEL         R6=A(1ST OVERSCALE % SUB-ELEM)               
                                                                                
         USING TAOPSBEL,R6                                                      
NCAX100A MVC   0(L'CAXUA1,R2),TAOPUSE                                           
         CLC   TAOPUSE,SPACES                                                   
         JNE   *+10                                                             
         MVC   0(3,R2),=C'ALL'                                                  
         MVC   L'CAXUP1(L'CAXOP1,R2),TAOPPCT                                    
         DROP  R6                                                               
                                                                                
         LA    R2,CAXOPLNQ(R2)     BUMP TO NEXT OUTPUT FIELD                    
         LA    R6,L'TAOPSBEL(R6)   BUMP TO NEXT SUB-ELEMENT                     
         BCT   R1,NCAX100A                                                      
         J     NCAX30                                                           
         DROP  R4                                                               
                                                                                
         USING TATRD,R4                                                         
NCAX110  CLI   0(R4),TATRELQ       PROCESS MUSIC CONTRACT/TRACK                 
         JNE   NCAX120             ELEMENT                                      
         OC    CAXACO,CAXACO                                                    
         JNZ   NCAX110A                                                         
         MVC   CAXACONC,TATRCOM                                                 
         GOTO1 VHEXOUT,DMCB,TATRCOM,CAXACO,L'TATRCOM,0                          
         GOTO1 (RF),(R1),TATRCSQ,CAXASQ,L'TATRCSQ,0                             
NCAX110A BRAS  RE,SETAFM           GET AFM CONTRACT RECORD                      
         LA    RE,CAXTR1                                                        
NCAX110B CLI   0(RE),0                                                          
         JNE   NCAX110C                                                         
         MVC   0(1,RE),TATRTRK                                                  
         J     NCAX30                                                           
NCAX110C LA    RE,1(RE)                                                         
         J     NCAX110B                                                         
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
NCAX120  CLI   0(R4),TAFNELQ       PROCESS FREE FORM NAME ELEMENT               
         JNE   NCAX30                                                           
                                                                                
         CLI   TAFNTYPE,TAFNTVER   PASS BACK ALL VERSIONS INDICATOR             
         JNE   NCAX120A                                                         
         CLI   TAFNNAME,251                                                     
         JNE   NCAX30                                                           
         MVI   CAXALL,C'Y'                                                      
         J     NCAX30                                                           
NCAX120A LA    RE,CAXTR1           PASS BACK TRACKS                             
         CLI   TAFNTYPE,TAFNTTRK                                                
         JE    NCAX120B                                                         
         LA    RE,CAXWID           PASS BACK WEB APPLICATION ID                 
         CLI   TAFNTYPE,TAFNTWEB                                                
         JNE   NCAX30                                                           
NCAX120B ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     NCAX30                                                           
         MVC   0(0,RE),TAFNNAME                                                 
         J     NCAX30                                                           
         DROP  R4                                                               
                                                                                
NCAX130  BAS   RE,OUTORE           OUTPUT COMMERICAL PAID REUSE STATUS          
         BAS   RE,OUTPCS           OUTPUT PER CYCLE STATUS                      
                                                                                
         OC    RQCASLIM,RQCASLIM   IF RECORD LIMIT WAS PROVIDED                 
         JZ    NCAXX                                                            
         LH    R0,ABOVELIM         DECREMENT IT NOW                             
         SHI   R0,1                                                             
         STH   R0,ABOVELIM                                                      
                                                                                
NCAXX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CAXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE OUTPUTS COMMERCIAL PAID REUSE STATUS                 *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
OUTORE   NTR1                                                                   
         CLI   RQCASORE,C'Y'       IF REQUESTING COMMERCIAL PAID                
         JNE   XIT                 REUSE STATUS                                 
         MVI   CAXORE,C'N'         SET DEFAULT VALUE                            
                                                                                
         USING TGTABLES,R1                                                      
         L     R1,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     R2,TGAUSES          RF=DISPLACEMENT OF USES TABLE                
         AR    R2,R1               R2=A(USES TABLE)                             
         DROP  R1                                                               
                                                                                
         USING TLINPD,R3                                                        
         XC    TLINPKEY,TLINPKEY   READ ALL INVOICES FOR THIS                   
         MVI   TLINPCD,TLINHCDQ    COMMERCIAL                                   
         MVC   TLINHCOM,SVCOM                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     OORE20                                                           
OORE10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
OORE20   CLC   IOKEY(TLINHINV-TLINPD),IOKEYSAV                                  
         JNE   OORE50                                                           
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   OORE10                                                           
         TM    TAINSTAT,TAINSCIN+TAINSCAN                                       
         JNZ   OORE10                                                           
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BRAS  RE,GETEL                                                         
         JNE   OORE10                                                           
                                                                                
         USING USETABD,RF                                                       
         LR    RF,R2                                                            
OORE30   CLC   TAPDUSE,USECDE      FIND USE CODE IN USE TABLE                   
         JE    OORE40                                                           
         LH    RE,USELEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   OORE30                                                           
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
OORE40   TM    USESTAT,SESSION     IF USE IS NOT A SESSION                      
         JO    OORE10                                                           
         TM    USESTAT2,HLDTYPE    OR HOLDING FEE                               
         JO    OORE10                                                           
         MVI   CAXORE,C'Y'         SET COMMERCIAL HAS BEEN PAID REUSE           
         DROP  RF                                                               
                                                                                
OORE50   MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE OUTPUTS PER CYCLE GUARANTEE STATUS                   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     AIO4=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
OUTPCS   NTR1                                                                   
         OC    RQCASGUA,RQCASGUA   EXIT IF GUARANTEE CODE WAS NOT               
         JZ    XIT                 PROVIDED IN REQUEST                          
         OC    PRICOM,PRICOM       OR PROVIDED GUARANTEE IS NOT A               
         JZ    XIT                 PER CYCLE                                    
                                                                                
         MVI   CAXPCS,CAXPCSP      RETURN PRIMARY COMMERCIAL STATUS             
         CLC   PRICOM,SVCOM        IF THIS IS PRIMARY COMMERCIAL                
         JE    XIT                                                              
                                                                                
         MVI   CAXPCS,CAXPCSX      RETURN PAYMENT PENDING STATUS                
         OC    FPCYSTRT,FPCYSTRT   IF PAYMENT IS PENDING                        
         JZ    XIT                                                              
                                                                                
         MVI   CAXPCS,CAXPCSI      RETURN RELEASED/LAST SERVICED                
         CLC   CAXLSV,SPACES       IF LAST SERVICED                             
         JNE   XIT                                                              
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO3             R4=A(CAST RECORD)                            
         MVI   ELCODE,TACRELQ      READ ALL APPLIED CREDIT ELEMENTS             
         BRAS  RE,GETEL                                                         
         J     OPCS20                                                           
OPCS10   BRAS  RE,NEXTEL                                                        
OPCS20   JNE   OPCS40                                                           
         CLC   TACRUSE,=C'HLD'     REJECT IF NOT FOR HOLDING FEE                
         JE    OPCS30                                                           
         CLC   TACRUSE,=C'SHL'     OR SPANISH HOLDING FEE                       
         JE    OPCS30                                                           
         CLC   TACRUSE,=C'ADH'     OR ADDENDUM HOLDING FEE                      
         JE    OPCS30                                                           
         CLC   TACRUSE,=C'REN'     OR REINSTATEMENT                             
         JE    OPCS30                                                           
         CLC   TACRUSE,=C'SRE'     OR SPANISH REINSTATEMENT                     
         JNE   OPCS10                                                           
OPCS30   MVC   LFTRSTRT,TACRSTRT   SAVE LATEST FTRACK START DATE                
         J     OPCS10                                                           
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
OPCS40   L     R4,AIO4             R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                 MAY NOT BE LOCKED, RELEASED OR               
         DC    H'00'               SET UP FOR CANADIAN RATES                    
         TM    TACOSTAT,TACOSTLO+TACOSTRL+TACOSCRT                              
         JNZ   XIT                                                              
         CLI   TACOMED,TACOMEDT    MEDIA MUST BE TELEVISION                     
         JE    OPCS50                                                           
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         JE    OPCS50                                                           
         CLI   TACOMED,TACOMEDN    OR NEW MEDIA                                 
         JNE   XIT                                                              
                                                                                
OPCS50   OC    LFTRSTRT,LFTRSTRT   IF LATEST FTRACK START DATE                  
         JNZ   OPCS60              STILL IS NOT SET                             
         MVC   LFTRSTRT,TACOFCYC   SET IT AS COMMERCIAL'S FFC                   
         DROP  R4                                                               
                                                                                
OPCS60   CLC   LPCYSTRT,LFTRSTRT   IF LATEST PER CYCLE PAYMENT DOES             
         JE    OPCS70              NOT MATCH CAST'S LATEST FTRACK               
         OC    LPCYSTRT,LPCYSTRT   AND PER CYCLE PAYMENT HAS BEEN MADE          
         JNZ   XIT                 COMM'L IS NOT ELIGIBLE TO BE PRIMARY         
                                                                                
         CLC   LFTRSTRT,FPCYSTRT   IF PER CYCLE PAYMENT HAS NOT BEEN            
         JH    XIT                 MADE,FFC MUST BE THE SAME OR EARLIER         
OPCS70   MVI   CAXPCS,CAXPCSE      ELIGIBLE TO BECOME SUBSIDIARY                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT CAST VERSION RECORD                                   *         
***********************************************************************         
                                                                                
NXTCAV   J     *+12                                                             
         DC    C'*NXTCAV*'                                                      
         LR    RB,RF                                                            
         USING NXTCAV,RB                                                        
                                                                                
         CLI   CASSTAT,CASSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
                                                                                
         XC    CAVVALS(CAVVALL),CAVVALS                                         
                                                                                
         L     R2,SVADDR                                                        
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   *+8                                                              
         LA    R2,CAVRTAB                                                       
                                                                                
         CLI   0(R2),X'FF'                                                      
         JE    NOMORE                                                           
         MVC   CAVVER,0(R2)                                                     
         CLI   RQCASPAD,C'Y'                                                    
         JNE   NCAVX                                                            
         MVC   CAVPAD,1(R2)                                                     
         CLI   CAVPAD,C'Y'                                                      
         JE    NCAVX                                                            
         MVI   CAVPAD,C'N'                                                      
                                                                                
NCAVX    LA    R2,2(R2)                                                         
         ST    R2,SVADDR                                                        
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CAVVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT CAST SEARCH LIMIT STATUS RECORD                       *         
***********************************************************************         
                                                                                
NXTCAL   J     *+12                                                             
         DC    C'*NXTCAL*'                                                      
         LR    RB,RF                                                            
         USING NXTCAL,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         OC    RQCASLIM,RQCASLIM                                                
         JZ    NOMORE                                                           
         MVI   CALSTAT,C'N'                                                     
         CLI   RQCASLIM,X'FF'                                                   
         JNE   NCALX                                                            
         MVI   CALSTAT,C'Y'                                                     
                                                                                
NCALX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CALVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT INVOICE SEARCH STATUS RECORD                          *         
***********************************************************************         
                                                                                
NXTINS   J     *+12                                                             
         DC    C'*NXTINS*'                                                      
         LR    RB,RF                                                            
         USING NXTINS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         MVI   INSSTAT,INSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,INVALREQ         VALIDATE REQUEST                             
         JNE   NINSX                                                            
                                                                                
         MVI   INSSTAT,INSSUNS     INITIALIZE STATUS TO UNSUCCESSFUL            
         MVI   CMTFLAG,0                                                        
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
         XC    0(L'TLDRKEY,R3),0(R3)                                            
                                                                                
         USING TLIND,R3                                                         
         OC    RQINSINV,RQINSINV   IF INVOICE NUMBER IS PROVIDED                
         JZ    NINS10              BUILD KEY                                    
         CLI   RQINSINV+5,C' '                                                  
         JE    NINSX                                                            
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,RQINSAGY                                                 
         GOTOR (#CVTINV,ACVTINV),DMCB,RQINSINV,TLININV                          
         XC    TLININV,=6X'FF'                                                  
         LHI   R0,L'TLINKEY-1      AND SET R0 WITH LENGTH FOR COMPARE           
         J     NINS40                                                           
         DROP  R3                                                               
                                                                                
         USING TLINPD,R3                                                        
NINS10   OC    RQINSCOM,RQINSCOM   IF INTERNAL COMEMRCIAL NUMBER                
         JZ    NINS20              IS PROVIDED                                  
         MVI   TLINPCD,TLINHCDQ    BUILD KEY                                    
         MVC   TLINHCOM,RQINSCOM   AND SET R0 WITH LENGTH FOR COMPARE           
         LHI   R0,TLINHINV-TLINPD-1                                             
         J     NINS40                                                           
                                                                                
NINS20   OC    RQINSWID,RQINSWID   IF WEB APPLICATION ID IS PROVIDED            
         JZ    NINS30                                                           
         MVI   TLINPCD,TLINWCDQ    BUILD KEY                                    
         MVC   TLINWWID,RQINSWID   AND SET R0 WITH LENGTH FOR COMPARE           
         LHI   R0,TLINWAGY-TLINPD-1                                             
         J     NINS40                                                           
                                                                                
NINS30   DC    H'00'                                                            
                                                                                
NINS40   STC   R0,LKEYCOMP         SAVE L' KEY COMPARE                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTINV           IF CAST RECORD MATCHES ALL FILTERS           
         JNE   NINSX               RETURN SEARCH SUCCESSFUL STATUS              
         MVI   INSSTAT,INSSSUC                                                  
         TM    ERRSTAT,ESREVIW                                                  
         JZ    NINSX                                                            
         MVI   INSSTAT,INSSEMB                                                  
                                                                                
NINSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,INSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
INVALREQ NTR1                                                                   
         OC    RQINSSTF,RQINSSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
                                                                                
         OC    RQINSCOM,RQINSCOM   ASSERT THAT INTERNAL COMMERCIAL              
         JNZ   IVR10               NUMBER                                       
         OC    RQINSWID,RQINSWID   OR WEB APPLICATION ID                        
         JNZ   IVR10                                                            
         OC    RQINSAGY,RQINSAGY   OR AGENCY IS PROVIDED                        
         JZ    NO                                                               
                                                                                
         OC    RQINSINV,RQINSINV   IF AGENCY IS PROVIDED, ASSERT THAT           
         JZ    NO                  INVOICE NUMBER IS PROVIDED                   
                                                                                
IVR10    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQINSCNR)                       
         JNE   NO                                                               
                                                                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQINSCNL)                       
         JNE   NO                                                               
                                                                                
         OC    RQINSSEQ,RQINSSEQ   IF CAST SEQUENCE NUMBER IS                   
         JZ    YES                 PROVIDED                                     
         OC    RQINSCOM,RQINSCOM   ASSERT THAT INTERNAL COMMERCIAL              
         JNZ   YES                 NUMBER IS PROVIDED                           
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT INVOICE EXPANDED DETAILS RECORD                       *         
***********************************************************************         
                                                                                
NXTINX   J     *+12                                                             
         DC    C'*NXTINX*'                                                      
         LR    RB,RF                                                            
         USING NXTINX,RB                                                        
                                                                                
         CLI   INSSTAT,INSSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NINX10              READ NEXT INVOICE RECORD                     
         OC    RQINSINV,RQINSINV                                                
         JNZ   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTINV                                                        
         JNE   NOMORE                                                           
                                                                                
NINX10   BRAS  RE,BLDINX                                                        
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,INXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT INVOICE SPLIT DETAILS RECORD                          *         
***********************************************************************         
                                                                                
NXTINL   J     *+12                                                             
         DC    C'*NXTINL*'                                                      
         LR    RB,RF                                                            
         USING NXTINL,RB                                                        
                                                                                
         OC    RQINSINV,RQINSINV   EXIT IF INVOICE NUMBER                       
         JZ    NOMORE              WAS NOT PROVIDED IN REQUEST                  
         CLI   INSSTAT,INSSSUC     OR IF INITIAL SEARCH WAS                     
         JNE   NOMORE              NOT SUCCESSFUL                               
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NINL10                                                           
         XC    INLVALS(INLVALL),INLVALS                                         
                                                                                
         USING TASID,R4                                                         
         L     R4,IOADDR                                                        
         MVI   ELCODE,TASIELQ      READ FIRST/NEXT SPLIT INVOICE                
         BRAS  RE,GETEL            HISTORY ELEMENT                              
         J     NINL20                                                           
NINL10   L     R4,SVADDR                                                        
         BRAS  RE,NEXTEL                                                        
NINL20   JNE   NOMORE                                                           
         ST    R4,SVADDR                                                        
                                                                                
         GOTOR (#CVTINV,ACVTINV),DMCB,TASIINV,INLINV                            
         MVC   INLPCT,TASIPCT                                                   
                                                                                
         ZIC   RE,TASILEN                                                       
         SHI   RE,TASILNQ+1                                                     
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   INLEST(0),TASIEST                                                
         DROP  R4                                                               
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,INLVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT INVOICE NTWK/SYS/MKT DETAILS RECORD                   *         
***********************************************************************         
                                                                                
NXTINM   J     *+12                                                             
         DC    C'*NXTINM*'                                                      
         LR    RB,RF                                                            
         USING NXTINM,RB                                                        
                                                                                
         OC    RQINSINV,RQINSINV   EXIT IF INVOICE NUMBER                       
         JZ    NOMORE              WAS NOT PROVIDED IN REQUEST                  
         CLI   INSSTAT,INSSSUC     OR IF INITIAL SEARCH WAS                     
         JNE   NOMORE              NOT SUCCESSFUL                               
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NINM10                                                           
         XC    INMVALS(INMVALL),INMVALS                                         
                                                                                
         USING TAMTD,R4                                                         
         L     R4,IOADDR                                                        
         MVI   ELCODE,TAMTELQ      READ FIRST/NEXT CNET/CSYS/MARKET             
         BRAS  RE,GETEL            ELEMENT                                      
         J     NINM20                                                           
NINM10   L     R4,SVADDR                                                        
         BRAS  RE,NEXTEL                                                        
NINM20   JNE   NOMORE                                                           
         ST    R4,SVADDR                                                        
                                                                                
         MVC   INMCOD,TAMTCODE                                                  
         GOTO1 VDATCON,DMCB,(1,TAMTCYCS),(8,INMCYS)                             
         GOTO1 (RF),(R1),(1,TAMTCYCE),(8,INMCYE)                                
         DROP  R4                                                               
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,INMVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT INVOICE PROGRAM DETAILS RECORD                        *         
***********************************************************************         
                                                                                
NXTINP   J     *+12                                                             
         DC    C'*NXTINP*'                                                      
         LR    RB,RF                                                            
         USING NXTINP,RB                                                        
                                                                                
         OC    RQINSINV,RQINSINV   EXIT IF INVOICE NUMBER                       
         JZ    NOMORE              WAS NOT PROVIDED IN REQUEST                  
         CLI   INSSTAT,INSSSUC     OR IF INITIAL SEARCH WAS                     
         JNE   NOMORE              NOT SUCCESSFUL                               
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NINP10                                                           
         XC    INPVALS(INPVALL),INPVALS                                         
                                                                                
         USING TANPD,R4                                                         
         L     R4,IOADDR                                                        
         MVI   ELCODE,TANPELQ      READ FIRST/NEXT NETWORK/CLASS A              
         BRAS  RE,GETEL            PROGRAM DETAILS ELEMENT                      
         J     NINP20                                                           
NINP10   L     R4,SVADDR                                                        
         BRAS  RE,NEXTEL                                                        
NINP20   JNE   NOMORE                                                           
         ST    R4,SVADDR                                                        
                                                                                
         GOTO1 VDATCON,DMCB,(1,TANPDATE),(8,INPUDT)                             
         MVC   INPPNM,TANPPNME                                                  
         MVC   INPLFT,TANPLFT                                                   
         MVC   INPNWK,TANPNWK                                                   
         DROP  R4                                                               
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,INPVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT INVOICE ATTACHED US/CAN INVOICE DETAILS RECORD        *         
***********************************************************************         
                                                                                
NXTINA   J     *+12                                                             
         DC    C'*NXTINA*'                                                      
         LR    RB,RF                                                            
         USING NXTINA,RB                                                        
                                                                                
         OC    INXUSI(12),INXUSI   EXIT IF INVOICE DOES NOT HAVE                
         JZ    NOMORE              AN ATTACHED US/CAN INVOICE                   
         OC    RQINSINV,RQINSINV   OR IF INVOICE NUMBER WAS NOT                 
         JZ    NOMORE              PROVIDED IN REQUEST                          
         CLI   INSSTAT,INSSSUC     OR IF INITIAL SEARCH WAS                     
         JH    NOMORE              NOT SUCCESSFUL                               
         CLI   LP_RMODE,LP_RFRST   OR IF THIS IS NOT THE FIRST TIME             
         JNE   NOMORE              IN                                           
                                                                                
         LA    R2,INXUSI                                                        
         CLC   RQINSINV,INXUSI     R2=A(ATTACHED US/CAN INVOICE)                
         JNE   *+8                                                              
         LA    R2,INXCAI                                                        
                                                                                
         USING TLIND,R3                                                         
         LA    R3,IOKEY            READ ATTACHED INVOICE KEY/RECORD             
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,RQINSAGY                                                 
         GOTOR (#CVTINV,ACVTINV),DMCB,0(R2),TLININV                             
         XC    TLININV,=6X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NOMORE                                                           
         MVC   SVIOKEY,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         BRAS  RE,BLDINX                                                        
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,INXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT INVOICE SEARCH COMMENT RECORDS                        *         
***********************************************************************         
                                                                                
NXTINC   J     *+12                                                             
         DC    C'*NXTINC*'                                                      
         LR    RB,RF                                                            
         USING NXTINC,RB                                                        
                                                                                
         MVI   BYTE,0                                                           
         TM    CMTFLAG,TPCDONE     SET TO READ TPC OR CLIENT                    
         JO    NINC10              LEVEL COMMENT RECORD                         
         MVI   BYTE,TLCMTPC                                                     
         OI    CMTFLAG,TPCDONE                                                  
                                                                                
NINC10   CLI   LP_RMODE,LP_RFRST   ONLY EXECUTE FIRST TIME IN                   
         JNE   NOMORE                                                           
         OC    RQINSINV,RQINSINV   IF INVOICE NUMBER WAS PROVIDED               
         JZ    NOMORE              IN REQUEST                                   
         CLI   INSSTAT,INSSSUC     AND INVOICE WAS FOUND                        
         JH    NOMORE                                                           
                                                                                
         USING TLCMD,R3                                                         
         LA    R3,IOKEY                                                         
         XC    0(L'TLCMKEY,R3),0(R3)                                            
         MVI   TLCMCD,TLCMCDQ                                                   
         MVI   TLCMTYP,TLCMTINV                                                 
         MVC   TLCMAGY,RQINSAGY                                                 
         GOTOR (#CVTINV,ACVTINV),DMCB,RQINSINV,TLCMINV                          
         XC    TLCMINV,=6X'FF'                                                  
         MVC   TLCMLEV,BYTE                                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NOMORE                                                           
         DROP  R3                                                               
                                                                                
         LA    R0,INCVALS          CLEAR VARIABLES                              
         LHI   R1,INCVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TAXCD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAXCELQ      READ FIRST EXTENDED COMMENT ELEMENT          
         BRAS  RE,GETEL                                                         
         JNE   NOMORE                                                           
                                                                                
NINC20   LA    RF,INCCM1           COPY EXTENDED COMMENT5 INTO                  
         CLI   TAXCSEQ,1           CORRESPONDING OUTPUT LINE                    
         JE    NINC30                                                           
         ZIC   RE,TAXCSEQ                                                       
         SHI   RE,1                                                             
         MHI   RE,L'INCCM1                                                      
         AR    RF,RE                                                            
NINC30   ZIC   RE,TAXCLEN                                                       
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RF),TAXCCMNT                                                 
         DROP  R4                                                               
                                                                                
         BRAS  RE,NEXTEL           GET NEXT EXTENDED COMMENT ELEMENT            
         JE    NINC20                                                           
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,INCVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD EXPANDED INVOICE DETAILS RECORD                        *         
*        ON ENTRY ... IOADDR=A(INVOICE RECORD)                        *         
***********************************************************************         
                                                                                
BLDINX   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,INXVALS          CLEAR EXPANDED W4 DETAILS                    
         LHI   R1,INXVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   INVSTAT,0           INITIALIZE INVOICE STATUS                    
                                                                                
         USING TLIND,R4                                                         
         L     R4,IOADDR           R4=A(INVOICE RECORD)                         
         MVC   INXAGY,TLINAGY                                                   
         GOTOR (#CVTINV,ACVTINV),DMCB,TLININV,INXINV                            
         DROP  R4                                                               
                                                                                
         MVI   INXRAD,C'N'         INITIALIZE RESERVED FOR ADVICE?              
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
BINX10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         USING TABDD,R4                                                         
         CLI   0(R4),TABDELQ3      PROCESS BILLING DETAILS ELEMENT              
         JNE   BINX20              IN EUROS                                     
         MVC   INXEUBTO,TABDTOT    PASS BACK ... BILLING TOTAL AMOUNT           
         MVC   INXEUBPT,TABDTAX                  PAYROLL TAX AMOUNT             
         MVC   INXEUBHA,TABDHND                  HANDLING AMOUNT                
         MVC   INXEUBCH,TABDHNDC                 CORP HANDLING AMOUNT           
         MVC   INXEUBCS,TABDCSF                  CSF AMOUNT                     
         MVC   INXEUBFC,TABDFICR                 FICA CREDIT AMOUNT             
         MVC   INXEUBGC,TABDGST                  CANADIAN GST AMOUNT            
         CLI   TABDLEN,TABDLN2Q                                                 
         JL    BINX10                                                           
         MVC   INXEUBAC,TABDACOM                 AGY COM AMOUNT                 
         MVC   INXEUBSF,TABDSIGN                 SIGNATORY FEE AMOUNT           
         MVC   INXEUBPS,TABDPST                  PROVINCIAL SERVICE TAX         
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
BINX20   CLI   0(R4),TAAIELQ       PROCESS ADVICE/INVOICE ELEMENT               
         JNE   BINX30                                                           
         MVI   INXRAD,C'Y'                                                      
         J     BINX10                                                           
                                                                                
         USING TABDD,R4                                                         
BINX30   CLI   0(R4),TABDELQ2      PROCESS EMS BILL DETAILS ELEMENT             
         JNE   BINX40                                                           
         MVC   INXETO,TABDTOT      PASS BACK ... BILLING TOTAL AMOUNT           
         MVC   INXEPT,TABDTAX                    PAYROLL TAX AMOUNT             
         MVC   INXEHA,TABDHND                    HANDLING AMOUNT                
         MVC   INXECH,TABDHNDC                   CORP HANDLING AMOUNT           
         MVC   INXEFE,TABDACOM                   EMS FEE AMOUNT                 
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TADDD,R4                                                         
BINX40   CLI   0(R4),TADDELQ       PROCESS DUE DATE ELEMENT                     
         JNE   BINX50                                                           
         GOTO1 VDATCON,DMCB,(1,TADDDATE),(8,INXDUD)                             
         GOTO1 (RF),(R1),(1,TADDPREV),(8,INXPDD)                                
         GOTO1 (RF),(R1),(1,TADDCHNG),(8,INXDCD)                                
         CLI   TADDLEN,TADDLNQ                                                  
         JL    BINX10                                                           
         GOTO1 (RF),(R1),(1,TADDOVRD),(8,INXODD)                                
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
BINX50   CLI   0(R4),TAEUELQ       PROCESS PAYMENT DETAILS ELEMENT              
         JNE   BINX60              IN EUROS                                     
BINX50A  MVC   INXEUPGR,TAPDGRS    PASS BACK ... GROSS AMOUNT                   
         MVC   INXEUPAC,TAPDAPPL                 APPLIED CREDITS                
         MVC   INXEUPGC,TAPDGUAR                 GUARANTEE CREDITS              
         MVC   INXEUPIA,TAPDPAYI                 INDIVIDUAL GROSS               
         MVC   INXEUPCA,TAPDPAYC                 CORPORATION GROSS              
         MVC   INXEUPRE,TAPDREXP                 REIMBURSED EXPENSES            
         MVC   INXEUPSP,TAPDSPNH                 SUBJECT TO P&H                 
         MVC   INXEUPMD,TAPDMDED                 MISC DEDUCTIONS                
         MVC   INXEUPNH,TAPDPNH                  P&H AMOUNT                     
         MVC   INXEUPHW,TAPDHNW                  H&W AMOUNT                     
         MVC   INXEUPIR,TAPDINR                  I&R AMOUNT                     
         MVC   INXEUPUD,TAPDDUES                 UNION DUES AMOUNT              
         OI    INVSTAT,INEUPROC                                                 
         B     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TACMD,R4                                                         
BINX60   CLI   0(R4),TACMELQ       PROCESS COMMENT ELEMENT                      
         JNE   BINX70                                                           
         LA    RE,INXHCM           PASS BACK ... HISTORY COMMENT                
         CLI   TACMTYPE,TACMTYPH                                                
         JE    BINX60A                                                          
         LA    RE,INXICM                         INVOICE COMMENT                
         CLI   TACMTYPE,TACMTYPG                                                
         JNE   BINX10                                                           
BINX60A  ZIC   RF,TACMLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),TACMCOMM                                                 
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TABDD,R4                                                         
BINX70   CLI   0(R4),TABDELQ       PROCESS BILLING DETAILS ELEMENT              
         JNE   BINX80                                                           
         MVC   INXBTY,TABDTYPE     PASS BACK ... BILLING TYPE                   
         MVC   INXBTO,TABDTOT                    BILLING TOTAL AMOUNT           
         MVC   INXBPT,TABDTAX                    PAYROLL TAX AMOUNT             
         MVC   INXBHA,TABDHND                    HANDLING AMOUNT                
         MVC   INXBCH,TABDHNDC                   CORP HANDLING AMOUNT           
         MVC   INXBCS,TABDCSF                    CSF AMOUNT                     
         MVC   INXBFC,TABDFICR                   FICA CREDIT AMOUNT             
         MVC   INXBGC,TABDGST                    CANADIAN GST AMOUNT            
         CLI   INXCDP,C'Y'                                                      
         JE    BINX70A                                                          
         CLI   INXEUP,C'Y'                                                      
         JNE   BINX70B                                                          
BINX70A  OC    TABDCCVT,TABDCCVT                                                
         JZ    BINX70B                                                          
         ZAP   DUB,TABDCCVT                                                     
         CVB   R1,DUB                                                           
         STCM  R1,15,INXCCR                                                     
BINX70B  CLI   TABDLEN,TABDLN2Q                                                 
         JL    BINX10                                                           
         MVC   INXBAC,TABDACOM                   AGY COM AMOUNT                 
         MVC   INXBSF,TABDSIGN                   SIGNATORY FEE AMOUNT           
         MVC   INXBPS,TABDPST                    PROVINCIAL SERVICE TAX         
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TAPAD,R4                                                         
BINX80   CLI   0(R4),TAPAELQ       PROCESS PAY OPTIONS ELEMENT                  
         JNE   BINX90                                                           
                                                                                
         LA    RE,INXPHO                                                        
         CLI   TAPATYPE,TAPATPHR   PASS BACK P&H OVERRIDE RATE                  
         JE    BINX80A                                                          
         LA    RE,INXSOO                                                        
         CLI   TAPATYPE,TAPATSOC   PASS BACK SOC OVERRIDE RATE                  
         JNE   BINX80B                                                          
BINX80A  MVC   0(2,RE),TAPADATA                                                 
         J     BINX10                                                           
                                                                                
BINX80B  LA    RE,INXGAO                                                        
         CLI   TAPATYPE,TAPATGST   PASS BACK GST OVERRIDE AMOUNT                
         JE    BINX80C                                                          
         LA    RE,INXTOO                                                        
         CLI   TAPATYPE,TAPATTAX   PASS BACK TAX OVERRIDE AMOUNT                
         JE    BINX80C                                                          
         LA    RE,INXHOO                                                        
         CLI   TAPATYPE,TAPATHND   PASS BACK HAND OVERRIDE AMOUNT               
         JE    BINX80C                                                          
         LA    RE,INXFCO                                                        
         CLI   TAPATYPE,TAPATFIC   PASS BACK FICA OVERRIDE AMOUNT               
         JE    BINX80C                                                          
         LA    RE,INXHWO                                                        
         CLI   TAPATYPE,TAPATHNW   PASS BACK H&W ADJUSTMENT AMOUNT              
         JE    BINX80C                                                          
         LA    RE,INXFEO                                                        
         CLI   TAPATYPE,TAPATFEE   PASS BACK FEE OVERRIDE AMOUNT                
         JE    BINX80C                                                          
         LA    RE,INXCAO                                                        
         CLI   TAPATYPE,TAPATCSF   PASS BACK CSF OVERRIDE AMOUNT                
         JNE   BINX10                                                           
BINX80C  MVC   0(4,RE),TAPADATA                                                 
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
BINX90   CLI   0(R4),TAPDELQ       PROCESS PAYMENT DETAILS ELEMENT              
         JNE   BINX100                                                          
         MVC   INXUSE,TAPDUSE      PASS BACK ... USE                            
         MVC   INXTYP,TAPDTYPE                   TYPE                           
         MVC   INXCLI,TAPDCLI                    CLIENT                         
         MVC   INXPRD,TAPDPRD                    PRODUCT                        
         GOTO1 VHEXOUT,DMCB,TAPDCOM,INXCOM,L'TAPDCOM,0                          
         GOTO1 VDATCON,DMCB,(1,TAPDCYCS),(8,INXCYS)                             
         GOTO1 (RF),(R1),(1,TAPDCYCE),(8,INXCYE)                                
         MVC   INXPER,TAPDESPD                   ESTIMATE PERIOD                
         MVC   INXTAG,TAPDTAGS                   TAGS                           
         MVC   INXINS,TAPDINS                    INSERTS                        
         MVC   INXDEM,TAPDDEMS                   DEMOS                          
         MVC   INXSTU,TAPDSTUS                   STARTING USE                   
         MVC   INXTUS,TAPDUSES                   TOTAL USES                     
         MVC   INXUNI,TAPDUNIT                   UNITS                          
         MVC   INXOFF,TAPDOFF                    OFFICE                         
         MVC   INXEMP,TAPDEMP                    EMPLOYER                       
                                                                                
         MVI   INXMOV,C'N'         PASS BACK MANUAL OVERRIDE?                   
         TM    TAPDSTAT,TAPDSMAN                                                
         JZ    *+8                                                              
         MVI   INXMOV,C'Y'                                                      
                                                                                
         MVI   INXCDP,C'N'         PASS BACK CANADIAN DOLLAR PAYMENT?           
         TM    TAPDSTAT,TAPDSCAN                                                
         JZ    *+8                                                              
         MVI   INXCDP,C'Y'                                                      
                                                                                
         MVI   INXJOV,C'N'         PASS BACK JOB OVERRIDE?                      
         TM    TAPDSTAT,TAPDSOVJ                                                
         JZ    *+8                                                              
         MVI   INXJOV,C'Y'                                                      
                                                                                
         MVI   INXPCP,C'N'         PASS BACK PER CYCLE GRT PAYMENT?             
         TM    TAPDSTA2,TAPDSPRI                                                
         JZ    *+8                                                              
         MVI   INXPCP,C'Y'                                                      
                                                                                
         MVI   INXFUP,C'N'         PASS BACK FORCED UPGRADE?                    
         TM    TAPDSTA2,TAPDSFUP                                                
         JZ    *+8                                                              
         MVI   INXFUP,C'Y'                                                      
                                                                                
         MVI   INXSUB,C'N'         PASS BACK SUBSIDIARY INVOICE?                
         TM    TAPDSTA2,TAPDSSUB                                                
         JZ    *+8                                                              
         MVI   INXSUB,C'Y'                                                      
                                                                                
         MVI   INXPVC,C'N'         PASS BACK PO VALID FOR CLOSE?                
         TM    TAPDSTA2,TAPDSCPO                                                
         JZ    *+8                                                              
         MVI   INXPVC,C'Y'                                                      
                                                                                
         MVI   INXACP,C'N'         PASS BACK AFTRA CAST ON PAYMENT?             
         TM    TAPDSTA2,TAPDAFTR                                                
         JZ    *+8                                                              
         MVI   INXACP,C'Y'                                                      
                                                                                
         MVI   INXCSL,C'N'         PASS BACK CAST SELECTED?                     
         TM    TAPDPST1,TAPDPSEL                                                
         JZ    *+8                                                              
         MVI   INXCSL,C'Y'                                                      
                                                                                
         MVI   INXCPY,C'N'         PASS BACK CREDIT PAYMENT?                    
         TM    TAPDPST1,TAPDPCRD                                                
         JZ    *+8                                                              
         MVI   INXCPY,C'Y'                                                      
                                                                                
         MVI   INXBNP,C'N'         PASS BACK BILL NOT PAY PAYMENT?              
         TM    TAPDPST1,TAPDPBNP                                                
         JZ    *+8                                                              
         MVI   INXBNP,C'Y'                                                      
                                                                                
         MVI   INXPWC,C'N'         PASS BACK PAYMENT WAS CHANGED?               
         TM    TAPDPST1,TAPDPCHG                                                
         JZ    *+8                                                              
         MVI   INXPWC,C'Y'                                                      
                                                                                
         MVI   INXMIO,C'N'         PASS BACK MARKET INPUT OVERRIDDEN?           
         TM    TAPDPST1,TAPDOVER                                                
         JZ    *+8                                                              
         MVI   INXMIO,C'Y'                                                      
                                                                                
         MVI   INXDMO,C'N'         PASS BACK DUP MKTS OVERRIDDEN?               
         TM    TAPDPST1,TAPDELIM                                                
         JZ    *+8                                                              
         MVI   INXDMO,C'Y'                                                      
                                                                                
         MVI   INXPPC,C'N'         PASS BACK PAYMENT TO PERCYC COMML?           
         TM    TAPDPST2,TAPDPCYC                                                
         JZ    *+8                                                              
         MVI   INXPPC,C'Y'                                                      
                                                                                
         MVI   INXEUP,C'N'         PASS BACK EURO PAYMENT?                      
         TM    TAPDPST2,TAPDPEUR                                                
         JZ    *+8                                                              
         MVI   INXEUP,C'Y'                                                      
                                                                                
         MVI   INXACH,C'N'         PASS BACK USED APPLHLD OPTION?               
         TM    TAPDOPT1,TAPDOAPH                                                
         JZ    *+8                                                              
         MVI   INXACH,C'Y'                                                      
                                                                                
         MVI   INXDAC,C'N'         PASS BACK USED NO APPLY CRED OPTION?         
         TM    TAPDOPT1,TAPDONAC                                                
         JZ    *+8                                                              
         MVI   INXDAC,C'Y'                                                      
                                                                                
         MVI   INXDAG,C'N'         PASS BACK USED NO APPLY GRT OPTION?          
         TM    TAPDOPT1,TAPDONGC                                                
         JZ    *+8                                                              
         MVI   INXDAG,C'Y'                                                      
                                                                                
         MVI   INXCTO,C'N'         PASS BACK USED CANADIAN TAX OPTION?          
         TM    TAPDOPT1,TAPDOCAN                                                
         JZ    *+8                                                              
         MVI   INXCTO,C'Y'                                                      
                                                                                
         MVI   INXURO,C'N'         PASS BACK USED URGENT OPTION?                
         TM    TAPDOPT2,TAPDOURG                                                
         JZ    *+8                                                              
         MVI   INXURO,C'Y'                                                      
                                                                                
         MVI   INXDCO,C'N'         PASS BACK USED DUECOMP CLI OPTION?           
         TM    TAPDOPT2,TAPDODCL                                                
         JZ    *+8                                                              
         MVI   INXDCO,C'Y'                                                      
                                                                                
         MVI   INXDAO,C'N'         PASS BACK USED DUECOMP AGY OPTION?           
         TM    TAPDOPT2,TAPDODAY                                                
         JZ    *+8                                                              
         MVI   INXDAO,C'Y'                                                      
                                                                                
         MVI   INXDLO,C'N'         PASS BACK USED DUECOMP ALL OPTION?           
         TM    TAPDOPT2,TAPDODAL                                                
         JZ    *+8                                                              
         MVI   INXDLO,C'Y'                                                      
                                                                                
         MVI   INXASO,C'N'         PASS BACK USED APPLSESS OPTION?              
         TM    TAPDOPT2,TAPDOAPS                                                
         JZ    *+8                                                              
         MVI   INXASO,C'Y'                                                      
                                                                                
         MVI   INXPTO,C'N'         PASS BACK USED PREV TOT USES OPTION?         
         TM    TAPDOPT2,TAPDOPTU                                                
         JZ    *+8                                                              
         MVI   INXPTO,C'Y'                                                      
                                                                                
         MVI   INXPLO,C'N'         PASS BACK USED PREV LFT USES OPTION?         
         TM    TAPDOPT2,TAPDOPLU                                                
         JZ    *+8                                                              
         MVI   INXPLO,C'Y'                                                      
                                                                                
         MVI   INXPUO,C'N'         PASS USED USE OVERRIDE OPTION?               
         TM    TAPDOPT3,TAPDONUS                                                
         JZ    *+8                                                              
         MVI   INXPUO,C'Y'                                                      
                                                                                
         MVI   INXCRO,C'N'         PASS USED CAN CONV RATE OPTION?              
         TM    TAPDOPT3,TAPDOCCR                                                
         JZ    *+8                                                              
         MVI   INXCRO,C'Y'                                                      
                                                                                
         MVI   INXRPO,C'N'         PASS USED RETRO PAYMENT OPTION?              
         TM    TAPDOPT3,TAPDORET                                                
         JZ    *+8                                                              
         MVI   INXRPO,C'Y'                                                      
                                                                                
         MVI   INXRPO,C'N'         PASS USED RETRO PAYMENT OPTION?              
         TM    TAPDOPT3,TAPDORET                                                
         JZ    *+8                                                              
         MVI   INXRPO,C'Y'                                                      
                                                                                
         MVI   INXGPO,C'N'         PASS USED GREY PAYMENT OPTION?               
         TM    TAPDOPT3,TAPDOGRY                                                
         JZ    *+8                                                              
         MVI   INXGPO,C'Y'                                                      
                                                                                
         MVI   INXPRO,C'N'         PASS USED PUR PAYMENT OPTION?                
         TM    TAPDOPT3,TAPDOCOD                                                
         JZ    *+8                                                              
         MVI   INXPRO,C'Y'                                                      
                                                                                
         MVI   INXCHO,C'N'         PASS USED CORP HAND OVER OPTION?             
         TM    TAPDOPT3,TAPDOHNC                                                
         JZ    *+8                                                              
         MVI   INXCHO,C'Y'                                                      
                                                                                
         MVI   INXCSO,C'N'         PASS USES CSF OPTION?                        
         TM    TAPDOPT4,TAPDOCSF                                                
         JZ    *+8                                                              
         MVI   INXCSO,C'Y'                                                      
                                                                                
         MVI   INXNIO,C'N'         PASS USED NO INTERFACE OPTION?               
         TM    TAPDOPT4,TAPDONOI                                                
         JZ    *+8                                                              
         MVI   INXNIO,C'Y'                                                      
                                                                                
         MVI   INXGEO,C'N'         PASS USED G=E OPTION?                        
         TM    TAPDOPT4,TAPDGRTE                                                
         JZ    *+8                                                              
         MVI   INXGEO,C'Y'                                                      
                                                                                
         CLC   TAPDUSE,=C'WSP'     IF USE IS WILDSPOT ...                       
         JNE   BINX90A                                                          
         MVI   INXINY,C'N'         PASS BACK NY MAJOR INDICATOR                 
         TM    TAPDMAJ,NY                                                       
         JZ    *+8                                                              
         MVI   INXINY,C'Y'                                                      
         MVI   INXICH,C'N'         PASS BACK CHI MAJOR INDICATOR                
         TM    TAPDMAJ,CHI                                                      
         JZ    *+8                                                              
         MVI   INXICH,C'Y'                                                      
         MVI   INXILA,C'N'         PASS BACK LA MAJOR INDICATOR                 
         TM    TAPDMAJ,LA                                                       
         JZ    BINX90A                                                          
         MVI   INXILA,C'Y'                                                      
                                                                                
BINX90A  TM    TAPDPST2,TAPDPEUR   IF NOT A EURO PAYMENT                        
         JZ    BINX90B                                                          
         TM    INVSTAT,INEUPROC    OR A EURO PAYMENT THAT HAS                   
         JZ    BINX50A             ALREADY BEEN BILLED                          
BINX90B  MVC   INXPGR,TAPDGRS      PASS BACK ... GROSS AMOUNT                   
         MVC   INXPAC,TAPDAPPL                   APPLIED CREDITS                
         MVC   INXPGC,TAPDGUAR                   GUARANTEE CREDITS              
         MVC   INXPIA,TAPDPAYI                   INDIVIDUAL GROSS               
         MVC   INXPCA,TAPDPAYC                   CORPORATION GROSS              
         MVC   INXPRE,TAPDREXP                   REIMBURSED EXPENSES            
         MVC   INXPSP,TAPDSPNH                   SUBJECT TO P&H                 
         MVC   INXPMD,TAPDMDED                   MISC DEDUCTIONS                
         MVC   INXPNH,TAPDPNH                    P&H AMOUNT                     
         MVC   INXPHW,TAPDHNW                    H&W AMOUNT                     
         MVC   INXPIR,TAPDINR                    I&R AMOUNT                     
         MVC   INXPUD,TAPDDUES                   UNION DUES AMOUNT              
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TAVRD,R4                                                         
BINX100  CLI   0(R4),TAVRELQ       PROCESS VERSIONS ELEMENT                     
         JNE   BINX110                                                          
         MVC   INXVER,TAVRVERS     PASS BACK ... VERSION NUMBER                 
         MVC   INXVID,TAVRCID                    VERSION ID                     
         MVC   INXVLN,TAVRSEC                    VERSION LENGTH                 
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
BINX110  CLI   0(R4),TACOELQ       PROCESS COMMERCIAL DETAILS ELEMENT           
         JNE   BINX120                                                          
         MVC   INXCID,TACOCID      PASS BACK ... COMMERCIAL ID                  
         MVC   INXLEN,TACOSEC                    COMMERCIAL LENGTH              
         MVC   INXMED,TACOMED                    COMMERCIAL MEDIA               
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TALFD,R4                                                         
BINX120  CLI   0(R4),TALFELQ       PROCESS LIFT DETAILS ELEMENT                 
         JNE   BINX130                                                          
         MVC   INXLID,TALFLID      PASS BACK ... LIFT ID                        
         MVC   INXLLN,TALFSEC                    LIFT LENGTH                    
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TAUCD,R4                                                         
BINX130  CLI   0(R4),TAUCELQ       PROCESS US/CAN INVOICE ELEMENT               
         JNE   BINX140                                                          
         GOTOR (#CVTINV,ACVTINV),DMCB,TAUCINU,INXUSI                            
         GOTOR (#CVTINV,ACVTINV),DMCB,TAUCINC,INXCAI                            
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
BINX140  CLI   0(R4),TAINELQ       PROCESS INVOICE STATUS ELEMENT               
         JNE   BINX150             PASS BACK ...                                
         GOTO1 OUTUID,DMCB,TAINIID,INXAUI        ASSIGNMENT USER ID             
         MVC   INXASI,TAINIST                    ASSIGNMENT STAFF ID            
         GOTO1 VDATCON,DMCB,(1,TAINIDTE),(8,INXADT)             DATE            
         GOTOR (#OUTTIME,AOUTTIME),DMCB,TAINITIM,INXATM         TIME            
         GOTO1 OUTUID,DMCB,TAINPID,INXPUI        PAYMENT USER ID                
         MVC   INXPSI,TAINPST                    PAYMENT STAFF ID               
         GOTO1 VDATCON,DMCB,(1,TAINPDTE),(8,INXPDT)          DATE               
         GOTOR (#OUTTIME,AOUTTIME),DMCB,TAINPTIM,INXPTM      TIME               
         GOTO1 OUTUID,DMCB,TAINQID,INXQUI        QC USER ID                     
         MVC   INXQSI,TAINQST                    QC STAFF ID                    
         GOTO1 VDATCON,DMCB,(1,TAINQDTE),(8,INXQDT)     DATE                    
         GOTOR (#OUTTIME,AOUTTIME),DMCB,TAINQTIM,INXQTM TIME                    
         GOTO1 VDATCON,DMCB,(1,TAINBDTE),(8,INXBDT) BILLED DATE                 
         MVC   INXEWN,TAINTERR                   ERROR/WARNING NUMBER           
         GOTO1 (RF),(R1),(1,TAINCDTE),(8,INXCDT) CHECK DATE                     
         GOTO1 (RF),(R1),(1,TAINHDTE),(8,INXPPD) PUR PRINT DATE                 
         MVC   INXCRU,TAINCKID                   CHECK RUN ID                   
         GOTO1 (RF),(R1),(1,TAINCKRN),(8,INXCRD) CHECKR RUN DATE                
                                                                                
         MVI   INXRTS,C'N'         PASS BACK RESERVED FOR TIMESHEET?            
         OC    TAINTMCO,TAINTMCO                                                
         JZ    *+8                                                              
         MVI   INXRTS,C'Y'                                                      
                                                                                
         MVI   INXCER,C'N'         PASS BACK CANCELLER INVOICE?                 
         TM    TAINSTAT,TAINSCIN                                                
         JZ    *+8                                                              
         MVI   INXCER,C'Y'                                                      
                                                                                
         MVI   INXCED,C'N'         PASS BACK CANCELLED?                         
         TM    TAINSTAT,TAINSCAN                                                
         JZ    *+8                                                              
         MVI   INXCED,C'Y'                                                      
                                                                                
         MVI   INXPHL,C'N'         PASS BACK PUR HOLD?                          
         TM    TAINSTAT,TAINSHLD                                                
         JZ    *+8                                                              
         MVI   INXPHL,C'Y'                                                      
                                                                                
         MVI   INXPHR,C'N'         PASS BACK PUR HOLD RELEASED?                 
         TM    TAINSTA2,TAINSHLR                                                
         JZ    *+8                                                              
         MVI   INXPHR,C'Y'                                                      
                                                                                
         MVI   INXFPA,C'N'         PASS BACK FOR PAYMENT ADJUSTMENT?            
         TM    TAINSTA2,TAINSADJ                                                
         JZ    *+8                                                              
         MVI   INXFPA,C'Y'                                                      
                                                                                
         MVI   INXRPH,C'N'         PASS BACK RETRO PAYMENT HOLD?                
         TM    TAINSTA2,TAINSRTH                                                
         JZ    *+8                                                              
         MVI   INXRPH,C'Y'                                                      
                                                                                
         MVI   INXCDF,C'N'         PASS CHECK DATE FORCED?                      
         TM    TAINSTA2,TAINSFRC                                                
         JZ    *+8                                                              
         MVI   INXCDF,C'Y'                                                      
                                                                                
         MVI   INXPRI,C'N'         PASS BACK PRIMARY INVOICE?                   
         TM    TAINSTA2,TAINSPRM                                                
         JZ    BINX10                                                           
         MVI   INXPRI,C'Y'                                                      
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
BINX150  CLI   0(R4),TAFNELQ       PROCESS FREE FORM NAME ELEMENT               
         JNE   BINX160                                                          
         LA    RE,INXPRN           PASS BACK ... ASSIGNMENT STAFF ID            
         CLI   TAFNTYPE,TAFNTPRD                                                
         JE    BINX150A                                                         
         LA    RE,INXCTI                                                        
         CLI   TAFNTYPE,TAFNTTTL                 COMMERCIAL TITLE               
         JE    BINX150A                                                         
         LA    RE,INXADV                         ADVICE NUMBER                  
         CLI   TAFNTYPE,TAFNTADV                                                
         JE    BINX150A                                                         
         LA    RE,INXWID                         WEB APPLICATION ID             
         CLI   TAFNTYPE,TAFNTWEB                                                
         JNE   BINX10                                                           
BINX150A ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),TAFNNAME                                                 
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
BINX160  CLI   0(R4),TANUELQ       PROCESS FREE FORM NUMBER ELEMENT             
         JNE   BINX10                                                           
         LA    RE,INXAPO           PASS BACK ... AUTH/PO NUMBER                 
         CLI   TANUTYPE,TANUTAUT                                                
         JE    BINX160A                                                         
         LA    RE,INXEST                         ESTIMATE/JOB NUMBER            
         CLI   TANUTYPE,TANUTEST                                                
         JE    BINX160A                                                         
         LA    RE,INXEWI                         ERROR INVOICE NUMBER           
         CLI   TANUTYPE,TANUTINV                                                
         JE    BINX160A                                                         
         LA    RE,INXEPI                         ERROR PID                      
         CLI   TANUTYPE,TANUEPID                                                
         JNE   BINX10                                                           
BINX160A ZIC   RF,TANULEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),TANUMBER                                                 
         J     BINX10                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        OUTPUT USER ID                                               *         
*        ON ENTRY ... P1=A(USER ID INPUT FIELD)                       *         
*                     P2=A(USER ID EBCDIC OUTPUT FIELD)               *         
*                     R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
OUTUID   NTR1                                                                   
         L     R2,0(R1)                                                         
         OC    0(2,R2),0(R2)       EXIT IF USER ID INPUT FIELD IS               
         JZ    XIT                 NOT PROVIDED                                 
                                                                                
         L     R4,4(R1)            R4=A(USER ID EBCDIC OUTPUT FIELD)            
                                                                                
         CLC   SVUID,0(R2)         IF USER ID SAME AS LAST TIME                 
         JNE   OUID10              OUTPUT SAME EBCDIC USER ID                   
         MVC   0(L'SVEUID,R4),SVEUID                                            
         J     XIT                                                              
                                                                                
         USING CTIREC,R3                                                        
OUID10   XC    CTIKEY,CTIKEY       READ FOR ID RECORD                           
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOCTFILE+IO4'                            
         JNE   OUID60                                                           
         DROP  R3                                                               
                                                                                
         USING CTIREC,R1                                                        
         L     R1,AIO4                                                          
                                                                                
         USING CTDSCD,R2                                                        
         LA    R2,CTIDATA                                                       
OUID20   CLI   0(R2),CTDSCELQ      GET ID DESCRIPTION ELEMENT                   
         JE    OUID30                                                           
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         CLI   0(R2),0                                                          
         JNE   OUID20                                                           
         DC    H'00'                                                            
                                                                                
OUID30   CLI   CTDSCLEN,4          IF ELEMENT LENGTH IS 4                       
         JNE   OUID40                                                           
         MVC   0(8,R4),CTIKID      SET OUTPUT FIELD WITH KEY'S                  
         J     OUID50              EBCDIC USER ID                               
                                                                                
OUID40   MVC   0(6,R4),CTDSC       OTHERWISE SET OUTPUT FIELD WITH              
         MVC   6(2,R4),SPACES      ELEMENT'S EBCDIC USER ID                     
         DROP  R2                                                               
                                                                                
OUID50   MVC   SVUID,CTIKNUM       SAVE USER ID                                 
         MVC   SVEUID,0(R4)        AND LAST EBCDIC USER ID                      
         DROP  R1                                                               
                                                                                
OUID60   MVC   IOKEY,SVIOKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
NY       EQU   X'80'               NEW YORK                                     
LA       EQU   X'40'               LOS ANGELES                                  
CHI      EQU   X'20'               CHICAGO                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT FIXED CYCLE SEARCH STATUS RECORD                      *         
***********************************************************************         
                                                                                
NXTFCS   J     *+12                                                             
         DC    C'*NXTFCS*'                                                      
         LR    RB,RF                                                            
         USING NXTFCS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         MVI   FCSSTAT,FCSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,FCVALREQ         VALIDATE REQUEST                             
         JNE   NCMSX                                                            
                                                                                
         MVI   FCSSTAT,FCSSUNS     REINITIALIZE STATUS TO UNSUCCESSFUL          
         XC    SVADDR,SVADDR       AND A(FIXED CYCLE ELEMENT)                   
                                                                                
         USING TLCAD,R3                                                         
         LA    R3,IOKEY            READ FOR CAST KEY                            
         XC    0(L'TLCAKEY,R3),0(R3)                                            
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,RQFCSCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTFCS           IF CAST RECORD MATCHES ALL FILTERS           
         JNE   NFCSX               RETURN SEARCH SUCCESSFUL STATUS              
         MVI   FCSSTAT,FCSSSUC                                                  
         TM    ERRSTAT,ESREVIW                                                  
         JZ    NFCSX                                                            
         MVI   FCSSTAT,FCSSEMB                                                  
         DROP  R3                                                               
                                                                                
NFCSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,FCSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
FCVALREQ NTR1                                                                   
         OC    RQFCSSTF,RQFCSSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
         OC    RQFCSCOM,RQFCSCOM   ASSERT THAT INTERNAL COMMERCIAL              
         JZ    NO                  NUMBER IS PROVIDED                           
                                                                                
         OC    RQFCSCYE,RQFCSCYE   IF CYCLE END DATE IS PROVIDED                
         JZ    FVR10                                                            
         OC    RQFCSCYS,RQFCSCYS   ASSERT THAT CYCLE START DATE                 
         JZ    NO                  IS PROVIDED                                  
                                                                                
FVR10    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQFCSMIC)                       
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT FIXED CYCLE RECORD                                    *         
***********************************************************************         
                                                                                
NXTFCX   J     *+12                                                             
         DC    C'*NXTFCX*'                                                      
         LR    RB,RF                                                            
         USING NXTFCX,RB                                                        
                                                                                
         CLI   FCSSTAT,FCSSSUC     EXIT IF SEARCH WAS NOT SUCCESSFUL            
         JNE   NOMORE              NOT SUCCESSFUL                               
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NFCX10              READ FOR NEXT FIXED CYCLE ELEMENT            
         BRAS  RE,FLTFCS                                                        
         JNE   NO                                                               
                                                                                
NFCX10   XC    FCXVALS(FCXVALL),FCXVALS                                         
                                                                                
         USING TLCAD,R4                                                         
         L     R4,AIO3                                                          
*                                                                               
         L     RE,AIO4                                                          
         USING TLFTD,RE                                                         
         XC    TLFTKEY,TLFTKEY                                                  
         MVC   TLFTSSN,TLCASSN                                                  
         MVC   TLFTCOM,TLCACOM                                                  
         MVC   TLFTCAST,TLCASEQ                                                 
         DROP  RE                                                               
*                                                                               
         GOTO1 VHEXOUT,DMCB,TLCASEQ,FCXSEQ,L'TLCASEQ,0                          
         DROP  R4                                                               
                                                                                
         MVC   FCXSRT,NUMFCEL                                                   
         ZICM  R0,NUMFCEL,(3)                                                   
         SHI   R0,1                                                             
         STCM  R0,3,NUMFCEL                                                     
         CLC   FCXSRT,=H'1'                                                     
         BNE   NFCX20                                                           
         MVI   ELCODE,TAHFELQ                                                   
         L     R4,AIO3                                                          
         BRAS  RE,GETEL                                                         
         JNE   NFCX20                                                           
         USING TAHFD,R4                                                         
         GOTO1 VDATCON,DMCB,(1,TAHFNXTS),(8,FCXHFN) HFN DATE                    
                                                                                
         USING TACRD,R4                                                         
NFCX20   L     R4,SVADDR                                                        
                                                                                
         GOTO1 VDATCON,DMCB,(1,TACRSTRT),(8,FCXCYS)                             
         GOTO1 (RF),(R1),(1,TACREND),(8,FCXCYE)                                 
         MVC   FCXSCL,TACRSCAL                                                  
         MVC   FCXAPL,TACRAPPL                                                  
         MVC   FCXBAL,TACRBAL                                                   
                                                                                
         MVI   FCXTRK,C'Y'                                                      
         TM    TACRSTAT,TACRSTRK                                                
         JO    *+8                                                              
         MVI   FCXTRK,C'N'                                                      
                                                                                
         MVI   FCXGUA,C'N'                                                      
         TM    TACRSTAT,TACRSGUA                                                
         JZ    *+8                                                              
         MVI   FCXGUA,C'Y'                                                      
                                                                                
         MVI   FCXDLR,C'N'                                                      
         TM    TACRSTAT,TACRHDLR                                                
         JZ    *+8                                                              
         MVI   FCXDLR,C'Y'                                                      
                                                                                
         MVC   FCXUSE,TACRUSE                                                   
         GOTOR (#CVTINV,ACVTINV),DMCB,TACRINV,FCXINV                            
         CLI   TACRTYPE,0                                                       
         JE    NFCX50                                                           
                                                                                
         USING TGTABLES,RE                                                      
         L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGAUSES          RF=DISPLACEMENT OF USES TABLE                
         DROP  RE                                                               
                                                                                
         USING USETABD,RF                                                       
         AR    RF,RE               RF=A(USE TABLE)                              
                                                                                
NFCX30   CLC   TACRTYPE,USEEQU     FIND CAST CATEGORY IN                        
         JE    NFCX40              CATEGORY TABLE                               
         LH    RE,USELEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   NFCX30                                                           
         DC    H'00'                                                            
                                                                                
NFCX40   MVC   FCXGRR,USECDE                                                    
         DROP  RF                                                               
                                                                                
* IF REQUEST FOR FTRACK COMMENT THEN READ TLFTCDQ RECORD :                      
* IF TACRINV IS BLANK = FTRACK COMMENT ADDED MANUALLY                           
NFCX50   CLI   RQFCSMIC,C'Y'                                                    
         JNE   NFCXX                                                            
                                                                                
         MVC   SVIOKEY,IOKEY       SAVE CURRENT CAST KEY                        
                                                                                
         USING TLFTD,R3                                                         
         LA    R3,IOKEY                                                         
                                                                                
         L     RE,AIO4                                                          
         MVC   IOKEY,0(RE)                                                      
                                                                                
         MVI   TLFTCD,TLFTCDQ                                                   
         MVC   TLFTSTRT,TACRSTRT                                                
         XC    TLFTSTRT(3),=X'FFFFFF'                                           
         MVC   TLFTEND,TACREND                                                  
         XC    TLFTEND(3),=X'FFFFFF'                                            
                                                                                
         MVC   TLFTTRK,=X'FFFE'                                                 
         OC    TACRINV,TACRINV                                                  
         JZ    *+10                                                             
         MVC   TLFTTRK,=X'FFFF'                                                 
         DROP  R3,R4                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NFCX70                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
FCX60    GOTOR (#GETELEM,AGETELEM),DMCB,('TACMELQ',AIO4),0                      
         JNE   NFCX70                                                           
         L     R4,AELEM                                                         
         USING TACMD,R4                                                         
         ZIC   RE,TACMLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FCXFTC(0),TACMCOMM                                               
         DROP  R4                                                               
                                                                                
NFCX70   MVC   IOKEY,SVIOKEY       RESTORE CAST READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
                                                                                
NFCXX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,FCXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMENT SEARCH STATUS RECORD                          *         
***********************************************************************         
                                                                                
NXTCMS   J     *+12                                                             
         DC    C'*NXTCMS*'                                                      
         LR    RB,RF                                                            
         USING NXTCMS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         MVI   CMSSTAT,CMSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,CMVALREQ         VALIDATE REQUEST                             
         JNE   NCMSX                                                            
                                                                                
         MVI   CMSSTAT,CMSSUNS     REINITIALIZE STATUS TO UNSUCCESSFUL          
                                                                                
         USING TLCMD,R3                                                         
         LA    R3,IOKEY                                                         
         XC    TLCMKEY,TLCMKEY                                                  
         MVI   TLCMCD,TLCMCDQ      INITIALIZE KEY WITH RECORD EQUATE            
         MVC   TLCMAGY,RQCMSAGY    AGENCY                                       
         MVC   TLCMTYP,RQCMSTYP    AND TYPE                                     
         LHI   R0,TLCMLEV-TLCMD-1  AND SET R0 WITH LENGTH FOR COMPARE           
                                                                                
         CLI   RQCMSTYP,TLCMTCON   IF TYPE IS CONTRACT                          
         JNE   NCMS10                                                           
         MVC   TLCMCNID,RQCMSCON   ADD CONTRACT ID                              
         MVC   TLCMTRMS,RQCMSTST   TERM START DATE                              
         MVC   TLCMTRME,RQCMSTEN   AND TERM END DATE TO KEY                     
         J     NCMS60                                                           
                                                                                
NCMS10   CLI   RQCMSTYP,TLCMTCOM   IF TYPE IS COMMERCIAL                        
         JNE   NCMS20                                                           
         MVC   TLCMCID,RQCMSCID    ADD COMMERCIAL ID TO KEY                     
         LHI   R0,TLCMICOM-TLCMD-1 RESET R0 WITH LENGTH FOR COMPARE             
         J     NCMS60                                                           
                                                                                
NCMS20   CLI   RQCMSTYP,TLCMTGUA   IF TYPE IS GUARANTEE                         
         JNE   NCMS30                                                           
         MVC   TLCMSSN,RQCMSSSN    ADD SOCIAL SECURITY NUMBER                   
         MVC   TLCMGUA,RQCMSGUA    AND GUARANTEE CODE TO KEY                    
         XC    TLCMGUA,=4X'FF'     AND COMPLEMENT IT                            
         J     NCMS60                                                           
                                                                                
NCMS30   CLI   RQCMSTYP,TLCMTINV   IF TYPE IS INVOICE                           
         JNE   NCMS40                                                           
         MVC   TLCMINV,RQCMSINV    ADD INVOICE NUMBER TO KEY                    
         XC    TLCMINV,=6X'FF'     AND COMPLEMENT IT                            
         J     NCMS60                                                           
                                                                                
NCMS40   CLI   RQCMSTYP,TLCMTADV   IF TYPE IS ADVICE                            
         JNE   NCMS60                                                           
         MVC   TLCMVCID,RQCMSCID   ADD COMMERCIAL ID                            
         XC    TLCMVADV,RQCMSADV   AND ADVICE NUMBER TO KEY                     
         J     NCMS60                                                           
                                                                                
NCMS50   DC    H'00'               UNSUPPORTED COMMENT TYPE                     
                                                                                
NCMS60   STC   R0,LKEYCOMP                                                      
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTCMT                                                        
         JNE   NFCSX                                                            
         DROP  R3                                                               
                                                                                
         MVI   CMSSTAT,CMSSSUC     IF COMMENT RECORD IS FOUND                   
         TM    ERRSTAT,ESREVIW     THAT PASSES ALL FILTERS                      
         JZ    NCMSX               RETURN SUCCESSFUL STATUS                     
         MVI   CMSSTAT,CMSSEMB                                                  
                                                                                
NCMSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CMSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
CMVALREQ NTR1                                                                   
         OC    RQCMSSTF,RQCMSSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
                                                                                
         CLI   RQCMSLEV,0          ENFORCE VALID VALUES FOR LEVEL               
         JE    CMVR10                                                           
         CLI   RQCMSLEV,RQCMLEVC                                                
         JE    CMVR10                                                           
         CLI   RQCMSLEV,RQCMLEVT                                                
         JNE   NO                                                               
                                                                                
CMVR10   CLI   RQCMSTYP,TLCMTCON   ENFORCE RULES FOR CONTRACT                   
         JNE   CMVR20              COMMENTS                                     
         OC    RQCMSSSN,RQCMSSSN                                                
         JNZ   NO                                                               
         OC    RQCMSGUA,RQCMSGUA                                                
         JNZ   NO                                                               
         OC    RQCMSCID,RQCMSCID                                                
         JNZ   NO                                                               
         CLI   RQCMSVER,0                                                       
         JNE   NO                                                               
         OC    RQCMSAGY,RQCMSAGY                                                
         JZ    NO                                                               
         OC    RQCMSCON,RQCMSCON                                                
         JZ    NO                                                               
         OC    RQCMSTST,RQCMSTST                                                
         JZ    NO                                                               
         OC    RQCMSTEN,RQCMSTEN                                                
         JZ    NO                                                               
         OC    RQCMSINV,RQCMSINV                                                
         JNZ   NO                                                               
         OC    RQCMSADV,RQCMSADV                                                
         JZ    YES                                                              
         J     NO                                                               
                                                                                
CMVR20   CLI   RQCMSTYP,TLCMTCOM   ENFORCE RULES FOR COMMERCIAL                 
         JNE   CMVR30              COMMENTS                                     
         OC    RQCMSSSN,RQCMSSSN                                                
         JNZ   NO                                                               
         OC    RQCMSGUA,RQCMSGUA                                                
         JNZ   NO                                                               
         OC    RQCMSCID,RQCMSCID                                                
         JZ    NO                                                               
         OC    RQCMSAGY,RQCMSAGY                                                
         JZ    NO                                                               
         OC    RQCMSCON,RQCMSCON                                                
         JNZ   NO                                                               
         OC    RQCMSTST,RQCMSTST                                                
         JNZ   NO                                                               
         OC    RQCMSTEN,RQCMSTEN                                                
         JNZ   NO                                                               
         OC    RQCMSINV,RQCMSINV                                                
         JNZ   NO                                                               
         OC    RQCMSADV,RQCMSADV                                                
         JZ    YES                                                              
         J     NO                                                               
                                                                                
CMVR30   CLI   RQCMSTYP,TLCMTGUA   ENFORCE RULES FOR GUARANTEE                  
         JNE   CMVR40              COMMENTS                                     
         OC    RQCMSSSN,RQCMSSSN                                                
         JZ    NO                                                               
         OC    RQCMSGUA,RQCMSGUA                                                
         JZ    NO                                                               
         OC    RQCMSCID,RQCMSCID                                                
         JNZ   NO                                                               
         CLI   RQCMSVER,0                                                       
         JNE   NO                                                               
         OC    RQCMSAGY,RQCMSAGY                                                
         JNZ   NO                                                               
         OC    RQCMSCON,RQCMSCON                                                
         JNZ   NO                                                               
         OC    RQCMSTST,RQCMSTST                                                
         JNZ   NO                                                               
         OC    RQCMSTEN,RQCMSTEN                                                
         JNZ   NO                                                               
         OC    RQCMSINV,RQCMSINV                                                
         JNZ   NO                                                               
         OC    RQCMSADV,RQCMSADV                                                
         JZ    YES                                                              
         J     NO                                                               
                                                                                
CMVR40   CLI   RQCMSTYP,TLCMTINV   ENFORCE RULES FOR INVOICE                    
         JNE   CMVR50              COMMENTS                                     
         OC    RQCMSSSN,RQCMSSSN                                                
         JNZ   NO                                                               
         OC    RQCMSGUA,RQCMSGUA                                                
         JNZ   NO                                                               
         OC    RQCMSCID,RQCMSCID                                                
         JNZ   NO                                                               
         CLI   RQCMSVER,0                                                       
         JNE   NO                                                               
         OC    RQCMSAGY,RQCMSAGY                                                
         JZ    NO                                                               
         OC    RQCMSCON,RQCMSCON                                                
         JNZ   NO                                                               
         OC    RQCMSTST,RQCMSTST                                                
         JNZ   NO                                                               
         OC    RQCMSTEN,RQCMSTEN                                                
         JNZ   NO                                                               
         OC    RQCMSINV,RQCMSINV                                                
         JZ    NO                                                               
         OC    RQCMSADV,RQCMSADV                                                
         JZ    YES                                                              
         J     NO                                                               
                                                                                
CMVR50   CLI   RQCMSTYP,TLCMTADV   ENFORCE RULES FOR ADVICE                     
         JNE   NO                  COMMENTS                                     
         OC    RQCMSSSN,RQCMSSSN                                                
         JNZ   NO                                                               
         OC    RQCMSGUA,RQCMSGUA                                                
         JNZ   NO                                                               
         OC    RQCMSCID,RQCMSCID                                                
         JZ    NO                                                               
         OC    RQCMSAGY,RQCMSAGY                                                
         JZ    NO                                                               
         OC    RQCMSCON,RQCMSCON                                                
         JNZ   NO                                                               
         OC    RQCMSTST,RQCMSTST                                                
         JNZ   NO                                                               
         OC    RQCMSTEN,RQCMSTEN                                                
         JNZ   NO                                                               
         OC    RQCMSINV,RQCMSINV                                                
         JNZ   NO                                                               
         OC    RQCMSADV,RQCMSADV                                                
         JNZ   YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMENT RECORD                                        *         
***********************************************************************         
                                                                                
NXTCMX   J     *+12                                                             
         DC    C'*NXTCMX*'                                                      
         LR    RB,RF                                                            
         USING NXTCMX,RB                                                        
                                                                                
         CLI   CMSSTAT,CMSSSUC     EXIT IF SEARCH WAS NOT SUCCESSFUL            
         JH    NOMORE                                                           
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NCMX10              READ NEXT COMMENT RECORD                     
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTCMT                                                        
         JNE   NOMORE                                                           
                                                                                
NCMX10   LA    R0,CMXVALS          CLEAR EXPANDED COMMENT DETAILS               
         LHI   R1,CMXVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING TLCMD,R4                                                         
         L     R4,AIO3             R4=A(COMMENT RECORD)                         
         MVI   CMXLEV,RQCMLEVC                                                  
         TM    TLCMLEV,TLCMTPC     PASS BACK LEVEL                              
         JZ    *+8                                                              
         MVI   CMXLEV,RQCMLEVT                                                  
                                                                                
         CLI   RQCMSTYP,TLCMTCOM   IF TYPE IS COMMERCIAL                        
         JNE   *+10                                                             
         MVC   CMXVER,TLCMVER      PASS BACK VERSION                            
         DROP  R4                                                               
                                                                                
         USING TAXCD,R4                                                         
         MVI   ELCODE,TAXCELQ      READ FIRST EXTENDED COMMENT ELEMENT          
         BRAS  RE,GETEL                                                         
         JNE   NOMORE                                                           
                                                                                
NCMX20   LA    RF,CMXCM1           COPY EXTENDED COMMENT5 INTO                  
         CLI   TAXCSEQ,1           CORRESPONDING OUTPUT LINE                    
         JE    NCMX30                                                           
         ZIC   RE,TAXCSEQ                                                       
         SHI   RE,1                                                             
         MHI   RE,L'INCCM1                                                      
         AR    RF,RE                                                            
NCMX30   ZIC   RE,TAXCLEN                                                       
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RF),TAXCCMNT                                                 
         DROP  R4                                                               
                                                                                
         BRAS  RE,NEXTEL           GET NEXT EXTENDED COMMENT ELEMENT            
         JE    NCMX20                                                           
                                                                                
NCMXX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,CMXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT W4 KEY/RECORD MATCH SEARCH FILTERS       *         
*        AND RETURNS THE RECORD                                       *         
*        ON ENTRY ... IOKEY = A(W4 KEY)                               *         
***********************************************************************         
                                                                                
FLTW4    NTR1  BASE=*,LABEL=*                                                   
FW410    ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   ENSURE KEY MATCHES FILTERS                   
         JE    FW420                                                            
         BAS   RE,ADJW4KEY         IF NOT WE MAY NEED TO ADJUST KEY             
         JE    FW410               TO READ FOR ADDITIONAL CRITERIA              
         J     NO                                                               
                                                                                
FW420    GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLW4D,R4                                                         
         L     R4,AIO3                                                          
                                                                                
         CLI   RQW4SES9,C'Y'       IF EXCLUDING SSNS THAT BEGIN                 
         JNE   *+12                WITH 9                                       
         CLI   TLW4SSN,C'9'        ENSURE THAN SSN DOES NOT BEGIN               
         JE    FW490               WITH 9                                       
                                                                                
         MVC   SVSSN,TLW4SSN       SAVE SS#                                     
         DROP  R4                                                               
                                                                                
         USING TAW4D,R4                                                         
         MVI   ELCODE,TAW4ELQ      GET EMPLOYEE W4 DETAILS ELEMENT              
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   RQW4STIN,0          IF W4 TYPE FILTER IS DEFINED                 
         JE    FW440                                                            
         ZIC   RE,RQW4STIN         RE=TYPE FILTER COUNTER                       
         ZICM  RF,ARQW4STF,3       RF=A(TYPE FILTER LIST)                       
                                                                                
FW430    CLC   TAW4TYPE,0(RF)      ENSURE W4 TYPE MATCHES ONE OF                
         JE    FW440               THE FILTERS                                  
         LA    RF,1(RF)                                                         
         BCT   RE,FW430                                                         
         J     FW490                                                            
                                                                                
FW440    CLI   IOKEY,TLW4NCDQ      IF READING BY NAME                           
         JNE   FW450                                                            
         CLI   FNAMELEN,0          AND FIRST NAME WAS ENTERED                   
         JE    FW450               ENSURE THAT IT MATCHES THIS W4               
         ZIC   RE,FNAMELEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   TAW4NAM1(0),RQW4SFNM                                             
         JNE   FW490                                                            
         DROP  R4                                                               
                                                                                
FW450    LA    R2,CHKFIDS                                                       
                                                                                
         USING TATID,R4                                                         
         L     R4,IOADDR                                                        
         MVI   ELCODE,TATIELQ      READ ALL ATTACHED CORPORATIONS               
         BRAS  RE,GETEL            AND BUILD LIST OF THEM                       
         J     *+8                                                              
FW460    BRAS  RE,NEXTEL                                                        
         JNE   FW470                                                            
         CLI   TATITYPE,TATITYCO                                                
         JNE   FW460                                                            
         MVC   0(L'CHKFIDS,R2),TATIID                                           
         LA    R2,L'CHKFIDS(R2)                                                 
         J     FW460                                                            
         DROP  R4                                                               
                                                                                
FW470    MVI   0(R2),X'FF'                                                      
                                                                                
         GOTOR CKW4ACC,DMCB,SVSSN  ENSURE STAFF HAS ACCESS TO W4                
         JE    YES                                                              
                                                                                
         LA    R2,CHKFIDS                                                       
FW480    CLI   0(R2),X'FF'                                                      
         JE    FW490                                                            
         GOTOR CKW4ACC,DMCB,0(R2)  OR ONE OF ITS ATTACHED CORPORATIONS          
         JE    YES                                                              
         LA    R2,L'CHKFIDS(R2)                                                 
         J     FW480                                                            
                                                                                
FW490    CLI   IOKEY,TLW4NCDQ                                                   
         JNE   NO                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FW410                                                            
                                                                                
***********************************************************************         
*        ROUTINE RESETS IOKEY TO READ FOR ADDITONAL CRITERIA          *         
***********************************************************************         
                                                                                
ADJW4KEY NTR1                                                                   
         L     R2,ASSNTAB          IF SEARCH REQUEST CONTAINED                  
         CLI   0(R2),X'FF'         SOCIAL SECURITY NUMBER FILTER                
         JE    NO                                                               
                                                                                
AWK10    OC    0(L'TLW4SSN,R2),0(R2)                                            
         JNZ   AWK20                                                            
         LA    R2,L'TLW4SSN(R2)    FIND THE NEXT SOCIAL SECURITY                
         CLI   0(R2),X'FF'         NUMBER THAT WE WANT TO READ FOR              
         JNE   AWK10                                                            
         J     NO                                                               
                                                                                
AWK20    MVC   RQW4SSSN,0(R2)                                                   
         XC    0(L'TLW4SSN,R2),0(R2)                                            
                                                                                
         USING TLW4D,R3                                                         
         LA    R3,IOKEY                                                         
         XC    0(L'TLW4KEY,R3),0(R3)                                            
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,RQW4SSSN    AND READ W4 KEY                              
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS THAT AGENT KEY/RECORD MATCHES SEARCH FILTERS  *         
*        AND RETURNS THE RECORD                                       *         
*        ON ENTRY ... R3=A(FIRST READ KEY)                            *         
***********************************************************************         
                                                                                
FLTAGT   NTR1  BASE=*,LABEL=*                                                   
FAN10    ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   ENSURE KEY MATCHES FILTERS                   
         JE    FAN20                                                            
         BAS   RE,ADJANKEY         IF NOT WE MAY NEED TO ADJUST KEY             
         JE    FAN10               TO READ FOR ADDITIONAL CRITERIA              
         J     NO                                                               
                                                                                
         USING TLANPD,R3                                                        
FAN20    CLI   IOKEYSAV,TLANBCDQ  IF USING SUPER SEARCH OPTION                  
         JNE   FAN50                                                            
         L     RE,ASVPTRS         ENSURE THAT EACH AGENT IS                     
FAN30    CLI   0(RE),X'FF'        ONLY RETURNED ONCE                            
         JE    FAN40                                                            
         CLC   TLANBAGT,0(RE)                                                   
         JE    FAN70                                                            
         LA    RE,L'TLANBAGT(RE)                                                
         J     FAN30                                                            
FAN40    MVC   0(L'TLANBAGT,RE),TLANBAGT                                        
         MVI   L'TLANBAGT(RE),X'FF'                                             
         DROP  R3                                                               
                                                                                
FAN50    GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         CLI   RQANSAOV,0          IF FILTERING ON ALLOWABLE                    
         JE    YES                 OVERRIDES                                    
                                                                                
         USING TAAND,R4                                                         
         L     R4,AIO3             GET AGENT ELEMENT                            
         MVI   ELCODE,TAANELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FAN70                                                            
                                                                                
         CLI   RQANSAOV,C'Y'       IF ONLY WANT ALLOWABLE OVERRIDES             
         JNE   FAN60                                                            
         TM    TAANSTAT,TAANSOVR   EXCLUDE NON-ALLOWABLE OVERRIDES              
         JZ    FAN70                                                            
         J     YES                                                              
                                                                                
FAN60    TM    TAANSTAT,TAANSOVR   IF ONLY WANT NON-ALLOWABLE OVERRIDES         
         JZ    YES                 EXCLUDE ALLOWABLE OVERRIDES                  
         DROP  R4                                                               
                                                                                
FAN70    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FAN10                                                            
                                                                                
***********************************************************************         
*        ROUTINE RESETS IOKEY TO READ FOR ADDITONAL CRITERIA          *         
***********************************************************************         
                                                                                
ADJANKEY NTR1                                                                   
         L     R2,AAGTTAB          IF SEARCH REQUEST CONTAINED                  
         CLI   0(R2),X'FF'         AGENT CODE FILTER                            
         JE    NO                                                               
                                                                                
AAK10    OC    0(L'TLANAGT,R2),0(R2)                                            
         JNZ   AAK20                                                            
         LA    R2,L'TLANAGT(R2)    FIND THE NEXT AGENT CODE THAT                
         CLI   0(R2),X'FF'         WE WANT TO READ FOR                          
         JNE   AAK10                                                            
         J     NO                                                               
                                                                                
AAK20    MVC   RQANSAGT,0(R2)                                                   
         XC    0(L'TLANAGT,R2),0(R2)                                            
                                                                                
         USING TLAND,R3                                                         
         LA    R3,IOKEY                                                         
         XC    0(L'TLANKEY,R3),0(R3)                                            
         MVI   TLANCD,TLANCDQ                                                   
         MVC   TLANAGT,RQANSAGT    AND READ AGENT KEY                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT CAST KEY/RECORD MATCHES SEARCH FILTERS   *         
*        AND RETURNS THE RECORD                                       *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
FLTCAST  NTR1  BASE=*,LABEL=*                                                   
FCAST10  ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   ENSURE KEY MATCHES FILTERS                   
         JE    FCAST20                                                          
         BRAS  RE,ADJCAKEY         IF NOT WE MAY NEED TO ADJUST KEY             
         JE    FCAST10             TO READ FOR ADDITIONAL CRITERIA              
         J     NO                                                               
                                                                                
FCAST20  MVC   SVIOKEY,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLCAD,R4                                                         
         L     R4,IOADDR                                                        
         OC    RQCASSSN,RQCASSSN   IF SS# IS PROVIDED                           
         JZ    *+14                                                             
         CLC   RQCASSSN,TLCASSN    ENSURE IT MATCHES CAST                       
         JNE   FCAST120                                                         
                                                                                
         CLI   RQCASES9,C'Y'       IF EXCLUDING SSNS THAT BEGIN                 
         JNE   *+12                WITH 9                                       
         CLI   TLCASSN,C'9'        ENSURE THAN SSN DOES NOT BEGIN               
         JE    FCAST120            WITH 9                                       
                                                                                
         OC    RQCASSEQ,RQCASSEQ   IF SEQUENCE NUMBER IS PROVIDED               
         JZ    *+14                                                             
         CLC   RQCASSEQ,TLCASEQ    ENSURE IT MATCHES CAST                       
         JNE   FCAST120                                                         
                                                                                
         CLC   RQCASONO,=C'ON '    IF ON/OFF CAMERA IS ON                       
         JNE   *+12                                                             
         TM    TLCASORT,X'08'      ENSURE IT MATCHES CAST                       
         JO    FCAST120                                                         
                                                                                
         CLC   RQCASONO,=C'OFF'    IF ON/OFF CAMERA IS OFF                      
         JNE   *+12                                                             
         TM    TLCASORT,X'08'      ENSURE IT MATCHES CAST                       
         JZ    FCAST120                                                         
                                                                                
         GOTOR FLTCACAT,DMCB,0(R4),RQCASCGR,RQCASEMU                            
         JNE   FCAST120            ENSURE CATEGORY MATCHES FILTERS              
                                                                                
         LA    R0,CAXVALS          CLEAR EXPANDED CAST DETAILS                  
         LHI   R1,CAXVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   SVCOM,TLCACOM       SAVE INTERNAL COMMERCIAL NUMBER              
         MVC   SVSEQ,TLCASORT+4    CAST SEQUENCE NUMBER                         
         MVC   SVSSN,TLCASSN       SOCIAL SECURITY NUMBER                       
         MVC   SVCAT,TLCACAT       AND CATEGORY                                 
                                                                                
         MVC   CAXSSN,TLCASSN      PASS BACK SOCIAL SECURITY NUMBER             
         MVC   CAXCAT,TLCACAT      AND CATEGORY                                 
         DROP  R4                                                               
                                                                                
         OC    CURTRKFL,CURTRKFL   IF TRACK FILTER PROVIDED                     
         JZ    *+12                                                             
         BAS   RE,FLTCATRK         ENSURE IT MATCHES CAST                       
         JNE   FCAST120                                                         
                                                                                
         USING TACAD,R4                                                         
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    RQCASAGT,RQCASAGT   IF AGENT FILTER PROVIDED                     
         JZ    *+14                                                             
         CLC   TACANCDE,AGTFILT    ENSURE IT MATCHES CAST                       
         JNE   FCAST120                                                         
                                                                                
         OC    RQCASGUA,RQCASGUA   IF GUARANTEE FILTER PROVIDED                 
         JZ    *+14                                                             
         CLC   TACAGUA,RQCASGUA    ENSURE IT MATCHES CAST                       
         JNE   FCAST120                                                         
                                                                                
         CLI   RQCASMST,C'Y'       IF FILTERING ON MASTER COMMERCIAL            
         JNE   *+12                                                             
         TM    TACASTAT,TACASTLO   EXCLUDE CAST ONLY ON LIFT                    
         JO    FCAST120                                                         
                                                                                
         CLI   RQCASLFT,C'Y'       IF FILTERING ON LIFT VERSION                 
         JNE   *+12                EXCLUDE CAST ONLY ON VERSION                 
         TM    TACASTAT,TACASTLF+TACASTLO                                       
         JZ    FCAST120                                                         
                                                                                
         CLI   RQCASUIN,0          IF FILTERING ON UNION                        
         JE    *+12                                                             
         BAS   RE,FLTCAUNI         ENSURE IT MATCHES CAST                       
         JNE   FCAST120                                                         
                                                                                
         CLI   RQCASEP6,C'Y'       IF EXCLUDING PRE-06 CONTRACT                 
         JNE   FCAST30             YEARS                                        
         CLC   TACAYEAR(2),=C'06'  ENSURE CAST DOES NOT HAVE                    
         JL    FCAST120            CONTRACT LATER THAN 2006                     
         CLC   TACAYEAR(2),=C'70'  BUT NOT BETWEEN 1970 AND 1999                
         JH    FCAST120                                                         
                                                                                
FCAST30  MVC   SVCORP,TACACORP     SAVE CORPORATION NUMBER                      
         MVC   SVLCO,TACALFTF      LIFTED FROM COMMERCIAL NUMBER                
         MVC   SVLAST,TACALAST     LAST SERVICES DATE                           
         MVC   SVTCO,TACATCOM      TRANSFERRED FROM COMMERCIAL NUMBER           
         MVC   CAXGUA,TACAGUA      AND PASS BACK GUARANTEE CODE                 
                                                                                
         XC    SVAGT,SVAGT                                                      
         OC    TACANCDE,TACANCDE   SAVE AGENT CODE                              
         JZ    FCAST40                                                          
         GOTOR (#TRNSAGT,ATRNSAGT),DMCB,(X'40',TACANCDE),SVAGT                  
         DROP  R4                                                               
                                                                                
FCAST40  BAS   RE,FLTCACOM         FILTER BASED ON COMMERCIAL VALUES            
         JNE   FCAST110                                                         
                                                                                
         OC    RQCASLNM,RQCASLNM                                                
         JNZ   FCAST50                                                          
         OC    RQCASSSN,RQCASSSN   IF SOCIAL SECURITY NUMBER FILTER             
         JNZ   *+12                WAS NOT PROVIDED                             
FCAST50  BAS   RE,FLTCAW4          FILTER/SAVE W4 VALUES                        
         JNE   FCAST110                                                         
                                                                                
         BAS   RE,FLTCAAGY         FILTER BASED ON AGENCY VALUES                
         JNE   FCAST110                                                         
                                                                                
         USING TLW4D,R3                                                         
         CLI   SVCORP,0            IF CAST HAS AN ATTACHED CORPORATION          
         JE    FCAST60                                                          
         XC    TLW4KEY,TLW4KEY     READ FOR W4 KEY/RECORD                       
         MVI   TLW4CD,TLW4CDQ      FOR ATTACHED CORPORATION                     
         MVC   TLW4SSN,CAXATC                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   FCAST60                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL            GET W4 DETAILS ELEMENT                       
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   CAXATN,TAW4CRPN     PASS BACK ATTACHED CORPORATION NAME          
         DROP  R4                                                               
                                                                                
FCAST60  OC    RQCASAGT,RQCASAGT   IF AGENT FILTER IS NOT PROVIDED              
         JNZ   FCAST70                                                          
         OC    SVAGT,SVAGT         AND AGENT CODE IS PRESENT                    
         JZ    FCAST70                                                          
         MVC   CAXAGT,SVAGT        PASS BACK AGENT CODE                         
                                                                                
         USING TLAND,R3                                                         
         XC    TLANKEY,TLANKEY     READ FOR AGENT KEY/RECORD                    
         MVI   TLANCD,TLANCDQ                                                   
         MVC   TLANAGT,CAXAGT                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   FCAST70                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TANAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL            GET NAME ELEMENT                             
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CAXAGN(0),TANANAME  PASS BACK AGENT NAME                         
         OC    CAXAGN,SPACES                                                    
         DROP  R4                                                               
                                                                                
FCAST70  OC    SVLCO,SVLCO         IF LIFTED FROM COMMERCIAL IS PRESENT         
         JZ    FCAST80             PASS BACK INTERNAL COMMERCIAL NUMBER         
         GOTO1 VHEXOUT,DMCB,SVLCO,CAXLCO,L'SVLCO,0                              
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   AND READ FOR LIFTED FROM COMMERCIAL          
         MVI   TLCOPCD,TLCOCCDQ    KEY/RECORD                                   
         MVC   TLCOCCOM,SVLCO                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   FCAST100                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO4                                                          
         MVC   CAXLAY,TLCOAGY      PASS BACK LIFTED FROM AGENCY                 
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   CAXLCI,TACOCID      PASS BACK LIFTED FROM ID                     
         DROP  R4                                                               
                                                                                
         USING TACSD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACSELQ      COMMERCIAL STUDIO ELEMENTS                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
FCAST72  BRAS  RE,NEXTEL                                                        
         JNE   FCAST80                                                          
                                                                                
         LA    R6,CAXLFDAT                                                      
         CLI   TACSTYPE,TACSTYPF   FILM?                                        
         JE    FCAST75                                                          
         LA    R6,CAXLRDAT                                                      
         CLI   TACSTYPE,TACSTYPR   RECORD?                                      
         JNE   FCAST80                                                          
                                                                                
FCAST75  GOTO1 VDATCON,DMCB,(1,TACSDATE),(8,0(R6)) DATE                         
         AHI   R6,L'CAXFDAT                                                     
         MVC   0(L'CAXFSTU,R6),TACSSTUD STUDIO                                  
         AHI   R6,L'CAXFSTU                                                     
         MVC   0(L'CAXFCIT,R6),TACSCITY CITY                                    
         CLI   TACSLEN,TACSLNQ                                                  
         JL    FCAST72                                                          
         AHI   R6,L'CAXFCIT                                                     
         MVC   0(L'CAXFSTA,R6),TACSSTAT STATE                                   
         J     FCAST72                                                          
         DROP  R4                                                               
                                                                                
FCAST80  OC    SVTCO,SVTCO         IF TRANSFERRED COMMERCIAL IS PRESENT         
         JZ    FCAST100            PASS BACK INTERNAL COMMERCIAL NUMBER         
         GOTO1 VHEXOUT,DMCB,SVTCO,CAXTCO,L'SVTCO,0                              
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY   AND READ FOR TRANSFERRED COMMERCIAL          
         MVI   TLCOPCD,TLCOCCDQ    KEY/RECORD                                   
         MVC   TLCOCCOM,SVTCO                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   FCAST100                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO4                                                          
         MVC   CAXTAY,TLCOAGY      PASS BACK TRANSFERRED FROM AGENCY            
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   CAXTCI,TACOCID      PASS BACK TRANSFERRED FROM ID                
         DROP  R4                                                               
                                                                                
         USING TACSD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACSELQ      COMMERCIAL STUDIO ELEMENTS                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
FCAST90  BRAS  RE,NEXTEL                                                        
         JNE   FCAST100                                                         
                                                                                
         LA    R6,CAXTFDAT                                                      
         CLI   TACSTYPE,TACSTYPF   FILM?                                        
         JE    FCAST95                                                          
         LA    R6,CAXTRDAT                                                      
         CLI   TACSTYPE,TACSTYPR   RECORD?                                      
         JNE   FCAST100                                                         
                                                                                
FCAST95  GOTO1 VDATCON,DMCB,(1,TACSDATE),(8,0(R6)) DATE                         
         AHI   R6,L'CAXFDAT                                                     
         MVC   0(L'CAXFSTU,R6),TACSSTUD STUDIO                                  
         AHI   R6,L'CAXFSTU                                                     
         MVC   0(L'CAXFCIT,R6),TACSCITY CITY                                    
         CLI   TACSLEN,TACSLNQ                                                  
         JL    FCAST90                                                          
         AHI   R6,L'CAXFCIT                                                     
         MVC   0(L'CAXFSTA,R6),TACSSTAT STATE                                   
         J     FCAST90                                                          
         DROP  R4                                                               
                                                                                
FCAST100 BAS   RE,TRNTATR          TRANSLATE TATR ELEMENTS TO TAFNTVER          
         CLI   RQCASVER,0          IF VERSION FILTER PROVIDED                   
         JE    FCAST105            ENSURE IT MATCHES CAST                       
         GOTOR FLTCAVER,DMCB,0(R4),RQCASVER                                     
         JNE   FCAST110                                                         
FCAST105 BAS   RE,BLDVRTAB         BUILD VERSION TABLE                          
                                                                                
         BAS   RE,FLTCAGRT         FILTER/SAVE GUARANTEE VALUES                 
                                                                                
         MVC   IOKEY,SVIOKEY       RESTORE CAST READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     YES                 AND RETURN POSITIVE CONDITION CODE           
                                                                                
FCAST110 MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
FCAST120 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FCAST10                                                          
                                                                                
***********************************************************************         
*        ROUTINE CHECKS CAST AGAINST AFM CONTRACT AND TRACK FILTER    *         
*        ON ENTRY ... R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
FLTCATRK NTR1                                                                   
         OC    RQCASACO,RQCASACO   IF AFM CONTRACT INTERNAL COMMERCIAL          
         JNZ   FCT10               NUMBER IS NOT PROVIDED                       
         BRAS  RE,FLTTRK           ENSURE CAST IS ASSOCIATED TO THE             
         JE    YES                 TRACK                                        
         J     NO                                                               
                                                                                
         USING TATRD,R4                                                         
FCT10    MVI   ELCODE,TATRELQ      IF AFM CONTRACT INTERNAL                     
         BRAS  RE,GETEL            COMMERCIAL NUMBER IS PROVIDED                
         J     *+8                                                              
FCT20    BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
         CLC   TATRCOM,RQCASACO    ENSURE CAST IS ON THE AFM CONTRACT           
         JNE   NO                                                               
         LHI   RE,L'CURTRKFL                                                    
         LA    RF,CURTRKFL                                                      
FCT30    CLC   TATRTRK,0(RF)       AND TRACK                                    
         JE    YES                                                              
         LA    RF,1(RF)                                                         
         BCT   RE,FCT30                                                         
         J     FCT20                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS CAST AGAINST UNION FILTER                     *         
*        ON ENTRY ... R4=A(CAST DETAILS ELEMENT)                      *         
***********************************************************************         
                                                                                
         USING TACAD,R4                                                         
FLTCAUNI NTR1                                                                   
         ZIC   RE,RQCASUIN         RE=UNION FILTER COUNTER                      
         ZICM  RF,ARQCAUNI,3       RF=A(UNION FILTER LIST)                      
FCU10    CLC   TACAUN,0(RF)        ENSURE UNION MATCHES                         
         JE    YES                 ONE OF THE FILTERS                           
         LA    RF,3(RF)                                                         
         BCT   RE,FCU10                                                         
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS COMMERCIAL AGAINST FILTERS AND SAVES VALUES   *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
FLTCACOM NTR1                                                                   
         XC    TLCOPKEY,TLCOPKEY   READ ATTACHED COMMERCIAL RECORD              
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVCOM                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NO                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO4                                                          
         OC    RQCASAGY,RQCASAGY   IF AGENCY FILTER PROVIDED                    
         JZ    *+14                                                             
         CLC   TLCOAGY,RQCASAGY    ENSURE IT MATCHES COMMERCIAL                 
         JNE   NO                                                               
                                                                                
         OC    RQCASCLI,RQCASCLI   IF CLIENT FILTER PROVIDED                    
         JZ    *+14                                                             
         CLC   TLCOCLI,RQCASCLI    ENSURE IT MATCHES COMMERCIAL                 
         JNE   NO                                                               
                                                                                
         OC    RQCASPRD,RQCASPRD   IF PRODUCT FILTER PROVIDED                   
         JZ    *+14                                                             
         CLC   TLCOPRD,RQCASPRD    ENSURE IT MATCHES COMMERCIAL                 
         JNE   NO                                                               
                                                                                
         BRAS  RE,CKCOACC          ENSURE STAFF HAS ACCESS TO                   
         JNE   NO                  COMMERCIAL                                   
                                                                                
         MVC   CAXAGY,TLCOAGY      PASS BACK AGENCY                             
         MVC   CAXCLI,TLCOCLI      CLIENT                                       
         MVC   CAXPRD,TLCOPRD      AND PRODUCT CODES                            
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            GET COMMERCIAL DETAILS                       
         JE    *+6                 ELEMENT                                      
         DC    H'00'                                                            
                                                                                
         CLI   RQCASREL,C'O'       IF ONLY RETURNING RELEASED                   
         JNE   FCC10               COMMERCIALS                                  
         TM    TACOSTAT,TACOSTRL   REJECT IF COMMERCIAL IS NOT                  
         JO    FCC20               RELEASED                                     
         OC    SVLAST,SVLAST       AND CAST IS NOT LAST SERVICED                
         JZ    NO                                                               
                                                                                
FCC10    CLI   RQCASREL,C'N'       IF ONLY RETURNING UNRELEASED                 
         JNE   FCC20               COMMERCIALS                                  
         TM    TACOSTAT,TACOSTRL   REJECT IF COMMERCIAL IS RELEASED             
         JO    NO                                                               
         OC    SVLAST,SVLAST       OR CAST IS LAST SERVICED                     
         JNZ   NO                                                               
                                                                                
FCC20    MVI   CAXREL,C'N'                                                      
         TM    TACOSTAT,TACOSTRL   PASS BACK COMMERCIAL RELEASED                
         JZ    *+8                 STATUS                                       
         MVI   CAXREL,C'Y'                                                      
                                                                                
         MVC   CAXCID,TACOCID                  PASS BACK COMMERCIAL ID          
         GOTO1 VDATCON,DMCB,(1,TACOFCYC),(8,CAXCFFC) FIRST FIXED CYCLE          
         GOTO1 (RF),(R1),(1,TACOEXP),(8,CAXCOX) AND EXPIRATION DATE             
                                                                                
         MVC   SVCOTYPE,TACOTYPE   SAVE COMMERCIAL TYPE                         
                                                                                
         CLI   RQCASEMU,C'C'       IF EXCLUDING MUSICIANS FROM                  
         JNE   FCC30               NON-TYPE M COMMERCIALS ONLY                  
         CLI   TACOTYPE,CTYMUS     REJECT IF COMMERCIAL IS NOT MUSIC            
         JE    FCC30                                                            
         TM    SVCATUNI,AFM        AND CAST IS MUSICIAN                         
         JZ    FCC30                                                            
         CLC   SVCAT,=C'ZZZ'                                                    
         JNE   NO                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    NO                                                               
         DROP  R4                                                               
                                                                                
         USING TACSD,R4                                                         
FCC30    L     R4,AIO4                                                          
         MVI   ELCODE,TACSELQ      COMMERCIAL STUDIO ELEMENTS                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
FCC40    BRAS  RE,NEXTEL                                                        
         JNE   FCC50                                                            
                                                                                
         LA    R6,CAXFDAT                                                       
         CLI   TACSTYPE,TACSTYPF   FILM?                                        
         JE    FCC45                                                            
         LA    R6,CAXMDAT                                                       
         CLI   TACSTYPE,TACSTYPM   MUSIC?                                       
         JE    FCC45                                                            
         LA    R6,CAXRDAT                                                       
         CLI   TACSTYPE,TACSTYPR   RECORD?                                      
         JNE   FCC50                                                            
                                                                                
FCC45    GOTO1 VDATCON,DMCB,(1,TACSDATE),(8,0(R6)) DATE                         
         AHI   R6,L'CAXFDAT                                                     
         MVC   0(L'CAXFSTU,R6),TACSSTUD STUDIO                                  
         AHI   R6,L'CAXFSTU                                                     
         MVC   0(L'CAXFCIT,R6),TACSCITY CITY                                    
         CLI   TACSLEN,TACSLNQ                                                  
         JL    FCC40                                                            
         AHI   R6,L'CAXFCIT                                                     
         MVC   0(L'CAXFSTA,R6),TACSSTAT STATE                                   
         J     FCC40                                                            
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
FCC50    CLI   RQCASRCT,C'Y'       IF REQUESTING COMMERCIAL TITLE               
         JNE   YES                                                              
         L     R4,AIO4                                                          
         MVI   ELCODE,TANAELQ      AND NAME ELEMENT EXISTS                      
         BRAS  RE,GETEL                                                         
         JNE   YES                                                              
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   CAXCOT(0),TANANAME  PASS BACK COMMERCIAL TITLE                   
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS W4 AGAINST W4 FILTER AND SAVES VALUES         *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLW4D,R3                                                         
FLTCAW4  NTR1                                                                   
         XC    TLW4KEY,TLW4KEY     READ FOR W4 KEY/RECORD                       
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,CAXSSN                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TLW4D,R4                                                         
         L     R4,AIO4                                                          
         LA    R4,TLW4ELEM                                                      
FCW10    CLI   0(R4),TATIELQ       SEARCH FOR TAX ID                            
         JE    FCW30                                                            
         CLI   0(R4),TAW4ELQ       AND W4 DETAILS ELEMENTS                      
         JE    FCW40                                                            
FCW20    ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),0                                                          
         JNE   FCW10                                                            
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
         USING TATID,R4                                                         
FCW30    CLC   SVCORP,TATICRPN     IF CORPORATION NUMBER MATCHES CAST           
         JNE   FCW20                                                            
         MVC   CAXATC,TATIID       PASS BACK FEDERAL ID#                        
         J     FCW20                                                            
         DROP  R4                                                               
                                                                                
         USING TAW4D,R4                                                         
FCW40    BAS   RE,FLTW4LN          FILTER BASED ON LAST NAME                    
         JNE   NO                                                               
         BRAS  RE,FLTW4FN          FILTER BASED ON FIRST NAME                   
         JNE   NO                                                               
         BRAS  RE,FLTW4TY          FILTER BASED ON W4 TYPE                      
         JNE   NO                                                               
                                                                                
         CLI   TAW4TYPE,TAW4TYIN   IF INDIVIDUAL                                
         JE    FCW50                                                            
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         JE    FCW50                                                            
         CLI   TAW4TYPE,TAW4TYFO   OR FOREIGNER                                 
         JNE   FCW60                                                            
FCW50    MVC   CAXLNM,TAW4NAM2     PASS BACK LAST NAME                          
         MVC   CAXFNM,TAW4NAM1     AND FIRST NAME                               
         J     YES                                                              
FCW60    MVC   CAXCNM,TAW4CRPN     ELSE, PASS BACK CORPORATION NAME             
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS W4 LAST NAME AGAINST FILTER                   *         
*        ON ENTRY ... R4=A(W4 DETAILS ELEMENT)                        *         
***********************************************************************         
                                                                                
         USING TAW4D,R4                                                         
FLTW4LN  NTR1                                                                   
         CLI   LNAMELEN,0          IF LAST NAME FILTER WAS PROVIDED             
         JE    YES                                                              
         ZIC   RE,LNAMELEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   TAW4NAM2(0),RQCASLNM ENSURE LAST NAME MATCHES                    
         JE    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS AGENCY AGAINST AGENCY FILTERS                 *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
FLTCAAGY NTR1                                                                   
         CLI   RQCASELA,C'Y'       IF EXCLUDING LOCKED AGENCIES                 
         JNE   YES                                                              
                                                                                
         USING TLAYD,R3                                                         
         XC    TLAYKEY,TLAYKEY     READ FOR AGENCY KEY/RECORD                   
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,CAXAGY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL            GET AGENCY ELEMENT                           
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAAYSTA3,TAAYSLCK   ENSURE AGENCY IS NOT LOCKED                  
         JZ    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS GUARANTEE AGAISNT GUARANTEE FILTERS AND       *         
*        SAVES VALUES                                                 *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
FLTCAGRT NTR1                                                                   
         OC    CAXGUA,CAXGUA       IF PERFORMER HAS AN ATTACHED                 
         JZ    XIT                 GUARANTEE ...                                
                                                                                
         OC    RQCASCGA,RQCASCGA   IF CHECKING WHETHER GUARANTEE IS             
         JZ    *+8                 VALID FOR SUPPLIED AGENCY/CLIENT             
         MVI   CAXGVA,C'N'         INITIALIZE TO NO                             
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY     READ FOR GUARANTEE RECORD                    
         MVI   TLGUCD,TLGUCDQ      KEY/RECORD                                   
         MVC   TLGUSSN,CAXSSN                                                   
         MVC   TLGUGUA,CAXGUA                                                   
         XC    TLGUGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TAGUD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAGUELQ                                                   
         BRAS  RE,GETEL            SEARCH FOR GUARANTEE DETAILS                 
         JNE   XIT                 ELEMENT                                      
                                                                                
         CLI   CAXGVA,C'N'         CHECK IF GUARANTEE IS VALID FOR              
         JNE   FCG10               SUPPLIED AGENCY                              
         GOTO1 SETGVA,DMCB,TAGUAGY,TAGUCLI                                      
                                                                                
FCG10    MVI   CAXGTY,C'L'                                                      
         MVC   CAXGAM,TAGUAMT      IF LARGE OVERSCALE, OUTPUT TYPE              
         OC    TAGUCOM,TAGUCOM     AND AMOUNT                                   
         JZ    FCG20                                                            
                                                                                
         MVI   CAXGTY,C'P'         IF PER CYCLE, OUTPUT TYPE ...                
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FCG20                                                            
         MVC   CAXGAM,TAGCAMT      ... AND AMOUNT                               
         DROP  R4                                                               
                                                                                
FCG20    CLI   CAXGVA,C'N'         IF STILL CHECKING WHETHER GUARANTEE          
         JNE   XIT                 IS VALID FOR SUPPLIED AGENCY ...             
                                                                                
         USING TAVAD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAVAELQ      READ THROUGH ALL OF THE GUARANTEE'S          
         BRAS  RE,GETEL            LIMITED AGENCY/CLIENTS                       
         J     *+8                                                              
FCG30    BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
                                                                                
         ZIC   RF,TAVALEN          IF GUARANTEE HAS AN AGENCY WITH NO           
         SHI   RF,TAVALNQ          LIMITED CLIENTS, CHECK IF AGENCY             
         LTR   RF,RF               MATCHES SUPPLIED AGENCY                      
         JNZ   FCG40                                                            
         GOTO1 SETGVA,DMCB,TAVAAGY,0                                            
         JNE   FCG30                                                            
         J     XIT                                                              
                                                                                
FCG40    XR    RE,RE                                                            
         D     RE,=A(L'TAVACLI)    OTHERWISE CHECK AGAINST EACH OF              
         LR    R2,RF               THE AGENCY'S LIMITED CLIENTS                 
         LA    R3,TAVACLI                                                       
FCG50    GOTO1 SETGVA,DMCB,TAVAAGY,0(R3)                                        
         JE    XIT                                                              
         LA    R3,L'TAVACLI(R3)                                                 
         BCT   R2,FCG50                                                         
         J     FCG30                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS TRADITIONAL VERSION ELEMENT (TAFNTVER)        *         
*        BASED ON TATRELQ ELEMENTS                                    *         
*        ON ENTRY ... AIO3 = A(CAST RECORD)                           *         
***********************************************************************         
                                                                                
TRNTATR  NTR1                                                                   
         USING TATRD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TATRELQ    IF PERFORMER HAS ANY TRACK                     
         BRAS  RE,GETEL          ASSOCIATIONS ...                               
         JNE   XIT                                                              
                                                                                
         USING TRKTABD,R2                                                       
         LA    R2,TRKTAB                                                        
         XC    TRKTAB,TRKTAB                                                    
                                                                                
TTATR10  MVC   TRKCOM,TATRCOM    SAVE ALL TRACK ASSOCIATIONS                    
         MVC   TRKTRK,TATRTRK    INTO TABLE                                     
         LA    R2,TRKLNQ(R2)                                                    
                                                                                
         BRAS  RE,NEXTEL                                                        
         JE    TTATR10                                                          
         MVI   0(R2),X'FF'                                                      
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
                                                                                
         XC    ELEM2,ELEM2                                                      
                                                                                
         USING TRKTABD,R2                                                       
         LA    R2,TRKTAB         FOR EACH TRACK IN THE TABLE ...                
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,IOKEY                                                         
TTATR20  XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOTCDQ                                                 
         MVC   TLCOTMCO,TRKCOM                                                  
         MVC   TLCOTTRK,TRKTRK                                                  
         MVC   TLCOTCOM,SVCOM                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     TTATR40                                                          
TTATR30  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
TTATR40  CLC   IOKEY(TLCOTVER-TLCOPD),IOKEYSAV                                  
         JNE   TTATR50                                                          
                                                                                
         LA    RE,ELEM2                                                         
         ZIC   RF,TLCOTVER                                                      
         SHI   RF,1                                                             
         AR    RE,RF                                                            
         MVI   0(RE),C'X'                                                       
         J     TTATR30                                                          
                                                                                
TTATR50  LA    R2,TRKLNQ(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         JNE   TTATR20                                                          
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TAFNEL,TAFNELQ                                                   
         LHI   R0,TAFNLNQ                                                       
         MVI   TAFNTYPE,TAFNTVER                                                
         LA    R2,TAFNNAME                                                      
                                                                                
         LHI   RE,1                                                             
         LA    RF,ELEM2                                                         
         MVI   250(RF),X'FF'                                                    
                                                                                
TTATR60  CLI   0(RF),C'X'                                                       
         JNE   TTATR70                                                          
         STC   RE,0(R2)                                                         
         LA    R2,1(R2)                                                         
         AHI   R0,1                                                             
                                                                                
TTATR70  AHI   RE,1                                                             
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         JNE   TTATR60                                                          
                                                                                
         CHI   R0,TAFNLNQ                                                       
         JE    XIT                                                              
         STC   R0,TAFNLEN                                                       
         GOTO1 VHELLO,DMCB,(C'P',=C'TALFIL'),AIO3,ELEM,0                        
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VERSION TABLE                                 *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
*                     AIO3=A(CAST RECORD)                             *         
***********************************************************************         
                                                                                
BLDVRTAB NTR1                                                                   
         CLC   LASTCOM,SVCOM                                                    
         JE    BVT40                                                            
         MVC   LASTCOM,SVCOM                                                    
                                                                                
         LA    R0,COVRTAB                                                       
         LHI   R1,L'COVRTAB        INITIALIZE COMMERCIAL VERSION                
         XR    RE,RE               TABLE                                        
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   COVRTAB,X'FF'                                                    
                                                                                
         LR    R2,R0                                                            
                                                                                
         USING TLVRD,R3                                                         
         XC    TLVRKEY,TLVRKEY                                                  
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,SVCOM                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         CLC   IOKEY(TLVRVER-TLVRD),IOKEYSAV                                    
         JNE   BVT30                                                            
         MVI   COVRTAB,1                                                        
         LA    R2,COVRTAB+2                                                     
         J     BVT20                                                            
                                                                                
BVT10    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
         CLC   IOKEY(TLVRVER-TLVRD),IOKEYSAV                                    
         JNE   BVT30                                                            
                                                                                
BVT20    MVC   0(1,R2),TLVRVER                                                  
         LA    R2,2(R2)                                                         
         J     BVT10                                                            
         DROP  R3                                                               
                                                                                
BVT30    MVI   0(R2),X'FF'                                                      
                                                                                
***********************************************************************         
                                                                                
BVT40    LA    R0,CAVRTAB                                                       
         LHI   R1,L'CAVRTAB        INITIALIZE CAST VERSION TABLE                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   CAVRTAB,X'FF'                                                    
                                                                                
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO3),('TAFNTVER',0)         
         JNE   NOMORE                                                           
         L     R4,AELEM                                                         
                                                                                
         LA    RE,TAFNNAME         RE=A(VERSIONS)                               
         ZIC   RF,TAFNLEN                                                       
         SHI   RF,3                RF=VERSIONS COUNTER                          
         DROP  R4                                                               
                                                                                
         LA    R2,CAVRTAB          R2=A(CAST VERSION TABLE)                     
                                                                                
         CLI   0(RE),251           IF PERFORMER IS ON ALL VERSIONS              
         JNE   BVT50               COPY COMMERCIAL VERSION TABLE                
         LR    R0,R2                                                            
         LHI   R1,L'CAVRTAB                                                     
         LA    RE,COVRTAB                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     BVT60                                                            
                                                                                
BVT50    MVC   0(1,R2),0(RE)       OTHERWISE, COPY PERFORMERS VERSIONS          
         LA    R2,2(R2)            INTO CAST VERSION TABLE                      
         LA    RE,1(RE)                                                         
         BCT   RF,BVT50                                                         
         MVI   0(R2),X'FF'                                                      
                                                                                
         USING TLCKPD,R3                                                        
BVT60    CLI   RQCASPAD,C'Y'       IF RETURNING VERSION PAID STATUS             
         JNE   XIT                                                              
         XC    TLCKPKEY,TLCKPKEY   READ ALL USAGE HISTORY FOR THIS              
         MVI   TLCKPCD,TLCKHCDQ    PERFORMER ON THIS COMMERCIAL                 
         MVC   TLCKHCOM,SVCOM                                                   
         MVC   TLCKHSSN,SVSSN                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     BVT80                                                            
BVT70    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
BVT80    CLC   IOKEY(TLCKHCAT-TLCKPD),IOKEYSAV                                  
         JNE   XIT                                                              
         CLC   TLCKHSEQ,SVSEQ                                                   
         JNE   BVT70                                                            
         MVC   SVCKPKEY,IOKEY                                                   
         DROP  R3                                                               
                                                                                
         USING TLINPD,R3                                                        
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINHCDQ                                                 
         MVC   TLINHCOM,SVCOM                                                   
         MVC   TLINHINV,SVCKPKEY+TLCKHINV-TLCKPD                                
         MVI   TLINHSEQ,X'FF'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   BVT110                                                           
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAINSTAT,TAINSCIN+TAINSCAN                                       
         JNZ   BVT110                                                           
         DROP  R4                                                               
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BVT110                                                           
                                                                                
         LA    R2,CAVRTAB                                                       
BVT90    CLI   0(R2),X'FF'                                                      
         JE    BVT110                                                           
         CLC   TAVRVERS,0(R2)                                                   
         JE    BVT100                                                           
         LA    R2,2(R2)                                                         
         J     BVT90                                                            
         DROP  R4                                                               
                                                                                
BVT100   MVI   1(R2),C'Y'                                                       
                                                                                
BVT110   MVC   IOKEY,SVCKPKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     BVT70                                                            
                                                                                
***********************************************************************         
*        ROUTINE SETS VARIABLE INDICATING IF GUARANTEE IS VALID FOR   *         
*        SUPPLIED AGENCY/CLIENT                                       *         
*        ON ENTRY ... P1=A(GUARANTEE AGENCY)                          *         
*                     P2=A(GUARANTEE CLIENT)                          *         
***********************************************************************         
                                                                                
SETGVA   NTR1                                                                   
         L     R2,0(R1)            IF GUARANTEE AGENCY IS SET                   
         OC    0(L'TAGUAGY,R2),0(R2)                                            
         JZ    SGVA10                                                           
         CLC   RQCASCGA,0(R2)      AND DOES NOT MATCH SUPPLIED AGENCY           
         JNE   NO                  LEAVE INDICATOR AS NO                        
                                                                                
         OC    RQCASCGC,RQCASCGC   IF GHECKING WHETHER GUARANTEE IS             
         JZ    SGVA10              VALID FOR SUPPLIED CLIENT                    
         ZICM  R3,4(R1),4          AND GUARANTEE CLIENT IS SET                  
         JZ    SGVA10                                                           
         OC    0(L'TAGUCLI,R3),0(R3)                                            
         JZ    SGVA10                                                           
         CLC   RQCASCGC,0(R3)      OR GUARANTEE CLIENT MATCHES                  
         JNE   NO                  LEAVE INDICATOR AS NO                        
                                                                                
SGVA10   MVI   CAXGVA,C'Y'         OTHERWISE SET INDICATOR AS YES               
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE SETS NEXT TRACK FILTER IN CURTRKFL                   *         
*        ON ENTRY ... P1 BYTE 0 = TRACK FILTER COUNT                  *         
*                     P1        = A(TRACK FILTER LIST)                *         
***********************************************************************         
                                                                                
SETCURTF NTR1  BASE=*,LABEL=*                                                   
         L     R1,0(R1)                                                         
         ZICM  RE,0(R1),1                                                       
         JZ    XIT                                                              
                                                                                
         ZICM  RF,1(R1),3                                                       
SCTF10   OC    0(L'CURTRKFL,RF),0(RF)                                           
         JNZ   SCTF20                                                           
         LA    RF,L'CURTRKFL(RF)                                                
         BCT   RE,SCTF10                                                        
         DC    H'00'                                                            
                                                                                
SCTF20   MVC   CURTRKFL,0(RF)                                                   
         XC    0(L'CURTRKFL,RF),0(RF)                                           
                                                                                
         CLI   CURTRKFL,C'!'                                                    
         JNE   XIT                                                              
         XC    CURTRKFL,CURTRKFL                                                
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RESETS IOKEY TO READ FOR ADDITONAL CRITERIA          *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
ADJCAKEY NTR1  BASE=*,LABEL=*                                                   
         L     R2,ACOMTAB          IF SEARCHING FOR MULTIPLE                    
         CLI   0(R2),X'FF'         INTERNAL COMMERCIAL NUMBERS                  
         JE    ACK30                                                            
                                                                                
ACK10    OC    0(L'TLCOBCOM,R2),0(R2)                                           
         JNZ   ACK20                                                            
         LA    R2,L'TLCOBCOM(R2)   FIND THE NEXT COMMERCIAL THAT WE             
         CLI   0(R2),X'FF'         WANT TO READ FOR                             
         JNE   ACK10                                                            
         J     NO                                                               
                                                                                
ACK20    MVC   RQCASCOM,0(R2)                                                   
         XC    0(L'TLCOBCOM,R2),0(R2)                                           
                                                                                
         USING TLCAD,R3                                                         
         XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,RQCASCOM    AND READ FIRST CAST KEY                      
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         DROP  R3                                                               
                                                                                
         GOTOR SETCURTF,DMCB,RQCASTIN                                           
         J     YES                                                              
                                                                                
***********************************************************************         
                                                                                
ACK30    OC    RQCASLNM,RQCASLNM   IF SEARCH REQUEST INCLUDES LAST              
         JZ    NO                  NAME                                         
         OC    RQCASCOM,RQCASCOM   BUT DOES NOT INCLUDE INTERNAL                
         JNZ   NO                  COMMERCIAL NUMBER                            
         OC    RQCASCID,RQCASCID   OR COMMERCIAL ID ...                         
         JNZ   NO                                                               
                                                                                
         L     R2,ASSNTAB          FIND THE NEXT SS# THAT WE WANT               
ACK40    CLI   0(R2),X'FF'         TO READ FOR                                  
         JE    NO                                                               
         OC    0(L'SVSSN,R2),0(R2)                                              
         JNZ   ACK50                                                            
         LA    R2,L'SVSSN(R2)                                                   
         J     ACK40                                                            
                                                                                
ACK50    MVC   RQCASSSN,0(R2)                                                   
                                                                                
         USING TLCAPD,R3                                                        
         XC    TLCAPKEY,TLCAPKEY   BUILD KEY WITH NEXT SS#                      
         MVI   TLCAPCD,TLCACCDQ    AND READ FOR IT                              
         MVC   TLCACSSN,RQCASSSN                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         DROP  R3                                                               
                                                                                
         XC    0(L'SVSSN,R2),0(R2)                                              
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS CAST AGAINST CATEGORY GROUPING FILTER         *         
*        ON ENTRY ... P1=A(CAST RECORD)                               *         
*                     P2=A(CATEGORY GROUPING FILTER)                  *         
*                     P3=A(EXCLUDING MUSICIANS FILTER)                *         
***********************************************************************         
                                                                                
         USING TLCAD,R4                                                         
FLTCACAT NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)                                                         
         L     RE,4(R1)                                                         
         MVC   BYTE1,0(RE)         BYTE1=CATEGORY GROUPING FILTER               
         L     RE,8(R1)                                                         
         MVC   BYTE2,0(RE)         BYTE2=EXCLUDING MUSICIANS FILTER             
                                                                                
         CLI   BYTE1,0             IF FILTERING BY CATEGORY GROUPING            
         JNE   FCCAT10                                                          
         CLI   BYTE2,C'Y'          OR EXCLUDING MUSICIANS                       
         JE    FCCAT10                                                          
         CLI   BYTE2,C'C'                                                       
         JNE   YES                                                              
                                                                                
         USING TGTABLES,RE                                                      
FCCAT10  L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGACATS          RF=DISPLACEMENT OF CATEGORY TABLE            
         DROP  RE                                                               
                                                                                
         USING CATTABD,RF                                                       
         AR    RF,RE               RF=A(CATEGORY TABLE)                         
                                                                                
FCCAT20  CLC   TLCACAT,CATCDE      FIND CAST CATEGORY IN                        
         JE    FCCAT30             CATEGORY TABLE                               
         ZIC   RE,CATLEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   FCCAT20                                                          
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
FCCAT30  MVC   SVCATUNI,CATUNI     SAVE VALID UNIONS FOR CATEGORY               
                                                                                
         CLI   BYTE1,C'P'          IF ONLY WANT PRINCIPALS                      
         JNE   FCCAT40                                                          
         TM    CATTYPE,EXTRA       ENSURE PERFORMER IS A PRINCIPAL              
         JO    NO                                                               
                                                                                
FCCAT40  CLI   BYTE1,C'E'          IF ONLY WANT EXTRAS                          
         JNE   FCCAT50                                                          
         TM    CATTYPE,EXTRA       ENSURE PERFORMER IS AN EXTRA                 
         JZ    NO                                                               
                                                                                
FCCAT50  CLI   BYTE2,C'Y'          IF EXCLUDING MUSICIANS                       
         JNE   YES                                                              
         TM    CATUNI,AFM          ENSURE PERFORMER IS NOT A MUSICIAN           
         JZ    YES                                                              
         CLC   CATCDE,=C'ZZZ'                                                   
         JNE   NO                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,TATRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   YES                                                              
         J     NO                                                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE GETS AFM MUSIC CONTRACT RECORD AND SAVES VALUES      *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
         USING TLCOPD,R3                                                        
SETAFM   NTR1  BASE=*,LABEL=*                                                   
         XC    TLCOPKEY,TLCOPKEY   READ AFM MUSIC COMMERCIAL                    
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,CAXACONC                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NO                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TACSD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TACSELQ      COMMERCIAL STUDIO ELEMENT                    
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SAFM10   BRAS  RE,NEXTEL                                                        
         JNE   SAFM20                                                           
         CLI   TACSTYPE,TACSTYPM   MUSIC?                                       
         JNE   SAFM10                                                           
                                                                                
         GOTO1 VDATCON,DMCB,(1,TACSDATE),(8,CAXADAT) DATE                       
         MVC   CAXASTU,TACSSTUD    STUDIO                                       
         MVC   CAXACIT,TACSCITY    CITY                                         
         CLI   TACSLEN,TACSLNQ                                                  
         JL    SAFM20                                                           
         MVC   CAXASTA,TACSSTAT    STATE                                        
                                                                                
SAFM20   MVC   IOKEY,SVIOKEY       RESTORE CAST READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         MVI   ELCODE,0                                                         
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS CAST AGAINST TRACK FILTER                     *         
*        ON ENTRY ... R4=A(CAST RECORD)                               *         
***********************************************************************         
                                                                                
FLTTRK   NTR1  BASE=*,LABEL=*                                                   
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',(R4)),('TAFNTTRK',0)         
         JNE   NO                                                               
         L     R4,AELEM                                                         
         CLI   TAFNNAME,C'*'       AND ENSURE CAST IS ON THE TRACK              
         JE    YES                                                              
         LHI   RE,L'CURTRKFL       RE=TRACK FILTER COUNTER                      
         LA    RF,CURTRKFL         RF=A(TRACK FILTER LIST)                      
FT10     ZIC   R0,TAFNLEN                                                       
         SHI   R0,3                                                             
         LA    R1,TAFNNAME                                                      
FT20     CLC   0(1,R1),0(RF)                                                    
         JE    YES                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,FT20                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,FT10                                                          
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS W4 FIRST NAME AGAINST FILTER                  *         
*        ON ENTRY ... R4=A(W4 DETAILS ELEMENT)                        *         
***********************************************************************         
                                                                                
         USING TAW4D,R4                                                         
FLTW4FN  NTR1  BASE=*,LABEL=*                                                   
         CLI   FNAMELEN,0          IF FIRST NAME WAS ENTERED                    
         JE    YES                                                              
         ZIC   RE,FNAMELEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   TAW4NAM1(0),RQCASFNM ENSURE FIRST NAME MATCHES                   
         JE    YES                                                              
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS CAST'S W4 TYPE AGAINST CAST SEARCH W4 TYPE    *         
*        FILTERS                                                      *         
*        ON ENTRY ... R4=A(W4 DETAILS ELEMENT)                        *         
***********************************************************************         
                                                                                
         USING TAW4D,R4                                                         
FLTW4TY  NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCASWIN,0          IF W4 TYPE FILTER IS DEFINED                 
         JE    YES                                                              
         ZIC   RE,RQCASWIN         RE=W4 TYPE FILTER COUNTER                    
         ZICM  RF,ARQCAW4T,3       RF=A(W4 TYPE FILTER LIST)                    
FW4T10   CLC   TAW4TYPE,0(RF)      ENSURE W4 TYPE MATCHES                       
         JE    YES                 ONE OF THE FILTERS                           
         LA    RF,1(RF)                                                         
         BCT   RE,FW4T10                                                        
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT INVOICE KEY/RECORD MATCHES SEARCH        *         
*        FILTERS AND RETURNS THE RECORD                               *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
FLTINV   NTR1  BASE=*,LABEL=*                                                   
FINV10   ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   READ INVOICE RECORD INTO AIO3                
         JNE   NO                                                               
         TM    IOKEY+TLDRSTAT-TLDRD,TLINSDEL                                    
         JO    FINV70                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLIND,R4                                                         
         L     R4,AIO3                                                          
         TM    TLINSTAT,TLINSDEL                                                
         JO    FINV70                                                           
         MVC   SVAGY,TLINAGY                                                    
         MVC   SVINV,TLININV                                                    
         DROP  R4                                                               
                                                                                
         MVC   SVIOKEY,IOKEY                                                    
                                                                                
         USING TAVRD,R4                                                         
         CLI   RQINSVER,0          IF VERSION FILTER IS PROVIDED                
         JE    FINV20                                                           
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FINV70                                                           
         CLC   TAVRVERS,RQINSVER   ENSURE IT MATCHES THE INVOICE                
         JNE   FINV70                                                           
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
FINV20   CLI   RQINSCNR,C'N'       IF CANCELLER FILTER IS PROVIDED              
         JE    *+12                                                             
         CLI   RQINSCNL,C'N'       OR CANCELLED FILTER IS PROVIDED              
         JNE   FINV30                                                           
         L     R4,AIO3             GET INVOICE DETAILS ELEMENT                  
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FINV70                                                           
                                                                                
         CLI   RQINSCNR,C'N'       IF CANCELLER FILTER IS PROVIDED              
         JNE   *+12                                                             
         TM    TAINSTAT,TAINSCIN   ENSURE IT MATCHES THE INVOICE                
         JO    FINV70                                                           
                                                                                
         CLI   RQINSCNL,C'N'       IF CANCELLED FILTER IS PROVIDED              
         JNE   FINV30                                                           
         TM    TAINSTAT,TAINSCAN   ENSURE IT MATCHES THE INVOICE                
         JO    FINV70                                                           
         DROP  R4                                                               
                                                                                
         USING TLCKD,R3                                                         
FINV30   OC    RQINSSEQ,RQINSSEQ  IF CAST SEQUENCE NUMBER FILTER                
         JZ    YES                IS PROVIDED                                   
         XC    TLCKKEY,TLCKKEY                                                  
         MVI   TLCKCD,TLCKCDQ     ENSURE CAST CHECK IS ATTACHED                 
         MVC   TLCKAGY,SVAGY      TO THE INVOICE                                
         MVC   TLCKINV,SVINV                                                    
         XC    TLCKINV,=6X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     FINV50                                                           
FINV40   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
FINV50   CLC   IOKEY(TLCKSORT-TLCKD),IOKEYSAV                                   
         JNE   FINV60                                                           
         CLC   RQINSSEQ,TLCKSORT+4                                              
         JNE   FINV40                                                           
         DROP  R3                                                               
                                                                                
         MVC   IOKEY,SVIOKEY      RESTOR INVOICE READ SEQUENCE                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     YES                AND RETURN POSITIVE CONDITION CODE            
                                                                                
FINV60   MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
FINV70   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FINV10                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT COMMENT KEY/RECORD MATCHES SEARCH        *         
*        FILTER AND RETURNS THE RECORD IN AIO3                        *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLCMD,R3                                                         
FLTCMT   NTR1  BASE=*,LABEL=*                                                   
FCMT10   ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   ENSURE KEY MATCHES FILTERS                   
         JNE   NO                                                               
                                                                                
         CLI   RQCMSLEV,RQCMLEVC   IF FILTERING ON CLIENT-LEVEL                 
         JNE   FCMT20                                                           
         CLI   TLCMLEV,0           ENSURE LEVEL MATCHES                         
         JNE   FCMT50                                                           
                                                                                
FCMT20   CLI   RQCMSLEV,RQCMLEVT   IF FILTERING ON TPC-LEVEL                    
         JNE   FCMT30                                                           
         TM    TLCMLEV,TLCMTPC     ENSURE LEVEL MATCHES                         
         JZ    FCMT50                                                           
                                                                                
FCMT30   CLI   RQCMSVER,0          IF FILTERING ON VERSION                      
         JE    FCMT40                                                           
         CLC   RQCMSVER,TLCMVER    ENSURE VERSION MATCHES                       
         JE    FCMT40                                                           
         CLI   RQCMSVER,1          UNLESS FILTERING ON VERSION 1                
         JNE   FCMT50                                                           
         CLI   TLCMVER,0           AND VERSION IS NOT SET                       
         JNE   FCMT50              ALLOW IT TO PASS                             
         DROP  R3                                                               
                                                                                
FCMT40   MVC   SVIOKEY,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         J     YES                                                              
                                                                                
FCMT50   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FCMT10                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT FIXED CYCLE SEARCH STATUS RECORD                      *         
*        ROUTINE CHECKS THAT CAST KEY/RECORD MATCHES SEARCH FILTER    *         
*        FILTER AND RETURNS THE RECORD IN AIO3                        *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLCAD,R3                                                         
FLTFCS   NTR1  BASE=*,LABEL=*                                                   
         OC    SVADDR,SVADDR       IF A(FIXED CYCLE ELEMENT) IS                 
         JZ    FFCS10              SAVED, GO READ THE NEXT FIXED                
         MVI   ELCODE,TACRELQ      CYCLE ELEMENT                                
         L     R4,SVADDR                                                        
         J     FFCS40                                                           
                                                                                
FFCS10   CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   NO                                                               
                                                                                
         OC    RQFCSSEQ,RQFCSSEQ   IF FILTERING ON CAST SEQUENCE                
         JZ    FFCS30              NUMBER, DO SO NOW                            
         CLC   TLCASEQ,RQFCSSEQ                                                 
         JE    FFCS30                                                           
         DROP  R3                                                               
                                                                                
FFCS20   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FFCS10                                                           
                                                                                
FFCS30   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         XC    SVADDR,SVADDR                                                    
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACRELQ      READ FIXED CYCLE ELEMENTS                    
         BRAS  RE,GETEL                                                         
         JNE   FFCS20                                                           
         LA    R0,1                START OFF AS 1 FIXED CYCLE ELEMENT           
         LR    R1,R4               SAVE  R4                                     
FFCS34   BRAS  RE,NEXTEL           COUNT NUMBER OF FIXED CYCLE ELEMENTS         
         JNE   *+12                IN CAST RECORD                               
         AHI   R0,1                                                             
         J     FFCS34              GET NEXT FIXED CYCLE ELEMENT                 
         LR    R4,R1               RESTORE R4                                   
         STCM  R0,3,NUMFCEL        SAVE OFF NUMBER OF FIXED CYCLE ELEM          
         J     FFCS45                                                           
*                                                                               
FFCS40   BRAS  RE,NEXTEL                                                        
         JNE   FFCS20                                                           
                                                                                
FFCS45   OC    RQFCSCYS,RQFCSCYS   IF REQUEST CONTAINS CYCLE START              
         JZ    FFCS50              DATE                                         
         CLC   TACRSTRT,RQFCSCYS   ENSURE FIXED CYCLE EXISTS WITH               
         JNE   FFCS40              THAT CYCLE START DATE                        
                                                                                
         OC    RQFCSCYE,RQFCSCYE   IF REQUEST CONTAINS CYCLE END DATE           
         JZ    FFCS50                                                           
         CLC   TACREND,RQFCSCYE    ENSURE FIXED CYCLE EXISTS WITH               
         JNE   FFCS40              THAT CYCLE END DATE                          
                                                                                
FFCS50   ST    R4,SVADDR           SAVED A(FIXED CYCLE ELEMENT)                 
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK STAFF ACCESS TO W4 RECORD                   *         
*        ON ENTRY ... P1=A(SS# TO CHECK)                              *         
*                     R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
CKW4ACC  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACBLKMAX,0          IF AGENCY/CLIENT LIMITATIONS                 
         JE    YES                 WERE SAVED ...                               
                                                                                
         MVC   SVIOKEY(L'TLW4KEY),IOKEY                                         
         L     R1,0(R1)                                                         
                                                                                
         USING TLCKPD,R3                                                        
         XC    TLCKPKEY,TLCKPKEY   READ ALL CHECK KEYS FOR PERFORMER            
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,0(R1)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO4'                            
         J     CW4A20                                                           
CW4A10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO4'                            
CW4A20   CLC   IOKEY(TLCKECUR-TLCKPD),IOKEYSAV                                  
         JNE   CW4ANO                                                           
                                                                                
         XR    R0,R0               INIT CURRENT AGY/CLI LIMIT BLOCK             
                                                                                
CW4A30   ZIC   RE,ACBLKMAX                                                      
         CR    R0,RE               IF ALL AGY/CLI BLOCKS HAVE BEEN              
         JE    CW4A10              CHECKED, STAFF DOES NOT HAVE                 
         AHI   R0,1                ACCESS TO THIS CHECK                         
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WSSVRBLK                                                      
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R0,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF RECORD VIA WSSVR                
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,AIO5                                                     
         GOTO1 VWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         XC    ACURTAVA,ACURTAVA                                                
                                                                                
         USING TAVAD,R2                                                         
         L     R2,AIO5                                                          
CW4A40   CLI   0(R2),TAVAELQ       IF NO MORE TAVA ELEMENTS FOUND,              
         JNE   CW4A30              RECALL NEXT RECORD FROM WSSVR                
                                                                                
         CLC   TLCKEAGY,TAVAAGY    IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   CW4A60                                                           
         CLI   TAVALEN,TAVALNQ     AND NO CLIENT LIMITS ARE DEFINED             
         JE    CW4AYES             ACCESS IS GRANTED                            
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO4'                           
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO4                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CW4A50   CLC   TAPDCLI,0(RF)       IF CLIENT IS FOUND IN STAFF LIMITS           
         JE    CW4AYES             ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CW4A50                                                           
         DROP  R4                                                               
                                                                                
CW4A60   ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     CW4A40                                                           
         DROP  R2                                                               
                                                                                
CW4AYES  LHI   R2,1                                                             
         J     *+8                                                              
CW4ANO   LHI   R2,0                                                             
         MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         CHI   R2,1                                                             
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK STAFF ACCESS TO COMMERCIAL RECORD           *         
*        ON ENTRY ... IOADDR = A(COMMERCIAL RECORD)                   *         
***********************************************************************         
                                                                                
CKCOACC  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACBLKMAX,0          IF AGY/CLI LIMITS WERE NOT SAVED,            
         JE    YES                 STAFF HAS ACCESS TO ALL COMMERCIALS          
                                                                                
         USING TLCOD,R2                                                         
         L     R2,IOADDR           R2=A(COMMERCIAL RECORD)                      
                                                                                
         ZIC   R3,ACBLKMAX         INIT # OF SAVED AGY/CLI LIMIT BLKS           
         XR    R0,R0               INIT CURRENT AGY/CLI LIMIT BLOCK             
                                                                                
CCOA10   CR    R0,R3               IF ALL AGY/CLI BLOCKS HAVE BEEN              
         JE    NO                  CHECKED, STAFF DOES NOT HAVE                 
         AHI   R0,1                ACCESS TO THIS COMMERCIAL                    
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WSSVRBLK                                                      
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R0,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF RECORD VIA WSSVR                
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,AIO5                                                     
         GOTO1 VWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         USING TAVAD,R4                                                         
CCOA20   L     R4,AIO5                                                          
CCOA30   CLI   0(R4),TAVAELQ       IF NO MORE TAVA ELEMENTS FOUND,              
         JNE   CCOA10              RECALL NEXT RECORD FROM WSSVR                
                                                                                
         CLC   TLCOAGY,TAVAAGY     IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   CCOA50                                                           
         CLI   TAVALEN,TAVALNQ     AND NO CLIENT LIMITS ARE DEFINED             
         JE    YES                 ACCESS IS GRANTED                            
                                                                                
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CCOA40   CLC   TLCOCLI,0(RF)       IF CLIENT IS FOUND IN STAFF LIMITS           
         JE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CCOA40                                                           
                                                                                
CCOA50   ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         J     CCOA30                                                           
         DROP  R2,R4                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SAVE LENGTH OF INPUT IN NAME FIELDS                          *         
*        ON ENTRY ... P1 BYTE 0 = MAXIMUM LENGTH FOR COMPARE          *         
*                     P1 = A(NAME FIELD)                              *         
*                     P2 = A(LENGTH FIELD)                            *         
*                     P3 = A(SEARCH INSTRUCTIONS)                               
***********************************************************************         
                                                                                
SVLENGTH NTR1  BASE=*,LABEL=*                                                   
         ZICM  R2,1(R1),3          R2=A(NAME FIELD)                             
         L     R3,4(R1)            R3=A(FIELD TO SAVE INPUT LENGTH IN)          
         ZIC   R4,0(R1)            R4=MAXIMUM LENGTH                            
         L     R1,8(R1)            R1=A(SEARCH FIELD INSTRUCTIONS)              
                                                                                
         XR    RF,RF               SAVE LENGTH OF INPUT                         
         OC    0(L'RQW4SLNM,R2),0(R2)                                           
         BZ    SVLEN20                                                          
         LA    RE,L'RQW4SLNM-1(R2)                                              
         LHI   RF,L'RQW4SLNM                                                    
SVLEN10  LTR   RF,RF                                                            
         JNZ   *+6                                                              
         DC    H'00'                                                            
         CLI   0(RE),C' '                                                       
         JNE   SVLEN20                                                          
         SHI   RE,1                                                             
         SHI   RF,1                                                             
         J     SVLEN10                                                          
                                                                                
SVLEN20  LTR   R1,R1               IF SEARCH INSTRUCTIONS ARE                   
         JZ    SVLEN30             PROVIDED                                     
         CLI   0(R1),C'E'          AND SEARCHING FOR EXACT MATCHES              
         JNE   SVLEN30                                                          
         AHI   RF,1                ADD 1 TO LENGTH                              
                                                                                
SVLEN30  CR    RF,R4               IF LENGTH EXCEEDS MAXIMUM LENGTH             
         JNH   *+6                 SET AS MAXIUMUM LENGTH INSTEAD               
         LR    RF,R4                                                            
                                                                                
         STC   RF,0(R3)                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS CAST AGAINST VERSION FILTER                   *         
*        ON ENTRY ... P1=A(CAST RECORD)                               *         
*                     P2=A(VERSION TO SEARCH FOR)                               
***********************************************************************         
                                                                                
FLTCAVER NTR1  BASE=*,LABEL=*                                                   
         L     R4,0(R1)            R4=A(CAST RECORD)                            
         L     R2,4(R1)            R2=A(VERSION TO SEARCH FOR)                  
                                                                                
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',(R4)),('TAFNTVER',0)         
         JNE   NO                                                               
         L     R4,AELEM            SEARCH FOR VERSION ELEMENT                   
         CLI   TAFNNAME,251        AND ENSURE CAST IS ON THE VERSION            
         JE    YES                                                              
         ZIC   R0,TAFNLEN                                                       
         SHI   R0,3                                                             
         LA    RE,TAFNNAME                                                      
FCV10    CLC   0(1,R2),0(RE)                                                    
         JE    YES                                                              
         LA    RE,1(RE)                                                         
         BCT   R0,FCV10                                                         
         J     NO                                                               
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* KEY DRIVER TABLES                                                   *         
***********************************************************************         
                                                                                
STFKEYT  LKKEY H,TLSTKEY,SAVED     STAFF RECORD                                 
         LKKEY LIT,TLSTCD,TLSTCDQ                                               
         LKKEY LIT,TLSSPR1,0                                                    
         LKKEY SIN,TLSTUSER,USERID                                              
         LKKEY LIT,TLSSPR2,0                                                    
         LKKEY SIN,TLSTSTAF,RQSTFCD                                             
         LKKEY LIT,TLSSPR3,0                                                    
         LKKEY ALL,TLSTSSEQ                                                     
         LKKEY E                                                                
                                                                                
CLIKEYT  LKKEY H,TLCLKEY           CLIENT RECORD                                
         LKKEY LIT,TLCLCD,TLCLCDQ                                               
         LKKEY LIT,TLCLSPR1,0                                                   
         LKKEY SIN,TLCLAGY,SVAGY                                                
         LKKEY LIT,TLCLSPR2,0                                                   
         LKKEY RNG,TLCLCLI,CLIRANGE                                             
         LKKEY LIT,TLCLSPR3,0                                                   
         LKKEY E                                                                
                                                                                
CGRKEYT  LKKEY H,TLCLPKEY          CLIENT GROUP PASSIVE                         
         LKKEY LIT,TLCLPCD,TLCLGCDQ                                             
         LKKEY LIT,TLCLGCS1,0                                                   
         LKKEY SIN,TLCLGCLG,SVCLIGRP                                            
         LKKEY LIT,TLCLGCS2,0                                                   
         LKKEY SIN,TLCLGAGY,SVAGY                                               
         LKKEY LIT,TLCLGCS3,0                                                   
         LKKEY RNG,TLCLGCLI,CLIRANGE                                            
         LKKEY LIT,TLCLGCS4,0                                                   
         LKKEY E                                                                
                                                                                
ACMKEYR  LKKEY H,TLCMKEY           ATS COMMENT RECORD                           
         LKKEY LIT,TLCMCD,TLCMCDQ                                               
         LKKEY LIT,TLCMSPR1,0                                                   
         LKKEY SIN,TLCMAGY,RQRLAGY                                              
         LKKEY LIT,TLCMTYP,TLCMTCOM                                             
         LKKEY SIN,TLCMCID,RQRLCID                                              
         LKKEY SIN,TLCMICOM,RQRLCOM                                             
         LKKEY RNG,TLCMVER,VERRANGE                                             
         LKKEY LIT,TLCMSEQ,0                                                    
         LKKEY LIT,TLCMLEV,0                                                    
         LKKEY E                                                                
                                                                                
TCMKEYR  LKKEY H,TLCMKEY           TPC COMMENT RECORD                           
         LKKEY LIT,TLCMCD,TLCMCDQ                                               
         LKKEY LIT,TLCMSPR1,0                                                   
         LKKEY SIN,TLCMAGY,RQRLAGY                                              
         LKKEY LIT,TLCMTYP,TLCMTCOM                                             
         LKKEY SIN,TLCMCID,RQRLCID                                              
         LKKEY SIN,TLCMICOM,RQRLCOM                                             
         LKKEY RNG,TLCMVER,VERRANGE                                             
         LKKEY LIT,TLCMSEQ,0                                                    
         LKKEY LIT,TLCMLEV,TLCMTPC                                              
         LKKEY E                                                                
                                                                                
HCMKEYR  LKKEY H,TLHCKEY           HISTORY COMMENT RECORD                       
         LKKEY LIT,TLHCCD,TLHCCDQ                                               
         LKKEY LIT,TLHCSPR1,0                                                   
         LKKEY SIN,TLHCCOM,RQRLCOM                                              
         LKKEY LIT,TLHCSPR2,0                                                   
         LKKEY RNG,TLHCINV,ALLRANG6                                             
         LKKEY LIT,TLHCSPR3,0                                                   
         LKKEY RNG,TLHCSEQ,ALLRANG1                                             
         LKKEY LIT,TLHCSPR4,0                                                   
         LKKEY E                                                                
         EJECT                                                                  
***********************************************************************         
* EXIT CONDITIONS                                                     *         
***********************************************************************         
                                                                                
NOMORE   MVI   LP_RMODE,LP_RLAST   SET NO MORE RECORDS AND EXIT                 
YES      LHI   RE,1                                                             
         J     *+8                                                              
NO       LHI   RE,0                                                             
         CHI   RE,1                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR INITIAL DOWNLOAD 2                                  *         
***********************************************************************         
                                                                                
REQIN2   LKREQ H,I#IN2DLD,OUTIN2                                                
RqSt2    LKREQ F,D#ISTAF,(D,B#SAVED,RQI2STF),CHAR,TEXT=TA#STAFF,COL=*           
RqPw2    LKREQ F,D#IPASW,(D,B#SAVED,RQI2PSW),CHAR,TEXT=TA#STFPW,COL=*           
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR COMMERCIAL TYPE DOWNLOAD                            *         
***********************************************************************         
                                                                                
REQCTY   LKREQ H,I#CTYDLD,OUTCTY                                                
REQCTYX  DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR STAFF VALIDATION DOWNLOAD                           *         
***********************************************************************         
                                                                                
REQSTV   LKREQ H,I#STVDLD,OUTSTF                                                
RqUsr    LKREQ F,D#USRID,(D,B#SAVED,RQUSRID),CHAR,TEXT=TA#AGYCD,COL=*           
RqStf    LKREQ F,D#STFCD,(D,B#SAVED,RQSTFCD),CHAR,TEXT=TA#STAFF,COL=*           
RqSpw    LKREQ F,D#PSW,(D,B#SAVED,RQSTFPW),CHAR,TEXT=TA#STFPW,COL=*             
RqRSS    LKREQ F,D#RSTAT,(D,B#SAVED,RQSTRST),CHAR,TEXT=TA#RSSTA,COL=*           
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR STAFF DOWNLOAD                                      *         
***********************************************************************         
                                                                                
REQSTF   LKREQ H,I#STFDLD,OUTSTF                                                
RqUsr    LKREQ F,D#USRID,(D,B#SAVED,RQUSRID),CHAR,TEXT=TA#AGYCD,COL=*           
RqStf    LKREQ F,D#STFCD,(D,B#SAVED,RQSTFCD),CHAR,TEXT=TA#STAFF,COL=*           
RqRSS    LKREQ F,D#RSTAT,(D,B#SAVED,RQSTRST),CHAR,TEXT=TA#RSSTA,COL=*           
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR RELEASE                                             *         
***********************************************************************         
                                                                                
REQREL   LKREQ H,I#RELDLD,OUTREL                                                
RqMod    LKREQ F,D#RLMOD,(D,B#SAVED,RQRLMOD),UBIN,TEXT=TA#PMODE,COL=*           
RqStf    LKREQ F,D#RLSTF,(D,B#SAVED,RQRLSTF),CHAR,TEXT=TA#STAFF,COL=*           
RqAgy    LKREQ F,D#RLAGY,(D,B#SAVED,RQRLAGY),CHAR,TEXT=TA#AGYCD,COL=*           
RqCli    LKREQ F,D#RLCLI,(D,B#SAVED,RQRLCLI),CHAR,TEXT=TA#CLICD,COL=*           
RqPrd    LKREQ F,D#RLPRD,(D,B#SAVED,RQRLPRD),VSTR,TEXT=TA#PRDCD,COL=*           
RqCid    LKREQ F,D#RLCID,(D,B#SAVED,RQRLCID),CHAR,TEXT=TA#CIDCD,COL=*           
RqCom    LKREQ F,D#RLCOM,(D,B#SAVED,RQRLCOM),HEXD,TEXT=TA#COMCD,COL=*           
RqCyS    LKREQ F,D#RLCYS,(D,B#SAVED,RQRLCYS),PDAT,TEXT=TA#CYCST,COL=*           
RqCyE    LKREQ F,D#RLCYE,(D,B#SAVED,RQRLCYE),PDAT,TEXT=TA#CYCED,COL=*           
RqCst    LKREQ F,D#RLCST,(I,B#SAVED,RQRLCIN),HEXD,OLEN=2,LIST=F,       *        
               TEXT=TA#CSTSQ,COL=*                                              
RqEOv    LKREQ F,D#RLEOV,(I,B#SAVED,RQRLEIN),UBIN,OLEN=2,LIST=F,       *        
               TEXT=TA#EOVER,COL=*                                              
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR W4 SEARCH                                           *         
***********************************************************************         
                                                                                
REQW4S   LKREQ H,I#W4SDLD,OUTW4S                                                
RqStf    LKREQ F,D#W4SSTF,(D,B#SAVED,RQW4SSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqSSN    LKREQ F,D#W4SSSN,(I,B#SAVED,RQW4SSIN),CHAR,OLEN=9,LIST=F,     *        
               TEXT=TA#SSN,COL=*                                                
RqLNm    LKREQ F,D#W4SLNM,(D,B#SAVED,RQW4SLNM),CHAR,TEXT=TA#LNAME,COL=*         
RqFNm    LKREQ F,D#W4SFNM,(D,B#SAVED,RQW4SFNM),CHAR,TEXT=TA#FNAME,COL=*         
RqTyF    LKREQ F,D#W4SFTY,(I,B#SAVED,RQW4STIN),CHAR,OLEN=1,LIST=F,     *        
               TEXT=TA#W4TYP,COL=*                                              
RqCom    LKREQ F,D#W4SCOM,(I,B#SAVED,RQW4SCIN),HEXD,OLEN=4,LIST=F,     *        
               SORT=N,TEXT=TA#COMCD,COL=*                                       
RqSkA    LKREQ F,D#W4SSKA,(D,B#SAVED,RQW4SSKA),CHAR,TEXT=TA#SKACC,COL=*         
RqTrk    LKREQ F,D#W4STRK,(I,B#SAVED,RQW4SKIN),CHAR,OLEN=L'CURTRKFL,   *        
               LIST=F,SORT=N,TEXT=TA#ONTRK,COL=*                                
RqES9    LKREQ F,D#W4SES9,(D,B#SAVED,RQW4SES9),CHAR,TEXT=TA#EXS9,COL=*          
RqLim    LKREQ F,D#ANSLM,(D,B#SAVED,RQW4SLM),UBIN,TEXT=TA#LIMIT,COL=*           
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR AGENT SEARCH                                        *         
***********************************************************************         
                                                                                
REQANS   LKREQ H,I#ANSDLD,OUTANS                                                
RqStf    LKREQ F,D#ANSTAF,(D,B#SAVED,RQANSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqACd    LKREQ F,D#ANCOD,(I,B#SAVED,RQANSAIN),CHAR,OLEN=4,LIST=F,      *        
               TEXT=TA#ANCD,COL=*                                               
RqANm    LKREQ F,D#ANNAM,(D,B#SAVED,RQANSNAM),CHAR,TEXT=TA#ANNM,COL=*           
RQAOv    LKREQ F,D#ANAOV,(D,B#SAVED,RQANSAOV),CHAR,TEXT=TA#ALOV,COL=*           
RQSuS    LKREQ F,D#ANSUS,(D,B#SAVED,RQANSSUP),CHAR,TEXT=TA#SUSRC,COL=*          
RqLim    LKREQ F,D#ANSLM,(D,B#SAVED,RQANSLM),UBIN,TEXT=TA#LIMIT,COL=*           
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR CAST SEARCH                                         *         
***********************************************************************         
                                                                                
REQCAS   LKREQ H,I#CASDLD,OUTCAS                                                
RqStf    LKREQ F,D#CASSTF,(D,B#SAVED,RQCASSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqCom    LKREQ F,D#CASCOM,(I,B#SAVED,RQCASCIN),HEXD,OLEN=L'RQCASCOM,   *        
               LIST=F,SORT=N,TEXT=TA#COMCD,COL=*                                
RqAgy    LKREQ F,D#CASAGY,(D,B#SAVED,RQCASAGY),CHAR,TEXT=TA#AGYCD,COL=*         
RqCli    LKREQ F,D#CASCLI,(D,B#SAVED,RQCASCLI),CHAR,TEXT=TA#CLICD,COL=*         
RqPrd    LKREQ F,D#CASPRD,(D,B#SAVED,RQCASPRD),CHAR,TEXT=TA#PRDCD,COL=*         
RqCid    LKREQ F,D#CASCID,(D,B#SAVED,RQCASCID),CHAR,TEXT=TA#CIDCD,COL=*         
RqSeq    LKREQ F,D#CASSEQ,(D,B#SAVED,RQCASSEQ),HEXD,TEXT=TA#CSTSQ,COL=*         
RqSsn    LKREQ F,D#CASSSN,(D,B#SAVED,RQCASSSN),CHAR,TEXT=TA#SSN,COL=*           
RqLNm    LKREQ F,D#CASLNM,(D,B#SAVED,RQCASLNM),CHAR,TEXT=TA#LNAME,COL=*         
RqFNm    LKREQ F,D#CASFNM,(D,B#SAVED,RQCASFNM),CHAR,TEXT=TA#FNAME,COL=*         
RqOnO    LKREQ F,D#CASONO,(D,B#SAVED,RQCASONO),CHAR,TEXT=TA#ONO,COL=*           
RqUni    LKREQ F,D#CASUNI,(I,B#SAVED,RQCASUIN),CHAR,OLEN=3,LIST=F,     *        
               TEXT=TA#UNI,COL=*                                                
RqAgt    LKREQ F,D#CASAGT,(D,B#SAVED,RQCASAGT),CHAR,TEXT=TA#ANCD,COL=*          
RqGrt    LKREQ F,D#CASGUA,(D,B#SAVED,RQCASGUA),CHAR,TEXT=TA#GRTCD,COL=*         
RqTrk    LKREQ F,D#CASTRK,(I,B#SAVED,RQCASTIN),CHAR,OLEN=L'CURTRKFL,   *        
               LIST=F,SORT=N,TEXT=TA#ONTRK,COL=*                                
RqMst    LKREQ F,D#CASMST,(D,B#SAVED,RQCASMST),CHAR,TEXT=TA#MSTVR,COL=*         
RqLft    LKREQ F,D#CASLFT,(D,B#SAVED,RQCASLFT),CHAR,TEXT=TA#LFTVR,COL=*         
RqVer    LKREQ F,D#CASVER,(D,B#SAVED,RQCASVER),UBIN,TEXT=TA#ONVER,COL=*         
RqRel    LKREQ F,D#CASREL,(D,B#SAVED,RQCASREL),CHAR,TEXT=TA#RELSD,COL=*         
RqCoT    LKREQ F,D#CASCOT,(D,B#SAVED,RQCASCOT),CHAR,TEXT=TA#COTIT,COL=*         
RqROR    LKREQ F,D#CASORE,(D,B#SAVED,RQCASORE),CHAR,TEXT=TA#COREU,COL=*         
RqACo    LKREQ F,D#CASACO,(D,B#SAVED,RQCASACO),HEXD,TEXT=TA#ACCOM,COL=*         
RqW4T    LKREQ F,D#CASW4T,(I,B#SAVED,RQCASWIN),CHAR,OLEN=1,LIST=F,     *        
               TEXT=TA#W4TYP,COL=*                                              
RqLNI    LKREQ F,D#CASLNI,(D,B#SAVED,RQCASLNI),CHAR,TEXT=TA#LNSIN,COL=*         
RqFNI    LKREQ F,D#CASFNI,(D,B#SAVED,RQCASFNI),CHAR,TEXT=TA#FNSIN,COL=*         
RqCGr    LKREQ F,D#CASCGR,(D,B#SAVED,RQCASCGR),CHAR,TEXT=TA#CAGRP,COL=*         
RqRCN    LKREQ F,D#CASRCT,(D,B#SAVED,RQCASRCT),CHAR,TEXT=TA#RCOMT,COL=*         
RqCGA    LKREQ F,D#CASCGA,(D,B#SAVED,RQCASCGA),CHAR,TEXT=TA#CGRTA,COL=*         
RqCGC    LKREQ F,D#CASCGC,(D,B#SAVED,RQCASCGC),CHAR,TEXT=TA#CGRTC,COL=*         
RqEP6    LKREQ F,D#CASEP6,(D,B#SAVED,RQCASEP6),CHAR,TEXT=TA#EXP06,COL=*         
RqEMU    LKREQ F,D#CASEMU,(D,B#SAVED,RQCASEMU),CHAR,TEXT=TA#EXMUS,COL=*         
RqPad    LKREQ F,D#CASPAD,(D,B#SAVED,RQCASPAD),CHAR,TEXT=TA#REVPD,COL=*         
RqES9    LKREQ F,D#CASES9,(D,B#SAVED,RQCASES9),CHAR,TEXT=TA#EXS9,COL=*          
RqELA    LKREQ F,D#CASELA,(D,B#SAVED,RQCASELA),CHAR,TEXT=TA#EXLKA,COL=*         
RqLim    LKREQ F,D#CASLIM,(D,B#SAVED,RQCASLIM),UBIN,TEXT=TA#LIMIT,COL=*         
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR INVOICE SEARCH                                      *         
***********************************************************************         
                                                                                
REQINS   LKREQ H,I#INSDLD,OUTINS                                                
RqStf    LKREQ F,D#INSSTF,(D,B#SAVED,RQINSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqAgy    LKREQ F,D#INSAGY,(D,B#SAVED,RQINSAGY),CHAR,TEXT=TA#AGYCD,COL=*         
RqInv    LKREQ F,D#INSINV,(D,B#SAVED,RQINSINV),CHAR,TEXT=TA#INV,COL=*           
RqCom    LKREQ F,D#INSCOM,(D,B#SAVED,RQINSCOM),HEXD,TEXT=TA#COMCD,COL=*         
RqVer    LKREQ F,D#INSVER,(D,B#SAVED,RQINSVER),UBIN,TEXT=TA#VER,COL=*           
RqCnr    LKREQ F,D#INSCNR,(D,B#SAVED,RQINSCNR),CHAR,TEXT=TA#RECNR,COL=*         
RqCnl    LKREQ F,D#INSCNL,(D,B#SAVED,RQINSCNL),CHAR,TEXT=TA#RECNL,COL=*         
RqWid    LKREQ F,D#INSWID,(D,B#SAVED,RQINSWID),CHAR,TEXT=TA#WAPID,COL=*         
RqSeq    LKREQ F,D#INSSEQ,(D,B#SAVED,RQINSSEQ),HEXD,TEXT=TA#CSTSQ,COL=*         
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR FIXED CYCLE SEARCH                                  *         
***********************************************************************         
                                                                                
REQFTS   LKREQ H,I#FCSDLD,OUTFCS                                                
RqStf    LKREQ F,D#FCSSTF,(D,B#SAVED,RQFCSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqCom    LKREQ F,D#FCSCOM,(D,B#SAVED,RQFCSCOM),HEXD,TEXT=TA#COMCD,COL=*         
RqSeq    LKREQ F,D#FCSSEQ,(D,B#SAVED,RQFCSSEQ),HEXD,TEXT=TA#CSTSQ,COL=*         
RqCyS    LKREQ F,D#FCSCYS,(D,B#SAVED,RQFCSCYS),PDAT,TEXT=TA#CYCST,COL=*         
RqCyE    LKREQ F,D#FCSCYE,(D,B#SAVED,RQFCSCYE),PDAT,TEXT=TA#CYCED,COL=*         
RQIMC    LKREQ F,D#FCSIMC,(D,B#SAVED,RQFCSMIC),CHAR,TEXT=TA#SIMC,COL=*          
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR COMMENT SEARCH                                      *         
***********************************************************************         
                                                                                
REQCMS   LKREQ H,I#CMSDLD,OUTCMS                                                
RqStf    LKREQ F,D#CMSSTF,(D,B#SAVED,RQCMSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqLev    LKREQ F,D#CMSLEV,(D,B#SAVED,RQCMSLEV),CHAR,TEXT=TA#CMLEV,COL=*         
RqTyp    LKREQ F,D#CMSTYP,(D,B#SAVED,RQCMSTYP),CHAR,TEXT=TA#CMTYP,COL=*         
RqSSN    LKREQ F,D#CMSSSN,(D,B#SAVED,RQCMSSSN),CHAR,TEXT=TA#SSN,COL=*           
RqGua    LKREQ F,D#CMSGUA,(D,B#SAVED,RQCMSGUA),CHAR,TEXT=TA#GRTCD,COL=*         
RqCID    LKREQ F,D#CMSCID,(D,B#SAVED,RQCMSCID),CHAR,TEXT=TA#CIDCD,COL=*         
RqVer    LKREQ F,D#CMSVER,(D,B#SAVED,RQCMSVER),CHAR,TEXT=TA#VER,COL=*           
RqAgy    LKREQ F,D#CMSAGY,(D,B#SAVED,RQCMSAGY),CHAR,TEXT=TA#AGYCD,COL=*         
RqCon    LKREQ F,D#CMSCON,(D,B#SAVED,RQCMSCON),CHAR,TEXT=TA#CNTID,COL=*         
RqTSt    LKREQ F,D#CMSTST,(D,B#SAVED,RQCMSTST),PDAT,TEXT=TA#CNTST,COL=*         
RqTEn    LKREQ F,D#CMSTEN,(D,B#SAVED,RQCMSTEN),PDAT,TEXT=TA#CNTEN,COL=*         
RqInv    LKREQ F,D#CMSINV,(D,B#SAVED,RQCMSINV),CHAR,TEXT=TA#INV,COL=*           
RqAdv    LKREQ F,D#CMSADV,(D,B#SAVED,RQCMSADV),CHAR,TEXT=TA#DVNUM,COL=*         
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - INITIAL DOWNLOAD 2                                     *         
***********************************************************************         
                                                                                
OUTIN2   LKOUT H                                                                
                                                                                
NAMREC   LKOUT R,E#STAFF                    ** NAME VALUES **                   
Array    LKOUT C,E#STAFF,(A,ARYNAM)                                             
         LKOUT E                                                                
                                                                                
COTREC   LKOUT R,E#COMT                     ** COMM'L TYPE VALUES **            
Array    LKOUT C,E#COMT,(A,ARYCOT)                                              
         LKOUT E                                                                
                                                                                
ALMREC   LKOUT R,E#ALIM                     ** LIMITED AGENCY VALUES **         
Array    LKOUT C,E#ALIM,(A,ARYALM)                                              
         LKOUT E                                                                
                                                                                
CLMREC   LKOUT R,E#CLIM                     ** LIMITED CLIENT VALUES **         
Array    LKOUT C,E#CLIM,(A,ARYCLM)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - COMMERCIAL TYPE DOWNLOAD                               *         
***********************************************************************         
                                                                                
OUTCTY   LKOUT H                                                                
                                                                                
COYREC   LKOUT R,E#COMT                     ** COMM'L TYPE VALUES **            
Array    LKOUT C,E#COMT,(A,ARYCOT)                                              
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - STAFF DOWNLOAD                                         *         
***********************************************************************         
                                                                                
OUTSTF   LKOUT H                                                                
                                                                                
STSREC   LKOUT R,O#STFSTA          ** STAFF STATUS **                           
Array    LKOUT C,O#STFVAL,(A,ARYSTS)                                            
         LKOUT E                                                                
                                                                                
STFREC   LKOUT R,O#STFVAL          ** STAFF VALUES **                           
Array    LKOUT C,O#STFVAL,(A,ARYSTF)                                            
         LKOUT E                                                                
                                                                                
LIMREC   LKOUT R,O#LIMVAL          ** LIMIT ACCESS VALUES **                    
Array    LKOUT C,O#LIMVAL,(A,ARYLIM)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - RELEASE RECORD DOWNLOAD                                *         
***********************************************************************         
                                                                                
OUTREL   LKOUT H                                                                
                                                                                
RELREC   LKOUT R,O#RELSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#RELSTA,(A,ARYREL)                                            
         LKOUT E                                                                
                                                                                
ERRREC   LKOUT R,O#RELERR                   ** ERROR VALUES **                  
Array    LKOUT C,O#RELERR,(A,ARYERR)                                            
         LKOUT E                                                                
                                                                                
CMTREC   LKOUT R,O#RELCMT                   ** COMMENT VALUES **                
Array    LKOUT C,O#RELCMT,(A,ARYCMT)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - W4 SEARCH DOWNLOAD                                     *         
***********************************************************************         
                                                                                
OUTW4S   LKOUT H                                                                
                                                                                
W4SREC   LKOUT R,O#W4SSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#W4SSTA,(A,ARYW4S)                                            
         LKOUT E                                                                
                                                                                
W4DREC   LKOUT R,O#W4SDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#W4SDET,(A,ARYW4D)                                            
         LKOUT E                                                                
                                                                                
W4XREC   LKOUT R,O#W4XDET                   ** EXP. DETAILS VALUES **           
Array    LKOUT C,O#W4XDET,(A,ARYW4X)                                            
         LKOUT E                                                                
                                                                                
W4LREC   LKOUT R,O#W4SLIM                   ** LIMIT VALUES **                  
Array    LKOUT C,O#W4SLIM,(A,ARYW4L)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - AGENT SEARCH DOWNLOAD                                  *         
***********************************************************************         
                                                                                
OUTANS   LKOUT H                                                                
                                                                                
ANSREC   LKOUT R,O#ANSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#ANSSTA,(A,ARYANS)                                            
         LKOUT E                                                                
                                                                                
ANDREC   LKOUT R,O#ANSDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#ANSDET,(A,ARYANX)                                            
         LKOUT E                                                                
                                                                                
ANLREC   LKOUT R,O#ANSLIM                   ** LIMIT VALUES **                  
Array    LKOUT C,O#ANSLIM,(A,ARYANL)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - CAST SEARCH DOWNLOAD                                   *         
***********************************************************************         
                                                                                
OUTCAS   LKOUT H                                                                
                                                                                
CASREC   LKOUT R,O#CASSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#CASSTA,(A,ARYCAS)                                            
         LKOUT E                                                                
                                                                                
CAXREC   LKOUT R,O#CAXDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#CAXDET,(A,ARYCAX)                                            
         LKOUT E                                                                
                                                                                
CALREC   LKOUT R,O#CASLIM                   ** LIMIT VALUES **                  
Array    LKOUT C,O#CASLIM,(A,ARYCAL)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - INVOICE SEARCH DOWNLOAD                                *         
***********************************************************************         
                                                                                
OUTINS   LKOUT H                                                                
                                                                                
INSREC   LKOUT R,O#INSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#INSSTA,(A,ARYINS)                                            
         LKOUT E                                                                
                                                                                
INXREC   LKOUT R,O#INXDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#INXDET,(A,ARYINX)                                            
         LKOUT E                                                                
                                                                                
INLREC   LKOUT R,O#INSPLT                   ** INVOICE SPLIT VALUES **          
Array    LKOUT C,O#INSPLT,(A,ARYINL)                                            
         LKOUT E                                                                
                                                                                
INMREC   LKOUT R,O#INNSMD                   ** NTWK/SYS/MKT VALUES **           
Array    LKOUT C,O#INNSMD,(A,ARYINM)                                            
         LKOUT E                                                                
                                                                                
INPREC   LKOUT R,O#INPROG                   ** PROGRAM VALUES **                
Array    LKOUT C,O#INPROG,(A,ARYINP)                                            
         LKOUT E                                                                
                                                                                
INAREC   LKOUT R,O#INAUCI                   ** US/CAN INVOICE VALUES **         
Array    LKOUT C,O#INAUCI,(A,ARYINA)                                            
         LKOUT E                                                                
                                                                                
INTREC   LKOUT R,O#INTCMT                   ** TPC COMMENT VALUES **            
Array    LKOUT C,O#INTCMT,(A,ARYINC)                                            
         LKOUT E                                                                
                                                                                
INCREC   LKOUT R,O#INCCMT                   ** CLIENT COMMENT VALUES **         
Array    LKOUT C,O#INCCMT,(A,ARYINC)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - FIXED CYCLE SEARCH DOWNLOAD                            *         
***********************************************************************         
                                                                                
OUTFCS   LKOUT H                                                                
                                                                                
FCSREC   LKOUT R,O#FCSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#FCSSTA,(A,ARYFCS)                                            
         LKOUT E                                                                
                                                                                
FCXREC   LKOUT R,O#FCXDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#FCXDET,(A,ARYFCX)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - COMMENT SEARCH DOWNLOAD                                *         
***********************************************************************         
                                                                                
OUTCMS   LKOUT H                                                                
                                                                                
CMSREC   LKOUT R,O#CMSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#CMSSTA,(A,ARYCMS)                                            
         LKOUT E                                                                
                                                                                
CMXREC   LKOUT R,O#CMXDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#CMXDET,(A,ARYCMX)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR STAFF NAME RECORDS                             *         
***********************************************************************         
                                                                                
ARYNAM   LKOUT A,(R,NXTNAM),MULTIROW=Y,ROWNAME=NAMVALS                          
                                                                                
StNme    LKOUT C,3,(D,,NAMNAME),CHAR,ND=Y                                       
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL TYPE RECORDS                        *         
***********************************************************************         
                                                                                
ARYCOT   LKOUT A,(R,NXTCOT),MULTIROW=Y,ROWNAME=COTVALS                          
                                                                                
CoTyC    LKOUT C,9,(D,,COTCODE),CHAR,ND=Y                                       
CoTyD    LKOUT C,10,(D,,COTDESC),CHAR,ND=Y                                      
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR LIMITED AGENCY RECORDS                         *         
***********************************************************************         
                                                                                
ARYALM   LKOUT A,(R,NXTALM),MULTIROW=Y,ROWNAME=ALMVALS                          
                                                                                
LmAgy    LKOUT C,1,(D,,ALMAGY),CHAR,ND=Y                                        
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR LIMITED CLIENT RECORDS                         *         
***********************************************************************         
                                                                                
ARYCLM   LKOUT A,(R,NXTCLM),MULTIROW=Y,ROWNAME=CLMVALS                          
                                                                                
LmCli    LKOUT C,1,(D,,CLMCLI),CHAR,ND=Y                                        
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR STAFF SEARCH STATUS RECORDS                    *         
***********************************************************************         
                                                                                
ARYSTS   LKOUT A,(R,NXTSTS),ROWNAME=STSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,STSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR STAFF RECORDS                                  *         
***********************************************************************         
                                                                                
ARYSTF   LKOUT A,(R,NXTSTF),MULTIROW=Y,ROWNAME=STFVALS                          
                                                                                
RtUsr    LKOUT C,1,(D,,STAFFUID),CHAR,ND=Y                                      
RtStf    LKOUT C,2,(D,,STAFFCD),CHAR,ND=Y                                       
FsNme    LKOUT C,3,(D,,STAFFFN),CHAR,ND=Y                                       
LsNme    LKOUT C,4,(D,,STAFFLN),CHAR,ND=Y                                       
StTyp    LKOUT C,5,(D,,STAFFTP),CHAR,ND=Y                                       
StEml    LKOUT C,6,(D,,STAFFEML),CHAR,ND=Y                                      
StSt1    LKOUT C,7,(D,,STAFFST1),CHAR,ND=Y                                      
StSt2    LKOUT C,8,(D,,STAFFST2),CHAR,ND=Y                                      
StSt3    LKOUT C,9,(D,,STAFFST3),CHAR,ND=Y                                      
StSt4    LKOUT C,10,(D,,STAFFST4),CHAR,ND=Y                                     
StSt5    LKOUT C,11,(D,,STAFFST5),CHAR,ND=Y                                     
StSt6    LKOUT C,12,(D,,STAFFST6),CHAR,ND=Y                                     
StSt7    LKOUT C,13,(D,,STAFFST7),CHAR,ND=Y                                     
StSt8    LKOUT C,14,(D,,STAFFST8),CHAR,ND=Y                                     
StPho    LKOUT C,15,(D,,STAFFPHO),CHAR,ND=Y                                     
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR LIMITING AGENCY/CLIENTS                        *         
***********************************************************************         
                                                                                
ARYLIM   LKOUT A,(R,NXTLIM),MULTIROW=Y,ROWNAME=LIMVALS                          
                                                                                
LimAg    LKOUT C,1,(D,,LIMAGY),CHAR                                             
Array    LKOUT C,255,(A,ARYCLI)                                                 
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR AGENCIES AND CLIENTS                           *         
***********************************************************************         
                                                                                
ARYCLI   LKOUT A,(D,B#SAVED,LIMENT),NROWS=(B#SAVED,LIMNUM),            *        
               ROWWIDTH=L'LIMENT                                                
                                                                                
LimCl    LKOUT C,2,(D,,LIMCLI),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR RELEASE RECORDS                                *         
***********************************************************************         
                                                                                
ARYREL   LKOUT A,(R,NXTREL),ROWNAME=RELVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,RELSTAT),UBIN,ND=Y                                       
RtStf    LKOUT C,2,(D,,RELSTAF),CHAR,ND=Y                                       
RtDat    LKOUT C,3,(D,,RELDATE),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR ERROR MESSAGE RECORDS                          *         
***********************************************************************         
                                                                                
ARYERR   LKOUT A,(R,NXTERR),MULTIROW=Y,ROWNAME=ERRVALS                          
                                                                                
RtNum    LKOUT C,1,(D,,ERRNUMB),UBIN,ND=Y                                       
RtCat    LKOUT C,2,(D,,ERRCATY),UBIN,ND=Y                                       
RtFld    LKOUT C,3,(D,,ERRFILD),UBIN,ND=Y                                       
RtErM    LKOUT C,4,(D,,ERREMSG),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR COMMENT RECORDS                                *         
***********************************************************************         
                                                                                
ARYCMT   LKOUT A,(R,NXTCMT),MULTIROW=Y,ROWNAME=CMTVALS                          
                                                                                
RtCty    LKOUT C,1,(D,,CMTTYPE),UBIN,ND=Y                                       
RtCmt    LKOUT C,2,(D,,CMTCOMT),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR W4 SEARCH STATUS RECORDS                       *         
***********************************************************************         
                                                                                
ARYW4S   LKOUT A,(R,NXTW4S),ROWNAME=W4SVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,W4SSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR W4 SEARCH DETAILS RECORDS                      *         
***********************************************************************         
                                                                                
ARYW4D   LKOUT A,(R,NXTW4D),MULTIROW=Y,ROWNAME=W4XVALS                          
                                                                                
RtSsn    LKOUT C,4,(D,,W4XSSN),CHAR,ND=Y                                        
RtPid    LKOUT C,5,(D,,W4XPID),CHAR,ND=Y                                        
RtLNm    LKOUT C,6,(D,,W4XLNM),CHAR,ND=Y                                        
RtFNm    LKOUT C,7,(D,,W4XFNM),CHAR,ND=Y                                        
RtMNm    LKOUT C,8,(D,,W4XMNM),CHAR,ND=Y                                        
RtTyp    LKOUT C,9,(D,,W4XTYP),CHAR,ND=Y                                        
RtAd1    LKOUT C,10,(D,,W4XAD1),CHAR,ND=Y                                       
RtAd2    LKOUT C,11,(D,,W4XAD2),CHAR,ND=Y                                       
RtAd3    LKOUT C,12,(D,,W4XAD3),CHAR,ND=Y                                       
RtCty    LKOUT C,13,(D,,W4XCTY),CHAR,ND=Y                                       
RtSta    LKOUT C,14,(D,,W4XSTA),CHAR,ND=Y                                       
RtZip    LKOUT C,15,(D,,W4XZIP),CHAR,ND=Y                                       
RtSuf    LKOUT C,16,(D,,W4XSUF),CHAR,ND=Y                                       
RtAkF    LKOUT C,18,(D,,W4XAKF),CHAR,ND=Y                                       
RtAkL    LKOUT C,19,(D,,W4XAKL),CHAR,ND=Y                                       
RtCry    LKOUT C,71,(D,,W4XCRY),CHAR,ND=Y                                       
RtFTx    LKOUT C,73,(D,,W4XFTX),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR W4 EXPANDED DETAILS RECORDS                    *         
***********************************************************************         
                                                                                
ARYW4X   LKOUT A,(R,NXTW4X),MULTIROW=Y,ROWNAME=W4XVALS                          
                                                                                
RtSsn    LKOUT C,4,(D,,W4XSSN),CHAR,ND=Y                                        
RtPid    LKOUT C,5,(D,,W4XPID),CHAR,ND=Y                                        
RtLNm    LKOUT C,6,(D,,W4XLNM),CHAR,ND=Y                                        
RtFNm    LKOUT C,7,(D,,W4XFNM),CHAR,ND=Y                                        
RtMNm    LKOUT C,8,(D,,W4XMNM),CHAR,ND=Y                                        
RtTyp    LKOUT C,9,(D,,W4XTYP),CHAR,ND=Y                                        
RtAd1    LKOUT C,10,(D,,W4XAD1),CHAR,ND=Y                                       
RtAd2    LKOUT C,11,(D,,W4XAD2),CHAR,ND=Y                                       
RtAd3    LKOUT C,12,(D,,W4XAD3),CHAR,ND=Y                                       
RtCty    LKOUT C,13,(D,,W4XCTY),CHAR,ND=Y                                       
RtSta    LKOUT C,14,(D,,W4XSTA),CHAR,ND=Y                                       
RtZip    LKOUT C,15,(D,,W4XZIP),CHAR,ND=Y                                       
RtSuf    LKOUT C,16,(D,,W4XSUF),CHAR,ND=Y                                       
RtPho    LKOUT C,17,(D,,W4XPHO),CHAR,ND=Y                                       
RtAkF    LKOUT C,18,(D,,W4XAKF),CHAR,ND=Y                                       
RtAkL    LKOUT C,19,(D,,W4XAKL),CHAR,ND=Y                                       
RtCpn    LKOUT C,20,(D,,W4XCPN),CHAR,ND=Y                                       
RtSxc    LKOUT C,21,(D,,W4XSXC),CHAR,ND=Y                                       
RtEth    LKOUT C,22,(D,,W4XETH),CHAR,ND=Y                                       
RtFil    LKOUT C,23,(D,,W4XFIL),CHAR,ND=Y                                       
RtTFq    LKOUT C,24,(D,,W4XTFQ),CHAR,ND=Y                                       
RtCp1    LKOUT C,25,(D,,W4XCP1),CHAR,ND=Y                                       
RtCp2    LKOUT C,26,(D,,W4XCP2),CHAR,ND=Y                                       
RtCp3    LKOUT C,27,(D,,W4XCP3),CHAR,ND=Y                                       
RtCp4    LKOUT C,28,(D,,W4XCP4),CHAR,ND=Y                                       
RtCp5    LKOUT C,29,(D,,W4XCP5),CHAR,ND=Y                                       
RtCp6    LKOUT C,30,(D,,W4XCP6),CHAR,ND=Y                                       
RtIDt    LKOUT C,31,(D,,W4XIDT),CHAR,ND=Y                                       
RtAFM    LKOUT C,32,(D,,W4XAFM),CHAR,ND=Y                                       
RtMNu    LKOUT C,33,(D,,W4XMNU),CHAR,ND=Y                                       
RtYTD    LKOUT C,34,(D,,W4XYTD),CHAR,ND=Y                                       
RtCkF    LKOUT C,35,(D,,W4XCKF),CHAR,ND=Y                                       
RtILF    LKOUT C,36,(D,,W4XILF),CHAR,ND=Y                                       
RtLck    LKOUT C,37,(D,,W4XLCK),CHAR,ND=Y                                       
RtFIW    LKOUT C,38,(D,,W4XFIW),CHAR,ND=Y                                       
RtDCW    LKOUT C,39,(D,,W4XDCW),CHAR,ND=Y                                       
RtPen    LKOUT C,40,(D,,W4XPEN),CHAR,ND=Y                                       
RtFAd    LKOUT C,41,(D,,W4XFAD),CHAR,ND=Y                                       
RtDir    LKOUT C,42,(D,,W4XDIR),CHAR,ND=Y                                       
RtWir    LKOUT C,43,(D,,W4XWIR),CHAR,ND=Y                                       
RtSpL    LKOUT C,44,(D,,W4XSPL),CHAR,ND=Y                                       
RtFMS    LKOUT C,45,(D,,W4XFMS),CHAR,ND=Y                                       
RtFEx    LKOUT C,46,(D,,W4XFEX),CHAR,ND=Y                                       
RtFFx    LKOUT C,47,(D,,W4XFFX),UBIN,ND=Y                                       
RtSAr    LKOUT C,48,(D,,W4XSAR),CHAR,ND=Y                                       
RtSMS    LKOUT C,49,(D,,W4XSMS),CHAR,ND=Y                                       
RtSEx    LKOUT C,50,(D,,W4XSEX),CHAR,ND=Y                                       
RtSFx    LKOUT C,51,(D,,W4XSFX),UBIN,ND=Y                                       
RtCAr    LKOUT C,52,(D,,W4XCAR),CHAR,ND=Y                                       
RtCMS    LKOUT C,53,(D,,W4XCMS),CHAR,ND=Y                                       
RtCEx    LKOUT C,54,(D,,W4XCEX),CHAR,ND=Y                                       
RtCFx    LKOUT C,55,(D,,W4XCFX),UBIN,ND=Y                                       
RtErn    LKOUT C,56,(D,,W4XERN),UBIN,ND=Y                                       
RtRSt    LKOUT C,57,(D,,W4XRST),CHAR,ND=Y                                       
RtDOB    LKOUT C,58,(D,,W4XDOB),CHAR,ND=Y                                       
RtDed    LKOUT C,59,(D,,W4XDED),UBIN,ND=Y                                       
RtTrs    LKOUT C,60,(D,,W4XTRS),CHAR,ND=Y                                       
RtMPR    LKOUT C,61,(D,,W4XMPR),UBIN,ND=Y                                       
RtPCh    LKOUT C,62,(D,,W4XPCH),UBIN,ND=Y                                       
RtGST    LKOUT C,63,(D,,W4XGST),CHAR,ND=Y                                       
RtCmt    LKOUT C,64,(D,,W4XCMT),CHAR,ND=Y                                       
RtPNm    LKOUT C,65,(D,,W4XPNM),CHAR,ND=Y                                       
RtPA1    LKOUT C,66,(D,,W4XPA1),CHAR,ND=Y                                       
RtPA2    LKOUT C,67,(D,,W4XPA2),CHAR,ND=Y                                       
RtPA3    LKOUT C,68,(D,,W4XPA3),CHAR,ND=Y                                       
RtPA4    LKOUT C,69,(D,,W4XPA4),CHAR,ND=Y                                       
RtPEx    LKOUT C,70,(D,,W4XPEX),CHAR,ND=Y                                       
RtCry    LKOUT C,71,(D,,W4XCRY),CHAR,ND=Y                                       
RtEml    LKOUT C,72,(D,,W4XEML),CHAR,ND=Y                                       
RtFTx    LKOUT C,73,(D,,W4XFTX),CHAR,ND=Y                                       
RtPCy    LKOUT C,74,(D,,W4XPCY),CHAR,ND=Y                                       
RtPSt    LKOUT C,75,(D,,W4XPST),CHAR,ND=Y                                       
RtPZp    LKOUT C,76,(D,,W4XPZP),CHAR,ND=Y                                       
RtPCy    LKOUT C,77,(D,,W4XPCT),CHAR,ND=Y                                       
RtPAc    LKOUT C,78,(D,,W4XPAC),CHAR,ND=Y                                       
RtWid    LKOUT C,79,(D,,W4XWID),CHAR,ND=Y                                       
RtAcD    LKOUT C,80,(D,,W4XACD),CHAR,ND=Y                                       
RtNHA    LKOUT C,81,(D,,W4XNHA),CHAR,ND=Y                                       
RtNHP    LKOUT C,82,(D,,W4XNHP),CHAR,ND=Y                                       
RtNHD    LKOUT C,83,(D,,W4XNHD),CHAR,ND=Y                                       
RtEFT    LKOUT C,85,(D,,W4XEFT),CHAR,ND=Y                                       
RtReg    LKOUT C,86,(D,,W4XREG),CHAR,ND=Y                                       
RtTCP    LKOUT C,87,(D,,W4XTCP),CHAR,ND=Y                                       
RtFNC    LKOUT C,88,(D,,W4XFNC),UBIN,ND=Y                                       
RtFD1    LKOUT C,89,(D,,W4XFD1),CHAR,ND=Y                                       
RtFEt    LKOUT C,90,(D,,W4XFET),CHAR,ND=Y                                       
RtFPZ    LKOUT C,91,(D,,W4XFPZ),UBIN,ND=Y                                       
RtPNC    LKOUT C,92,(D,,W4XPNC),UBIN,ND=Y                                       
RtPD1    LKOUT C,93,(D,,W4XPD1),CHAR,ND=Y                                       
RtPEt    LKOUT C,94,(D,,W4XPET),CHAR,ND=Y                                       
RtPPZ    LKOUT C,95,(D,,W4XPPZ),UBIN,ND=Y                                       
RtPHD    LKOUT C,96,(D,,W4XPHD),UBIN,ND=Y                                       
RtPSP    LKOUT C,97,(D,,W4XPSP),UBIN,ND=Y                                       
RtPHC    LKOUT C,98,(D,,W4XPHC),CHAR,ND=Y                                       
RtFEC    LKOUT C,99,(D,,W4XFEC),CHAR,ND=Y                                       
Array    LKOUT C,O#W4CDET,(A,ARYW4CD)                                           
Array    LKOUT C,O#W4TDET,(A,ARYW4TD)                                           
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR W4 CORPORATION DETAILS RECORDS                 *         
***********************************************************************         
                                                                                
ARYW4CD  LKOUT A,(R,NXTW4C),MULTIROW=Y,ROWNAME=W4XVALS                          
                                                                                
RtSsn    LKOUT C,4,(D,,W4XSSN),CHAR,ND=Y                                        
RtPid    LKOUT C,5,(D,,W4XPID),CHAR,ND=Y                                        
RtLNm    LKOUT C,6,(D,,W4XLNM),CHAR,ND=Y                                        
RtFNm    LKOUT C,7,(D,,W4XFNM),CHAR,ND=Y                                        
RtMNm    LKOUT C,8,(D,,W4XMNM),CHAR,ND=Y                                        
RtTyp    LKOUT C,9,(D,,W4XTYP),CHAR,ND=Y                                        
RtAd1    LKOUT C,10,(D,,W4XAD1),CHAR,ND=Y                                       
RtAd2    LKOUT C,11,(D,,W4XAD2),CHAR,ND=Y                                       
RtAd3    LKOUT C,12,(D,,W4XAD3),CHAR,ND=Y                                       
RtCty    LKOUT C,13,(D,,W4XCTY),CHAR,ND=Y                                       
RtSta    LKOUT C,14,(D,,W4XSTA),CHAR,ND=Y                                       
RtZip    LKOUT C,15,(D,,W4XZIP),CHAR,ND=Y                                       
RtSuf    LKOUT C,16,(D,,W4XSUF),CHAR,ND=Y                                       
RtPho    LKOUT C,17,(D,,W4XPHO),CHAR,ND=Y                                       
RtAkF    LKOUT C,18,(D,,W4XAKF),CHAR,ND=Y                                       
RtAkL    LKOUT C,19,(D,,W4XAKL),CHAR,ND=Y                                       
RtCpn    LKOUT C,20,(D,,W4XCPN),CHAR,ND=Y                                       
RtSxc    LKOUT C,21,(D,,W4XSXC),CHAR,ND=Y                                       
RtEth    LKOUT C,22,(D,,W4XETH),CHAR,ND=Y                                       
RtFil    LKOUT C,23,(D,,W4XFIL),CHAR,ND=Y                                       
RtTFq    LKOUT C,24,(D,,W4XTFQ),CHAR,ND=Y                                       
RtCp1    LKOUT C,25,(D,,W4XCP1),CHAR,ND=Y                                       
RtCp2    LKOUT C,26,(D,,W4XCP2),CHAR,ND=Y                                       
RtCp3    LKOUT C,27,(D,,W4XCP3),CHAR,ND=Y                                       
RtCp4    LKOUT C,28,(D,,W4XCP4),CHAR,ND=Y                                       
RtCp5    LKOUT C,29,(D,,W4XCP5),CHAR,ND=Y                                       
RtCp6    LKOUT C,30,(D,,W4XCP6),CHAR,ND=Y                                       
RtIDt    LKOUT C,31,(D,,W4XIDT),CHAR,ND=Y                                       
RtAFM    LKOUT C,32,(D,,W4XAFM),CHAR,ND=Y                                       
RtMNu    LKOUT C,33,(D,,W4XMNU),CHAR,ND=Y                                       
RtYTD    LKOUT C,34,(D,,W4XYTD),CHAR,ND=Y                                       
RtCkF    LKOUT C,35,(D,,W4XCKF),CHAR,ND=Y                                       
RtILF    LKOUT C,36,(D,,W4XILF),CHAR,ND=Y                                       
RtLck    LKOUT C,37,(D,,W4XLCK),CHAR,ND=Y                                       
RtFIW    LKOUT C,38,(D,,W4XFIW),CHAR,ND=Y                                       
RtDCW    LKOUT C,39,(D,,W4XDCW),CHAR,ND=Y                                       
RtPen    LKOUT C,40,(D,,W4XPEN),CHAR,ND=Y                                       
RtFAd    LKOUT C,41,(D,,W4XFAD),CHAR,ND=Y                                       
RtDir    LKOUT C,42,(D,,W4XDIR),CHAR,ND=Y                                       
RtWir    LKOUT C,43,(D,,W4XWIR),CHAR,ND=Y                                       
RtSpL    LKOUT C,44,(D,,W4XSPL),CHAR,ND=Y                                       
RtFMS    LKOUT C,45,(D,,W4XFMS),CHAR,ND=Y                                       
RtFEx    LKOUT C,46,(D,,W4XFEX),UBIN,ND=Y                                       
RtFFx    LKOUT C,47,(D,,W4XFFX),UBIN,ND=Y                                       
RtSAr    LKOUT C,48,(D,,W4XSAR),CHAR,ND=Y                                       
RtSMS    LKOUT C,49,(D,,W4XSMS),CHAR,ND=Y                                       
RtSEx    LKOUT C,50,(D,,W4XSEX),UBIN,ND=Y                                       
RtSFx    LKOUT C,51,(D,,W4XSFX),UBIN,ND=Y                                       
RtCAr    LKOUT C,52,(D,,W4XCAR),CHAR,ND=Y                                       
RtCMS    LKOUT C,53,(D,,W4XCMS),CHAR,ND=Y                                       
RtCEx    LKOUT C,54,(D,,W4XCEX),UBIN,ND=Y                                       
RtCFx    LKOUT C,55,(D,,W4XCFX),UBIN,ND=Y                                       
RtErn    LKOUT C,56,(D,,W4XERN),UBIN,ND=Y                                       
RtRSt    LKOUT C,57,(D,,W4XRST),CHAR,ND=Y                                       
RtDOB    LKOUT C,58,(D,,W4XDOB),CHAR,ND=Y                                       
RtDed    LKOUT C,59,(D,,W4XDED),UBIN,ND=Y                                       
RtTrs    LKOUT C,60,(D,,W4XTRS),CHAR,ND=Y                                       
RtMPR    LKOUT C,61,(D,,W4XMPR),UBIN,ND=Y                                       
RtPCh    LKOUT C,62,(D,,W4XPCH),UBIN,ND=Y                                       
RtGST    LKOUT C,63,(D,,W4XGST),CHAR,ND=Y                                       
RtCmt    LKOUT C,64,(D,,W4XCMT),CHAR,ND=Y                                       
RtPNm    LKOUT C,65,(D,,W4XPNM),CHAR,ND=Y                                       
RtPA1    LKOUT C,66,(D,,W4XPA1),CHAR,ND=Y                                       
RtPA2    LKOUT C,67,(D,,W4XPA2),CHAR,ND=Y                                       
RtPA3    LKOUT C,68,(D,,W4XPA3),CHAR,ND=Y                                       
RtPA4    LKOUT C,69,(D,,W4XPA4),CHAR,ND=Y                                       
RtPEx    LKOUT C,70,(D,,W4XPEX),CHAR,ND=Y                                       
RtCry    LKOUT C,71,(D,,W4XCRY),CHAR,ND=Y                                       
RtEml    LKOUT C,72,(D,,W4XEML),CHAR,ND=Y                                       
RtPCy    LKOUT C,74,(D,,W4XPCY),CHAR,ND=Y                                       
RtPSt    LKOUT C,75,(D,,W4XPST),CHAR,ND=Y                                       
RtPZp    LKOUT C,76,(D,,W4XPZP),CHAR,ND=Y                                       
RtPCy    LKOUT C,77,(D,,W4XPCT),CHAR,ND=Y                                       
RtPAc    LKOUT C,78,(D,,W4XPAC),CHAR,ND=Y                                       
RtWid    LKOUT C,79,(D,,W4XWID),CHAR,ND=Y                                       
RtAcD    LKOUT C,80,(D,,W4XACD),CHAR,ND=Y                                       
RtEFT    LKOUT C,85,(D,,W4XEFT),CHAR,ND=Y                                       
RtReg    LKOUT C,86,(D,,W4XREG),CHAR,ND=Y                                       
RtTCP    LKOUT C,87,(D,,W4XTCP),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR W4 TRUSTEE DETAILS RECORDS                     *         
***********************************************************************         
                                                                                
ARYW4TD  LKOUT A,(R,NXTW4T),MULTIROW=Y,ROWNAME=W4XVALS                          
                                                                                
RtSsn    LKOUT C,4,(D,,W4XSSN),CHAR,ND=Y                                        
RtPid    LKOUT C,5,(D,,W4XPID),CHAR,ND=Y                                        
RtLNm    LKOUT C,6,(D,,W4XLNM),CHAR,ND=Y                                        
RtFNm    LKOUT C,7,(D,,W4XFNM),CHAR,ND=Y                                        
RtMNm    LKOUT C,8,(D,,W4XMNM),CHAR,ND=Y                                        
RtTyp    LKOUT C,9,(D,,W4XTYP),CHAR,ND=Y                                        
RtAd1    LKOUT C,10,(D,,W4XAD1),CHAR,ND=Y                                       
RtAd2    LKOUT C,11,(D,,W4XAD2),CHAR,ND=Y                                       
RtAd3    LKOUT C,12,(D,,W4XAD3),CHAR,ND=Y                                       
RtCty    LKOUT C,13,(D,,W4XCTY),CHAR,ND=Y                                       
RtSta    LKOUT C,14,(D,,W4XSTA),CHAR,ND=Y                                       
RtZip    LKOUT C,15,(D,,W4XZIP),CHAR,ND=Y                                       
RtSuf    LKOUT C,16,(D,,W4XSUF),CHAR,ND=Y                                       
RtPho    LKOUT C,17,(D,,W4XPHO),CHAR,ND=Y                                       
RtAkF    LKOUT C,18,(D,,W4XAKF),CHAR,ND=Y                                       
RtAkL    LKOUT C,19,(D,,W4XAKL),CHAR,ND=Y                                       
RtCpn    LKOUT C,20,(D,,W4XCPN),CHAR,ND=Y                                       
RtSxc    LKOUT C,21,(D,,W4XSXC),CHAR,ND=Y                                       
RtEth    LKOUT C,22,(D,,W4XETH),CHAR,ND=Y                                       
RtFil    LKOUT C,23,(D,,W4XFIL),CHAR,ND=Y                                       
RtTFq    LKOUT C,24,(D,,W4XTFQ),CHAR,ND=Y                                       
RtCp1    LKOUT C,25,(D,,W4XCP1),CHAR,ND=Y                                       
RtCp2    LKOUT C,26,(D,,W4XCP2),CHAR,ND=Y                                       
RtCp3    LKOUT C,27,(D,,W4XCP3),CHAR,ND=Y                                       
RtCp4    LKOUT C,28,(D,,W4XCP4),CHAR,ND=Y                                       
RtCp5    LKOUT C,29,(D,,W4XCP5),CHAR,ND=Y                                       
RtCp6    LKOUT C,30,(D,,W4XCP6),CHAR,ND=Y                                       
RtIDt    LKOUT C,31,(D,,W4XIDT),CHAR,ND=Y                                       
RtAFM    LKOUT C,32,(D,,W4XAFM),CHAR,ND=Y                                       
RtMNu    LKOUT C,33,(D,,W4XMNU),CHAR,ND=Y                                       
RtYTD    LKOUT C,34,(D,,W4XYTD),CHAR,ND=Y                                       
RtCkF    LKOUT C,35,(D,,W4XCKF),CHAR,ND=Y                                       
RtILF    LKOUT C,36,(D,,W4XILF),CHAR,ND=Y                                       
RtLck    LKOUT C,37,(D,,W4XLCK),CHAR,ND=Y                                       
RtFIW    LKOUT C,38,(D,,W4XFIW),CHAR,ND=Y                                       
RtDCW    LKOUT C,39,(D,,W4XDCW),CHAR,ND=Y                                       
RtPen    LKOUT C,40,(D,,W4XPEN),CHAR,ND=Y                                       
RtFAd    LKOUT C,41,(D,,W4XFAD),CHAR,ND=Y                                       
RtDir    LKOUT C,42,(D,,W4XDIR),CHAR,ND=Y                                       
RtWir    LKOUT C,43,(D,,W4XWIR),CHAR,ND=Y                                       
RtSpL    LKOUT C,44,(D,,W4XSPL),CHAR,ND=Y                                       
RtFMS    LKOUT C,45,(D,,W4XFMS),CHAR,ND=Y                                       
RtFEx    LKOUT C,46,(D,,W4XFEX),UBIN,ND=Y                                       
RtFFx    LKOUT C,47,(D,,W4XFFX),UBIN,ND=Y                                       
RtSAr    LKOUT C,48,(D,,W4XSAR),CHAR,ND=Y                                       
RtSMS    LKOUT C,49,(D,,W4XSMS),CHAR,ND=Y                                       
RtSEx    LKOUT C,50,(D,,W4XSEX),UBIN,ND=Y                                       
RtSFx    LKOUT C,51,(D,,W4XSFX),UBIN,ND=Y                                       
RtCAr    LKOUT C,52,(D,,W4XCAR),CHAR,ND=Y                                       
RtCMS    LKOUT C,53,(D,,W4XCMS),CHAR,ND=Y                                       
RtCEx    LKOUT C,54,(D,,W4XCEX),UBIN,ND=Y                                       
RtCFx    LKOUT C,55,(D,,W4XCFX),UBIN,ND=Y                                       
RtErn    LKOUT C,56,(D,,W4XERN),UBIN,ND=Y                                       
RtRSt    LKOUT C,57,(D,,W4XRST),CHAR,ND=Y                                       
RtDOB    LKOUT C,58,(D,,W4XDOB),CHAR,ND=Y                                       
RtDed    LKOUT C,59,(D,,W4XDED),UBIN,ND=Y                                       
RtTrs    LKOUT C,60,(D,,W4XTRS),CHAR,ND=Y                                       
RtMPR    LKOUT C,61,(D,,W4XMPR),UBIN,ND=Y                                       
RtPCh    LKOUT C,62,(D,,W4XPCH),UBIN,ND=Y                                       
RtGST    LKOUT C,63,(D,,W4XGST),CHAR,ND=Y                                       
RtCmt    LKOUT C,64,(D,,W4XCMT),CHAR,ND=Y                                       
RtPNm    LKOUT C,65,(D,,W4XPNM),CHAR,ND=Y                                       
RtPA1    LKOUT C,66,(D,,W4XPA1),CHAR,ND=Y                                       
RtPA2    LKOUT C,67,(D,,W4XPA2),CHAR,ND=Y                                       
RtPA3    LKOUT C,68,(D,,W4XPA3),CHAR,ND=Y                                       
RtPA4    LKOUT C,69,(D,,W4XPA4),CHAR,ND=Y                                       
RtPEx    LKOUT C,70,(D,,W4XPEX),CHAR,ND=Y                                       
RtCry    LKOUT C,71,(D,,W4XCRY),CHAR,ND=Y                                       
RtEml    LKOUT C,72,(D,,W4XEML),CHAR,ND=Y                                       
RtPCy    LKOUT C,74,(D,,W4XPCY),CHAR,ND=Y                                       
RtPSt    LKOUT C,75,(D,,W4XPST),CHAR,ND=Y                                       
RtPZp    LKOUT C,76,(D,,W4XPZP),CHAR,ND=Y                                       
RtPCy    LKOUT C,77,(D,,W4XPCT),CHAR,ND=Y                                       
RtPAc    LKOUT C,78,(D,,W4XPAC),CHAR,ND=Y                                       
RtWid    LKOUT C,79,(D,,W4XWID),CHAR,ND=Y                                       
RtAcD    LKOUT C,80,(D,,W4XACD),CHAR,ND=Y                                       
RtEFT    LKOUT C,85,(D,,W4XEFT),CHAR,ND=Y                                       
RtReg    LKOUT C,86,(D,,W4XREG),CHAR,ND=Y                                       
RtTCP    LKOUT C,87,(D,,W4XTCP),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR W4 LIMIT RECORDS                               *         
***********************************************************************         
                                                                                
ARYW4L   LKOUT A,(R,NXTW4L),MULTIROW=Y,ROWNAME=W4LVALS                          
                                                                                
RtSta    LKOUT C,1,(D,,W4LSTAT),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR AGENT SEARCH STATUS RECORDS                    *         
***********************************************************************         
                                                                                
ARYANS   LKOUT A,(R,NXTANS),ROWNAME=ANSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,ANSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AGENT SEARCH DETAILS RECORDS                   *         
***********************************************************************         
                                                                                
ARYANX   LKOUT A,(R,NXTAND),MULTIROW=Y,ROWNAME=ANDVALS                          
                                                                                
RtCod    LKOUT C,2,(D,,ANDCOD),CHAR,ND=Y                                        
RtNam    LKOUT C,3,(D,,ANDNAME),CHAR,ND=Y                                       
RtSNa    LKOUT C,4,(D,,ANDSNAM),CHAR,ND=Y                                       
RtAd1    LKOUT C,5,(D,,ANDADD1),CHAR,ND=Y                                       
RtAd2    LKOUT C,6,(D,,ANDADD2),CHAR,ND=Y                                       
RtAd3    LKOUT C,7,(D,,ANDADD3),CHAR,ND=Y                                       
RtAd4    LKOUT C,8,(D,,ANDADD4),CHAR,ND=Y                                       
RtEMa    LKOUT C,9,(D,,ANDEMAL),CHAR,ND=Y                                       
RtAtt    LKOUT C,10,(D,,ANDATTN),CHAR,ND=Y                                      
RtSsn    LKOUT C,11,(D,,ANDSSN),CHAR,ND=Y                                       
RtPho    LKOUT C,12,(D,,ANDPHON),CHAR,ND=Y                                      
RtFax    LKOUT C,13,(D,,ANDFAXN),CHAR,ND=Y                                      
RtISg    LKOUT C,14,(D,,ANDISAG),CHAR,ND=Y                                      
RtAOv    LKOUT C,15,(D,,ANDAOVR),CHAR,ND=Y                                      
RtOTR    LKOUT C,16,(D,,ANDOTRC),CHAR,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR AGENT LIMIT RECORDS                            *         
***********************************************************************         
                                                                                
ARYANL   LKOUT A,(R,NXTANL),MULTIROW=Y,ROWNAME=ANLVALS                          
                                                                                
RtSta    LKOUT C,1,(D,,ANLSTAT),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR CAST SEARCH STATUS RECORDS                     *         
***********************************************************************         
                                                                                
ARYCAS   LKOUT A,(R,NXTCAS),ROWNAME=CASVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,CASSTAT),UBIN,ND=Y                                       
RtSDt    LKOUT C,2,(D,,CASSDAT),CHAR,ND=Y                                       
RtSTm    LKOUT C,3,(D,,CASSTIM),CHAR,ND=Y                                       
RtANM    LKOUT C,4,(D,,CASSTST),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CAST SEARCH DETAILS RECORDS                    *         
***********************************************************************         
                                                                                
ARYCAX   LKOUT A,(R,NXTCAX),MULTIROW=Y,ROWNAME=CAXVALS                          
                                                                                
RtCom    LKOUT C,4,(D,,CAXCOM),CHAR,ND=Y                                        
RtAgy    LKOUT C,5,(D,,CAXAGY),CHAR,ND=Y                                        
RtCli    LKOUT C,6,(D,,CAXCLI),CHAR,ND=Y                                        
RtPrd    LKOUT C,7,(D,,CAXPRD),CHAR,ND=Y                                        
RtCid    LKOUT C,8,(D,,CAXCID),CHAR,ND=Y                                        
RtSeq    LKOUT C,9,(D,,CAXSEQ),CHAR,ND=Y                                        
RtSSN    LKOUT C,10,(D,,CAXSSN),CHAR,ND=Y                                       
RtLNm    LKOUT C,11,(D,,CAXLNM),CHAR,ND=Y                                       
RtFNm    LKOUT C,12,(D,,CAXFNM),CHAR,ND=Y                                       
RtCNm    LKOUT C,13,(D,,CAXCNM),CHAR,ND=Y                                       
RtCat    LKOUT C,14,(D,,CAXCAT),CHAR,ND=Y                                       
RtLCo    LKOUT C,15,(D,,CAXLCO),CHAR,ND=Y                                       
RtLAy    LKOUT C,16,(D,,CAXLAY),CHAR,ND=Y                                       
RtLCi    LKOUT C,17,(D,,CAXLCI),CHAR,ND=Y                                       
RtOnO    LKOUT C,18,(D,,CAXONO),CHAR,ND=Y                                       
RtTax    LKOUT C,19,(D,,CAXTAX),CHAR,ND=Y                                       
RtUni    LKOUT C,20,(D,,CAXUNI),CHAR,ND=Y                                       
RtLcl    LKOUT C,21,(D,,CAXLCL),CHAR,ND=Y                                       
RtCYr    LKOUT C,22,(D,,CAXCYR),CHAR,ND=Y                                       
RtFFC    LKOUT C,23,(D,,CAXFFC),CHAR,ND=Y                                       
RtFSv    LKOUT C,24,(D,,CAXFSV),CHAR,ND=Y                                       
RtLSv    LKOUT C,25,(D,,CAXLSV),CHAR,ND=Y                                       
RtRlL    LKOUT C,26,(D,,CAXRLL),CHAR,ND=Y                                       
RtRlD    LKOUT C,27,(D,,CAXRLD),CHAR,ND=Y                                       
RtEff    LKOUT C,28,(D,,CAXEFF),CHAR,ND=Y                                       
RtPRL    LKOUT C,29,(D,,CAXPRL),CHAR,ND=Y                                       
RtPRD    LKOUT C,30,(D,,CAXPLD),CHAR,ND=Y                                       
RtAtC    LKOUT C,31,(D,,CAXATC),CHAR,ND=Y                                       
RtAtN    LKOUT C,32,(D,,CAXATN),CHAR,ND=Y                                       
RtAgt    LKOUT C,33,(D,,CAXAGT),CHAR,ND=Y                                       
RtAgN    LKOUT C,34,(D,,CAXAGN),CHAR,ND=Y                                       
RtGua    LKOUT C,35,(D,,CAXGUA),CHAR,ND=Y                                       
RtExp    LKOUT C,36,(D,,CAXEXP),CHAR,ND=Y                                       
RtUP1    LKOUT C,37,(D,,CAXUP1),CHAR,ND=Y                                       
RtOP1    LKOUT C,38,(D,,CAXOP1),UBIN,ND=Y                                       
RtUP2    LKOUT C,39,(D,,CAXUP2),CHAR,ND=Y                                       
RtOP2    LKOUT C,40,(D,,CAXOP2),UBIN,ND=Y                                       
RtUP3    LKOUT C,41,(D,,CAXUP3),CHAR,ND=Y                                       
RtOP3    LKOUT C,42,(D,,CAXOP3),UBIN,ND=Y                                       
RtUP4    LKOUT C,43,(D,,CAXUP4),CHAR,ND=Y                                       
RtOP4    LKOUT C,44,(D,,CAXOP4),UBIN,ND=Y                                       
RtUP5    LKOUT C,45,(D,,CAXUP5),CHAR,ND=Y                                       
RtOP5    LKOUT C,46,(D,,CAXOP5),UBIN,ND=Y                                       
RtUP6    LKOUT C,47,(D,,CAXUP6),CHAR,ND=Y                                       
RtOP6    LKOUT C,48,(D,,CAXOP6),UBIN,ND=Y                                       
Rt2OP    LKOUT C,49,(D,,CAX2OP),UBIN,ND=Y                                       
RtDbl    LKOUT C,50,(D,,CAXDBL),CHAR,ND=Y                                       
RtUA1    LKOUT C,51,(D,,CAXUA1),CHAR,ND=Y                                       
RtOA1    LKOUT C,52,(D,,CAXOA1),UBIN,ND=Y                                       
RtUA2    LKOUT C,53,(D,,CAXUA2),CHAR,ND=Y                                       
RtOA2    LKOUT C,54,(D,,CAXOA2),UBIN,ND=Y                                       
RtUA3    LKOUT C,55,(D,,CAXUA3),CHAR,ND=Y                                       
RtOA3    LKOUT C,56,(D,,CAXOA3),UBIN,ND=Y                                       
RtUA4    LKOUT C,57,(D,,CAXUA4),CHAR,ND=Y                                       
RtOA4    LKOUT C,58,(D,,CAXOA4),UBIN,ND=Y                                       
RtUA5    LKOUT C,59,(D,,CAXUA5),CHAR,ND=Y                                       
RtOA5    LKOUT C,60,(D,,CAXOA5),UBIN,ND=Y                                       
RtUA6    LKOUT C,61,(D,,CAXUA6),CHAR,ND=Y                                       
RtOA6    LKOUT C,62,(D,,CAXOA6),UBIN,ND=Y                                       
RtTr1    LKOUT C,63,(D,,CAXTR1),CHAR,ND=Y                                       
RtTr2    LKOUT C,64,(D,,CAXTR2),CHAR,ND=Y                                       
RtTr3    LKOUT C,65,(D,,CAXTR3),CHAR,ND=Y                                       
RtTr4    LKOUT C,66,(D,,CAXTR4),CHAR,ND=Y                                       
RtCmt    LKOUT C,67,(D,,CAXCMT),CHAR,ND=Y                                       
RtRDe    LKOUT C,68,(D,,CAXRDE),CHAR,ND=Y                                       
RtMst    LKOUT C,69,(D,,CAXMST),CHAR,ND=Y                                       
RtLft    LKOUT C,70,(D,,CAXLFT),CHAR,ND=Y                                       
RtCPA    LKOUT C,71,(D,,CAXCPA),CHAR,ND=Y                                       
RtPCR    LKOUT C,72,(D,,CAXPCR),CHAR,ND=Y                                       
RtPUR    LKOUT C,73,(D,,CAXPUR),CHAR,ND=Y                                       
RtFgn    LKOUT C,74,(D,,CAXFGN),CHAR,ND=Y                                       
RtIna    LKOUT C,75,(D,,CAXINA),CHAR,ND=Y                                       
RtInr    LKOUT C,76,(D,,CAXINR),CHAR,ND=Y                                       
RtACP    LKOUT C,77,(D,,CAXACP),CHAR,ND=Y                                       
RtAPP    LKOUT C,78,(D,,CAXAPP),CHAR,ND=Y                                       
RtPDT    LKOUT C,79,(D,,CAXPDT),CHAR,ND=Y                                       
RtClb    LKOUT C,80,(D,,CAXCLB),CHAR,ND=Y                                       
RtGRR    LKOUT C,81,(D,,CAXGRR),CHAR,ND=Y                                       
RtPCS    LKOUT C,83,(D,,CAXPCS),CHAR,ND=Y                                       
RtRel    LKOUT C,84,(D,,CAXREL),CHAR,ND=Y                                       
RtWID    LKOUT C,85,(D,,CAXWID),CHAR,ND=Y                                       
RtEur    LKOUT C,86,(D,,CAXEUR),CHAR,ND=Y                                       
Rt2UP1   LKOUT C,87,(D,,CAX2UP1),CHAR,ND=Y                                      
Rt2OP1   LKOUT C,88,(D,,CAX2OP1),UBIN,ND=Y                                      
Rt2UP2   LKOUT C,89,(D,,CAX2UP2),CHAR,ND=Y                                      
Rt2UP2   LKOUT C,90,(D,,CAX2OP2),UBIN,ND=Y                                      
Rt2UP3   LKOUT C,91,(D,,CAX2UP3),CHAR,ND=Y                                      
Rt2UP3   LKOUT C,92,(D,,CAX2OP3),UBIN,ND=Y                                      
Rt2UP4   LKOUT C,93,(D,,CAX2UP4),CHAR,ND=Y                                      
Rt2UP4   LKOUT C,94,(D,,CAX2OP4),UBIN,ND=Y                                      
Rt2UP5   LKOUT C,95,(D,,CAX2UP5),CHAR,ND=Y                                      
Rt2UP5   LKOUT C,96,(D,,CAX2OP5),UBIN,ND=Y                                      
RtReR    LKOUT C,97,(D,,CAXRER),CHAR,ND=Y                                       
RtCoN    LKOUT C,99,(D,,CAXCOT),CHAR,ND=Y                                       
RtGTy    LKOUT C,100,(D,,CAXGTY),CHAR,ND=Y                                      
RtGAm    LKOUT C,101,(D,,CAXGAM),UBIN,ND=Y                                      
RtCoX    LKOUT C,102,(D,,CAXCOX),CHAR,ND=Y                                      
RtFCE    LKOUT C,103,(D,,CAXFCE),CHAR,ND=Y                                      
RtORe    LKOUT C,104,(D,,CAXORE),CHAR,ND=Y                                      
RtAll    LKOUT C,105,(D,,CAXALL),CHAR,ND=Y                                      
RtAFT    LKOUT C,106,(D,,CAXAFT),CHAR,ND=Y                                      
RtGVA    LKOUT C,121,(D,,CAXGVA),CHAR,ND=Y                                      
RtACo    LKOUT C,122,(D,,CAXACO),CHAR,ND=Y                                      
RtTr5    LKOUT C,123,(D,,CAXTR5),CHAR,ND=Y                                      
RtTr6    LKOUT C,124,(D,,CAXTR6),CHAR,ND=Y                                      
RtTr7    LKOUT C,125,(D,,CAXTR7),CHAR,ND=Y                                      
RtASq    LKOUT C,126,(D,,CAXASQ),CHAR,ND=Y                                      
RtTCo    LKOUT C,127,(D,,CAXTCO),CHAR,ND=Y                                      
RtTAy    LKOUT C,128,(D,,CAXTAY),CHAR,ND=Y                                      
RtTCi    LKOUT C,129,(D,,CAXTCI),CHAR,ND=Y                                      
RtTSq    LKOUT C,130,(D,,CAXTSQ),CHAR,ND=Y                                      
RtVSq    LKOUT C,131,(D,,CAXVSQ),CHAR,ND=Y                                      
RtFDat   LKOUT C,132,(D,,CAXFDAT),CHAR,ND=Y                                     
RtFStu   LKOUT C,133,(D,,CAXFSTU),CHAR,ND=Y                                     
RtFCit   LKOUT C,134,(D,,CAXFCIT),CHAR,ND=Y                                     
RtFSta   LKOUT C,135,(D,,CAXFSTA),CHAR,ND=Y                                     
RtRDat   LKOUT C,136,(D,,CAXRDAT),CHAR,ND=Y                                     
RtRStu   LKOUT C,137,(D,,CAXRSTU),CHAR,ND=Y                                     
RtRCit   LKOUT C,138,(D,,CAXRCIT),CHAR,ND=Y                                     
RtRSta   LKOUT C,139,(D,,CAXRSTA),CHAR,ND=Y                                     
RtMDat   LKOUT C,140,(D,,CAXMDAT),CHAR,ND=Y                                     
RtMStu   LKOUT C,141,(D,,CAXMSTU),CHAR,ND=Y                                     
RtMCit   LKOUT C,142,(D,,CAXMCIT),CHAR,ND=Y                                     
RtMSta   LKOUT C,143,(D,,CAXMSTA),CHAR,ND=Y                                     
RtTFDat  LKOUT C,144,(D,,CAXTFDAT),CHAR,ND=Y                                    
RtTFStu  LKOUT C,145,(D,,CAXTFSTU),CHAR,ND=Y                                    
RtTFCit  LKOUT C,146,(D,,CAXTFCIT),CHAR,ND=Y                                    
RtTFSta  LKOUT C,147,(D,,CAXTFSTA),CHAR,ND=Y                                    
RtTRDat  LKOUT C,148,(D,,CAXTRDAT),CHAR,ND=Y                                    
RtTRStu  LKOUT C,149,(D,,CAXTRSTU),CHAR,ND=Y                                    
RtTRCit  LKOUT C,150,(D,,CAXTRCIT),CHAR,ND=Y                                    
RtTRSta  LKOUT C,151,(D,,CAXTRSTA),CHAR,ND=Y                                    
RtLFDat  LKOUT C,152,(D,,CAXLFDAT),CHAR,ND=Y                                    
RtLFStu  LKOUT C,153,(D,,CAXLFSTU),CHAR,ND=Y                                    
RtLFCit  LKOUT C,154,(D,,CAXLFCIT),CHAR,ND=Y                                    
RtLFSta  LKOUT C,155,(D,,CAXLFSTA),CHAR,ND=Y                                    
RtLRDat  LKOUT C,156,(D,,CAXLRDAT),CHAR,ND=Y                                    
RtLRStu  LKOUT C,157,(D,,CAXLRSTU),CHAR,ND=Y                                    
RtLRCit  LKOUT C,158,(D,,CAXLRCIT),CHAR,ND=Y                                    
RtLRSta  LKOUT C,159,(D,,CAXLRSTA),CHAR,ND=Y                                    
RtADat   LKOUT C,160,(D,,CAXADAT),CHAR,ND=Y                                     
RtAStu   LKOUT C,161,(D,,CAXASTU),CHAR,ND=Y                                     
RtACit   LKOUT C,162,(D,,CAXACIT),CHAR,ND=Y                                     
RtASta   LKOUT C,163,(D,,CAXASTA),CHAR,ND=Y                                     
RtASta   LKOUT C,164,(D,,CAXCFFC),CHAR,ND=Y                                     
RtEqy    LKOUT C,165,(D,,CAXEQY),CHAR,ND=Y                                      
Array    LKOUT C,O#CAVERS,(A,ARYCAV)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CAST VERSION DETAILS RECORDS                   *         
***********************************************************************         
                                                                                
ARYCAV   LKOUT A,(R,NXTCAV),MULTIROW=Y,ROWNAME=CAVVALS                          
                                                                                
RtVer    LKOUT C,1,(D,,CAVVER),UBIN,ND=Y                                        
RtPad    LKOUT C,2,(D,,CAVPAD),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR CAST LIMIT RECORDS                             *         
***********************************************************************         
                                                                                
ARYCAL   LKOUT A,(R,NXTCAL),MULTIROW=Y,ROWNAME=CALVALS                          
                                                                                
RtSta    LKOUT C,1,(D,,CALSTAT),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE SEARCH STATUS RECORDS                  *         
***********************************************************************         
                                                                                
ARYINS   LKOUT A,(R,NXTINS),ROWNAME=INSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,INSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE EXPANDED DETAILS RECORDS               *         
***********************************************************************         
                                                                                
ARYINX   LKOUT A,(R,NXTINX),MULTIROW=Y,ROWNAME=INXVALS                          
                                                                                
RtAgy    LKOUT C,4,(D,,INXAGY),CHAR,ND=Y                                        
RtInv    LKOUT C,5,(D,,INXINV),CHAR,ND=Y                                        
RtAUI    LKOUT C,6,(D,,INXAUI),CHAR,ND=Y                                        
RtASI    LKOUT C,7,(D,,INXASI),CHAR,ND=Y                                        
RtADt    LKOUT C,8,(D,,INXADT),CHAR,ND=Y                                        
RtATm    LKOUT C,9,(D,,INXATM),CHAR,ND=Y                                        
RtRAd    LKOUT C,10,(D,,INXRAD),CHAR,ND=Y                                       
RtRTs    LKOUT C,11,(D,,INXRTS),CHAR,ND=Y                                       
RtPUI    LKOUT C,12,(D,,INXPUI),CHAR,ND=Y                                       
RtPSI    LKOUT C,13,(D,,INXPSI),CHAR,ND=Y                                       
RtPDt    LKOUT C,14,(D,,INXPDT),CHAR,ND=Y                                       
RtPTm    LKOUT C,15,(D,,INXPTM),CHAR,ND=Y                                       
RtUse    LKOUT C,16,(D,,INXUSE),CHAR,ND=Y                                       
RtTyp    LKOUT C,17,(D,,INXTYP),UBIN,ND=Y                                       
RtCli    LKOUT C,18,(D,,INXCLI),CHAR,ND=Y                                       
RtPrd    LKOUT C,19,(D,,INXPRD),CHAR,ND=Y                                       
RtPrN    LKOUT C,20,(D,,INXPRN),CHAR,ND=Y                                       
RtCom    LKOUT C,21,(D,,INXCOM),CHAR,ND=Y                                       
RtCid    LKOUT C,22,(D,,INXCID),CHAR,ND=Y                                       
RtCTi    LKOUT C,23,(D,,INXCTI),CHAR,ND=Y                                       
RtCLn    LKOUT C,24,(D,,INXLEN),UBIN,ND=Y                                       
RtMed    LKOUT C,25,(D,,INXMED),CHAR,ND=Y                                       
RtVer    LKOUT C,26,(D,,INXVER),UBIN,ND=Y                                       
RtVId    LKOUT C,27,(D,,INXVID),CHAR,ND=Y                                       
RtVLn    LKOUT C,28,(D,,INXVLN),UBIN,ND=Y                                       
RtLId    LKOUT C,29,(D,,INXLID),CHAR,ND=Y                                       
RtLLn    LKOUT C,30,(D,,INXLLN),UBIN,ND=Y                                       
RtCyS    LKOUT C,31,(D,,INXCYS),CHAR,ND=Y                                       
RtCyE    LKOUT C,32,(D,,INXCYE),CHAR,ND=Y                                       
RtAPo    LKOUT C,33,(D,,INXAPO),CHAR,ND=Y                                       
RtEst    LKOUT C,34,(D,,INXEST),CHAR,ND=Y                                       
RtPer    LKOUT C,35,(D,,INXPER),UBIN,ND=Y                                       
RtHCm    LKOUT C,36,(D,,INXHCM),CHAR,ND=Y                                       
RtICm    LKOUT C,37,(D,,INXICM),CHAR,ND=Y                                       
RtTag    LKOUT C,38,(D,,INXTAG),UBIN,ND=Y                                       
RtIns    LKOUT C,39,(D,,INXINS),UBIN,ND=Y                                       
RtDem    LKOUT C,40,(D,,INXDEM),UBIN,ND=Y                                       
RtStU    LKOUT C,41,(D,,INXSTU),UBIN,ND=Y                                       
RtTUs    LKOUT C,42,(D,,INXTUS),UBIN,ND=Y                                       
RtUni    LKOUT C,43,(D,,INXUNI),UBIN,ND=Y                                       
RtINY    LKOUT C,44,(D,,INXINY),CHAR,ND=Y                                       
RtICH    LKOUT C,45,(D,,INXICH),CHAR,ND=Y                                       
RtILA    LKOUT C,46,(D,,INXILA),CHAR,ND=Y                                       
RtPGr    LKOUT C,47,(D,,INXPGR),CBIN,ND=Y                                       
RtPAC    LKOUT C,48,(D,,INXPAC),CBIN,ND=Y                                       
RtPGC    LKOUT C,49,(D,,INXPGC),CBIN,ND=Y                                       
RtPIA    LKOUT C,50,(D,,INXPIA),CBIN,ND=Y                                       
RtPCA    LKOUT C,51,(D,,INXPCA),CBIN,ND=Y                                       
RtPRE    LKOUT C,52,(D,,INXPRE),CBIN,ND=Y                                       
RtPSP    LKOUT C,53,(D,,INXPSP),CBIN,ND=Y                                       
RtPMD    LKOUT C,54,(D,,INXPMD),CBIN,ND=Y                                       
RtPNH    LKOUT C,55,(D,,INXPNH),CBIN,ND=Y                                       
RtPHW    LKOUT C,56,(D,,INXPHW),CBIN,ND=Y                                       
RtPIR    LKOUT C,57,(D,,INXPIR),CBIN,ND=Y                                       
RtPUD    LKOUT C,58,(D,,INXPUD),CBIN,ND=Y                                       
RtOff    LKOUT C,59,(D,,INXOFF),CHAR,ND=Y                                       
RtEmp    LKOUT C,60,(D,,INXEMP),CHAR,ND=Y                                       
RtAdv    LKOUT C,61,(D,,INXADV),CHAR,ND=Y                                       
RtUSI    LKOUT C,62,(D,,INXUSI),CHAR,ND=Y                                       
RtCAI    LKOUT C,63,(D,,INXCAI),CHAR,ND=Y                                       
RtMOv    LKOUT C,64,(D,,INXMOV),CHAR,ND=Y                                       
RtCDP    LKOUT C,65,(D,,INXCDP),CHAR,ND=Y                                       
RtJOv    LKOUT C,66,(D,,INXJOV),CHAR,ND=Y                                       
RtPCP    LKOUT C,67,(D,,INXPCP),CHAR,ND=Y                                       
RtFUp    LKOUT C,68,(D,,INXFUP),CHAR,ND=Y                                       
RtPri    LKOUT C,69,(D,,INXPRI),CHAR,ND=Y                                       
RtSub    LKOUT C,70,(D,,INXSUB),CHAR,ND=Y                                       
RtPVC    LKOUT C,71,(D,,INXPVC),CHAR,ND=Y                                       
RtACP    LKOUT C,72,(D,,INXACP),CHAR,ND=Y                                       
RtCSl    LKOUT C,73,(D,,INXCSL),CHAR,ND=Y                                       
RtCPy    LKOUT C,74,(D,,INXCPY),CHAR,ND=Y                                       
RtBNP    LKOUT C,75,(D,,INXBNP),CHAR,ND=Y                                       
RtPWC    LKOUT C,76,(D,,INXPWC),CHAR,ND=Y                                       
RtMIO    LKOUT C,77,(D,,INXMIO),CHAR,ND=Y                                       
RtDMO    LKOUT C,78,(D,,INXDMO),CHAR,ND=Y                                       
RtPPC    LKOUT C,79,(D,,INXPPC),CHAR,ND=Y                                       
RtACH    LKOUT C,80,(D,,INXACH),CHAR,ND=Y                                       
RtDAC    LKOUT C,81,(D,,INXDAC),CHAR,ND=Y                                       
RtDAG    LKOUT C,82,(D,,INXDAG),CHAR,ND=Y                                       
RtGAO    LKOUT C,83,(D,,INXGAO),CBIN,ND=Y                                       
RtCTO    LKOUT C,84,(D,,INXCTO),CHAR,ND=Y                                       
RtPHO    LKOUT C,85,(D,,INXPHO),UBIN,ND=Y                                       
RtTOO    LKOUT C,86,(D,,INXTOO),CBIN,ND=Y                                       
RtHOO    LKOUT C,87,(D,,INXHOO),CBIN,ND=Y                                       
RtUrO    LKOUT C,88,(D,,INXURO),CHAR,ND=Y                                       
RtDCO    LKOUT C,89,(D,,INXDCO),CHAR,ND=Y                                       
RtDAO    LKOUT C,90,(D,,INXDAO),CHAR,ND=Y                                       
RtDLO    LKOUT C,91,(D,,INXDLO),CHAR,ND=Y                                       
RtFCO    LKOUT C,92,(D,,INXFCO),CBIN,ND=Y                                       
RtASO    LKOUT C,93,(D,,INXASO),CHAR,ND=Y                                       
RtPTO    LKOUT C,94,(D,,INXPTO),CHAR,ND=Y                                       
RtPLO    LKOUT C,95,(D,,INXPLO),CHAR,ND=Y                                       
RtPUO    LKOUT C,96,(D,,INXPUO),CHAR,ND=Y                                       
RtHWO    LKOUT C,97,(D,,INXHWO),CBIN,ND=Y                                       
RtCRO    LKOUT C,98,(D,,INXCRO),CHAR,ND=Y                                       
RtDPO    LKOUT C,99,(D,,INXDPO),CHAR,ND=Y                                       
RtGPO    LKOUT C,100,(D,,INXGPO),CHAR,ND=Y                                      
RtRPO    LKOUT C,101,(D,,INXRPO),CHAR,ND=Y                                      
RtPUO    LKOUT C,102,(D,,INXPRO),CHAR,ND=Y                                      
RtCHO    LKOUT C,103,(D,,INXCHO),CHAR,ND=Y                                      
RtFEO    LKOUT C,104,(D,,INXFEO),CBIN,ND=Y                                      
RtCSO    LKOUT C,105,(D,,INXCSO),CHAR,ND=Y                                      
RtCAO    LKOUT C,106,(D,,INXCAO),CBIN,ND=Y                                      
RtSOO    LKOUT C,107,(D,,INXSOO),UBIN,ND=Y                                      
RtNIO    LKOUT C,108,(D,,INXNIO),CHAR,ND=Y                                      
RtGEO    LKOUT C,109,(D,,INXGEO),CHAR,ND=Y                                      
RtQUI    LKOUT C,110,(D,,INXQUI),CHAR,ND=Y                                      
RtQSI    LKOUT C,111,(D,,INXQSI),CHAR,ND=Y                                      
RtQDt    LKOUT C,112,(D,,INXQDT),CHAR,ND=Y                                      
RtQTm    LKOUT C,113,(D,,INXQTM),CHAR,ND=Y                                      
RtBDt    LKOUT C,114,(D,,INXBDT),CHAR,ND=Y                                      
RtBTy    LKOUT C,115,(D,,INXBTY),UBIN,ND=Y                                      
RtCCR    LKOUT C,116,(D,,INXCCR),CBIN,ND=Y                                      
RtBTo    LKOUT C,117,(D,,INXBTO),CBIN,ND=Y                                      
RtBPT    LKOUT C,118,(D,,INXBPT),CBIN,ND=Y                                      
RtBHA    LKOUT C,119,(D,,INXBHA),CBIN,ND=Y                                      
RtBCH    LKOUT C,120,(D,,INXBCH),CBIN,ND=Y                                      
RtBCS    LKOUT C,121,(D,,INXBCS),CBIN,ND=Y                                      
RtBFC    LKOUT C,122,(D,,INXBFC),CBIN,ND=Y                                      
RtBCG    LKOUT C,123,(D,,INXBGC),CBIN,ND=Y                                      
RtBAC    LKOUT C,124,(D,,INXBAC),CBIN,ND=Y                                      
RtBSF    LKOUT C,125,(D,,INXBSF),CBIN,ND=Y                                      
RtETo    LKOUT C,126,(D,,INXETO),CBIN,ND=Y                                      
RtEPT    LKOUT C,127,(D,,INXEPT),CBIN,ND=Y                                      
RtEHA    LKOUT C,128,(D,,INXEHA),CBIN,ND=Y                                      
RtECH    LKOUT C,129,(D,,INXECH),CBIN,ND=Y                                      
RtEFe    LKOUT C,130,(D,,INXEFE),CBIN,ND=Y                                      
RtCer    LKOUT C,131,(D,,INXCER),CHAR,ND=Y                                      
RtCed    LKOUT C,132,(D,,INXCED),CHAR,ND=Y                                      
RtPHl    LKOUT C,133,(D,,INXPHL),CHAR,ND=Y                                      
RtPPD    LKOUT C,134,(D,,INXPPD),CHAR,ND=Y                                      
RtPHR    LKOUT C,135,(D,,INXPHR),CHAR,ND=Y                                      
RtFPA    LKOUT C,136,(D,,INXFPA),CHAR,ND=Y                                      
RtRPH    LKOUT C,137,(D,,INXRPH),CHAR,ND=Y                                      
RtEWN    LKOUT C,138,(D,,INXEWN),CBIN,ND=Y                                      
RtEWI    LKOUT C,139,(D,,INXEWI),CHAR,ND=Y                                      
RtEPi    LKOUT C,140,(D,,INXEPI),CHAR,ND=Y                                      
RtDuD    LKOUT C,141,(D,,INXDUD),CHAR,ND=Y                                      
RtPDD    LKOUT C,142,(D,,INXPDD),CHAR,ND=Y                                      
RtDCD    LKOUT C,143,(D,,INXDCD),CHAR,ND=Y                                      
RtODD    LKOUT C,144,(D,,INXODD),CHAR,ND=Y                                      
RtCDt    LKOUT C,145,(D,,INXCDT),CHAR,ND=Y                                      
RtCRu    LKOUT C,146,(D,,INXCRU),CHAR,ND=Y                                      
RtCRD    LKOUT C,147,(D,,INXCRD),CHAR,ND=Y                                      
RtCRu    LKOUT C,148,(D,,INXCDF),CHAR,ND=Y                                      
RtWID    LKOUT C,149,(D,,INXWID),CHAR,ND=Y                                      
RtBPS    LKOUT C,150,(D,,INXBPS),CBIN,ND=Y                                      
RtEuP    LKOUT C,151,(D,,INXEUP),CHAR,ND=Y                                      
RtEGr    LKOUT C,152,(D,,INXEUPGR),CBIN,ND=Y                                    
RtEAC    LKOUT C,153,(D,,INXEUPAC),CBIN,ND=Y                                    
RtEGC    LKOUT C,154,(D,,INXEUPGC),CBIN,ND=Y                                    
RtEIA    LKOUT C,155,(D,,INXEUPIA),CBIN,ND=Y                                    
RtECA    LKOUT C,156,(D,,INXEUPCA),CBIN,ND=Y                                    
RtERE    LKOUT C,157,(D,,INXEUPRE),CBIN,ND=Y                                    
RtESP    LKOUT C,158,(D,,INXEUPSP),CBIN,ND=Y                                    
RtEMD    LKOUT C,159,(D,,INXEUPMD),CBIN,ND=Y                                    
RtENH    LKOUT C,160,(D,,INXEUPNH),CBIN,ND=Y                                    
RtEHW    LKOUT C,161,(D,,INXEUPHW),CBIN,ND=Y                                    
RtEIR    LKOUT C,162,(D,,INXEUPIR),CHAR,ND=Y                                    
RtEUD    LKOUT C,163,(D,,INXEUPUD),CBIN,ND=Y                                    
RtETo    LKOUT C,164,(D,,INXEUBTO),CBIN,ND=Y                                    
RtEPT    LKOUT C,165,(D,,INXEUBPT),CBIN,ND=Y                                    
RtEHA    LKOUT C,166,(D,,INXEUBHA),CBIN,ND=Y                                    
RtECH    LKOUT C,167,(D,,INXEUBCH),CBIN,ND=Y                                    
RtECS    LKOUT C,168,(D,,INXEUBCS),CBIN,ND=Y                                    
RtEFC    LKOUT C,169,(D,,INXEUBFC),CBIN,ND=Y                                    
RtECG    LKOUT C,170,(D,,INXEUBGC),CBIN,ND=Y                                    
RtEAC    LKOUT C,171,(D,,INXEUBAC),CBIN,ND=Y                                    
RtESF    LKOUT C,172,(D,,INXEUBSF),CBIN,ND=Y                                    
RtEPS    LKOUT C,173,(D,,INXEUBPS),CBIN,ND=Y                                    
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR SPLIT INVOICE DETAILS RECORDS                  *         
***********************************************************************         
                                                                                
ARYINL   LKOUT A,(R,NXTINL),MULTIROW=Y,ROWNAME=INLVALS                          
                                                                                
RtInv    LKOUT C,1,(D,,INLINV),CHAR,ND=Y                                        
RtPct    LKOUT C,2,(D,,INLPCT),UBIN,ND=Y                                        
RtEst    LKOUT C,3,(D,,INLEST),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR NTWK/SYS/MKT DETAILS RECORDS                   *         
***********************************************************************         
                                                                                
ARYINM   LKOUT A,(R,NXTINM),MULTIROW=Y,ROWNAME=INMVALS                          
                                                                                
RtCod    LKOUT C,1,(D,,INMCOD),CHAR,ND=Y                                        
RtSDt    LKOUT C,2,(D,,INMCYS),CHAR,ND=Y                                        
RtEDt    LKOUT C,3,(D,,INMCYE),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR PROGRAM DETAILS RECORDS                        *         
***********************************************************************         
                                                                                
ARYINP   LKOUT A,(R,NXTINP),MULTIROW=Y,ROWNAME=INPVALS                          
                                                                                
RtUDt    LKOUT C,1,(D,,INPUDT),CHAR,ND=Y                                        
RtPNm    LKOUT C,2,(D,,INPPNM),CHAR,ND=Y                                        
RtLft    LKOUT C,3,(D,,INPLFT),CHAR,ND=Y                                        
RtNwk    LKOUT C,4,(D,,INPNWK),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE ATTACHED US/CAN INVOICE RECORDS        *         
***********************************************************************         
                                                                                
ARYINA   LKOUT A,(R,NXTINA),MULTIROW=Y,ROWNAME=INXVALS                          
                                                                                
RtAgy    LKOUT C,4,(D,,INXAGY),CHAR,ND=Y                                        
RtInv    LKOUT C,5,(D,,INXINV),CHAR,ND=Y                                        
RtAUI    LKOUT C,6,(D,,INXAUI),CHAR,ND=Y                                        
RtASI    LKOUT C,7,(D,,INXASI),CHAR,ND=Y                                        
RtADt    LKOUT C,8,(D,,INXADT),CHAR,ND=Y                                        
RtATm    LKOUT C,9,(D,,INXATM),CHAR,ND=Y                                        
RtRAd    LKOUT C,10,(D,,INXRAD),CHAR,ND=Y                                       
RtRTs    LKOUT C,11,(D,,INXRTS),CHAR,ND=Y                                       
RtPUI    LKOUT C,12,(D,,INXPUI),CHAR,ND=Y                                       
RtPSI    LKOUT C,13,(D,,INXPSI),CHAR,ND=Y                                       
RtPDt    LKOUT C,14,(D,,INXPDT),CHAR,ND=Y                                       
RtPTm    LKOUT C,15,(D,,INXPTM),CHAR,ND=Y                                       
RtUse    LKOUT C,16,(D,,INXUSE),CHAR,ND=Y                                       
RtTyp    LKOUT C,17,(D,,INXTYP),UBIN,ND=Y                                       
RtCli    LKOUT C,18,(D,,INXCLI),CHAR,ND=Y                                       
RtPrd    LKOUT C,19,(D,,INXPRD),CHAR,ND=Y                                       
RtPrN    LKOUT C,20,(D,,INXPRN),CHAR,ND=Y                                       
RtCom    LKOUT C,21,(D,,INXCOM),CHAR,ND=Y                                       
RtCid    LKOUT C,22,(D,,INXCID),CHAR,ND=Y                                       
RtCTi    LKOUT C,23,(D,,INXCTI),CHAR,ND=Y                                       
RtCLn    LKOUT C,24,(D,,INXLEN),UBIN,ND=Y                                       
RtMed    LKOUT C,25,(D,,INXMED),CHAR,ND=Y                                       
RtVer    LKOUT C,26,(D,,INXVER),UBIN,ND=Y                                       
RtVId    LKOUT C,27,(D,,INXVID),CHAR,ND=Y                                       
RtVLn    LKOUT C,28,(D,,INXVLN),UBIN,ND=Y                                       
RtLId    LKOUT C,29,(D,,INXLID),CHAR,ND=Y                                       
RtLLn    LKOUT C,30,(D,,INXLLN),UBIN,ND=Y                                       
RtCyS    LKOUT C,31,(D,,INXCYS),CHAR,ND=Y                                       
RtCyE    LKOUT C,32,(D,,INXCYE),CHAR,ND=Y                                       
RtAPo    LKOUT C,33,(D,,INXAPO),CHAR,ND=Y                                       
RtEst    LKOUT C,34,(D,,INXEST),CHAR,ND=Y                                       
RtPer    LKOUT C,35,(D,,INXPER),UBIN,ND=Y                                       
RtHCm    LKOUT C,36,(D,,INXHCM),CHAR,ND=Y                                       
RtICm    LKOUT C,37,(D,,INXICM),CHAR,ND=Y                                       
RtTag    LKOUT C,38,(D,,INXTAG),UBIN,ND=Y                                       
RtIns    LKOUT C,39,(D,,INXINS),UBIN,ND=Y                                       
RtDem    LKOUT C,40,(D,,INXDEM),UBIN,ND=Y                                       
RtStU    LKOUT C,41,(D,,INXSTU),UBIN,ND=Y                                       
RtTUs    LKOUT C,42,(D,,INXTUS),UBIN,ND=Y                                       
RtUni    LKOUT C,43,(D,,INXUNI),UBIN,ND=Y                                       
RtINY    LKOUT C,44,(D,,INXINY),CHAR,ND=Y                                       
RtICH    LKOUT C,45,(D,,INXICH),CHAR,ND=Y                                       
RtILA    LKOUT C,46,(D,,INXILA),CHAR,ND=Y                                       
RtPGr    LKOUT C,47,(D,,INXPGR),CBIN,ND=Y                                       
RtPAC    LKOUT C,48,(D,,INXPAC),CBIN,ND=Y                                       
RtPGC    LKOUT C,49,(D,,INXPGC),CBIN,ND=Y                                       
RtPIA    LKOUT C,50,(D,,INXPIA),CBIN,ND=Y                                       
RtPCA    LKOUT C,51,(D,,INXPCA),CBIN,ND=Y                                       
RtPRE    LKOUT C,52,(D,,INXPRE),CBIN,ND=Y                                       
RtPSP    LKOUT C,53,(D,,INXPSP),CBIN,ND=Y                                       
RtPMD    LKOUT C,54,(D,,INXPMD),CBIN,ND=Y                                       
RtPNH    LKOUT C,55,(D,,INXPNH),CBIN,ND=Y                                       
RtPHW    LKOUT C,56,(D,,INXPHW),CBIN,ND=Y                                       
RtPIR    LKOUT C,57,(D,,INXPIR),CBIN,ND=Y                                       
RtPUD    LKOUT C,58,(D,,INXPUD),CBIN,ND=Y                                       
RtOff    LKOUT C,59,(D,,INXOFF),CHAR,ND=Y                                       
RtEmp    LKOUT C,60,(D,,INXEMP),CHAR,ND=Y                                       
RtAdv    LKOUT C,61,(D,,INXADV),CHAR,ND=Y                                       
RtUSI    LKOUT C,62,(D,,INXUSI),CHAR,ND=Y                                       
RtCAI    LKOUT C,63,(D,,INXCAI),CHAR,ND=Y                                       
RtMOv    LKOUT C,64,(D,,INXMOV),CHAR,ND=Y                                       
RtCDP    LKOUT C,65,(D,,INXCDP),CHAR,ND=Y                                       
RtJOv    LKOUT C,66,(D,,INXJOV),CHAR,ND=Y                                       
RtPCP    LKOUT C,67,(D,,INXPCP),CHAR,ND=Y                                       
RtFUp    LKOUT C,68,(D,,INXFUP),CHAR,ND=Y                                       
RtPri    LKOUT C,69,(D,,INXPRI),CHAR,ND=Y                                       
RtSub    LKOUT C,70,(D,,INXSUB),CHAR,ND=Y                                       
RtPVC    LKOUT C,71,(D,,INXPVC),CHAR,ND=Y                                       
RtACP    LKOUT C,72,(D,,INXACP),CHAR,ND=Y                                       
RtCSl    LKOUT C,73,(D,,INXCSL),CHAR,ND=Y                                       
RtCPy    LKOUT C,74,(D,,INXCPY),CHAR,ND=Y                                       
RtBNP    LKOUT C,75,(D,,INXBNP),CHAR,ND=Y                                       
RtPWC    LKOUT C,76,(D,,INXPWC),CHAR,ND=Y                                       
RtMIO    LKOUT C,77,(D,,INXMIO),CHAR,ND=Y                                       
RtDMO    LKOUT C,78,(D,,INXDMO),CHAR,ND=Y                                       
RtPPC    LKOUT C,79,(D,,INXPPC),CHAR,ND=Y                                       
RtACH    LKOUT C,80,(D,,INXACH),CHAR,ND=Y                                       
RtDAC    LKOUT C,81,(D,,INXDAC),CHAR,ND=Y                                       
RtDAG    LKOUT C,82,(D,,INXDAG),CHAR,ND=Y                                       
RtGAO    LKOUT C,83,(D,,INXGAO),CBIN,ND=Y                                       
RtCTO    LKOUT C,84,(D,,INXCTO),CHAR,ND=Y                                       
RtPHO    LKOUT C,85,(D,,INXPHO),UBIN,ND=Y                                       
RtTOO    LKOUT C,86,(D,,INXTOO),CBIN,ND=Y                                       
RtHOO    LKOUT C,87,(D,,INXHOO),CBIN,ND=Y                                       
RtUrO    LKOUT C,88,(D,,INXURO),CHAR,ND=Y                                       
RtDCO    LKOUT C,89,(D,,INXDCO),CHAR,ND=Y                                       
RtDAO    LKOUT C,90,(D,,INXDAO),CHAR,ND=Y                                       
RtDLO    LKOUT C,91,(D,,INXDLO),CHAR,ND=Y                                       
RtFCO    LKOUT C,92,(D,,INXFCO),CBIN,ND=Y                                       
RtASO    LKOUT C,93,(D,,INXASO),CHAR,ND=Y                                       
RtPTO    LKOUT C,94,(D,,INXPTO),CHAR,ND=Y                                       
RtPLO    LKOUT C,95,(D,,INXPLO),CHAR,ND=Y                                       
RtPUO    LKOUT C,96,(D,,INXPUO),CHAR,ND=Y                                       
RtHWO    LKOUT C,97,(D,,INXHWO),CBIN,ND=Y                                       
RtCRO    LKOUT C,98,(D,,INXCRO),CHAR,ND=Y                                       
RtDPO    LKOUT C,99,(D,,INXDPO),CHAR,ND=Y                                       
RtGPO    LKOUT C,100,(D,,INXGPO),CHAR,ND=Y                                      
RtRPO    LKOUT C,101,(D,,INXRPO),CHAR,ND=Y                                      
RtPUO    LKOUT C,102,(D,,INXPRO),CHAR,ND=Y                                      
RtCHO    LKOUT C,103,(D,,INXCHO),CHAR,ND=Y                                      
RtFEO    LKOUT C,104,(D,,INXFEO),CBIN,ND=Y                                      
RtCSO    LKOUT C,105,(D,,INXCSO),CHAR,ND=Y                                      
RtCAO    LKOUT C,106,(D,,INXCAO),CBIN,ND=Y                                      
RtSOO    LKOUT C,107,(D,,INXSOO),UBIN,ND=Y                                      
RtNIO    LKOUT C,108,(D,,INXNIO),CHAR,ND=Y                                      
RtGEO    LKOUT C,109,(D,,INXGEO),CHAR,ND=Y                                      
RtQUI    LKOUT C,110,(D,,INXQUI),CHAR,ND=Y                                      
RtQSI    LKOUT C,111,(D,,INXQSI),CHAR,ND=Y                                      
RtQDt    LKOUT C,112,(D,,INXQDT),CHAR,ND=Y                                      
RtQTm    LKOUT C,113,(D,,INXQTM),CHAR,ND=Y                                      
RtBDt    LKOUT C,114,(D,,INXBDT),CHAR,ND=Y                                      
RtBTy    LKOUT C,115,(D,,INXBTY),UBIN,ND=Y                                      
RtCCR    LKOUT C,116,(D,,INXCCR),CBIN,ND=Y                                      
RtBTo    LKOUT C,117,(D,,INXBTO),CBIN,ND=Y                                      
RtBPT    LKOUT C,118,(D,,INXBPT),CBIN,ND=Y                                      
RtBHA    LKOUT C,119,(D,,INXBHA),CBIN,ND=Y                                      
RtBCH    LKOUT C,120,(D,,INXBCH),CBIN,ND=Y                                      
RtBCS    LKOUT C,121,(D,,INXBCS),CBIN,ND=Y                                      
RtBFC    LKOUT C,122,(D,,INXBFC),CBIN,ND=Y                                      
RtBCG    LKOUT C,123,(D,,INXBGC),CBIN,ND=Y                                      
RtBAC    LKOUT C,124,(D,,INXBAC),CBIN,ND=Y                                      
RtBSF    LKOUT C,125,(D,,INXBSF),CBIN,ND=Y                                      
RtETo    LKOUT C,126,(D,,INXETO),CBIN,ND=Y                                      
RtEPT    LKOUT C,127,(D,,INXEPT),CBIN,ND=Y                                      
RtEHA    LKOUT C,128,(D,,INXEHA),CBIN,ND=Y                                      
RtECH    LKOUT C,129,(D,,INXECH),CBIN,ND=Y                                      
RtEFe    LKOUT C,130,(D,,INXEFE),CBIN,ND=Y                                      
RtCer    LKOUT C,131,(D,,INXCER),CHAR,ND=Y                                      
RtCed    LKOUT C,132,(D,,INXCED),CHAR,ND=Y                                      
RtPHl    LKOUT C,133,(D,,INXPHL),CHAR,ND=Y                                      
RtPPD    LKOUT C,134,(D,,INXPPD),CHAR,ND=Y                                      
RtPHR    LKOUT C,135,(D,,INXPHR),CHAR,ND=Y                                      
RtFPA    LKOUT C,136,(D,,INXFPA),CHAR,ND=Y                                      
RtRPH    LKOUT C,137,(D,,INXRPH),CHAR,ND=Y                                      
RtEWN    LKOUT C,138,(D,,INXEWN),CBIN,ND=Y                                      
RtEWI    LKOUT C,139,(D,,INXEWI),CHAR,ND=Y                                      
RtEPi    LKOUT C,140,(D,,INXEPI),CHAR,ND=Y                                      
RtDuD    LKOUT C,141,(D,,INXDUD),CHAR,ND=Y                                      
RtPDD    LKOUT C,142,(D,,INXPDD),CHAR,ND=Y                                      
RtDCD    LKOUT C,143,(D,,INXDCD),CHAR,ND=Y                                      
RtODD    LKOUT C,144,(D,,INXODD),CHAR,ND=Y                                      
RtCDt    LKOUT C,145,(D,,INXCDT),CHAR,ND=Y                                      
RtCRu    LKOUT C,146,(D,,INXCRU),CHAR,ND=Y                                      
RtCRD    LKOUT C,147,(D,,INXCRD),CHAR,ND=Y                                      
RtCRu    LKOUT C,148,(D,,INXCDF),CHAR,ND=Y                                      
RtWID    LKOUT C,149,(D,,INXWID),CHAR,ND=Y                                      
RtBPS    LKOUT C,150,(D,,INXBPS),CBIN,ND=Y                                      
RtEuP    LKOUT C,151,(D,,INXEUP),CHAR,ND=Y                                      
RtEGr    LKOUT C,152,(D,,INXEUPGR),CBIN,ND=Y                                    
RtEAC    LKOUT C,153,(D,,INXEUPAC),CBIN,ND=Y                                    
RtEGC    LKOUT C,154,(D,,INXEUPGC),CBIN,ND=Y                                    
RtEIA    LKOUT C,155,(D,,INXEUPIA),CBIN,ND=Y                                    
RtECA    LKOUT C,156,(D,,INXEUPCA),CBIN,ND=Y                                    
RtERE    LKOUT C,157,(D,,INXEUPRE),CBIN,ND=Y                                    
RtESP    LKOUT C,158,(D,,INXEUPSP),CBIN,ND=Y                                    
RtEMD    LKOUT C,159,(D,,INXEUPMD),CBIN,ND=Y                                    
RtENH    LKOUT C,160,(D,,INXEUPNH),CBIN,ND=Y                                    
RtEHW    LKOUT C,161,(D,,INXEUPHW),CBIN,ND=Y                                    
RtEIR    LKOUT C,162,(D,,INXEUPIR),CHAR,ND=Y                                    
RtEUD    LKOUT C,163,(D,,INXEUPUD),CBIN,ND=Y                                    
RtETo    LKOUT C,164,(D,,INXEUBTO),CBIN,ND=Y                                    
RtEPT    LKOUT C,165,(D,,INXEUBPT),CBIN,ND=Y                                    
RtEHA    LKOUT C,166,(D,,INXEUBHA),CBIN,ND=Y                                    
RtECH    LKOUT C,167,(D,,INXEUBCH),CBIN,ND=Y                                    
RtECS    LKOUT C,168,(D,,INXEUBCS),CBIN,ND=Y                                    
RtEFC    LKOUT C,169,(D,,INXEUBFC),CBIN,ND=Y                                    
RtECG    LKOUT C,170,(D,,INXEUBGC),CBIN,ND=Y                                    
RtEAC    LKOUT C,171,(D,,INXEUBAC),CBIN,ND=Y                                    
RtESF    LKOUT C,172,(D,,INXEUBSF),CBIN,ND=Y                                    
RtEPS    LKOUT C,173,(D,,INXEUBPS),CBIN,ND=Y                                    
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR INVOICE COMMENT RECORDS                        *         
***********************************************************************         
                                                                                
ARYINC   LKOUT A,(R,NXTINC),MULTIROW=Y,ROWNAME=INCVALS                          
                                                                                
RtIC1    LKOUT C,1,(D,,INCCM1),CHAR,ND=Y                                        
RtIC2    LKOUT C,2,(D,,INCCM2),CHAR,ND=Y                                        
RtIC3    LKOUT C,3,(D,,INCCM3),CHAR,ND=Y                                        
RtIC4    LKOUT C,4,(D,,INCCM4),CHAR,ND=Y                                        
RtIC5    LKOUT C,5,(D,,INCCM5),CHAR,ND=Y                                        
RtIC6    LKOUT C,6,(D,,INCCM6),CHAR,ND=Y                                        
RtIC7    LKOUT C,7,(D,,INCCM7),CHAR,ND=Y                                        
RtIC8    LKOUT C,8,(D,,INCCM8),CHAR,ND=Y                                        
RtIC9    LKOUT C,9,(D,,INCCM9),CHAR,ND=Y                                        
RtICA    LKOUT C,10,(D,,INCC10),CHAR,ND=Y                                       
RtICB    LKOUT C,11,(D,,INCC11),CHAR,ND=Y                                       
RtICC    LKOUT C,12,(D,,INCC12),CHAR,ND=Y                                       
RtICD    LKOUT C,13,(D,,INCC13),CHAR,ND=Y                                       
RtICE    LKOUT C,14,(D,,INCC14),CHAR,ND=Y                                       
RtICF    LKOUT C,15,(D,,INCC15),CHAR,ND=Y                                       
RtICG    LKOUT C,16,(D,,INCC16),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR FIXED CYCLE SEARCH STATUS RECORD               *         
***********************************************************************         
                                                                                
ARYFCS   LKOUT A,(R,NXTFCS),ROWNAME=FCSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,FCSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR FIXED CYCLE EXPANDED DETAILS RECORDS           *         
***********************************************************************         
                                                                                
ARYFCX   LKOUT A,(R,NXTFCX),MULTIROW=Y,ROWNAME=FCXVALS                          
                                                                                
RtSeq    LKOUT C,9,(D,,FCXSEQ),CHAR,ND=Y                                        
RtCyS    LKOUT C,10,(D,,FCXCYS),CHAR,ND=Y                                       
RtCyE    LKOUT C,11,(D,,FCXCYE),CHAR,ND=Y                                       
RtScl    LKOUT C,12,(D,,FCXSCL),CBIN,ND=Y                                       
RtApl    LKOUT C,13,(D,,FCXAPL),CBIN,ND=Y                                       
RtBal    LKOUT C,14,(D,,FCXBAL),CBIN,ND=Y                                       
RtTrk    LKOUT C,15,(D,,FCXTRK),CHAR,ND=Y                                       
RtGua    LKOUT C,16,(D,,FCXGUA),CHAR,ND=Y                                       
RtDlr    LKOUT C,17,(D,,FCXDLR),CHAR,ND=Y                                       
RtUse    LKOUT C,18,(D,,FCXUSE),CHAR,ND=Y                                       
RtInv    LKOUT C,19,(D,,FCXINV),CHAR,ND=Y                                       
RtGRR    LKOUT C,20,(D,,FCXGRR),CHAR,ND=Y                                       
RtHfn    LKOUT C,21,(D,,FCXHFN),CHAR,ND=Y                                       
RtSrt    LKOUT C,22,(D,,FCXSRT),UBIN,ND=Y                                       
RtFTC    LKOUT C,23,(D,,FCXFTC),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR COMMENT STATUS RECORD                          *         
***********************************************************************         
                                                                                
ARYCMS   LKOUT A,(R,NXTCMS),ROWNAME=CMSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,CMSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR COMMENT EXPANDED DETAILS RECORDS               *         
***********************************************************************         
                                                                                
ARYCMX   LKOUT A,(R,NXTCMX),MULTIROW=Y,ROWNAME=CMXVALS                          
                                                                                
RtLev    LKOUT C,4,(D,,CMXLEV),CHAR,ND=Y                                        
RtVer    LKOUT C,9,(D,,CMXVER),UBIN,ND=Y                                        
RtCm1    LKOUT C,16,(D,,CMXCM1),CHAR,ND=Y                                       
RtCm2    LKOUT C,17,(D,,CMXCM2),CHAR,ND=Y                                       
RtCm3    LKOUT C,18,(D,,CMXCM3),CHAR,ND=Y                                       
RtCm4    LKOUT C,19,(D,,CMXCM4),CHAR,ND=Y                                       
RtCm5    LKOUT C,20,(D,,CMXCM5),CHAR,ND=Y                                       
RtCm6    LKOUT C,21,(D,,CMXCM6),CHAR,ND=Y                                       
RtCm7    LKOUT C,22,(D,,CMXCM7),CHAR,ND=Y                                       
RtCm8    LKOUT C,23,(D,,CMXCM8),CHAR,ND=Y                                       
RtCm9    LKOUT C,24,(D,,CMXCM9),CHAR,ND=Y                                       
RtCmA    LKOUT C,25,(D,,CMXCMA),CHAR,ND=Y                                       
RtCmB    LKOUT C,26,(D,,CMXCMB),CHAR,ND=Y                                       
RtCmC    LKOUT C,27,(D,,CMXCMC),CHAR,ND=Y                                       
RtCmD    LKOUT C,28,(D,,CMXCMD),CHAR,ND=Y                                       
RtCmE    LKOUT C,29,(D,,CMXCME),CHAR,ND=Y                                       
RtCmF    LKOUT C,30,(D,,CMXCMF),CHAR,ND=Y                                       
RtCmG    LKOUT C,31,(D,,CMXCMG),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
                                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
                                                                                
ADCONS   DS    0A                  ** RELOCATED ADDRESS CONOTS **               
ASTFKEYT DS    A                   A(STAFF RECORD KEY DRIVER)                   
ACLIKEYT DS    A                   A(CLIENT RECORD KEY DRIVER)                  
ACGRKEYT DS    A                   A(CLIENT GROUP PASSIVE KEY DRIVER)           
AACMKEYR DS    A                   A(ATC COMMENT RECORD DRIVER)                 
ATCMKEYR DS    A                   A(TPC COMMENT RECORD DRIVER)                 
AHCMKEYR DS    A                   A(HISTORY COMMENT DRIVER)                    
ADCONN   EQU   (*-ADCONS)/L'ADCONS                                              
                                                                                
FFFS     DS    XL4                                                              
                                                                                
STVDLD   DS    AL2                 STAFF VALIDATION DOWNLOAD EQUATE             
STFDLD   DS    AL2                 STAFF DOWNLOAD EQUATE                        
RELDLD   DS    AL2                 RELEASE DOWNLOAD EQUATE                      
W4SDLD   DS    AL2                 W4 SEARCH DOWNLOAD EQUATE                    
ANSDLD   DS    AL2                 AGENT SEARCH DOWNLOAD                        
                                                                                
CLIRANGE DS    XL(L'TAVACLI*2)     CLIENT RANGE                                 
VERRANGE DS    XL(L'TLCMVER*2)     VERSION RANGE                                
ALLRANG1 DS    XL2                 ALL RANGE FOR 1 BYTE                         
ALLRANG6 DS    XL12                ALL RANGE FOR 6 BYTES                        
                                                                                
USELST   DS    XL(USELSTL)         USE TYPE LIST                                
                                                                                
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
*** REQUEST VALUES ***                                                          
                                                                                
* INITIAL DOWNLOAD 2                                                            
                                                                                
REQVALS  DS    0F                  ** REQUEST VALUES **                         
RQI2STF  DS    CL8                 STAFF CODE                                   
RQI2PSW  DS    CL8                 PASSWORD                                     
                                                                                
* COMMERCIAL TYPE DOWNLOAD                                                      
                                                                                
         ORG   REQVALS                                                          
RQCTYLNQ EQU   *-REQVALS                                                        
                                                                                
* STAFF DOWNLOAD                                                                
                                                                                
         ORG   REQVALS                                                          
RQUSRID  DS    CL8                 USER ID                                      
RQSTFCD  DS    CL8                 STAFF CODE                                   
RQSTFPW  DS    CL8                 STAFF PASSWORD                               
RQSTRST  DS    CL1                 RETURN STAFF SEARCH STATUS?                  
RQSTFLNQ EQU   *-REQVALS                                                        
                                                                                
* RELEASE DOWNLOAD                                                              
                                                                                
         ORG   REQVALS                                                          
RQRLMOD  DS    X                   MODE                                         
RQRLRTV  EQU   1                   RETRIEVE                                     
RQRLVFY  EQU   2                   VERIFY                                       
RQRLEXE  EQU   3                   EXECUTE                                      
RQRLSTF  DS    CL8                 STAFF CODE                                   
RQRLAGY  DS    CL6                 AGENCY                                       
RQRLCLI  DS    CL6                 CLIENT                                       
RQRLPRD  DS    CL6                 PRODUCT                                      
RQRLCID  DS    CL12                COMMERCIAL ID                                
RQRLCOM  DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQRLCYS  DS    PL3                 CYCLE START                                  
RQRLCYE  DS    PL3                 CYCLE END                                    
RQRLCIN  DS    XL1                 CAST SEQUENCE COUNT                          
ARQRLCS  DS    AL3                 ADDRESS OF CAST SEQUENCE LIST                
RQRLEIN  DS    XL1                 ERROR OVERRIDE COUNT                         
ARQRLEO  DS    AL3                 ADDRESS OF ERROR OVERRIDE FIELD LIST         
                                                                                
* W4 SEARCH DOWNLOAD                                                            
                                                                                
         ORG   REQVALS                                                          
RQW4SSTF DS    CL8                 STAFF                                        
RQW4SSIN DS    XL1                 SSN FILTER COUNT                             
ARQW4SSF DS    AL3                 A(SSN FILTER LIST)                           
RQW4SSSN DS    CL9                 SOCIAL SECURITY NUMBER                       
RQW4SLNM DS    CL16                LAST NAME                                    
RQW4SFNM DS    CL16                FIRST NAME                                   
RQW4STIN DS    XL1                 TYPE FILTER COUNT                            
ARQW4STF DS    AL3                 A(TYPE FILTER LIST)                          
RQW4SCIN DS    XL1                 INT COM NUMBER FILTER COUNT                  
ARQW4SCF DS    AL3                 A(INT COM NUMBER FILTER LIST)                
RQW4SSKA DS    CL1                 SKIP ACCESS CHECK?                           
RQW4SKIN DS    XL1                 TRACK FILTER COUNT                           
ARQW4TRK DS    AL3                 A(TRACK FILTER LIST)                         
RQW4SES9 DS    CL1                 EXCLUDE SSNS THAT BEGIN WITH 9?              
RQW4SLM  DS    H                   LIMIT RESULTS TO ...                         
                                                                                
* AGENT SEARCH DOWNLOAD                                                         
                                                                                
         ORG   REQVALS                                                          
RQANSSTF DS    CL8                 STAFF                                        
RQANSAIN DS    XL1                 AGENT CODE COUNT                             
ARQANSAF DS    AL3                 ADDRESS OF AGENT CODE LIST                   
RQANSAGT DS    CL4                 AGENT CODE                                   
RQANSNAM DS    CL36                AGENT NAME                                   
RQANSAOV DS    CL1                 ALLOWABLE OVERRIDES?                         
RQANSSUP DS    CL1                 SUPER SEARCH?                                
RQANSLM  DS    H                   LIMIT RESULTS TO ...                         
                                                                                
* CAST SEARCH DOWNLOAD                                                          
                                                                                
         ORG   REQVALS                                                          
RQCASSTF DS    CL8                 STAFF                                        
RQCASCIN DS    XL1                 INTERNAL COMMERCIAL NUMBER COUNT             
ARQCACOM DS    AL3                 A(INT COM NUM FILTER LIST)                   
RQCASCOM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQCASAGY DS    CL6                 AGENCY CODE                                  
RQCASCLI DS    CL6                 CLIENT CODE                                  
RQCASPRD DS    CL6                 PRODUCT CODE                                 
RQCASCID DS    CL12                COMMERCIAL ID                                
RQCASSEQ DS    XL2                 SEQUENCE NUMBER                              
RQCASSSN DS    CL9                 SOCIAL SECURITY NUMBER                       
RQCASLNM DS    CL16                LAST NAME                                    
RQCASFNM DS    CL16                FIRST NAME                                   
RQCASONO DS    CL3                 ON/OFF CAMERA                                
RQCASUIN DS    XL1                 UNION FILTER COUNT                           
ARQCAUNI DS    AL3                 A(UNION FILTER LIST)                         
RQCASAGT DS    CL4                 AGENT CODE                                   
RQCASGUA DS    CL4                 GUARANTEE CODE                               
RQCASTIN DS    XL1                 TRACK FILTER COUNT                           
ARQCATRK DS    AL3                 A(TRACK FILTER LIST)                         
RQCASMST DS    CL1                 ON MASTER COMMERCIAL?                        
RQCASLFT DS    CL1                 ON LIFT VERSION?                             
RQCASVER DS    XL1                 ON VERSION X?                                
RQCASREL DS    CL1                 RELEASED?                                    
RQCASCOT DS    CL36                COMMERCIAL TITLE                             
RQCASORE DS    CL1                 CHECK COMM'L PAID REUSE?                     
RQCASACO DS    XL4                 AFM CONTRACT INTERNAL COMM'L NUMBER          
RQCASWIN DS    XL1                 W4 TYPE FILTER COUNT                         
ARQCAW4T DS    AL3                 A(W4 TYPE FILTER LIST)                       
RQCASLNI DS    XL1                 LAST NAME SEARCH INSTRUCTIONS                
RQCASFNI DS    CL1                 FIRST NAME SEARCH INSTRUCTIONS               
RQCASCGR DS    CL1                 CATEGORY GROUPING                            
RQCASRCT DS    CL1                 RETURN COMMERCIAL TITLE?                     
RQCASCGA DS    CL6                 CHECK GRT VALID FOR AGENCY ...               
RQCASCGC DS    CL6                 CHECK GRT VALID FOR CLIENT ...               
RQCASEP6 DS    CL1                 EXCLUDE PRE-06 CONTRACT YEARS?               
RQCASEMU DS    CL1                 EXCLUDE MUSICIANS?                           
RQCASPAD DS    CL1                 RETURN VERSION PAID STATUS?                  
RQCASES9 DS    CL1                 EXCLUDE SSNS THAT BEGIN WITH 9?              
RQCASELA DS    CL1                 EXCLUDE LOCKED AGENCIES?                     
RQCASLIM DS    H                   LIMIT RESULTS TO ...                         
         ORG                                                                    
                                                                                
* INVOICE SEARCH DOWNLOAD                                                       
                                                                                
         ORG   REQVALS                                                          
RQINSSTF DS    CL8                 STAFF                                        
RQINSAGY DS    CL6                 AGENCY CODE                                  
RQINSINV DS    XL6                 INVOICE NUMBER                               
RQINSCOM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQINSVER DS    XL1                 VERSION NUMBER                               
RQINSCNR DS    XL1                 RETURN CANCELLER INVOICES?                   
RQINSCNL DS    XL1                 RETURN CANCELLED INVOICES?                   
RQINSWID DS    CL30                WEB APPLICATION ID                           
RQINSSEQ DS    XL2                 SEQUENCE NUMBER                              
         ORG                                                                    
                                                                                
* FIXED CYCLE SEARCH DOWNLOAD                                                   
                                                                                
         ORG   REQVALS                                                          
RQFCSSTF DS    CL8                 STAFF                                        
RQFCSCOM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQFCSSEQ DS    XL2                 CAST SEQUENCE NUMBER                         
RQFCSCYS DS    XL3                 CYCLE START DATE                             
RQFCSCYE DS    XL3                 CYCLE END DATE                               
RQFCSMIC DS    CL1                 INCLUDE FIRST ADJUSTEMENT COMMENT            
         ORG                                                                    
                                                                                
* COMMENT SEARCH DOWNLOAD                                                       
                                                                                
         ORG   REQVALS                                                          
RQCMSSTF DS    CL8                 STAFF                                        
RQCMSLEV DS    CL1                 LEVEL                                        
RQCMLEVC EQU   C'C'                CLIENT                                       
RQCMLEVT EQU   C'T'                TPC                                          
RQCMSTYP DS    CL1                 TYPE                                         
RQCMSSSN DS    CL9                 SOCIAL SECURITY NUMBER                       
RQCMSGUA DS    CL4                 GUARANTEE CODE                               
RQCMSCID DS    CL12                COMMERCIAL ID                                
RQCMSVER DS    XL1                 VERSION                                      
RQCMSAGY DS    CL6                 AGENCY CODE                                  
RQCMSCON DS    CL12                CONTRACT ID                                  
RQCMSTST DS    PL3                 TERM START DATE                              
RQCMSTEN DS    PL3                 TERM END DATE                                
RQCMSINV DS    CL6                 INVOICE NUMBER                               
RQCMSADV DS    CL6                 ADVICE NUMBER                                
         ORG                                                                    
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
*** OUTPUT VALUES ***                                                           
                                                                                
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
                                                                                
* ** STAFF NAME RECORD **                                                       
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
NAMVALS  DS    0X                                                               
NAMNAME  DS    CL25                NAME                                         
NAMVALL  EQU   *-NAMVALS                                                        
                                                                                
* ** COMMERCIAL TYPE RECORD **                                                  
                                                                                
COTVALS  DS    0X                                                               
COTCODE  DS    CL1                 CODE                                         
COTDESC  DS    CL12                DESCRIPTION                                  
COTVALL  EQU   *-COTVALS                                                        
                                                                                
* ** LIMITED AGENCIES RECORD **                                                 
                                                                                
ALMVALS  DS    0X                                                               
ALMAGY   DS    CL6                 AGENCY                                       
ALMVALL  EQU   *-ALMVALS                                                        
                                                                                
* ** LIMITED CLIENTS RECORD **                                                  
                                                                                
CLMVALS  DS    0X                                                               
CLMCLI   DS    CL6                 CLIENT                                       
CLMVALL  EQU   *-ALMVALS                                                        
                                                                                
* ** STAFF SEARCH STATUS **                                                     
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
STSVALS  DS    0X                                                               
STSSTAT  DS    CL1                 STATUS                                       
STSTEMB  EQU   0                   SUCESSSFUL   - EMBEDDED DOWNLOAD             
STSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
STSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
STSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
STSVALL  EQU   *-STSVALS                                                        
                                                                                
* ** STAFF RECORD *                                                             
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
STFVALS  DS    0X                                                               
STAFFUID DS    CL(L'RQUSRID)       USER ID                                      
STAFFCD  DS    CL(L'TLSTSTAF)      STAFF CODE                                   
STAFFFN  DS    CL(L'TASTFST)       STAF FIRST NAME                              
STAFFLN  DS    CL(L'TASTLST)       STAFF LAST NAME                              
STAFFTP  DS    CL(L'TASTTYPE)      STAFF TYPE                                   
STAFFEML DS    CL50                STAFF EMAIL                                  
STAFFST1 DS    XL1                 WEB ACCESS STATUS 1 (CIHR)                   
STAFFST2 DS    XL1                 WEB ACCESS STATUS 2 (ACTION FORMS)           
STAFFST3 DS    XL1                 WEB ACCESS STATUS 3                          
STAFFST4 DS    XL1                 WEB ACCESS STATUS 4                          
STAFFST5 DS    XL1                 WEB ACCESS STATUS 5                          
STAFFST6 DS    XL1                 WEB ACCESS STATUS 6                          
STAFFST7 DS    XL1                 WEB ACCESS STATUS 7                          
STAFFST8 DS    XL1                 WEB ACCESS STATUS 8                          
STAFFPHO DS    CL12                STAFF PHONE                                  
STFVALL  EQU   *-STFVALS                                                        
                                                                                
* ** LIMIT ACCESS VALUES *                                                      
                                                                                
LIMVALS  DS    0X                                                               
LIMAGY   DS    CL(L'TAVAAGY)       AGENCY CODE                                  
LIMENT   DS    0CL(L'LIMCLI)       LIST OF LIMIT ACCESS CLIENTS                 
LIMCLI   DS    CL(L'TAVACLI)            LIMIT ACCESS CLIENT                     
LIMENTQ  EQU   *-LIMENT                                                         
         ORG   LIMENT                                                           
         DS    (LIMMAX)CL(LIMENTQ)                                              
LIMVALL  EQU   *-LIMVALS                                                        
LIMMAX   EQU   41                                                               
                                                                                
* ** RELEASE RECORD *                                                           
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
RELVALS  DS    0X                                                               
RELSTAT  DS    CL1                 STATUS                                       
RELSTOK1 EQU   1                   NO ERRORS - COMMERCIAL RELEASED              
RELSTOK2 EQU   2                   NO ERRORS - CAST LAST SERVICED               
RELSTER  EQU   3                   ERRORS                                       
RELSTAF  DS    CL8                 STAFF ID                                     
RELDATE  DS    CL8                 DATE                                         
RELVALL  EQU   *-RELVALS                                                        
                                                                                
* ** ERROR VALUES *                                                             
                                                                                
ERRVALS  DS    0X                                                               
ERRNUMB  DS    CL2                 NUMBER                                       
ERRCATY  DS    CL1                 CATEGORY                                     
ERRFILD  DS    CL1                 FIELD                                        
ERREMSG  DS    CL60                ERROR MESSAGE                                
ERRVALL  EQU   *-ERRVALS                                                        
                                                                                
* ** RELEASE COMMENT VALUES *                                                   
                                                                                
CMTVALS  DS    0X                                                               
CMTTYPE  DS    CL1                 TYPE                                         
CMTTYCOG EQU   1                   GENERAL COMMERCIAL COMMENT                   
CMTTYCOA EQU   2                   ATS COMMERCIAL COMMENT                       
CMTTYCOT EQU   3                   TPC COMMERCIAL COMMENT                       
CMTTYHST EQU   4                   HISTORY COMMENT                              
CMTCOMT  DS    CL78                COMMENT                                      
CMTVALL  EQU   *-CMTVALS                                                        
                                                                                
* ** W4 SEARCH STATUS *                                                         
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
W4SVALS  DS    0X                                                               
W4SSTAT  DS    CL1                 STATUS                                       
WSSTEMB  EQU   0                   SUCESSSFUL   - EMBEDDED DOWNLOAD             
W4SSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
W4SSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
W4SSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
W4SVALL  EQU   *-W4SVALS                                                        
                                                                                
* ** W4 EXPANDED DETAILS *                                                      
                                                                                
W4XVALS  DS    0X                                                               
W4XSSN   DS    CL9                 SSN                                          
W4XPID   DS    CL6                 PID                                          
W4XLNM   DS    CL16                LAST NAME                                    
W4XFNM   DS    CL16                FIRST NAME                                   
W4XMNM   DS    CL16                MIDDLE NAME                                  
W4XAD1   DS    CL30                ADDRESS LINE 1                               
W4XAD2   DS    CL30                ADDRESS LINE 2                               
W4XAD3   DS    CL30                ADDRESS LINE 3                               
W4XCTY   DS    CL25                CITY                                         
W4XSTA   DS    CL2                 STATE                                        
W4XZIP   DS    CL10                ZIP CODE                                     
W4XTYP   DS    CL1                 TYPE                                         
W4XSUF   DS    CL4                 SUFFIX                                       
W4XPHO   DS    CL12                PHONE NUMBER                                 
W4XAKF   DS    CL16                AKA FIRST NAME                               
W4XAKL   DS    CL16                AKA LAST NAME                                
W4XCPN   DS    CL32                CORPORATION NAME #1                          
W4XSXC   DS    CL1                 SEX CODE                                     
W4XETH   DS    CL2                 ETHNICITY                                    
W4XFIL   DS    CL4                 FILTERS                                      
W4XTFQ   DS    CL1                 TAX FREQUENCY                                
W4XCP1   DS    CL9                 CORPORATION ID #1                            
W4XCP2   DS    CL9                 CORPORATION ID #2                            
W4XCP3   DS    CL9                 CORPORATION ID #3                            
W4XCP4   DS    CL9                 CORPORATION ID #4                            
W4XCP5   DS    CL9                 CORPORATION ID #5                            
W4XCP6   DS    CL9                 CORPORATION ID #6                            
W4XIDT   DS    CL8                 INDEMINIFICATION DATE                        
W4XAFM   DS    CL3                 AFM LOCAL                                    
W4XMNU   DS    CL9                 MEMBERSHIP NUMBER                            
W4XYTD   DS    CL1                 YTD ON CHECKS?                               
W4XCKF   DS    CL1                 SORT CHECKS FIRST?                           
W4XILF   DS    CL1                 IRS LOCK ON FEDERAL WITHHOLDING?             
W4XLCK   DS    CL1                 W4 LOCKED?                                   
W4XFIW   DS    CL1                 FICA WITHHOLDING?                            
W4XDCW   DS    CL1                 DUE COMPANY WITHHOLDING?                     
W4XPEN   DS    CL1                 PENSION?                                     
W4XFAD   DS    CL1                 FOREIGN ADDRESS?                             
W4XDIR   DS    CL1                 DIRECT DEPOSIT?                              
W4XWIR   DS    CL1                 WIRE TRANSFER FOR CHECKS?                    
W4XSPL   DS    CL1                 SPECIAL LETTER ON FILE?                      
W4XFMS   DS    CL1                 FEDERAL TAX - MARRIED/SINGLE                 
W4XFEX   DS    CL3                 FEDERAL TAX - EXEMPTIONS                     
W4XFFX   DS    XL4                 FEDERAL TAX - FIXED PERCENTAGE               
W4XSAR   DS    CL3                 STATE TAX - AREA                             
W4XSMS   DS    CL1                 STATE TAX - MARRIED/SINGLE                   
W4XSEX   DS    CL3                 STATE TAX - EXEMPTIONS                       
W4XSFX   DS    XL4                 STATE TAX - FIXED PERCENTAGE                 
W4XCAR   DS    CL3                 CITY TAX - AREA                              
W4XCMS   DS    CL1                 CITY TAX - MARRIED/SINGLE                    
W4XCEX   DS    CL3                 CITY TAX - EXEMPTIONS                        
W4XCFX   DS    XL4                 CITY TAX - FIXED PERCENTAGE                  
W4XERN   DS    XL4                 EARNINGS                                     
W4XRST   DS    CL2                 RECIPROCAL STATE                             
W4XDOB   DS    CL8                 DATE OF BIRTH                                
W4XDED   DS    XL4                 DEDUCTION PERCENTAGE                         
W4XTRS   DS    CL9                 TRUSTEE SSN                                  
W4XMPR   DS    XL4                 MPR FUND PERCENTAGE                          
W4XPCH   DS    XL4                 PERMANENT CHARITY PERCENTAGE                 
W4XGST   DS    CL10                GST NUMBER                                   
W4XCMT   DS    CL70                COMMENT                                      
W4XPNM   DS    CL36                PAYEE NAME                                   
W4XPA1   DS    CL30                PAYEE ADDRESS LINE 1                         
W4XPA2   DS    CL30                PAYEE ADDRESS LINE 2                         
W4XPA3   DS    CL30                PAYEE ADDRESS LINE 3                         
W4XPA4   DS    CL30                PAYEE ADDRESS LINE 4                         
W4XPEX   DS    CL8                 PAYEE EXPIRATION DATE                        
W4XCRY   DS    CL2                 COUNTRY                                      
W4XEML   DS    CL35                EMAIL ADDRESS                                
W4XFTX   DS    CL1                 TAKE TAXES FOR FOREIGNER?                    
W4XPCY   DS    CL25                PAYEE CITY                                   
W4XPST   DS    CL2                 PAYEE STATE                                  
W4XPZP   DS    CL10                PAYEE ZIP                                    
W4XPCT   DS    CL2                 PAYEE COUNTRY                                
W4XPAC   DS    CL8                 PAYEE ACTIVE DATE                            
W4XWID   DS    CL18                WEB APPLICATION ID                           
W4XACD   DS    CL8                 LAST CHANGED DATE                            
W4XNHA   DS    CL1                 ELIGIBLE FOR NEW HIRE ACT?                   
W4XNHP   DS    CL1                 NEW HIRE ACT ELIGIBILITY PENDING?            
W4XNHD   DS    CL8                 NEW HIRE DATE                                
W4XEFT   DS    CL1                 EFT?                                         
W4XREG   DS    CL1                 REGRESSION TESTING?                          
W4XTCP   DS    CL2                 TAXABLE CANADIAN PROVINCE                    
W4XFNC   DS    XL4                 CAN FEDERAL NET CLAIM                        
W4XFD1   DS    CL1                 CAN FEDERAL DEFAULT TO CC1?                  
W4XFET   DS    CL1                 CAN FEDERAL EXEMPT?                          
W4XFPZ   DS    XL4                 CAN FEDERAL PRESCRIBED ZONE                  
W4XPNC   DS    XL4                 CAN PROVINCIAL NET CLAIM                     
W4XPD1   DS    CL1                 CAN PROVINCIAL DEFAULT TO CC1?               
W4XPET   DS    CL1                 CAN PROVINCIAL EXEMPT?                       
W4XPPZ   DS    XL4                 CAN PROVINCIAL PRESCRIBED ZONE               
W4XPHD   DS    XL4                 CAN PROVINCIAL HOUSING DEDUCTION             
W4XPSP   DS    XL4                 CAN PROVINCIAL SUPPORT PAYMENT               
W4XPHC   DS    CL1                 CAN PROVINCIAL EXEMPT HEALT CONT?            
W4XFEC   DS    CL1                 CAN FEDERAL EXEMPT FROM CPP?                 
W4XVALL  EQU   *-W4XVALS                                                        
                                                                                
* ** W4 SEARCH LIMIT STATUS *                                                   
                                                                                
W4LVALS  DS    0X                                                               
W4LSTAT  DS    CL1                 STATUS                                       
W4LVALL  EQU   *-W4LVALS                                                        
                                                                                
* ** AGENT SEARCH STATUS *                                                      
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
ANSVALS  DS    0X                                                               
ANSSTAT  DS    CL1                 STATUS                                       
ANSSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
ANSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
ANSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
ANSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
ANSVALL  EQU   *-ANSVALS                                                        
                                                                                
* ** AGENT SEARCH DETAILS *                                                     
                                                                                
ANDVALS  DS    0X                                                               
ANDCOD   DS    CL4                 CODE                                         
ANDNAME  DS    CL36                NAME                                         
ANDSNAM  DS    CL16                SHORT NAME                                   
ANDADD1  DS    CL30                ADDRESS 1                                    
ANDADD2  DS    CL30                ADDRESS 2                                    
ANDADD3  DS    CL30                ADDRESS 3                                    
ANDADD4  DS    CL30                ADDRESS 4                                    
ANDEMAL  DS    CL40                EMAIL ADDRESS                                
ANDATTN  DS    CL36                ATTENTION NAME                               
ANDSSN   DS    CL9                 SOCIAL SECURITY NUMBER                       
ANDPHON  DS    CL12                PHONE NUMBER                                 
ANDFAXN  DS    CL12                FAX NUMBER                                   
ANDISAG  DS    CL1                 IGNORE ON SAG CHECKS                         
ANDAOVR  DS    CL1                 ALLOWABLE OVERRIDE                           
ANDOTRC  DS    CL4                 OLD T&R CODE                                 
ANDVALL  EQU   *-ANDVALS                                                        
                                                                                
* ** AGENT SEARCH LIMIT STATUS *                                                
                                                                                
ANLVALS  DS    0X                                                               
ANLSTAT  DS    CL1                 STATUS                                       
ANLVALL  EQU   *-ANLVALS                                                        
                                                                                
* ** CAST SEARCH STATUS *                                                       
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
CASVALS  DS    0X                                                               
CASSTAT  DS    CL1                 STATUS                                       
CASSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
CASSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
CASSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
CASSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
CASSDAT  DS    CL8                 SEARCH DATE                                  
CASSTIM  DS    CL8                 SEARCH TIME                                  
CASSTST  DS    CL1                 TRACK'S AFM# MATCHES COMMERCIAL ID?          
CASSTS1  EQU   1                   SUCCESSFUL - TRACK FOUND                     
CASSTS2  EQU   2                   UNSUCCESSFUL - TRACK NOT FOUND               
CASSTS3  EQU   3                   UNSUCCESSFUL - AFM# NEQ CID                  
CASVALL  EQU   *-CASVALS                                                        
                                                                                
* ** CAST EXPANDED DETAILS *                                                    
                                                                                
CAXVALS  DS    0X                                                               
CAXCOM   DS    CL8                 INTERNAL COMMERCIAL NUMBER                   
CAXAGY   DS    CL6                 AGENCY CODE                                  
CAXCLI   DS    CL6                 CLIENT CODE                                  
CAXPRD   DS    CL6                 PRODUCT CODE                                 
CAXCID   DS    CL12                COMMERCIAL ID                                
CAXSEQ   DS    CL4                 SEQUENCE NUMBER                              
CAXVSQ   DS    CL4                 VITA CAST SEQUENCE NUMBER                    
CAXSSN   DS    CL9                 SOCIAL SECURITY NUMBER                       
CAXLNM   DS    CL16                LAST NAME                                    
CAXFNM   DS    CL16                FIRST NAME                                   
CAXCNM   DS    CL32                CORPORATION NAME                             
CAXCAT   DS    CL3                 CATEGORY                                     
CAXLCO   DS    CL8                 LIFTED FROM INTERNAL COMM'L NUMBER           
CAXLAY   DS    CL6                 LIFTED FROM AGENCY                           
CAXLCI   DS    CL12                LIFTED FROM COMMERCIAL ID                    
CAXONO   DS    CL3                 ON/OFF CAMERA                                
CAXTAX   DS    CL3                 TAX UNIT CODE                                
CAXUNI   DS    CL3                 UNION                                        
CAXLCL   DS    CL3                 LOCAL                                        
CAXCYR   DS    CL3                 CONTRACT YEAR                                
CAXFFC   DS    CL8                 FIRST FIXED CYCLE DATE                       
CAXFSV   DS    CL8                 FIRST SERVICES DATE                          
CAXLSV   DS    CL8                 LAST SERVICES DATE                           
CAXRLL   DS    CL1                 RELEASE LETTER                               
CAXRLD   DS    CL8                 RELEASE LETTER DATE                          
CAXEFF   DS    CL8                 EFFECTIVE DATE                               
CAXPRL   DS    CL1                 PREVIOUS RELEASE LETTER                      
CAXPLD   DS    CL8                 PREVIOUS RELEASE LETTER DATE                 
CAXATC   DS    CL9                 ATTACHED CORPORATION FEDERIAL ID#            
CAXATN   DS    CL32                ATTACHED CORPORATION NAME                    
CAXAGT   DS    CL4                 AGENT CODE                                   
CAXAGN   DS    CL36                AGENT NAME                                   
CAXGUA   DS    CL4                 GUARANTEE CODE                               
CAXEXP   DS    CL8                 EXPIRATION DATE                              
CAXUP1   DS    CL3                 OVERSCALE PERCENTAGE USE #1                  
CAXOP1   DS    XL4                 OVERSCALE PERCENTAGE #1                      
CAXOPLNQ EQU   *-CAXUP1                                                         
CAXUP2   DS    CL3                 OVERSCALE PERCENTAGE USE #2                  
CAXOP2   DS    XL4                 OVERSCALE PERCENTAGE #2                      
CAXUP3   DS    CL3                 OVERSCALE PERCENTAGE USE #3                  
CAXOP3   DS    XL4                 OVERSCALE PERCENTAGE #3                      
CAXUP4   DS    CL3                 OVERSCALE PERCENTAGE USE #4                  
CAXOP4   DS    XL4                 OVERSCALE PERCENTAGE #4                      
CAXUP5   DS    CL3                 OVERSCALE PERCENTAGE USE #5                  
CAXOP5   DS    XL4                 OVERSCALE PERCENTAGE #5                      
CAXUP6   DS    CL3                 OVERSCALE PERCENTAGE USE #6                  
CAXOP6   DS    XL4                 OVERSCALE PERCENTAGE #6                      
CAX2OP   DS    XL4                 SECOND OVERSCALE PERCENTAGE                  
CAXDBL   DS    CL1                 DOUBLES                                      
CAXUA1   DS    CL3                 OVERSCALE AMOUNT USE #1                      
CAXOA1   DS    XL4                 OVERSCALE AMOUNT #1                          
CAXOALNQ EQU   *-CAXUA1                                                         
CAXUA2   DS    CL3                 OVERSCALE AMOUNT USE #2                      
CAXOA2   DS    XL4                 OVERSCALE AMOUNT #2                          
CAXUA3   DS    CL3                 OVERSCALE AMOUNT USE #3                      
CAXOA3   DS    XL4                 OVERSCALE AMOUNT #3                          
CAXUA4   DS    CL3                 OVERSCALE AMOUNT USE #4                      
CAXOA4   DS    XL4                 OVERSCALE AMOUNT #4                          
CAXUA5   DS    CL3                 OVERSCALE AMOUNT USE #5                      
CAXOA5   DS    XL4                 OVERSCALE AMOUNT #5                          
CAXUA6   DS    CL3                 OVERSCALE AMOUNT USE #6                      
CAXOA6   DS    XL4                 OVERSCALE AMOUNT #6                          
CAXTR1   DS    CL1                 TRACK #1                                     
CAXTR2   DS    CL1                 TRACK #2                                     
CAXTR3   DS    CL1                 TRACK #3                                     
CAXTR4   DS    CL1                 TRACK #4                                     
CAXTR5   DS    CL1                 TRACK #5                                     
CAXTR6   DS    CL1                 TRACK #6                                     
CAXTR7   DS    CL1                 TRACK #7                                     
CAXCMT   DS    CL57                COMMENT                                      
CAXRDE   DS    CL57                ROLE DESCRIPTION                             
CAXMST   DS    CL1                 ON MASTER COMMERCIAL?                        
CAXLFT   DS    CL1                 ON LIFT VERSION?                             
CAXCPA   DS    CL1                 CHECKS PAYABLE TO AGENT?                     
CAXPCR   DS    CL1                 PAY CANADIAN RATES ON US COMMERCIAL?         
CAXPUR   DS    CL1                 PAY US RATES ON CANADIAN COMMERCIAL?         
CAXFGN   DS    CL1                 FOREIGN USE?                                 
CAXINA   DS    CL1                 INTERACTIVE USE?                             
CAXINR   DS    CL1                 INDUSTRIAL USE?                              
CAXACP   DS    CL1                 APPLY CABLE TO PER CYCLE AMOUNT?             
CAXAPP   DS    CL1                 APPLY PAX TO PER CYCLE AMOUNT?               
CAXPDT   DS    CL1                 PRINT DAILY FIXED CYCLE TRACKING?            
CAXCLB   DS    CL1                 CELEBRITY?                                   
CAXGRR   DS    CL1                 GRR COVERS ALL USE?                          
CAXPCS   DS    CL1                 PER CYCLE GUARANTEE STATUS                   
CAXPCSE  EQU   C'E'                ELIGIBLE SUBSIDIARY                          
CAXPCSI  EQU   C'I'                INELIGIBLE SUBIDIARY                         
CAXPCSP  EQU   C'P'                PRIMARY COMMERCIAL                           
CAXPCSX  EQU   C'X'                INELIGIBLE SUB - PAYMENT PENDING             
CAXREL   DS    CL1                 COMMERCIAL RELEASED?                         
CAXWID   DS    CL18                WEB APPLICATION ID                           
CAXEUR   DS    CL1                 PAID IN EUROS?                               
CAX2UP1  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #1              
CAX2OP1  DS    XL4                 2ND OVERSCALE PERCENTAGE #1                  
CAX2OLNQ EQU   *-CAX2UP1                                                        
CAX2UP2  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #2              
CAX2OP2  DS    XL4                 2ND OVERSCALE PERCENTAGE #2                  
CAX2UP3  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #3              
CAX2OP3  DS    XL4                 2ND OVERSCALE PERCENTAGE #3                  
CAX2UP4  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #4              
CAX2OP4  DS    XL4                 2ND OVERSCALE PERCENTAGE #4                  
CAX2UP5  DS    CL3                 2ND OVERSCALE PERCENTAGE USE #5              
CAX2OP5  DS    XL4                 2ND OVERSCALE PERCENTAGE #5                  
CAXRER   DS    CL8                 RERECORD DATE                                
CAXCOT   DS    CL36                COMMERCIAL TITLE                             
CAXGTY   DS    CL1                 GUARANTEE TYPE                               
CAXGAM   DS    XL4                 GUARANTEE AMOUNT                             
CAXCOX   DS    CL8                 COMMERCIAL EXPIRATION DATE                   
CAXFCE   DS    CL1                 FIXED CYCLE EXISTS?                          
CAXORE   DS    CL1                 COMM'L HAS BEEN PAID REUSE                   
CAXALL   DS    CL1                 ON ALL VERSIONS?                             
CAXAFT   DS    CL1                 APPLY USE TO FTRACKS?                        
CAXGVA   DS    CL1                 GUARANTEE VALID FOR AGENCY/CLIENT?           
CAXACO   DS    CL8                 AFM CONTRACT INT COM NUMBER                  
CAXACONC DS    CL4                 AFM CONTRACT INT COM # NOT CONVERTED         
CAXASQ   DS    CL4                 AFM CONTRACT CAST SEQ NUMBER                 
CAXTCO   DS    CL8                 TRANSFERRED INTERNAL COMM'L NUMBER           
CAXTAY   DS    CL6                 TRANSFERRED FROM AGENCY                      
CAXTCI   DS    CL12                TRANSFERRED FROM COMMERCIAL ID               
CAXTSQ   DS    CL4                 TRANSFERRED CAST SEQ NUMBER                  
CAXFDAT  DS    CL8                 FILM DATE                                    
CAXFSTU  DS    CL12                FILM STUDIO                                  
CAXFCIT  DS    CL12                FILM CITY                                    
CAXFSTA  DS    CL2                 FILM STATE                                   
CAXRDAT  DS    CL8                 RECORD DATE                                  
CAXRSTU  DS    CL12                RECORD STUDIO                                
CAXRCIT  DS    CL12                RECORD CITY                                  
CAXRSTA  DS    CL2                 RECORD STATE                                 
CAXMDAT  DS    CL8                 MUSIC DATE                                   
CAXMSTU  DS    CL12                MUSIC STUDIO                                 
CAXMCIT  DS    CL12                MUSIC CITY                                   
CAXMSTA  DS    CL2                 MUSIC STATE                                  
CAXTFDAT DS    CL8                 TRANSFERRED FROM FILM DATE                   
CAXTFSTU DS    CL12                TRANSFERRED FROM FILM STUDIO                 
CAXTFCIT DS    CL12                TRANSFERRED FROM FILM CITY                   
CAXTFSTA DS    CL2                 TRANSFERRED FROM FILM STATE                  
CAXTRDAT DS    CL8                 TRANSFERRED FROM RECORD DATE                 
CAXTRSTU DS    CL12                TRANSFERRED FROM RECORD STUDIO               
CAXTRCIT DS    CL12                TRANSFERRED FROM RECORD CITY                 
CAXTRSTA DS    CL2                 TRANSFERRED FROM RECORD STATE                
CAXLFDAT DS    CL8                 LIFTED FROM FILM DATE                        
CAXLFSTU DS    CL12                LIFTED FROM FILM STUDIO                      
CAXLFCIT DS    CL12                LIFTED FROM FILM CITY                        
CAXLFSTA DS    CL2                 LIFTED FROM FILM STATE                       
CAXLRDAT DS    CL8                 LIFTED FROM RECORD DATE                      
CAXLRSTU DS    CL12                LIFTED FROM RECORD STUDIO                    
CAXLRCIT DS    CL12                LIFTED FROM RECORD CITY                      
CAXLRSTA DS    CL2                 LIFTED FROM RECORD STATE                     
CAXADAT  DS    CL8                 AFM MUSIC DATE                               
CAXASTU  DS    CL12                AFM MUSIC STUDIO                             
CAXACIT  DS    CL12                AFM MUSIC CITY                               
CAXASTA  DS    CL2                 AFM MUSIC STATE                              
CAXCFFC  DS    CL8                 COMMERCIAL FIRST FIXED CYCLE                 
CAXEQY   DS    CL1                 EQUITY?                                      
CAXVALL  EQU   *-CAXVALS                                                        
                                                                                
* ** CAST VERSION DETAILS *                                                     
                                                                                
CAVVALS  DS    0X                                                               
CAVVER   DS    XL1                 VERSION NUMBER                               
CAVPAD   DS    CL1                 PAID?                                        
CAVVALL  EQU   *-CAVVALS                                                        
                                                                                
* ** CAST SEARCH LIMIT STATUS *                                                 
                                                                                
CALVALS  DS    0X                                                               
CALSTAT  DS    CL1                 STATUS                                       
CALVALL  EQU   *-CALVALS                                                        
                                                                                
* ** FIXED CYCLE SEARCH STATUS **                                               
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
FCSVALS  DS    0X                                                               
FCSSTAT  DS    CL1                 STATUS                                       
FCSSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
FCSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
FCSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
FCSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
FCSVALL  EQU   *-FCSVALS                                                        
                                                                                
* ** FIXED CYCLE EXPANDED DETAILS **                                            
                                                                                
FCXVALS  DS    0X                                                               
FCXSEQ   DS    CL4                 SEQUENCE NUMBER                              
FCXCYS   DS    CL8                 CYCLE START DATE                             
FCXCYE   DS    CL8                 CYCLE END DATE                               
FCXSCL   DS    F                   SCALE AMOUNT                                 
FCXAPL   DS    F                   AMOUNT TO BE APPLIED                         
FCXBAL   DS    F                   BALANCE                                      
FCXTRK   DS    CL1                 TRACKING ENABLED?                            
FCXGUA   DS    CL1                 IS A GUARANTEE?                              
FCXDLR   DS    CL1                 ADDED BY DEALER PAYMENT?                     
FCXUSE   DS    CL3                 ADDED BY USE                                 
FCXINV   DS    CL6                 ADDED BY INVOICE                             
FCXGRR   DS    CL3                 GRR COVERED USE                              
FCXHFN   DS    CL8                 HFN DATE                                     
FCXSRT   DS    H                   SORT SEQUENCE FOR VITA                       
FCXFTC   DS    CL40                FTRACK COMMENT                               
FCXVALL  EQU   *-FCXVALS                                                        
         ORG                                                                    
                                                                                
* ** INVOICE SEARCH STATUS *                                                    
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
INSVALS  DS    0X                                                               
INSSTAT  DS    CL1                 STATUS                                       
INSSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
INSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
INSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
INSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
INSVALL  EQU   *-INSVALS                                                        
                                                                                
* ** INVOICE EXPANDED DETAILS *                                                 
                                                                                
INXVALS  DS    0X                                                               
INXAGY   DS    CL6                AGENCY                                        
INXINV   DS    CL6                INVOICE NUMBER                                
INXAUI   DS    CL8                ASSIGNMENT USER ID                            
INXASI   DS    CL8                ASSIGNMENT STAFF ID                           
INXADT   DS    CL8                ASSIGNMENT DATE                               
INXATM   DS    CL8                ASSIGNMENT TIME                               
INXRAD   DS    CL1                RESERVED FOR ADVICE                           
INXRTS   DS    CL1                RESERVED FOR TIMESHEET                        
INXPUI   DS    CL8                PAYMENT USER ID                               
INXPSI   DS    CL8                PAYMENT STAFF ID                              
INXPDT   DS    CL8                PAYMENT DATE                                  
INXPTM   DS    CL8                PAYMENT TIME                                  
INXUSE   DS    CL3                USE                                           
INXTYP   DS    CL1                USE TYPE                                      
INXCLI   DS    CL6                CLIENT                                        
INXPRD   DS    CL6                PRODUCT                                       
INXPRN   DS    CL16               PRODUCT NAME                                  
INXCOM   DS    CL8                INTERNAL COMMERCIAL NUMBER                    
INXCID   DS    CL12               COMMERCIAL ID                                 
INXCTI   DS    CL36               COMMERCIAL TITLE                              
INXLEN   DS    XL1                COMMERCIAL LENGTH                             
INXMED   DS    CL1                COMMERCIAL MEDIA                              
INXVER   DS    XL1                VERSION NUMBER                                
INXVID   DS    CL12               VERSION ID                                    
INXVLN   DS    XL1                VERSION LENGTH                                
INXLID   DS    CL12               LIFT ID                                       
INXLLN   DS    XL1                LIFT LENGTH                                   
INXCYS   DS    CL8                CYCLE START DATE                              
INXCYE   DS    CL8                CYCLE END DATE                                
INXAPO   DS    CL16               AUTH/PO NUMBER                                
INXEST   DS    CL16               ESTIMATE/JOB NUMBER                           
INXPER   DS    XL1                PERIOD                                        
INXHCM   DS    CL40               HISTORY COMMENT                               
INXICM   DS    CL60               INVOICE COMMENT                               
INXTAG   DS    XL1                NUMBER OF TAGS                                
INXINS   DS    XL2                NUMBER OF INSERTS                             
INXDEM   DS    XL1                NUMBER OF DEMOS                               
INXSTU   DS    XL2                STARTING USE NUMBER                           
INXTUS   DS    XL2                TOTAL NUMBER OF USES                          
INXUNI   DS    XL2                NUMBER OF UNITS                               
INXINY   DS    CL1                INCLUDES NEW YORK?                            
INXICH   DS    CL1                INCLUDES CHICAGO?                             
INXILA   DS    CL1                INCLUDES LOS ANGELES?                         
INXPGR   DS    F                  GROSS PAYMENT AMOUNT                          
INXPAC   DS    F                  APPLIED CREDIT AMOUNT                         
INXPGC   DS    F                  APPLIED GUARANTEE AMOUNT                      
INXPIA   DS    F                  INDIVIDUALS' PAYMENT AMOUNT                   
INXPCA   DS    F                  CORPORATIONS' PAYMENT AMOUNT                  
INXPRE   DS    F                  REIMBURSED EXPENSE AMOUNT                     
INXPSP   DS    F                  SUBJECT TO P&H AMOUNT                         
INXPMD   DS    F                  MISCELLANEOUS DEDUCTION AMOUNT                
INXPNH   DS    F                  PENSION AND HEALTH AMOUNT                     
INXPHW   DS    F                  HEALTH AND WELFARE AMOUNT                     
INXPIR   DS    F                  INSURANCE AND RETIREMENT AMOUNT               
INXPUD   DS    F                  UNION DUES AMOUNT                             
INXOFF   DS    CL1                OFFICE                                        
INXEMP   DS    CL3                EMPLOYER OF RECORD                            
INXADV   DS    CL6                ADVICE NUMBER                                 
INXUSI   DS    CL6                US INVOICE                                    
INXCAI   DS    CL6                CANADIAN INVOICE                              
INXMOV   DS    CL1                MANUAL OVERRIDE?                              
INXCDP   DS    CL1                CANADIAN DOLLAR PAYMENT?                      
INXJOV   DS    CL1                JOB OVERRIDE?                                 
INXPCP   DS    CL1                PER CYCLE GUARANTEE PAYMENT?                  
INXFUP   DS    CL1                FORCED UPGRADE?                               
INXPRI   DS    CL1                PRIMARY INVOICE?                              
INXSUB   DS    CL1                SUBSIDIARY INVOICE?                           
INXPVC   DS    CL1                PO VALID FOR CLOSE?                           
INXACP   DS    CL1                AFTRA CAST ON PAYMENT?                        
INXCSL   DS    CL1                CAST SELECTED?                                
INXCPY   DS    CL1                CREDIT PAYMENT?                               
INXBNP   DS    CL1                BILL NO PAY PAYMENT?                          
INXPWC   DS    CL1                PAYMENT WAS CHANGED?                          
INXMIO   DS    CL1                MARKET INPUT OVERRIDEN?                       
INXDMO   DS    CL1                DUPLICATE MARKETS OVERRIDDEN?                 
INXPPC   DS    CL1                PAYMENT TO PER CYCLE COMMERCIAL?              
INXACH   DS    CL1                APPLHLD OPTION?                               
INXDAC   DS    CL1                DON'T APPLY CREDITS OPTION?                   
INXDAG   DS    CL1                DON'T APPLY TO GUARANTEE OPTION?              
INXGAO   DS    F                  GST OVERRIDE AMOUNT                           
INXCTO   DS    CL1                TAKE CANADIAN TAXES OPTION?                   
INXPHO   DS    XL2                P&H OVERRIDE OPTION RATE                      
INXTOO   DS    F                  TAX OVERRIDE OPTION AMOUNT                    
INXHOO   DS    F                  HANDLING OVERRIDE OPTION AMOUNT               
INXURO   DS    CL1                URGENT OPTION?                                
INXDCO   DS    CL1                DUE COMPANY CLIENT OPTION?                    
INXDAO   DS    CL1                DUE COMPANY AGENCY OPTION?                    
INXDLO   DS    CL1                DUE COMPANY ALL OPTION?                       
INXFCO   DS    F                  FICA CREDITS OVERRIDE OPTION AMOUNT           
INXASO   DS    CL1                APPLSESS OPTION?                              
INXPTO   DS    CL1                PREVIOUS TOTAL USES OPTION?                   
INXPLO   DS    CL1                PREVIOUS LIFT USES OPTION?                    
INXPUO   DS    CL1                NUMBER OF USES OPTION?                        
INXHWO   DS    F                  H&W ADJUSTMENT OPTION AMOUNT                  
INXCRO   DS    CL1                CANADIAN CONVERSION RATE OPTION?              
INXDPO   DS    CL1                DUMMY PAYMENT OPTION?                         
INXGPO   DS    CL1                GREY PAYMENT OPTION?                          
INXRPO   DS    CL1                RETROACTIVE PAYMENT OPTION?                   
INXPRO   DS    CL1                PUR INVOICE OPTION?                           
INXCHO   DS    CL1                CORPORATION HANDLING AMOUNT OPTION?           
INXFEO   DS    F                  FEE OVERRIDE OPTION AMOUNT                    
INXCSO   DS    CL1                CSF OVERRIDE OPTION?                          
INXCAO   DS    F                  CSF OVERRIDE OPTION AMOUNT                    
INXSOO   DS    XL2                SOC OVERRIDE OPTION RATE                      
INXNIO   DS    CL1                NO INTERFACE OPTION?                          
INXGEO   DS    CL1                G=E OPTION?                                   
INXQUI   DS    CL8                QC USER ID                                    
INXQSI   DS    CL8                QC STAFF ID                                   
INXQDT   DS    CL8                QC DATE                                       
INXQTM   DS    CL8                QC TIME                                       
INXBDT   DS    CL8                BILLED DATE                                   
INXBTY   DS    XL1                BILL TYPE                                     
INXCCR   DS    XL4                CANADIAN/EURO CONVERSION RATE                 
INXBTO   DS    F                  BILLING TOTAL AMOUNT                          
INXBPT   DS    F                  PAYROLL TAX AMOUNT                            
INXBHA   DS    F                  HANDLING AMOUNT                               
INXBCH   DS    F                  CORPORATION HANDLING AMOUNT                   
INXBCS   DS    F                  CONTRACT SERVICE FEE AMOUNT                   
INXBFC   DS    F                  FICA CREDIT AMOUNT                            
INXBGC   DS    F                  CANADIAN GST AMOUNT                           
INXBAC   DS    F                  AGENCY COMMISSION AMOUNT                      
INXBSF   DS    F                  SIGNATORY FEE AMOUNT                          
INXETO   DS    F                  EMS BILLING TOTAL AMOUNT                      
INXEPT   DS    F                  EMS PAYROLL TAX AMOUNT                        
INXEHA   DS    F                  EMS HANDLING AMOUNT                           
INXECH   DS    F                  EMS CORPORATION HANDLING AMOUNT               
INXEFE   DS    F                  EMS FEE AMOUNT                                
INXCER   DS    CL1                CANCELLER INVOICE?                            
INXCED   DS    CL1                CANCELLED?                                    
INXPHL   DS    CL1                PUR HOLD?                                     
INXPPD   DS    CL8                PUR PRINT DATE                                
INXPHR   DS    CL1                PUR HOLD RELEASED?                            
INXFPA   DS    CL1                FOR PAYMENT ADJUSTMENT?                       
INXRPH   DS    CL1                RETROACTIVE PAYMENT HOLD?                     
INXEWN   DS    XL1                ERROR/WARNING NUMBER                          
INXEWI   DS    CL6                ERROR INVOICE NUMBER                          
INXEPI   DS    CL6                ERROR PID                                     
INXDUD   DS    CL8                DUE DATE                                      
INXPDD   DS    CL8                PREVIOUS DUE DATE                             
INXDCD   DS    CL8                DUE DATE CHANGE DATE                          
INXODD   DS    CL8                ORIGINAL DUE DATE                             
INXCDT   DS    CL8                CHECK DATE                                    
INXCRU   DS    CL1                CHECK RUN                                     
INXCRD   DS    CL8                CHECK RUN DATE                                
INXCDF   DS    CL1                CHECK DATE FORCED?                            
INXWID   DS    CL18               WEB APPLICATION ID                            
INXBPS   DS    F                  PROVINCIAL SERVICE TAX                        
INXEUP   DS    CL1                EURO PAYMENT?                                 
INXEUPGR DS    F                  GROSS PAYMENT AMOUNT IN EUROS                 
INXEUPAC DS    F                  APPLIED CREDIT AMOUNT IN EUROS                
INXEUPGC DS    F                  APPLIED GUARANTEE AMOUNT IN EUROS             
INXEUPIA DS    F                  INDIVIDUALS' PAYMENT AMOUNT IN EUROS          
INXEUPCA DS    F                  CORPORATIONS' PAYMENT AMOUNT IN EUROS         
INXEUPRE DS    F                  REIMBURSED EXPENSE AMOUNT IN EUROS            
INXEUPSP DS    F                  SUBJECT TO P&H AMOUNT IN EUROS                
INXEUPMD DS    F                  MISCELLANEOUS DEDUCTION AMOUNT EUROS          
INXEUPNH DS    F                  PENSION AND HEALTH AMOUNT IN EUROS            
INXEUPHW DS    F                  HEALTH AND WELFARE AMOUNT IN EUROS            
INXEUPIR DS    F                  INSURANCE AND RETIREMENT AMOUNT EUROS         
INXEUPUD DS    F                  UNION DUES AMOUNT IN EUROS                    
INXEUBTO DS    F                  BILLING TOTAL AMOUNT IN EUROS                 
INXEUBPT DS    F                  PAYROLL TAX AMOUNT IN EUROS                   
INXEUBHA DS    F                  HANDLING AMOUNT IN EUROS                      
INXEUBCH DS    F                  CORPORATION HANDLING AMOUNT IN EUROS          
INXEUBCS DS    F                  CONTRACT SERVICE FEE AMOUNT IN EUROS          
INXEUBFC DS    F                  FICA CREDIT AMOUNT IN EUROS                   
INXEUBGC DS    F                  CANADIAN GST AMOUNT IN EUROS                  
INXEUBAC DS    F                  AGENCY COMMISSION AMOUNT IN EUROS             
INXEUBSF DS    F                  SIGNATORY FEE AMOUNT IN EUROS                 
INXEUBPS DS    F                  PROVINCIAL SERVICE TAX IN EUROS               
INXVALL  EQU   *-INXVALS                                                        
                                                                                
* ** INVOICE SPLIT DETAILS *                                                    
                                                                                
INLVALS  DS    0X                                                               
INLINV   DS    CL6                 SUBSIDIARY INVOICE NUMBER                    
INLPCT   DS    XL4                 PERCENTAGE                                   
INLEST   DS    CL16                ESTIMATE/JOB NUMBER                          
INLVALL  EQU   *-INLVALS                                                        
                                                                                
* ** INVOICE NWK/SYS/MKT DETAILS *                                              
                                                                                
INMVALS  DS    0X                                                               
INMCOD   DS    CL6                 NWK/SYS/MKT CODE                             
INMCYS   DS    CL8                 CYCLE START DATE                             
INMCYE   DS    CL8                 CYCLE END DATE                               
INMVALL  EQU   *-INMVALS                                                        
                                                                                
* ** INVOICE PROGRAM DETAILS *                                                  
                                                                                
INPVALS  DS    0X                                                               
INPUDT   DS    CL8                 USE DATE                                     
INPPNM   DS    CL15                PROGRAM NAME                                 
INPLFT   DS    CL1                 LIFT CODE                                    
INPNWK   DS    CL1                 NETWORK CODE                                 
INPVALL  EQU   *-INPVALS                                                        
                                                                                
* ** INVOICE COMMENTS **                                                        
                                                                                
INCVALS  DS    0X                                                               
INCCM1   DS    CL78                COMMENT #1                                   
INCCM2   DS    CL78                COMMENT #2                                   
INCCM3   DS    CL78                COMMENT #3                                   
INCCM4   DS    CL78                COMMENT #4                                   
INCCM5   DS    CL78                COMMENT #5                                   
INCCM6   DS    CL78                COMMENT #6                                   
INCCM7   DS    CL78                COMMENT #7                                   
INCCM8   DS    CL78                COMMENT #8                                   
INCCM9   DS    CL78                COMMENT #9                                   
INCC10   DS    CL78                COMMENT #10                                  
INCC11   DS    CL78                COMMENT #11                                  
INCC12   DS    CL78                COMMENT #12                                  
INCC13   DS    CL78                COMMENT #13                                  
INCC14   DS    CL78                COMMENT #14                                  
INCC15   DS    CL78                COMMENT #15                                  
INCC16   DS    CL78                COMMENT #16                                  
INCVALL  EQU   *-INCVALS                                                        
         ORG                                                                    
                                                                                
* ** COMMENT SEARCH STATUS **                                                   
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
CMSVALS  DS    0X                                                               
CMSSTAT  DS    CL1                 STATUS                                       
CMSSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
CMSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
CMSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
CMSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
CMSVALL  EQU   *-CMSVALS                                                        
                                                                                
* ** COMMENT EXPANDED DETAILS **                                                
                                                                                
CMXVALS  DS    0X                                                               
CMXLEV   DS    CL1                 LEVEL                                        
CMXVER   DS    XL1                 VERSION                                      
CMXCM1   DS    CL78                COMMENT #1                                   
CMXCM2   DS    CL78                COMMENT #2                                   
CMXCM3   DS    CL78                COMMENT #3                                   
CMXCM4   DS    CL78                COMMENT #4                                   
CMXCM5   DS    CL78                COMMENT #5                                   
CMXCM6   DS    CL78                COMMENT #6                                   
CMXCM7   DS    CL78                COMMENT #7                                   
CMXCM8   DS    CL78                COMMENT #8                                   
CMXCM9   DS    CL78                COMMENT #9                                   
CMXCMA   DS    CL78                COMMENT #10                                  
CMXCMB   DS    CL78                COMMENT #11                                  
CMXCMC   DS    CL78                COMMENT #12                                  
CMXCMD   DS    CL78                COMMENT #13                                  
CMXCME   DS    CL78                COMMENT #14                                  
CMXCMF   DS    CL78                COMMENT #15                                  
CMXCMG   DS    CL78                COMMENT #16                                  
CMXVALL  EQU   *-CMXVALS                                                        
         ORG                                                                    
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
*** REGULAR STORAGE **                                                          
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
USERID   DS    XL2                 HEX USER ID                                  
COMPANY  DS    XL(L'LP_AGYB)       COMPANY CODE                                 
CPYALPH  DS    CL(L'LP_AGY)        COMPANY ALPHA ID                             
                                                                                
MAP      DS    XL2                                                              
                                                                                
ASSNTAB  DS    A                   A(SSN TABLE)                                 
ACOMTAB  DS    A                   A(INTERNAL COMMERCIAL NUMBER TABLE)          
AAGTTAB  DS    A                   A(AGENT CODE TABLE)                          
                                                                                
SVAGY    DS    CL(L'TLCOAGY)       SAVED AGENCY CODE                            
SVAYSTA5 DS    XL(L'TAAYSTA5)      SAVED AGENCY 5TH STATUS                      
SVCLI    DS    CL(L'TLCLCLI)       SAVED CLIENT CODE                            
SVCOM    DS    CL(L'TLCOCOM)       SAVED INTERNAL COMMERCIAL NUMBER             
SVSEQ    DS    XL2                 SAVED CAST SEQUENCE NUMBER                   
SVCLIGRP DS    CL(L'TLCGCLG)       SAVED AREA FOR CLI GROUP (CGRP REC)          
SVCAHKEY DS    CL(L'TLCAPKEY)      SAVED AREA FOR PASSIVE CAST KEY              
         ORG   SVCAHKEY                                                         
SVCAKEY  DS    CL(L'TLCAKEY)       SAVED AREA FOR CAST KEY                      
SVGUA    DS    CL(L'TLGUGUA)       SAVED AREA FOR GUARANTEE CODE                
SVTIME   DS    XL3                 SAVED TIME                                   
SVSSN    DS    CL(L'TLW4SSN)       SAVED AREA FOR W4 SS#                        
SVCFID   DS    CL(6*L'TLW4FID)     SAVED AREA FOR FEDERAL ID#S                  
SVTSSN   DS    CL(L'TAWXTSSN)      SAVED AREA FOR TRUSTEE SS#                   
SVCORP   DS    CL(L'TACACORP)      SAVED ARED FOR CORPORATION NUMBER            
SVLCO    DS    XL(L'TACALFTF)      SAVED AREA FOR LIFTED FROM COMM'L            
SVLAST   DS    XL(L'TACALAST)      SAVED AREA FOR LAST SERVICES DATE            
SVTCO    DS    XL(L'TACATCOM)      SAVED AREA FOR TRANSFERRED COMM'L            
SVAGT    DS    CL(L'TACANCDE)      SAVED AREA FOR AGENT CODE                    
SVUID    DS    XL2                 SAVED AREA FOR USER ID                       
SVEUID   DS    CL8                 SAVED AREA FOR EBCDIC USER ID                
SVCOTYPE DS    CL(L'TACOTYPE)      SAVED COMMERCIAL TYPE                        
SVCAT    DS    CL(L'TLCACAT)       SAVED CATEGORY                               
SVYEAR   DS    CL(L'TACAYEAR)      SAVED CONTRACT YEAR                          
SVINV    DS    XL(L'TLININV)       SAVED INVOICE NUMBER                         
SVCATUNI DS    XL(L'CATUNI)        SAVED VALID UNIONS FOR CATEGORY              
                                                                                
CHKFIDS  DS    6CL(L'TLW4SSN)      FEDERAL IDS FOR ACCESS CHECK                 
         DS    XL1                 END OF FEDERAL IDS                           
                                                                                
PRICOM   DS    XL(L'TAGUCOM)       SAVED PRIMARY COMMERCIAL                     
FPCYSTRT DS    XL3                 FIRST PER CYCLE START DATE                   
LPCYSTRT DS    XL3                 LATEST PER CYCLE PAYMENT START DATE          
LFTRSTRT DS    XL3                 LATEST SUBSIDIARY FTRACK START DATE          
                                                                                
SVADDR   DS    A                   SAVED AREA FOR NEEDED ADDRESSES              
         ORG   SVADDR                                                           
ANXTCOT  DS    A                   A(NEXT COMMERCIAL TYPE TABLE ENTRY)          
         ORG   SVADDR                                                           
ACURTAVA DS    A                   A(CURRENT AGY/CLI LIMIT ELEMENT)             
         ORG   SVADDR                                                           
ATACOEL  DS    A                   A(COMMERCIAL DETAILS ELEMENT)                
         ORG   SVADDR                                                           
ANXTERR  DS    A                   A(LAST PROCESSED ERROR TABLE ENTRY)          
         ORG   SVADDR                                                           
ALASTVER DS    A                   A(LAST PROCESSED VERSION NUMBER)             
SVADDR2  DS    A                   SAVED AREA FOR NEEDED ADDRESSES              
SVRMODE  DS    XL(L'LP_RMODE)      SAVED AREA FOR ROUTINE MODES                 
                                                                                
RELFLAG  DS    XL1                                                              
         ORG                                                                    
W4FLAG   DS    XL1                                                              
EXECUTED EQU   X'80'                                                            
         ORG                                                                    
CMTFLAG  DS    XL1                                                              
COGDONE  EQU   X'80'                                                            
COADONE  EQU   X'40'                                                            
COTDONE  EQU   X'20'                                                            
HSTDONE  EQU   X'10'                                                            
TPCDONE  EQU   X'08'                                                            
                                                                                
LASTSEQ  DS    XL1                                                              
                                                                                
LSTAGY   DS    CL(L'TLCLAGY)       SAVED AREA FOR ELEM COMPARE                  
LIMNUM   DS    H                   NUMBER OF ENTRIES IN LIMENT                  
                                                                                
CURRCLI  DS    CL6                 CURRENT CLIENT CODE TO RETURN                
LASTCLI  DS    CL6                 LAST CLIENT CODE RETURNED                    
                                                                                
RECCOUNT DS    H                   RECORD COUNTER                               
VERCOUNT DS    X                   VERSION COUNTER                              
                                                                                
SVIOKEY  DS    XL(L'IOKEY)         SAVED KEY FOR NESTED ROUTINES                
SVCKPKEY DS    XL(L'IOKEY)         SAVED PASSIVE CHECK KEY                      
LASTFLAG DS    X                   ** FLAG BYTE **                              
LASTPROC EQU   X'80'               PROCESS RECORD IN IOKEY                      
LASTLAST EQU   X'40'               LAST TIME                                    
                                                                                
CASTSTAT DS    X                   CAST STATUS                                  
SPLITCST EQU   X'80'               SPLIT CAST                                   
ELIGBCST EQU   X'40'               ELIGIBLE CAST FOUND                          
HASTRKS  EQU   X'20'               MUSICIAN ON TRACKS                           
         ORG   CASTSTAT                                                         
INVSTAT  DS    X                   INVOICE STATUS                               
INEUPROC EQU   X'80'               EURO PAY DETAILS ENCOUNTERED                 
                                                                                
AGTFILT  DS    CL(L'TLCAAAGT)      AGENT FILTER                                 
                                                                                
ABOVELIM DS    H                   RECORDS LEFT BEFORE LIMIT                    
                                                                                
LNAMELEN DS    X                   LENGTH OF LAST NAME INPUT                    
FNAMELEN DS    X                   LENGTH OF FIRST NAME INPUT                   
                                                                                
LKEYCOMP DS    X                   LENGTH OF KEY COMPARE                        
NUMFCEL  DS    H                                                                
                                                                                
CURTRKFL DS    CL7                 CURRENT TRACK FILTER                         
TRKTAB   DS    XL(7*TRKLNQ+1)      ATTACHED AFM CONTRACT TRACK TABLE            
                                                                                
LASTCOM  DS    XL4                 LAST INTERNAL COMMERCIAL NUMBER              
COVRTAB  DS    XL501               COMMERCIAL VERSION TABLE                     
CAVRTAB  DS    XL501               CAST VERSION TABLE                           
         EJECT                                                                  
*              DSECT FOR COMMERCIAL TYPE TABLE                                  
         SPACE 1                                                                
COTTABD  DSECT                                                                  
COTCD    DS    CL1                 CODE                                         
COTDS    DS    CL12                DESCRIPTION                                  
COTLNQ   EQU   *-COTTABD                                                        
         EJECT                                                                  
*              DSECT FOR ATTACHED AFM CONTRACT TRACK TABLE                      
         SPACE 1                                                                
TRKTABD  DSECT                                                                  
TRKCOM   DS    CL(L'TLCOCOM)       INTERNAL COMMERCIAL NUMBER                   
TRKTRK   DS    CL(L'TAMCTRK)       TRACK                                        
TRKLNQ   EQU   *-TRKTABD                                                        
         EJECT                                                                  
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE TAUNIEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TALNK11   05/29/15'                                      
         END                                                                    
