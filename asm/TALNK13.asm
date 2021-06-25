*          DATA SET TALNK13    AT LEVEL 002 AS OF 06/17/15                      
*PHASE T70413E                                                                  
TALNK13  TITLE '- TALENT - GENERAL DOWNLOADS 2'                                 
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,CODE=CODE,REQUEST=*,SYSTEM=TALSYSQ,              *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                             
                                                                                
ERRTAB   EQU   7500                                                             
COMTAB   EQU   (7000*L'TLCOBCOM)+1                                              
SSNTAB   EQU   (3000*L'TLCASSN)+1                                               
WORKLNQ  EQU   COMTAB+SSNTAB                                                    
                                                                                
CODE     NMOD1 WORKLNQ,**TA13**,RR=RE                                           
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
                                                                                
         ST    RF,AERRTAB          SAVE A(ERROR TABLE)                          
         AHI   RF,ERRTAB                                                        
         ST    RF,ACOMTAB          SAVE A(COMMERCIAL TABLE)                     
         AHI   RF,COMTAB                                                        
         ST    RF,ASSNTAB          SAVE A(CAST SSN TABLE)                       
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNMODE,RRUNSTRQ    TEST 'FIRST FOR RUN' MODE                    
         JNE   PRCWRK                                                           
                                                                                
         MVC   WVALUES(WVALUEL),LVALUES                                         
         J     YES                                                              
                                                                                
***********************************************************************         
* LVALUES MUST MATCH WVALUES                                          *         
***********************************************************************         
                                                                                
LVALUES  DS    0D                                                               
         DC    XL4'FFFFFFFF'                                                    
         EJECT                                                                  
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNMODE,RPRCWRKQ    TEST 'PROCESS WORK' MODE                     
         JNE   RUNREQ                                                           
         XC    REQVALS(REQVALL),REQVALS                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   CLI   RUNMODE,RRUNREQQ    TEST 'RUN REQUEST' MODE                      
         JNE   YES                                                              
                                                                                
         LA    R0,OUTVALS          CLEAR OUTPUT VALUES                          
         LHI   R1,OUTVALL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   COMPANY,LP_AGYB     SET COMPANY CODE                             
         MVC   CPYALPH,LP_AGY      SET COMPANY ID                               
         MVC   MAP,LP_QMAPN        SET PROCESSING MAP NUMBER                    
                                                                                
         GOTOR LP_APUTO,LP_D       CALL DDLINK OUTPUT PROCESSOR                 
                                                                                
RUNREQX  J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB                                                               
***********************************************************************         
* PROCESS W4 STATUS RECORDS                                           *         
***********************************************************************         
                                                                                
NXTW4S   J     *+12                                                             
         DC    C'*NXTW4S*'                                                      
         LR    RB,RF                                                            
         USING NXTW4S,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE STATUS TO UNSUCCESSFUL            
         JNE   NOMORE                                                           
         MVI   W4SSTAT,W4SSTUNS                                                 
         XC    SVIOKEY,SVIOKEY                                                  
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,(X'80',RQW4STF)                           
         JNE   NW4SX                                                            
                                                                                
         GOTOR SVLENGTH,DMCB,RQW4LNM,LNAMELEN                                   
         GOTOR SVLENGTH,DMCB,RQW4FNM,FNAMELEN                                   
                                                                                
         LA    R2,IOKEY                                                         
         XC    0(L'TLDRKEY,R2),0(R2)                                            
                                                                                
         USING TLW4D,R2                                                         
         OC    RQW4SSN,RQW4SSN     IF SOCIAL SECURITY NUMBER ENTERED            
         JZ    NW4S10              READ FOR IT                                  
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,RQW4SSN                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NW4SX                                                            
         MVC   SVSSN,RQW4SSN                                                    
         BRAS  RE,CKW4ACC          ENSURE STAFF HAS ACCESS                      
         JE    NW4S20                                                           
         J     NW4SX                                                            
         DROP  R2                                                               
                                                                                
         USING TLW4PD,R2                                                        
NW4S10   MVI   TLW4PCD,TLW4NCDQ    IF NAMES WERE ENTERED, READ FOR THEM         
         MVC   TLW4NLST,RQW4LNM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,CKNAME                                                        
         JNE   NW4SX                                                            
         DROP  R2                                                               
                                                                                
NW4S20   MVI   W4SSTAT,W4SSTSUC    SEARCH SUCCESSFULLY FOUND 1 RECORD           
                                                                                
NW4SX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4SVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS W4 DETAILS RECORDS                                          *         
***********************************************************************         
                                                                                
NXTW4D   J     *+12                                                             
         DC    C'*NXTW4D*'                                                      
         LR    RB,RF                                                            
         USING NXTW4D,RB                                                        
                                                                                
         CLI   W4SSTAT,W4SSTSUC    EXIT IF INITIAL SEARCH WAS                   
         JNE   NOMORE              NOT SUCCESSFUL                               
                                                                                
         LA    R2,IOKEY            REREAD W4 KEY                                
         XC    W4DVALS(W4DVALL),W4DVALS                                         
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF FIRST TIME IN, RETURN W4 INFO             
         JE    NW4D10                                                           
                                                                                
         OC    RQW4SSN,RQW4SSN     IF SEARCHING BY SS#                          
         JNZ   NOMORE              ONLY RETURN W4 INFO FIRST TIME IN            
                                                                                
         USING TLW4PD,R2                                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,CKNAME                                                        
         JNE   NOMORE                                                           
         DROP  R2                                                               
                                                                                
NW4D10   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLW4D,R1                                                         
         L     R1,IOADDR           R1=A(W4 RECORD)                              
         LA    R1,TLW4ELEM         R1=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
NW4D20   CLI   0(R1),0             READ THROUGH ALL ELEMENTS                    
         JE    NW4DX                                                            
                                                                                
         USING TANUD,R1                                                         
         CLI   0(R1),TANUELQ       IF FREE FORM NUMBER ELEMENT                  
         JNE   NW4D30                                                           
         CLI   TANUTYPE,TANUPIDN   NUMBER TYPE PID NUMBER                       
         JNE   NW4D60                                                           
         MVC   W4DPID,TANUMBER     PASS BACK PID NUMBER                         
         J     NW4D60                                                           
         DROP  R1                                                               
                                                                                
         USING TAW4D,R1                                                         
NW4D30   CLI   0(R1),TAW4ELQ       IF EMPLOYEE W4 DETAILS ELEMENT               
         JNE   NW4D50                                                           
         MVC   W4DLNAM,TAW4NAM2    PASS BACK LAST NAME                          
                                                                                
         CLI   TAW4TYPE,TAW4TYIN   IF W4 TYPE IS INDIVIDUAL                     
         JE    NW4D40                                                           
         CLI   TAW4TYPE,TAW4TYCA   OR CANADIAN                                  
         JE    NW4D40                                                           
         CLI   TAW4TYPE,TAW4TYFO   OR FOREIGNER                                 
         JNE   NW4D50                                                           
NW4D40   MVC   W4DFNAM,TAW4NAM1    PASS BACK FIRST NAME                         
         CLI   TAW4LEN,TAW4LN2Q                                                 
         JL    NW4D60                                                           
         MVC   W4DMNAM,TAW4MIDN    AND MIDDLE NAME                              
         J     NW4D60                                                           
         DROP  R1                                                               
                                                                                
         USING TAA2D,R1                                                         
NW4D50   CLI   0(R1),TAA2ELQ       IF ADDRESS ELEMENT                           
         JNE   NW4D60                                                           
         MVC   W4DADD1,TAA2ADD1    PASS BACK ADDRESS LINE 1                     
         MVC   W4DADD2,TAA2ADD2    ADDRESS LINE 2                               
         MVC   W4DADD3,TAA2ADD3    ADDRESS LINE 3                               
         MVC   W4DCITY,TAA2CITY    CITY                                         
         MVC   W4DSTAT,TAA2ST      STATE                                        
         MVC   W4DZIPC,TAA2ZIP     AND ZIP                                      
         DROP  R1                                                               
                                                                                
NW4D60   ZIC   RF,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         J     NW4D20                                                           
                                                                                
NW4DX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,W4DVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL/VERSION SEARCH STATUS RECORD               *         
***********************************************************************         
                                                                                
NXTCOS   J     *+12                                                             
         DC    C'*NXTCOS*'                                                      
         LR    RB,RF                                                            
         USING NXTCOS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         ZICM  RE,ARQCOCOM,3       TEMPORARY CODE TO HANDLE BUG                 
         CLI   3(RE),X'0C'         WHERE TALNK16/18'S EMBEDDED                  
         JNE   NCOS00              DOWNLOAD CALLS BLOW AWAY                     
         CLI   7(RE),1             THE ERROR STATUS                             
         JNE   NCOS00                                                           
         OI    ERRSTAT,ESREVIW                                                  
                                                                                
NCOS00   GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCOSCIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCOSIIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCOSTIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCOSMIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCOSUIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCOSWIN                                
         GOTOR (#FMTLIST,AFMTLIST),DMCB,RQCOSAIN                                
                                                                                
         MVI   COSSTAT,COSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,COVALREQ         VALIDATE REQUEST                             
         JNE   NCOSX                                                            
                                                                                
         MVI   PROSTAT,0           INITIALIZE PROGRAM STATUS                    
         MVI   COSSTAT,COSSTUNS    SEARCH STATUS                                
         MVI   COSSPSST,COSSPUNS   PRIMARY SEARCH STATUS                        
         MVI   ACLIMSTA,COSSINIT   AGY/CLI ACCESS STATUS                        
         XC    LASTCOM,LASTCOM     LAST INTERNAL COMMERCIAL NUMBER              
         MVC   ABOVELIM,RQCOSLIM   RECORDS ABOVE LIMIT VARIABLES                
         XC    SRCHCID,SRCHCID     AND CURRENT SEARCHED COMMERCIAL ID           
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,(X'80',RQCOSSTF)                          
         JNE   NCOSX               SAVE AGENCY/CLIENT LIMITS                    
                                                                                
         L     RE,ACOMTAB          INITIALIZE INTERNAL COMMERCIAL               
         MVI   0(RE),X'FF'         NUMBER TABLE                                 
                                                                                
         BAS   RE,INITCID          INTIALIZE PROGRAM TO SEARCH USING            
         JNE   NCOSX               COMMERCIAL ID                                
                                                                                
         CLI   RQCOSTIN,0          IF TYPE FILTERS ARE PROVIDED                 
         JE    NCOS20                                                           
         ZIC   RE,RQCOSTIN                                                      
         ZICM  RF,ARQCOTYP,3                                                    
NCOS10   CLI   0(RF),C'!'          AND ANY OF THE ENTRIES ARE !                 
         JNE   *+8                                                              
         MVI   0(RF),0             REPLACE WITH ZERO                            
         LA    RF,1(RF)            (STANDARD COMMERCIAL)                        
         BCT   RE,NCOS10                                                        
                                                                                
NCOS20   CLI   RQCOSAIN,0          IF ACTRA TYPE FILTERS ARE PROVIDED           
         JE    NCOS40                                                           
         ZIC   RE,RQCOSAIN                                                      
         ZICM  RF,ARQCOATY,3                                                    
NCOS30   CLI   0(RF),255           AND ANY OF THE ENTRIES ARE !                 
         JNE   *+8                                                              
         MVI   0(RF),0             REPLACE WITH ZERO                            
         LA    RF,1(RF)            (COMMERCIAL WITHOUT AN ACTRA TYPE)           
         BCT   RE,NCOS30                                                        
                                                                                
NCOS40   CLI   RQCOSVER,0          IF VERSION IS INCLUDED IN REQUEST            
         JE    *+12                                                             
         MVI   RQCOSIVR,C'Y'       SET TO INCLUDE VERSIONS IN SEARCH            
         MVI   RQCOSEXT,C'Y'       AND TO SEARCH FOR COMML ID EXACTLY           
                                                                                
         CLI   RQCOSSUS,C'Y'       IF USING SUPER SEARCH OPTION                 
         JE    NCOS50              WITH WILDCARD                                
         CLI   RQCOSSUS,C'E'       OR WITH EXACT MATCH                          
         JNE   NCOS60                                                           
NCOS50   OI    PROSTAT,PSSUS       TURN ON SUPER SEARCH STATUS                  
                                                                                
NCOS60   LA    R3,IOKEY            INITIALIZE VARIABLES                         
         XC    0(L'TLDRKEY,R3),0(R3)                                            
                                                                                
         USING TLCOPD,R3                                                        
         CLI   RQCOSCIN,0          IF INTERNAL COMMERCIAL NUMBER                
         JE    NCOS70              FILTER IS DEFINED                            
         ZICM  RF,ARQCOCOM,3                                                    
         MVC   RQCOSCOM,0(RF)                                                   
         XC    0(L'RQCOSCOM,RF),0(RF)                                           
         MVI   TLCOPCD,TLCOCCDQ    READ FOR INTERNAL COMMERCIAL NUMBER          
         MVC   TLCOCCOM,RQCOSCOM                                                
         LHI   R0,TLCOCCOM+L'TLCOCOM-TLCOPD-1                                   
         J     NCOS120                                                          
                                                                                
NCOS70   OC    RQCOSCID,RQCOSCID   IF COMMERCIAL ID IS PROVIDED                 
         JZ    NCOS100                                                          
         OC    RQCOSAGY,RQCOSAGY   AND AGENCY CODE IS PROVIDED                  
         JZ    NCOS90                                                           
         MVI   TLCOPCD,TLCOICDQ    READ FOR AGENCY-COMMERCIAL ID                
         MVC   TLCOIAGY,RQCOSAGY                                                
         MVC   TLCOICID,RQCOSCID                                                
         LHI   R0,TLCOICOM-TLCOPD-1                                             
         CLI   RQCOSEXT,C'C'                                                    
         JE    NCOS120                                                          
         CLI   RQCOSEXT,C'Y'                                                    
         JE    NCOS120                                                          
         LHI   R0,TLCOICID+L'TLCOICID-TLCOPD-1                                  
         LA    R1,TLCOICID+L'TLCOICID-1                                         
NCOS80   CLI   0(R1),C' '                                                       
         JH    NCOS120                                                          
         BCTR  R1,0                                                             
         BCT   R0,NCOS80                                                        
         J     NCOS120                                                          
                                                                                
NCOS90   MVI   TLCOPCD,TLCOBCDQ    IF COMMERCIAL ID IS PROVIDED                 
         MVC   TLCOBNAM,SPACES     WITHOUT AGENCY CODE                          
         MVC   TLCOBNAM(L'RQCOSCID),RQCOSCID                                    
         MVI   TLCOBSTA,TLCOBSID   READ FOR COMMERCIAL ID ALONE                 
         GOTOR SVLENGTH,DMCB,(L'RQCOSCID,RQCOSCID),LKEYCOMP                     
         TM    RQCOSSUS,C'E'                                                    
         JZ    NCOS130                                                          
         LHI   R0,TLCOBCOM-TLCOPD-1                                             
         J     NCOS120                                                          
                                                                                
NCOS100  TM    PROSTAT,PSSUS       IF COMMERCIAL TITLE WAS ENTERED              
         JZ    NCOS110             WITH SUPER SEARCH OPTION                     
         MVI   TLCOPCD,TLCOBCDQ    AND READ FOR IT                              
         MVC   TLCOBNAM,RQCOSTIT                                                
         GOTOR SVLENGTH,DMCB,(L'TLCOBNAM,RQCOSTIT),LKEYCOMP                     
         CLI   RQCOSSUS,C'E'                                                    
         JNE   NCOS130                                                          
         ZIC   R0,LKEYCOMP                                                      
         AHI   R0,1                                                             
         J     NCOS120                                                          
                                                                                
NCOS110  MVI   TLCOPCD,TLCOVRDQ    ELSE, READ VERSION NUMBER KEY                
         MVC   TLCOVAGY,RQCOSAGY                                                
         MVC   TLCOVCLI,RQCOSCLI                                                
         LHI   R0,TLCOVPRD-TLCOPD-1                                             
         OC    RQCOSPRD,RQCOSPRD                                                
         JZ    NCOS120                                                          
         MVC   TLCOVPRD,RQCOSPRD                                                
         LHI   R0,TLCOVCID-TLCOPD-1                                             
                                                                                
NCOS120  STC   R0,LKEYCOMP         L' KEY COMPARE                               
                                                                                
NCOS130  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTCOM           IF COMM RECORD MATCHES ALL FILTERS           
         JNE   NCOS160             RETURN SEARCH SUCCESSFUL STATUS              
         MVI   COSSTAT,COSSTSUC                                                 
         TM    ERRSTAT,ESREVIW                                                  
         JZ    NCOS140                                                          
         MVI   COSSTAT,COSSTEMB                                                 
         MVI   COSSPSST,0                                                       
         J     NCOS150                                                          
         DROP  R3                                                               
                                                                                
NCOS140  GOTO1 VDATCON,DMCB,(5,0),(8,COSSDATE)                                  
         TIME  DEC                                                              
         STCM  R0,14,FULL1                                                      
         GOTOR (#OUTTIME,AOUTTIME),DMCB,FULL1,COSSTIME                          
                                                                                
NCOS150  MVC   FRSTCOM,SVCOKEY+TLCOCOM-TLCOD                                    
                                                                                
         CLI   RQCOSIIN,1          IF SEARCHING FOR ONLY ONE COMMERCIAL         
         JH    NCOS160             ID                                           
         CLI   RQCOSEXT,C'Y'       AND SEARCHING FOR IT EXACTLY                 
         JNE   NCOS160                                                          
         OC    RQCOSAGY,RQCOSAGY   AND AGENCY FILTER IS PROVIDED                
         JZ    NCOS160                      SET INTERNAL COMM'L NUMBER          
         MVC   RQCOSCOM,SVCOKEY+TLCOCOM-TLCOD           IN REQUEST NOW          
         MVI   RQCOSCIN,1                                                       
                                                                                
NCOS160  CLI   ACLIMSTA,COSSINIT   IF ANY COMMERCIALS WERE FOUND                
         JE    *+10                PASS BACK AGY/CLI ACCESS STATUS              
         MVC   COSSACST,ACLIMSTA                                                
                                                                                
NCOSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,COSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
COVALREQ NTR1                                                                   
         TM    ERRSTAT,ESREVIW                                                  
         JO    YES                                                              
                                                                                
         OC    RQCOSSTF,RQCOSSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
                                                                                
         CLI   RQCOSCIN,0          ASSERT THAT INTERNAL COMMERCIAL              
         JNE   CVR10               NUMBER                                       
         CLI   RQCOSIIN,0          OR COMMERCIAL ID                             
         JNE   CVR10                                                            
         OC    RQCOSTIT,RQCOSTIT   OR TITLE                                     
         JNZ   CVR10                                                            
         OC    RQCOSCLI,RQCOSCLI   OR CLIENT IS PROVIDED                        
         JZ    NO                                                               
                                                                                
         OC    RQCOSAGY,RQCOSAGY   IF CLIENT IS PROVIDED, ASSERT THAT           
         JZ    NO                  AGENCY IS PROVIDED                           
                                                                                
CVR10    CLI   RQCOSIIN,1          IF MORE THAN 1 COMMERCIAL ID IS              
         JNH   CVR20               PROVIDED                                     
         CLI   RQCOSCIN,0          ASSERT THAT INTERNAL COMMERCIAL              
         JNE   NO                  NUMBER IS NOT PROVIDED                       
         OC    RQCOSAGY,RQCOSAGY   ASSERT THAT AGENCY IS PROVIDED               
         JZ    NO                                                               
         CLI   RQCOSEXT,C'N'       ASSERT THAT SEARCH IS FOR EXACT              
         JE    NO                  COMMERCIAL ID                                
         CLI   RQCOSSUS,C'Y'       ASSERT THAT SUPER SEARCH IS NOT              
         JE    NO                  YES                                          
                                                                                
CVR20    CLI   RQCOSVER,0          IF VERSION NUMBER IS PROVIDED                
         JE    CVR30                                                            
         CLI   RQCOSCIN,1          ASSERT THAT 1 INTERNAL COMMERCIAL            
         JNE   NO                  NUMBER IS PROVIDED                           
                                                                                
CVR30    CLI   RQCOSTIN,0          ENFORCE VALID VALUES FOR                     
         JE    CVR60               TYPE FILTERS                                 
         ZIC   R2,RQCOSTIN                                                      
         ZICM  R3,ARQCOTYP,3                                                    
CVR40    CLI   0(R3),C'!'                                                       
         JE    CVR50                                                            
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFCOTY',0(R3))                          
         JNE   NO                                                               
CVR50    LA    R3,1(R3)                                                         
         BCT   R2,CVR40                                                         
                                                                                
CVR60    CLI   RQCOSMIN,0          ENFORCE VALID VALUES FOR                     
         JE    CVR80               MEDIA FILTERS                                
         ZIC   R2,RQCOSMIN                                                      
         ZICM  R3,ARQCOMED,3                                                    
CVR70    GOTOR (#VALFLD,AVALFLD),DMCB,('VFMED',0(R3))                           
         JNE   NO                                                               
         LA    R3,1(R3)                                                         
         BCT   R2,CVR70                                                         
                                                                                
CVR80    CLI   RQCOSAIN,0          ENFORCE VALID VALUES FOR                     
         JE    CVR110              ACTRA TYPE FILTERS                           
         ZIC   R2,RQCOSAIN                                                      
         ZICM  R3,ARQCOATY,3                                                    
CVR90    CLI   0(R3),255                                                        
         JE    CVR100                                                           
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFACTY',0(R3)),(0,0)                    
         JNE   NO                                                               
CVR100   LA    R3,1(R3)                                                         
         BCT   R2,CVR90                                                         
                                                                                
CVR110   GOTOR (#VALFLD,AVALFLD),DMCB,('VFOORN',RQCOSLCK)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFOORN',RQCOSREL)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYON',RQCOSCDO)                        
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYON',RQCOSCRT)                        
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYON',RQCOSSOP)                        
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYON',RQCOSPCY)                        
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYON',RQCOSPAD)                        
         JNE   NO                                                               
                                                                                
         CLI   RQCOSEXT,C'C'       IF SEARCHING FOR COMMERCIAL ID               
         JNE   CVR120              USING CLARUS RULES                           
         CLI   RQCOSIVR,0          INCLUDE VERSIONS IN SEARCH                   
         JNE   NO                  MUST BE LEFT BLANK                           
CVR120   GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSIVR)                       
         JNE   NO                                                               
                                                                                
         CLI   RQCOSIIN,1          IF 1 COMMERCIAL ID IS PROVIDED               
         JNE   CVR130              ENFORCE VALID VALUES FOR SEARCH FOR          
         CLI   RQCOSEXT,C'C'       COMMERCIAL ID EXACTLY                        
         JE    CVR130                                                           
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSEXT)                       
         JNE   NO                                                               
                                                                                
CVR130   GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSAVR)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSAFM)                       
         JNE   NO                                                               
                                                                                
         CLI   RQCOSEXT,C'C'       IF NOT SEARCHING FOR COMMERCIAL              
         JE    CVR150              ID USING CLARUS MATCHING RULES               
         CLI   RQCOSIIN,0          AND COMMERCIAL ID                            
         JNE   CVR140                                                           
         OC    RQCOSTIT,RQCOSTIT   OR TITLE IS PROVIDED                         
         JZ    CVR150                                                           
CVR140   CLI   RQCOSSUS,C'E'       ASSERT VALID VALUES FOR                      
         JE    CVR160              SUPER SEARCH?                                
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSSUS)                       
         JE    CVR160                                                           
         J     NO                                                               
CVR150   CLI   RQCOSSUS,0          OTHERWISE, ASSERT SUPER SEARCH               
         JNE   NO                  IS BLANK                                     
                                                                                
CVR160   GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSELA)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSELC)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSELP)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSENV)                       
         JNE   NO                                                               
                                                                                
         CLI   RQCOSUIN,0          ENFORCE VALID VALUES FOR                     
         JE    CVR180              EXCLUDED UNIONS                              
         ZIC   R2,RQCOSUIN                                                      
         ZICM  R3,ARQCOEUN,3                                                    
CVR170   GOTOR (#VALFLD,AVALFLD),DMCB,('VFUNI',0(R3))                           
         JNE   NO                                                               
         LA    R3,L'TACAUN(R3)                                                  
         BCT   R2,CVR170                                                        
                                                                                
CVR180   CLI   RQCOSWIN,0          ENFORCE VALID VALUES FOR                     
         JE    CVR200              EXCLUDED W4 TYPES                            
         ZIC   R2,RQCOSWIN                                                      
         ZICM  R3,ARQCOEWT,3                                                    
CVR190   GOTOR (#VALFLD,AVALFLD),DMCB,('VFW4TY',0(R3))                          
         JNE   NO                                                               
         LA    R3,1(R3)                                                         
         BCT   R2,CVR190                                                        
                                                                                
CVR200   GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSERL)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSEP6)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSTEX)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSEOM)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSTMA)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSAMT)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSES9)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSSKA)                       
         JNE   NO                                                               
         GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQCOSELS)                       
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE SETS UP PROGRAM TO SEARCH BY COMMERCIAL ID           *         
***********************************************************************         
                                                                                
INITCID  NTR1                                                                   
         CLI   RQCOSIIN,0          EXIT IF NO COMMERCIAL IDS ARE                
         JE    YES                 PROVIDED                                     
                                                                                
         ZICM  R5,ARQCOCID,3                                                    
         MVC   SRCHCID,0(R5)                                                    
                                                                                
         CLI   RQCOSIIN,1          IF 1 COMMERCIAL ID IS PROVIDED               
         JH    IC10                                                             
         CLI   RQCOSEXT,C'C'       AND NOT MATCHING VIA CLARUS RULES            
         JE    IC10                FILL IN TRADITIONAL COMMERCIAL ID            
         MVC   RQCOSCID,0(R5)      FIELD AND EXIT                               
         J     YES                                                              
                                                                                
IC10     MVC   ARQCOCOM,ACOMTAB+1  IF MORE THAN 1 COMMERCIAL ID IS              
         ZICM  R2,ARQCOCOM,3       PROVIDED, TRANSLATE THEM INTO                
         ZIC   R4,RQCOSIIN         INTERNAL COMMERCIAL NUMBER TABLE             
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,IOKEY                                                         
IC20     XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLCOPCD,TLCOICDQ                                                 
         MVC   TLCOIAGY,RQCOSAGY                                                
         MVC   TLCOICID,0(R5)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLCOICOM-TLCOPD),IOKEYSAV                                  
         JNE   IC30                                                             
         MVC   0(L'TLCOICOM,R2),TLCOICOM                                        
         J     IC40                                                             
         DROP  R3                                                               
                                                                                
         USING TLAKD,R3                                                         
IC30     CLI   RQCOSEXT,C'C'                                                    
         JNE   IC50                                                             
         XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLAKCD,TLAKCDQ                                                   
         MVC   TLAKAGY,RQCOSAGY                                                 
         MVC   TLAKADID,0(R5)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLAKNCLI-TLAKD),IOKEYSAV                                   
         JNE   IC50                                                             
         MVC   0(L'TLCOICOM,R2),TLAKCOM                                         
         DROP  R3                                                               
                                                                                
IC40     LA    R2,L'TLCOICOM(R2)                                                
         ZIC   RE,RQCOSCIN                                                      
         AHI   RE,1                                                             
         STC   RE,RQCOSCIN                                                      
         J     IC60                                                             
                                                                                
IC50     XC    0(L'TLCOICID,R5),0(R5)                                           
                                                                                
IC60     LA    R5,L'TLCOICID(R5)                                                
         BCT   R4,IC20                                                          
                                                                                
         CLI   RQCOSCIN,0                                                       
         JH    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL SEARCH COMPRESSED DETAILS RECORDS          *         
***********************************************************************         
                                                                                
NXTCOD   J     *+12                                                             
         DC    C'*NXTCOD*'                                                      
         LR    RB,RF                                                            
         USING NXTCOD,RB                                                        
                                                                                
         CLI   COSSTAT,COSSTSUC    EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
         CLI   RQCOSCIN,0          OR IF REQUEST CONTAINS ONLY                  
         JH    NOMORE              INTERNAL COMMERCIAL NUMBERS                  
                                                                                
         LA    R3,IOKEY            INITIALIZE VARIABLES                         
         L     R4,AIO3                                                          
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT FIRST TIME IN                 
         JE    NCOD20              READ NEXT COMMERCIAL                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
                                                                                
         BRAS  RE,FLTCOM                                                        
         JE    NCOD10                                                           
                                                                                
         USING TLCOPD,R3                                                        
         OC    FRSTCOM,FRSTCOM     IF ONLY ONE COMMERCIAL PASSED                
         JZ    NOMORE              THE FILTERS, SET INTERNAL                    
         MVC   RQCOSCOM,FRSTCOM    COMMERCIAL NUMBER IN REQUEST                 
         MVI   RQCOSCIN,1                                                       
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOCCDQ    AND GO RE-READ THE ONE RECORD                
         MVC   TLCOCCOM,RQCOSCOM                                                
         LHI   R0,TLCOCCOM+L'TLCOCOM-TLCOPD-1                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTCOM                                                        
         J     NOMORE                                                           
         DROP  R3                                                               
                                                                                
NCOD10   XC    FRSTCOM,FRSTCOM                                                  
                                                                                
         OC    RQCOSLIM,RQCOSLIM   DONE IF RECORD LIMIT WAS PROVIDED            
         JZ    NCOD20                                                           
         OC    ABOVELIM,ABOVELIM   AND HAS BEEN REACHED                         
         JNZ   NCOD20                                                           
         MVI   RQCOSLIM,X'FF'                                                   
         J     NOMORE                                                           
                                                                                
NCOD20   BRAS  RE,BLDCOX           BUILD EXPANDED COMMERCIAL DETAILS            
                                                                                
         CLI   SVVER,2             IF RETURNING VERSION 2 OR HIGHER             
         JL    NCOD30                                                           
         L     R4,AIO4             BUILD EXPANDED VERSION DETAILS               
         BRAS  RE,BLDCOX                                                        
                                                                                
NCOD30   CLI   COXTYP,CTYMUS       IF COMMERCIAL TYPE IS NOT MUSIC              
         JE    NCOD40              DO NOT PASS BACK TRACK INFO                  
         XC    COXA1A(COXA1LNQ),COXA1A                                          
         XC    COXA2A(COXA2LNQ),COXA2A                                          
         XC    COXA3A(COXA3LNQ),COXA3A                                          
         XC    COXA4A(COXA4LNQ),COXA4A                                          
         XC    COXA5A(COXA5LNQ),COXA5A                                          
         XC    COXA6A(COXA6LNQ),COXA6A                                          
         XC    COXA7A(COXA7LNQ),COXA7A                                          
         XC    COX1MA(COXMALNQ),COX1MA                                          
                                                                                
NCOD40   OC    RQCOSLIM,RQCOSLIM   IF RECORD LIMIT WAS PROVIDED                 
         JZ    NCODX                                                            
         LH    R0,ABOVELIM         DECREMENT IT NOW                             
         SHI   R0,1                                                             
         STH   R0,ABOVELIM                                                      
                                                                                
NCODX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,COXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL SEARCH EXPANDED DETAILS RECORDS            *         
***********************************************************************         
                                                                                
NXTCOX   J     *+12                                                             
         DC    C'*NXTCOX*'                                                      
         LR    RB,RF                                                            
         USING NXTCOX,RB                                                        
                                                                                
         CLI   COSSTAT,COSSTSUC    IF INITIAL SEARCH WAS SUCCESSFUL             
         JH    NOMORE                                                           
         CLI   RQCOSCIN,0          AND REQUEST CONTAINS ANY INTERNAL            
         JE    NOMORE              COMMERCIAL NUMBERS                           
                                                                                
         LA    R3,IOKEY            INITIALIZE VARIABLES                         
         L     R4,AIO3                                                          
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT FIRST TIME IN                 
         JE    NCOX10                                                           
         MVI   IOKEY,X'FF'                                                      
         BRAS  RE,FLTCOM           READ FOR NEXT INTERNAL COMMERCIAL            
         JNE   NOMORE              NUMBER                                       
                                                                                
NCOX10   MVI   SVAACNS,X'FF'       INITIALIZE AFM CONTRACT LIST                 
                                                                                
         BRAS  RE,BLDCOX           BUILD EXPANDED COMMERCIAL DETAILS            
                                                                                
         CLI   SVVER,2             IF RETURNING VERSION 2 OR HIGHER             
         JL    NCOX20                                                           
         L     R4,AIO4             BUILD EXPANDED VERSION DETAILS               
         BRAS  RE,BLDCOX                                                        
                                                                                
NCOX20   BRAS  RE,BLDAACNS         ADD MUSIC CONTRACTS TO LIST                  
         BRAS  RE,BLDALS           PASS BACK VERSION ALIAS                      
         BRAS  RE,BLDPAD           PASS BACK PAID STATUS                        
         BRAS  RE,BLDCMT           PASS BACK COMMENT INDICATORS                 
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,COXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL SEARCH ATTACHED VERSIONS RECORDS           *         
***********************************************************************         
                                                                                
NXTCOV   J     *+12                                                             
         DC    C'*NXTCOV*'                                                      
         LR    RB,RF                                                            
         USING NXTCOV,RB                                                        
                                                                                
         CLI   RQCOSAVR,C'Y'       IF RETURNING ATTACHED VERSIONS               
         JNE   NOMORE                                                           
         CLI   COSSTAT,COSSTSUC    AND INITIAL SEARCH WAS SUCCESSFUL            
         JH    NOMORE                                                           
******** TM    ERRSTAT,ESREVIW     AND NOT REVIEWING AN ERROR                   
******** JO    NOMORE                                                           
         CLI   RQCOSCIN,0          AND REQUEST CONTAINED ANY INTERNAL           
         JE    NOMORE              COMMERCIAL NUMBERS                           
                                                                                
         USING TLVRD,R3                                                         
         LA    R3,IOKEY            INITIALIZE VARIABLES                         
         L     R4,AIO4                                                          
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NCOV10                                                           
         XC    TLVRKEY,TLVRKEY     READ ALL ATTACHED VERSION RECORDS            
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,RQCOSCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
         J     NCOV20                                                           
                                                                                
NCOV10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO4'                               
NCOV20   CLC   IOKEY(TLVRVER-TLVRD),IOKEYSAV                                    
         JNE   NOMORE                                                           
         MVC   SVVRKEY,IOKEY                                                    
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         BRAS  RE,BLDCOX           BUILD EXPANDED VERSION DETAILS               
         BRAS  RE,BLDAACNS         ADD MUSIC CONTRACTS TO LIST                  
         BRAS  RE,BLDALS           PASS BACK VERSION ALIAS                      
         BRAS  RE,BLDPAD           PASS BACK PAID STATUS                        
                                                                                
         MVC   IOKEY,SVVRKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO4'                               
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,COXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL SEARCH ATTACHED AFM CONTRACT RECORDS       *         
***********************************************************************         
                                                                                
NXTCOC   J     *+12                                                             
         DC    C'*NXTCOC*'                                                      
         LR    RB,RF                                                            
         USING NXTCOC,RB                                                        
                                                                                
         CLI   RQCOSAFM,C'Y'       IF RETURNING ATTACHED AFM CONTRACTS          
         JNE   NOMORE                                                           
         CLI   COSSTAT,COSSTSUC    AND INITIAL SEARCH WAS SUCCESSFUL            
         JH    NOMORE                                                           
         TM    ERRSTAT,ESREVIW     AND NOT REVIEWING AN ERROR                   
         JO    NOMORE                                                           
         CLI   RQCOSCIN,0          AND REQUEST CONTAINED ANY INTERNAL           
         JE    NOMORE              COMMERCIAL NUMBERS                           
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,IOKEY            INITIALIZE VARIABLES                         
         L     R4,AIO4                                                          
                                                                                
         LA    R2,SVAACNS                                                       
NCOC10   CLI   0(R2),X'FF'         READ THROUGH SAVED AFM CONTRACT LIST         
         JE    NOMORE                                                           
         OC    0(L'TATRCOM,R2),0(R2)                                            
         JNZ   NCOC30                                                           
NCOC20   LA    R2,L'TATRCOM(R2)                                                 
         J     NCOC10                                                           
                                                                                
NCOC30   XC    TLCOPKEY,TLCOPKEY   READ ALL ATTACHED AFM CONTRACT               
         MVI   TLCOPCD,TLCOCCDQ    KEY/RECORDS                                  
         MVC   TLCOCCOM,0(R2)                                                   
                                                                                
         XC    0(L'TATRCOM,R2),0(R2)                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NCOC20                                                           
         DROP  R3                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
                                                                                
         BRAS  RE,BLDCOX           BUILD EXPANDED VERSION DETAILS               
         BRAS  RE,BLDALS           AND PASS BACK VERSION ALIAS                  
         BRAS  RE,BLDCMT           PASS BACK COMMENT INDICATORS                 
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,COXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        BUILD EXPANDED COMMERCIAL/VERSION DETAILS RECORD             *         
*        ON ENTRY ... R3=(IOKEY)                                      *         
*                     R4=(COMMERCIAL/VERSION RECORD)                  *         
***********************************************************************         
                                                                                
BLDCOX   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVRECEQU,0(R4)      SAVE RECORD EQUATE                           
                                                                                
         CLI   SVRECEQU,TLCOCDQ    IF PROCESSING COMMERCIAL RECORD              
         JNE   BCOX10              CLEAR ALL COMMERCIAL LEVEL VARIABLES         
         LA    R0,COXADS                                                        
         LHI   R1,COX1LNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING TLCOD,R4                                                         
         MVC   COXAGY,TLCOAGY      THEN PASS BACK AGENCY CODE                   
         MVC   COXCLI,TLCOCLI      CLIENT CODE                                  
         MVC   COXPRD,TLCOPRD      PRODUCT CODE AND INTERNAL NUMBER             
         MVC   SVCOM,TLCOCOM                                                    
         GOTO1 VHEXOUT,DMCB,SVCOM,COXCOM,L'SVCOM,0                              
         DROP  R4                                                               
                                                                                
BCOX10   LA    R0,COXVER           ALWAYS CLEAR VERSION LEVEL VARIABLES         
         LHI   R1,COX2LNQ                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING TLVRD,R4                                                         
         CLI   SVRECEQU,TLVRCDQ    IF PROCESSING VERSION RECORD                 
         JNE   BCOX20                                                           
         MVC   COXVER,TLVRVER      PASS BACK VERSION NUMBER                     
                                                                                
BCOX20   MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
BCOX30   BRAS  RE,NEXTEL                                                        
         JNE   BCOX150                                                          
                                                                                
         USING TACOD,R4                                                         
         CLI   0(R4),TACOELQ       IF COMMERCIAL DETAILS ELEMENT                
         JNE   BCOX40                                                           
         MVC   COXCID,TACOCID      ALWAYS PASS BACK COMMERCIAL ID               
         MVC   COXTYP,TACOTYPE                    COMMERCIAL TYPE               
         GOTO1 VDATCON,DMCB,(1,TACOAIR),(8,COXFAR) FIRST AIR DATE               
         EDIT  TACOSEC,COXSEC,ALIGN=LEFT        LENGTH IN SECONDS               
         MVC   COXATT,TACOATT            ATTENTION CODE (BILL TO)               
         GOTO1 VDATCON,DMCB,(1,TACODUB),(8,COXDUB)       DUB DATE               
                                                                                
         CLI   SVRECEQU,TLCOCDQ    IF PROCESSING COMMERCIAL RECORD              
         JNE   BCOX30A                                                          
         MVC   COXADS,TACOADST               PASS BACK ADDENDUM STATE           
         MVC   COXMED,TACOMED                                   MEDIA           
         GOTO1 VDATCON,DMCB,(1,TACOFCYC),(8,COXFFC) FIRST FIXED CYCLE           
         GOTO1 (RF),(R1),(1,TACOSAIR),(8,COXSAR)   2ND SEASON 1ST AIR           
         GOTO1 (RF),(R1),(1,TACOEXP),(8,COXEXP)           EXPIRY DATE           
         GOTO1 (RF),(R1),(1,TACOVDTE),(8,COXVDT)    VERIFICATION DATE           
         GOTOR (#OUTTIME,AOUTTIME),DMCB,TACOVTIM,COXVTI   VERIF. TIME           
         MVC   COXVST,TACOVST                   VERIFICATION STAFF ID           
         MVC   COXATY,TACOCTYP                             ACTRA TYPE           
         MVC   COXART,TACOAFM                                AFM RATE           
         MVC   COXSTY,TACOSES                            SESSION TYPE           
         MVC   COXCON,TACOCONT                      AND CONTRACT TYPE           
                                                                                
         MVI   COXSCY,C'N'         PASS BACK "HAS SPLIT CYCLES?"                
         CLI   TACOSPCY,C'Y'       STATUS                                       
         JNE   *+8                                                              
         MVI   COXSCY,C'Y'                                                      
                                                                                
         MVI   COXLCK,C'N'         PASS BACK "COMMERCIAL LOCKED?"               
         TM    TACOSTAT,TACOSTLO   STATUS                                       
         JZ    *+8                                                              
         MVI   COXLCK,C'Y'                                                      
                                                                                
         MVI   COXREL,C'N'         PASS BACK "COMMERCIAL RELEASED"              
         TM    TACOSTAT,TACOSTRL   STATUS                                       
         JZ    *+8                                                              
         MVI   COXREL,C'Y'                                                      
                                                                                
         MVI   COXCDO,C'N'         PASS BACK "CANADIAN DOLLARS"                 
         TM    TACOSTAT,TACOSCAN   STATUS                                       
         JZ    *+8                                                              
         MVI   COXCDO,C'Y'                                                      
                                                                                
         MVI   COXCRT,C'N'         PASS BACK "CANADIAN RATES"                   
         TM    TACOSTAT,TACOSCRT   STATUS                                       
         JZ    *+8                                                              
         MVI   COXCRT,C'Y'                                                      
                                                                                
         MVI   COXWRK,C'N'         PASS BACK "WORK DATE ON CHECKS"              
         TM    TACOSTAT,TACOSWDT   STATUS                                       
         JZ    *+8                                                              
         MVI   COXWRK,C'Y'                                                      
                                                                                
         MVI   COXSOP,C'N'         PASS BACK "SOAP RESIDUALS"                   
         TM    TACOSTAT,TACOSRES   STATUS                                       
         JZ    *+8                                                              
         MVI   COXSOP,C'Y'                                                      
                                                                                
         MVI   COXPRT,C'N'         PASS BACK "DISPLAY AS PRINT"                 
         TM    TACOSTAT,TACOSPRT   STATUS                                       
         JZ    *+8                                                              
         MVI   COXPRT,C'Y'                                                      
                                                                                
         MVI   COXNCS,C'Y'         PASS BACK "CHARGE CSF"                       
         TM    TACOSTA2,TACOSNCS   STATUS                                       
         JZ    *+8                                                              
         MVI   COXNCS,C'N'                                                      
                                                                                
         MVI   COXPCY,C'N'         PASS BACK "PER CYCLE"                        
         TM    TACOSTA2,TACOPCYC   STATUS                                       
         JZ    *+8                                                              
         MVI   COXPCY,C'Y'                                                      
                                                                                
         MVI   COXHFN,C'Y'         PASS BACK "GENERATE HOLDING FEE              
         TM    TACOSTA3,TACOSNHF   NOTICES?" STATUS                             
         JZ    *+8                                                              
         MVI   COXHFN,C'Y'                                                      
                                                                                
         MVC   SVCOSTA2,TACOSTA2   SAVE SECOND STATUS                           
                                                                                
BCOX30A  GOTO1 VDATCON,DMCB,(1,TACOACT),(8,COXADT) ALWAYS PASS BACK             
         GOTO1 (RF),(R1),(1,TACOINAC),(8,COXIDT)   ACTIVE & INACTIVE            
         EDIT  TACOAUSE,COXAUS,ALIGN=LEFT             ALLOWABLE USES            
         EDIT  TACORUSE,COXRUS,ALIGN=LEFT             REMAINING USES            
         OC    TACOEDT,TACOEDT                                                  
         JZ    BCOX30B                                                          
         MVC   FULL1(L'TACOEDYR),TACOEDYR                  EDIT YEAR            
         XI    FULL1,X'FF'                                                      
         MVC   FULL1+1(2),=X'0101'                                              
         GOTO1 VDATCON,DMCB,(1,FULL1),(20,COXEYR)                               
         MVC   COXETY,TACOEDT                          AND EDIT TYPE            
                                                                                
BCOX30B  CLI   SVRECEQU,TLCOCDQ    IF PROCESSING COMMERCIAL RECORD              
         JNE   BCOX30C                                                          
         MVC   COXPOL,TACOCGRP     PASS BACK COMMERCIAL POOL CODE AND           
         GOTO1 VDATCON,DMCB,(1,TACOAEXP),(8,COXAEX) ACTRA EXPIRY DATE           
                                                                                
BCOX30C  CLI   SVRECEQU,TLVRCDQ    IF PROCESSING VERSION RECORD                 
         JNE   BCOX30                                                           
         MVI   COX26K,C'N'         PASS BACK "26K?" STATUS                      
         TM    TACOSTA3,TACOS26K                                                
         JZ    BCOX30                                                           
         MVI   COX26K,C'Y'                                                      
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
BCOX40   CLI   0(R4),TANAELQ       IF NAME ELEMENT                              
         JNE   BCOX50                                                           
         ZIC   RE,TANALEN                                                       
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   COXTIT(0),TANANAME  PASS BACK COMMERCIAL TITLE                   
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
BCOX50   CLI   0(R4),TAFNELQ       IF FREE FORM NAME ELEMENT                    
         JNE   BCOX60                                                           
         LA    RE,COXATI                                                        
         CLI   TAFNTYPE,TAFNTMUS   PASS BACK ... AFM TITLE                      
         JE    BCOX50A                                                          
         LA    RE,COXWID                                                        
         CLI   TAFNTYPE,TAFNTWEB                 WEB APPLICATION ID             
         JE    BCOX50A                                                          
         CLI   TAFNTYPE,TAFNTPRD                                                
         JNE   BCOX50B                                                          
         OC    COXPRD,COXPRD                     PRODUCT NAME                   
         JNZ   BCOX50B                                                          
         LA    RE,COXPRN                                                        
BCOX50A  ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RE),TAFNNAME                                                 
                                                                                
BCOX50B  CLI   TAFNTYPE,TAFNTWEB   IF WEB APPLICATION ID                        
         JE    BCOX50C                                                          
         CLI   TAFNTYPE,TAFNTOWB   OR ORIGINAL WEB APPLICATION                  
         JNE   BCOX30                                                           
         CLC   =C'VS',COXVSI       AND HAVE NOT ALREADY PASSED BACK             
         JE    BCOX30              THE VITA SESSION ID                          
         CLC   =C'RS',COXVSI                                                    
         JE    BCOX30                                                           
BCOX50C  CLC   =C'VS',TAFNNAME                                                  
         JE    BCOX50D                                                          
         CLC   =C'RS',TAFNNAME                                                  
         JNE   BCOX30                                                           
BCOX50D  ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   COXVSI(0),TAFNNAME  PASS BACK VITA SESSION ID                    
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TAFLD,R4                                                         
BCOX60   CLI   0(R4),TAFLELQ       IF FILTER ELEMENT                            
         JNE   BCOX70                                                           
         MVC   COXFL1,TAFLFLT1     PASS BACK FILTERS                            
         MVC   COXFL2,TAFLFLT2                                                  
         MVC   COXFL3,TAFLFLT3                                                  
         MVC   COXFL4,TAFLFLT4                                                  
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TALFD,R4                                                         
BCOX70   CLI   0(R4),TALFELQ       IF LIFT DETAILS ELEMENT                      
         JNE   BCOX80                                                           
         MVC   COXLID,TALFLID      PASS BACK LIFT ID                            
         EDIT  TALFSEC,COXLLN,ALIGN=LEFT  AND LENGTH                            
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TACSD,R4                                                         
BCOX80   CLI   0(R4),TACSELQ       IF COMMERCIAL STUDIO ELEMENT                 
         JNE   BCOX90                                                           
         CLI   TACSTYPE,TACSTYPF   FILM ELEMENT?                                
         JNE   BCOX80A                                                          
         MVC   COXFSU,TACSSTUD     PASS BACK FILM STUDIO                        
         MVC   COXFCY,TACSCITY     FILM CITY, FILM DATE                         
         GOTO1 VDATCON,DMCB,(1,TACSDATE),(8,COXFDT)                             
         CLI   TACSLEN,TACSLNQ                                                  
         JL    BCOX30                                                           
         MVC   COXFST,TACSSTAT     AND FILM STATE                               
         J     BCOX30                                                           
                                                                                
BCOX80A  CLI   TACSTYPE,TACSTYPR   RECORDING ELEMENT?                           
         JNE   BCOX80B                                                          
         MVC   COXRSU,TACSSTUD     PASS BACK RECORDING STUDIO                   
         MVC   COXRCY,TACSCITY     RECORDING CITY, RECORDING DATE               
         GOTO1 VDATCON,DMCB,(1,TACSDATE),(8,COXRDT)                             
         CLI   TACSLEN,TACSLNQ                                                  
         JL    BCOX30                                                           
         MVC   COXRST,TACSSTAT     AND RECORDING STATE                          
         J     BCOX30                                                           
                                                                                
BCOX80B  CLI   TACSTYPE,TACSTYPM   MUSIC ELEMENT?                               
         JNE   BCOX90                                                           
         MVC   COXMSU,TACSSTUD     PASS BACK MUSIC STUDIO                       
         MVC   COXMCY,TACSCITY     MUSIC CITY, MUSIC DATE                       
         GOTO1 VDATCON,DMCB,(1,TACSDATE),(8,COXMDT)                             
         CLI   TACSLEN,TACSLNQ                                                  
         JL    BCOX30                                                           
         MVC   COXMST,TACSSTAT     AND MUSIC STATE                              
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TAMDD,R4                                                         
BCOX90   CLI   0(R4),TAMDELQ       IF INTERNET/NEW MEDIA ELEMENT                
         JNE   BCOX100                                                          
         CLC   COXIN1,SPACES                                                    
         JH    BCOX90A                                                          
         MVC   COXIN1,TAMDCODE     PASS BACK INTERNET/NEW MEDIA CODE 1          
         J     BCOX30                                                           
BCOX90A  CLC   COXIN2,SPACES                                                    
         JH    BCOX90B                                                          
         MVC   COXIN2,TAMDCODE     PASS BACK INTERNET/NEW MEDIA CODE 2          
         J     BCOX30                                                           
BCOX90B  CLC   COXIN3,SPACES                                                    
         JH    BCOX90C                                                          
         MVC   COXIN3,TAMDCODE     PASS BACK INTERNET/NEW MEDIA CODE 3          
         J     BCOX30                                                           
BCOX90C  CLC   COXIN4,SPACES                                                    
         JH    BCOX90D                                                          
         MVC   COXIN4,TAMDCODE     PASS BACK INTERNET/NEW MEDIA CODE 4          
         J     BCOX30                                                           
BCOX90D  CLC   COXIN5,SPACES                                                    
         JH    BCOX90E                                                          
         MVC   COXIN5,TAMDCODE     PASS BACK INTERNET/NEW MEDIA CODE 5          
         J     BCOX30                                                           
BCOX90E  MVC   COXIN6,TAMDCODE     PASS BACK INTERNET/NEW MEDIA CODE 6          
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TACMD,R4                                                         
BCOX100  CLI   0(R4),TACMELQ       IF COMMENT ELEMENT                           
         JNE   BCOX110                                                          
         CLI   TACMTYPE,TACMTYPG   TYPE GENERAL COMMENT                         
         JNE   BCOX30                                                           
         ZIC   RE,TACMLEN                                                       
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   COXCMT(0),TACMCOMM  PASS BACK COMMERCIAL COMMENT                 
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
         USING TACPD,R4                                                         
BCOX110  CLI   0(R4),TACPELQ       IF PUBLISHED MUSIC ELEMENT                   
         JNE   BCOX120                                                          
         CLC   COXMU1,SPACES                                                    
         JH    BCOX110A                                                         
         MVC   COXMU1,TACPMUS      PASS BACK MUSIC CODE 1                       
         J     BCOX30                                                           
BCOX110A CLC   COXMU2,SPACES                                                    
         JH    BCOX110B                                                         
         MVC   COXMU2,TACPMUS      PASS BACK MUSIC CODE 2                       
         J     BCOX30                                                           
BCOX110B CLC   COXMU3,SPACES                                                    
         JH    BCOX110C                                                         
         MVC   COXMU3,TACPMUS      PASS BACK MUSIC CODE 3                       
         J     BCOX30                                                           
BCOX110C MVC   COXMU4,TACPMUS      PASS BACK MUSIC CODE 4                       
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
BCOX120  CLI   0(R4),TATRELQ       IF MUSIC CONTRACT/TRACK ELEMENT              
         JNE   BCOX130                                                          
         BAS   RE,OUTTATR          PASS BACK DETAILS                            
         J     BCOX30                                                           
                                                                                
         USING TAMCD,R4                                                         
BCOX130  CLI   0(R4),TAMCELQ       IF MUSIC CONTRACT DETAILS ELEMENT            
         JNE   BCOX140                                                          
         GOTO1 OUTTAMC,DMCB,(TAMCSEQ,0)                                         
         J     BCOX30                                                           
         DROP  R4                                                               
                                                                                
BCOX140  CLI   0(R4),TAVRELQ       IF VERSION ELEMENT                           
         JNE   BCOX30                                                           
         MVI   COXVER,1            PASS BACK VERSION NUMBER 1                   
         J     BCOX30                                                           
                                                                                
         USING TACOD,R4                                                         
BCOX150  CLI   SVRECEQU,TLVRCDQ    IF PROCESSING VERSION RECORD                 
         JNE   BCOX160                                                          
         L     R4,AIO3                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BCOX210             ALSO COPY MASTER COMMERCIAL ID               
         MVC   COXMID,TACOCID      INTO OUTPUT MAP                              
         J     BCOX210                                                          
         DROP  R4                                                               
                                                                                
BCOX160  TM    SVCOSTA2,TACOSJPC   IF PROCESSING COMMERCIAL RECORD              
         JZ    BCOX210             AND CSF HAS BEEN PAID                        
         OC    RQCOSCOM,RQCOSCOM   AND REQUEST INCLUDES INTERNAL                
         JNZ   BCOX170             COMMERCIAL NUMBER                            
         CLI   RQCOSEXT,C'Y'       OR IF SEARCHING FOR COMML ID                 
         JE    BCOX170             EXACTLY                                      
         CLI   RQCOSEXT,C'C'       OR IF SEARCHING FOR COMML ID                 
         JNE   BCOX210             USING CLARUS MATCHING RULES                  
                                                                                
         USING TLINPD,R3                                                        
BCOX170  XC    TLINPKEY,TLINPKEY   READ ALL INVOICES FOR THIS                   
         MVI   TLINPCD,TLINHCDQ    COMMERCIAL                                   
         MVC   TLINHCOM,SVCOM                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         J     BCOX190                                                          
BCOX180  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO5'                               
BCOX190  CLC   IOKEY(TLINHINV-TLINPD),IOKEYSAV                                  
         JNE   BCOX210                                                          
         CLI   TLINHSEQ,128        SKIP HISTORY COMMENTS                        
         JNH   BCOX180                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TABDD,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TABDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BCOX180                                                          
         OC    TABDCSF,TABDCSF     SKIP IF CONTRACT SERVICE FEE                 
         JZ    BCOX180             WAS NOT CHARGED                              
         DROP  R4                                                               
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL            SKIP IF CANCELLED AND CANCELLER              
         JE    *+6                 INVOICES                                     
         DC    H'00'                                                            
         TM    TAINSTAT,TAINSCIN+TAINSCAN                                       
         JNZ   BCOX180                                                          
         DROP  R4                                                               
                                                                                
         USING TAPDD,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BCOX200                                                          
         TM    TAPDSTA2,TAPDSSUB   SKIP SUBSIDIARY INVOICES                     
         JO    BCOX180                                                          
         DROP  R4                                                               
                                                                                
         USING TLIND,R4                                                         
BCOX200  L     R4,AIO5                                                          
         MVC   COXCAY,TLINAGY                                                   
         GOTOR (#CVTINV,ACVTINV),DMCB,TLININV,COXCIN                            
         DROP  R4                                                               
                                                                                
BCOX210  CLI   RQCOSTMA,C'Y'       IF RETURNING TRACK HAS MUSICIANS             
         JNE   BCOX290             STATUS                                       
         CLI   COXVER,2            AND VERSION IS 2 OR LOWER ...                
         JH    BCOX290                                                          
                                                                                
         LA    R2,COXA1A           FOR EACH TRACK ON THE COMMERCIAL/            
         LA    R5,COX1MA           VERSION, CHECK IF THERE IS A                 
         LHI   R0,COXMALNQ         MUSICIAN ATTACHED                            
                                                                                
BCOX220  CLI   COXA1T-COXA1A(R2),0                                              
         JE    BCOX280                                                          
                                                                                
         MVI   0(R5),C'N'                                                       
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY                                                  
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,SVCOM                                                    
         OC    COXA1O,COXA1O                                                    
         JZ    BCOX230                                                          
         GOTO1 VHEXIN,DMCB,COXA1O-COXA1A(R2),TLCACOM,L'COXA1O                   
BCOX230  OI    TLCASORT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         J     BCOX250                                                          
BCOX240  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO5'                               
BCOX250  CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   BCOX280                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TAFND,R4                                                         
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO5),('TAFNTTRK',0)         
         JNE   BCOX240                                                          
         L     R4,AELEM                                                         
                                                                                
         CLI   TAFNNAME,C'*'                                                    
         JE    BCOX270                                                          
                                                                                
         ZIC   RE,TAFNLEN                                                       
         SHI   RE,3                                                             
         LA    R4,TAFNNAME                                                      
         DROP  R4                                                               
                                                                                
BCOX260  CLC   COXA1T-COXA1A(L'COXA1T,R2),0(R4)                                 
         JE    BCOX270                                                          
         LA    R4,1(R4)                                                         
         BCT   RE,BCOX260                                                       
         J     BCOX240                                                          
                                                                                
BCOX270  MVI   0(R5),C'Y'                                                       
                                                                                
BCOX280  LA    R2,COXA1LNQ(R2)                                                  
         LA    R5,1(R5)                                                         
         BCT   R0,BCOX220                                                       
                                                                                
BCOX290  CLI   RQCOSAMT,C'Y'       IF RETURNING ALL MUSICIANS ON                
         JNE   BCOX320             TRACK STATUS                                 
         CLI   COXTYP,CTYMUS       AND COMMERCIAL TYPE IS MUSIC ...             
         JNE   BCOX320                                                          
                                                                                
         MVI   COXAMT,C'Y'                                                      
                                                                                
         USING TLCAD,R3                                                         
         XC    TLCAKEY,TLCAKEY     FOR EACH MUSICIAN ON THE                     
         MVI   TLCACD,TLCACDQ      COMMERCIAL, CHECK IF THEY HAVE               
         MVC   TLCACOM,SVCOM       AN ASSOCIATED TRACK                          
         OI    TLCASORT,X'80'                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         J     BCOX310                                                          
BCOX300  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO5'                               
BCOX310  CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   BCOX320                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO5),('TAFNTTRK',0)         
         JE    BCOX300                                                          
         MVI   COXAMT,C'N'                                                      
                                                                                
BCOX320  CLI   RQCOSEXT,C'C'                                                    
         JNE   BCOX330                                                          
         CLC   COXCID,SRCHCID                                                   
         JE    BCOX330                                                          
         MVC   COXSCI,SRCHCID                                                   
                                                                                
BCOX330  OC    RQCOSPED,RQCOSPED                                                
         JZ    BCOX340                                                          
         MVC   COXEBD,EBD                                                       
                                                                                
BCOX340  MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OUTPUT MUSIC CONTRACT/TRACK DETAILS                          *         
*        ON ENTRY ... R4=(MUSIC CONTRACT/TRACK DETAILS ELEMENT)       *         
***********************************************************************         
                                                                                
OUTTATR  NTR1                                                                   
         USING TATRD,R6                                                         
         LR    R6,R4                                                            
         LA    R2,COXA1A                                                        
         ZIC   RE,TATRSEQ                                                       
         MHI   RE,COXA1LNQ                                                      
         AR    R2,RE                                                            
                                                                                
         USING TLCOPD,R3                                                        
         XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,TATRCOM                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JNE   XIT                                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO5                                                          
         MVC   0(L'COXA1A,R2),TLCOAGY                                           
         DROP  R4                                                               
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAMCELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
OTATR10  BRAS  RE,NEXTEL                                                        
         JNE   OTATRX                                                           
         CLC   TAMCTRK,TATRTRK                                                  
         JNE   OTATR10                                                          
                                                                                
         GOTO1 VHEXOUT,DMCB,TATRCOM,COXA1O-COXA1A(R2),L'TATRCOM,0               
         MVC   COXA1T-COXA1A(L'COXA1T,R2),TATRTRK                               
         GOTO1 OUTTAMC,DMCB,(TATRSEQ,0)                                         
         DROP  R6                                                               
                                                                                
         OC    TAMCCON,TAMCCON                                                  
         JNZ   OTATRX                                                           
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   COXA1C-COXA1A(L'COXA1C,R2),TACOCID                               
         DROP  R4                                                               
                                                                                
OTATRX   MVI   ELCODE,0                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        OUTPUT MUSIC CONTRACT DETAILS                                *         
*        ON ENTRY ... P1 BYTE 0 = SEQUENCE NUMBER                     *         
*                     R4=(MUSIC CONTRACT DETAILS ELEMENT)             *         
***********************************************************************         
                                                                                
         USING TAMCD,R4                                                         
OUTTAMC  NTR1                                                                   
         LA    R2,COXA1A                                                        
         ZIC   RE,0(R1)                                                         
         MHI   RE,COXA1LNQ                                                      
         AR    R2,RE                                                            
                                                                                
         MVC   COXA1C-COXA1A(L'COXA1C,R2),TAMCCON      CONTRACT CODE            
         MVC   COXA1T-COXA1A(L'COXA1T,R2),TAMCTRK      TRACK                    
         MVC   COXA1L-COXA1A(L'COXA1L,R2),TAMCLFT      LIFT                     
         EDIT  TAMCLLEN,(L'COXA1S,COXA1S-COXA1A(R2)),ALIGN=LEFT                 
         MVC   COXA1Y-COXA1A(L'COXA1Y,R2),TAMCTYP      MUSIC TYPE               
         CLI   TAMCLEN,TAMCLNQ                                                  
         JE    XIT                                                              
         ZIC   RE,TAMCLEN                                                       
         SHI   RE,TAMCLNQ                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   COXA1I-COXA1A(0,R2),TAMCTRKT            TITLE                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD MUSIC CONTRACTS TO TABLE                                 *         
*        ON ENTRY ... R4=(COMMERCIAL/VERSION RECORD)                  *         
***********************************************************************         
                                                                                
BLDAACNS NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCOSAFM,C'Y'       IF RETURNING ATTACHED AFM CONTRACTS          
         JNE   XIT                                                              
                                                                                
         USING TATRD,R4                                                         
         MVI   ELCODE,TATRELQ      READ THROUGH ALL MUSIC CONTRACT              
         BRAS  RE,GETEL            ELEMENTS AND ADD THEM TO MUSIC               
         J     *+8                 CONTRACT LIST                                
BAAC10   BRAS  RE,NEXTEL                                                        
         JNE   XIT                                                              
         LA    R2,SVAACNS                                                       
BAAC20   CLC   TATRCOM,0(R2)                                                    
         JE    BAAC10                                                           
         CLI   0(R2),X'FF'                                                      
         JE    BAAC30                                                           
         LA    R2,L'TATRCOM(R2)                                                 
         J     BAAC20                                                           
BAAC30   MVC   0(L'TATRCOM,R2),TATRCOM                                          
         MVI   L'TATRCOM(R2),X'FF'                                              
         J     BAAC10                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PASS BACK VERSION ALIAS                                      *         
*        ON ENTRY ... R4=(COMMERCIAL/VERSION RECORD)                  *         
***********************************************************************         
                                                                                
BLDALS   NTR1  BASE=*,LABEL=*                                                   
         USING TLAKD,R3                                                         
         CLI   COXVER,0            IF THIS IS A VERSION                         
         JE    XIT                                                              
         LA    R3,IOKEY                                                         
         XC    TLAKKEY,TLAKKEY     AND VERSION IS ALIASED TO ANOTHER            
         MVI   TLAKCD,TLAKCDQ      VERSION, RETURN THE ALIASED                  
         MVC   TLAKAGY,COXAGY      VERSION NUMBER                               
         MVC   TLAKADID,COXCID                                                  
         MVC   TLAKNCLI,COXCLI                                                  
         MVC   TLAKNPRD,COXPRD                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     BALS20                                                           
BALS10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
BALS20   CLC   IOKEY(TLAKMED-TLAKD),IOKEYSAV                                    
         JNE   BALS30                                                           
         CLC   TLAKCOM,SVCOM                                                    
         JNE   BALS10                                                           
         MVC   COXALS,TLAKVER                                                   
         DROP  R3                                                               
                                                                                
BALS30   MVC   IOKEY,SVIOKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PASS BACK VERSION PAID STATUS                                *         
***********************************************************************         
                                                                                
BLDPAD   NTR1  BASE=*,LABEL=*                                                   
         CLI   RQCOSPAD,C'Y'                                                    
         JNE   XIT                                                              
         MVI   COXPAD,C'N'                                                      
                                                                                
         LA    R2,PVRTAB                                                        
BPAD10   CLI   0(R2),X'FF'                                                      
         JE    XIT                                                              
         CLC   COXVER,0(R2)                                                     
         JE    BPAD20                                                           
         LA    R2,L'COXVER(R2)                                                  
         J     BPAD10                                                           
                                                                                
BPAD20   MVI   COXPAD,C'Y'                                                      
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PASS BACK ATTACHED COMMENT INDICATORS                        *         
*        ON ENTRY ... R3=(IOKEY)                                      *         
***********************************************************************         
                                                                                
         USING TLCMD,R3                                                         
BLDCMT   NTR1  BASE=*,LABEL=*                                                   
         MVI   COXATC,C'N'         INITIALIZE ATTACHED COMMENT                  
         MVI   COXACC,C'N'         INDICATORS                                   
                                                                                
         XC    TLCMKEY,TLCMKEY     READ ALL COMMENT RECORDS FOR THIS            
         MVI   TLCMCD,TLCMCDQ      COMMERCIAL                                   
         MVC   TLCMAGY,COXAGY                                                   
         MVI   TLCMTYP,TLCMTCOM                                                 
         MVC   TLCMCID,COXCID                                                   
         MVC   TLCMICOM,SVCOM                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     BCMT20                                                           
BCMT10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
BCMT20   CLC   IOKEY(TLCMVER-TLCMD),IOKEYSAV                                    
         JNE   BCMT40                                                           
         CLI   TLCMVER,C'1'        SKIP ANY THAT ARE FOR VERSION 2              
         JH    BCMT10              OR HIGHER                                    
                                                                                
         CLI   TLCMLEV,0           IF CLIENT LEVEL COMMENT                      
         JNE   BCMT30                                                           
         MVI   COXACC,C'Y'         SET INDICATOR                                
         J     BCMT10              AND GO LOOK FOR TP COMMENT                   
         DROP  R3                                                               
                                                                                
BCMT30   MVI   COXATC,C'Y'         IF TP COMMENT, SET INDICATOR                 
         J     BCMT10                                                           
                                                                                
BCMT40   MVC   IOKEY,SVIOKEY       RESTORE READ SEQUENCE                        
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT COMMERCIAL SEARCH LIMIT STATUS RECORD                 *         
***********************************************************************         
                                                                                
NXTCOL   J     *+12                                                             
         DC    C'*NXTCOL*'                                                      
         LR    RB,RF                                                            
         USING NXTCOL,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         OC    RQCOSLIM,RQCOSLIM                                                
         JZ    NOMORE                                                           
         MVI   COLSTAT,C'N'                                                     
         CLI   RQCOSLIM,X'FF'                                                   
         JNE   NCOLX                                                            
         MVI   COLSTAT,C'Y'                                                     
                                                                                
NCOLX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,COLVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT GUARANTEE SEARCH STATUS RECORD                        *         
***********************************************************************         
                                                                                
NXTGUS   J     *+12                                                             
         DC    C'*NXTGUS*'                                                      
         LR    RB,RF                                                            
         USING NXTGUS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         MVI   GUSSTAT,GUSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,GUVALREQ         VALIDATE REQUEST                             
         JNE   NGUSX                                                            
                                                                                
         MVI   GUSSTAT,GUSSUNS     REINITIALIZE STATUS TO UNSUCCESSFUL          
                                                                                
         OC    RQGUSPEN,RQGUSPEN   IF PERIOD END DATE IS NOT PROVIDED           
         JNZ   *+10                DEFAULT TO "THE END OF TIME"                 
         MVC   RQGUSPEN,=3X'FF'                                                 
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
                                                                                
         BAS   RE,SVGUSCRP         IF FILTERING ON CORPORATION CODE             
         JNE   NOMORE              TRANSLATE TO CORP. SEQUENCE NUMBER           
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,(X'80',RQGUSSTF)                          
         JNE   NOMORE              SAVE AGENCY/CLIENT LIMITS                    
                                                                                
         USING TLGUD,R3                                                         
         XC    TLGUKEY,TLGUKEY                                                  
         MVI   TLGUCD,TLGUCDQ      GUARANTEE RECORD                             
         MVC   TLGUSSN,RQGUSSSN    SSN                                          
         LHI   R0,TLGUSSN+L'TLGUSSN-TLGUD-1   R0 = LENGTH OF COMPARE            
         OC    RQGUSGUA,RQGUSGUA   IF GUARANTEE CODE IS PROVIDED                
         JZ    NGUS10                                                           
         MVC   TLGUGUA,RQGUSGUA     GUARANTEE CODE                              
         XC    TLGUGUA,=4X'FF'      COMPLEMENTED                                
         LHI   R0,TLGUGUA+L'TLGUGUA-TLGUD-1   R0 = LENGTH OF COMPARE            
         DROP  R3                                                               
                                                                                
NGUS10   STC   R0,LKEYCOMP         SAVE L' KEY COMPARE                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTGRT           IF CAST RECORD MATCHES ALL FILTERS           
         JNE   NGUSX               RETURN SEARCH SUCCESSFUL STATUS              
         MVI   GUSSTAT,GUSSSUC                                                  
         TM    ERRSTAT,ESREVIW                                                  
         JZ    NGUSX                                                            
         MVI   GUSSTAT,GUSSEMB                                                  
                                                                                
NGUSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,GUSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
GUVALREQ NTR1                                                                   
         OC    RQGUSSTF,RQGUSSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
         OC    RQGUSSSN,RQGUSSSN   ASSERT THAT SOCIAL SECURITY NUMBER           
         JZ    NO                  IS PROVIDED                                  
                                                                                
         OC    RQGUSCLI,RQGUSCLI   IF CLIENT IS PROVIDED                        
         JZ    GVR10                                                            
         OC    RQGUSAGY,RQGUSAGY   ASSERT THAT AGENCY IS PROVIDED               
         JZ    NO                                                               
                                                                                
GVR10    CLI   RQGUSTYP,0          ENFORCE VALID VALUES FOR TYPE FILTER         
         JE    GVR20                                                            
         CLI   RQGUSTYP,C'L'                                                    
         JE    GVR20                                                            
         CLI   RQGUSTYP,C'P'                                                    
         JNE   NO                                                               
                                                                                
GVR20    OC    RQGUSPEN,RQGUSPEN   IF PERIOD END DATE IS PROVIDED               
         JZ    GVR30                                                            
         OC    RQGUSPST,RQGUSPST   ENSURE THAN PERIOD START DATE                
         JZ    NO                  IS PROVIDED                                  
                                                                                
GVR30    GOTOR (#VALFLD,AVALFLD),DMCB,('VFYORN',RQGUSLCK)                       
         JE    YES                                                              
         J     NO                                                               
                                                                                
***********************************************************************         
*        IF FILTERING ON CORPORATION FID, TRANSLATE TO                *         
*        W4'S SEQUENCE NUMBER                                         *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLW4D,R3                                                         
SVGUSCRP NTR1                                                                   
         OC    RQGUSCRP,RQGUSCRP                                                
         JZ    YES                                                              
         XC    0(L'TLW4KEY,R3),0(R3)                                            
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,RQGUSSSN                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NO                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         DROP  R3                                                               
                                                                                
         USING TATID,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
SGC10    BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
         CLI   TATITYPE,TATITYCO                                                
         JNE   SGC10                                                            
         CLC   RQGUSCRP,TATIID                                                  
         JNE   SGC10                                                            
         MVC   RQGUSCPN,TATICRPN                                                
         J     YES                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT GUARANTEE SEARCH EXPANDED DETAILS RECORDS             *         
***********************************************************************         
                                                                                
NXTGUX   J     *+12                                                             
         DC    C'*NXTGUX*'                                                      
         LR    RB,RF                                                            
         USING NXTGUX,RB                                                        
                                                                                
         CLI   GUSSTAT,GUSSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
                                                                                
         LA    R0,GUXVALS          CLEAR VARIABLES                              
         LHI   R1,GUXVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   SVCORP,0                                                         
         XC    SVCOM,SVCOM                                                      
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NGUX10              READ NEXT GUARANTEE RECORD                   
         OC    RQGUSGUA,RQGUSGUA                                                
         JNZ   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTGRT                                                        
         JNE   NOMORE                                                           
                                                                                
         USING TLGUD,R4                                                         
NGUX10   L     R4,AIO3             R4=A(GRT RECORD)                             
         MVC   GUXSSN,TLGUSSN      SOCIAL SECURITY NUMBER                       
         MVC   GUXGRT,TLGUGUA      AND GUARANTEE CODE                           
         XC    GUXGRT,=4X'FF'      UNCOMPLEMENTED                               
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,0                                                         
         BRAS  RE,GETEL            READ THROUGH ALL ELEMENTS                    
         J     *+8                                                              
NGUX20   BRAS  RE,NEXTEL                                                        
         JNE   NGUX100                                                          
                                                                                
         USING TAGUD,R4                                                         
         CLI   0(R4),TAGUELQ       PROCESS GUARANTEE DETAILS ELEMENT            
         JNE   NGUX40                                                           
                                                                                
         MVC   GUXAGY,TAGUAGY      ALWAYS PASS BACK PRIMARY AGENCY              
         MVC   GUXCLI,TAGUCLI      PRIMARY CLIENT                               
         MVC   SVCORP,TAGUCRP      AND SAVE CORPORATION                         
                                                                                
         MVI   GUXLCK,C'N'         PASS BACK "LOCKED?"                          
         TM    TAGUSTAT,TAGUSLCK                                                
         JZ    *+8                                                              
         MVI   GUXLCK,C'Y'                                                      
                                                                                
         OC    TAGUCOM,TAGUCOM     IF TYPE IS LARGE OVERSCALE                   
         JNZ   NGUX30                                                           
         MVI   GUXTYP,C'L'         PASS BACK TYPE                               
         GOTO1 VDATCON,DMCB,(1,TAGUSTRT),(8,GUXPST) PERIOD START DATE           
         GOTO1 (RF),(R1),(1,TAGUEND),(8,GUXPEN)     PERIOD END DATE             
         MVC   GUXAMT,TAGUAMT      PASS BACK AMOUNT                             
         MVC   GUXBAL,TAGUBAL      BALANCE                                      
         MVC   GUXPAY,TAGUPAY      INSTALLMENT AMOUNT                           
         GOTOR (#CVTINV,ACVTINV),DMCB,TAGUINV,GUXINV    INVOICE                 
                                                                                
         MVI   GUXBDR,C'D'         PASS BACK BALANCE DIRECTION                  
         TM    TAGUSTAT,TAGUSDES                                                
         JO    *+8                                                              
         MVI   GUXBDR,C'A'                                                      
                                                                                
         MVC   GUXGAG,TAGUIAY      PASS BACK AGENCY THAT ADDED GRT              
                                                                                
         MVI   GUXPOV,C'N'         PASS BACK "PAY OVERAGE?"                     
         TM    TAGUSTAT,TAGUSOVR                                                
         JZ    *+8                                                              
         MVI   GUXPOV,C'Y'                                                      
                                                                                
         MVI   GUXIGP,C'N'         PASS BACK "IGNORE PAY OVERAGE EST?"          
         TM    TAGUSTA2,TAGUSIGP                                                
         JZ    *+8                                                              
         MVI   GUXIGP,C'Y'                                                      
                                                                                
         MVI   GUXPNH,C'N'         PASS BACK "PAY PNH?"                         
         TM    TAGUSTAT,TAGUSPNH                                                
         JZ    *+8                                                              
         MVI   GUXPNH,C'Y'                                                      
                                                                                
         MVI   GUXINP,C'N'         PASS BACK "INSTALLMENT PYMT PAID?"           
         TM    TAGUSTAT,TAGUSINS                                                
         JZ    NGUX20                                                           
         MVI   GUXINP,C'Y'                                                      
         J     NGUX20                                                           
                                                                                
NGUX30   MVI   GUXTYP,C'P'          IF TYPE IS PER CYCLE                        
         GOTO1 VHEXOUT,DMCB,TAGUCOM,GUXCOM,L'TAGUCOM,0                          
         MVC   SVCOM,TAGUCOM        PASS BACK PRIMARY COMMERCIAL ID             
         J     NGUX20                                                           
         DROP  R4                                                               
                                                                                
NGUX40   CLI   0(R4),TAGCELQ       PROCESS GUARANTEE CYCLE ELEMENT              
         JNE   NGUX50                                                           
         OC    RQGUSPST,RQGUSPST   IF NOT FILTERING ON PERIOD START             
         JNZ   NGUX50                                                           
         CLC   RQGUSPEN,=3X'FF'    OR PERIOD END DATES                          
         JNE   NGUX50                                                           
         ST    R4,ATAGCEL          SAVE A(LAST GUARANTEE CYCLE ELEMENT)         
         J     NGUX50                                                           
                                                                                
         USING TAGXD,R4                                                         
NGUX50   CLI   0(R4),TAGXELQ       PROCESS EXCLUDED USES ELEMENT                
         JNE   NGUX90              (PER CYCLE GRTS ONLY)                        
         ZIC   R0,TAGXLEN                                                       
         SHI   R0,TAGXLNQ          R3 = # OF EXCLUDED USES                      
         CHI   R0,13               CANNOT BE > 13                               
         JNH   *+8                                                              
         LHI   R0,13                                                            
         LA    R1,TAGXUSE          R1 = LIST OF EXCLUDED USES                   
         LA    R2,GUXEX1           EXCLUDE USE #1                               
                                                                                
         USING TGTABLES,RE                                                      
NGUX60   L     RE,VSYSTAB          RE=A(TALENT SYSTEM TABLES)                   
         L     RF,TGAUSES          RF=DISPLACEMENT OF USES TABLE                
         DROP  RE                                                               
                                                                                
         USING USETABD,RF                                                       
         AR    RF,RE               FIND USE EQUATE IN USE TABLE                 
NGUX70   CLC   0(L'TAGXUSE,R1),USEEQU                                           
         JE    NGUX80                                                           
         LH    RE,USELEN                                                        
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JNE   NGUX70                                                           
         DC    H'00'                                                            
                                                                                
NGUX80   MVC   0(L'GUXEX1,R2),USECDE                                            
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         JZ    NGUX20                                                           
         LA    R2,L'GUXEX1(R2)     NEXT EXCLUDED USE                            
         LA    R1,L'TAGXUSE(R1)    NEXT EXCLUDED USE IN LIST                    
         J     NGUX60                                                           
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
NGUX90   CLI   0(R4),TAFNELQ       IF FREE FORM NAME ELEMENT                    
         JNE   NGUX20                                                           
         CLI   TAFNTYPE,TAFNTWEB   NAME TYPE WEB APPLICATION ID                 
         JNE   NGUX20                                                           
         ZIC   RF,TAFNLEN                                                       
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   GUXWID,TAFNNAME     PASS BACK WEB APPLICATION ID                 
         J     NGUX20                                                           
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
NGUX100  OC    ATAGCEL,ATAGCEL     IF WE HAVE A(GUARANTEE CYCLE ELEM)           
         JZ    NGUX110                                                          
         L     R4,ATAGCEL                                                       
         GOTO1 VDATCON,DMCB,(1,TAGCSTRT),(8,GUXPST) PERIOD START DATE           
         GOTO1 (RF),(R1),(1,TAGCEND),(8,GUXPEN)     PERIOD END DATE             
         MVC   GUXAMT,TAGCAMT                       AMOUNT                      
         MVC   GUXBAL,TAGCBAL                       BALANCE                     
         DROP  R4                                                               
                                                                                
NGUX110  MVC   SVIOKEY,IOKEY        SAVE CURRENT GRT KEY                        
                                                                                
         USING TLCOPD,R3                                                        
         CLI   GUXTYP,C'P'          IF PER CYCLE GUARANTEE                      
         JNE   NGUX120                                                          
         XC    TLCOPKEY,TLCOPKEY    READ FOR PRIMARY COMMERCIAL                 
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVCOM                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NGUX120                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TLCOD,R4                                                         
         L     R4,IOADDR           R4=A(COMMERCIAL RECORD)                      
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL            GET COMMERCIAL DETAILS ELEMENT               
         JNE   NGUX120                                                          
         MVC   GUXISC,TACOCID      PASS BACK PRIMARY COMMERCIAL ID              
         DROP  R4                                                               
                                                                                
         USING TLW4D,R3                                                         
NGUX120  CLI   SVCORP,0            IF GUARANTEE HAS AN ATTACHED CORP            
         JE    NGUX140                                                          
         MVC   GUXFID,RQGUSCPN                                                  
         CLI   RQGUSCPN,0          AND WE'RE NOT FILTERING BY THE CORP          
         JE    NGUX140                                                          
         XC    TLW4KEY,TLW4KEY     READ CORPORATION'S W4 RECORD                 
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,GUXSSN                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NGUX140                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TATID,R4                                                         
         L     R4,IOADDR           R4=A(W4 RECORD)                              
         MVI   ELCODE,TATIELQ                                                   
         BRAS  RE,GETEL            GET TAX ID ELEMENT                           
         J     *+8                                                              
NGUX130  BRAS  RE,NEXTEL                                                        
         JNE   NGUX140                                                          
         CLC   TATICRPN,SVCORP     FIND MATCHING CORPORATION CODE               
         JNE   NGUX130                                                          
         MVC   GUXFID,TATIID       ATTACHED CORPORATION FID#                    
         DROP  R4                                                               
                                                                                
         USING TLW4D,R3                                                         
         XC    TLW4KEY,TLW4KEY     READ FOR CORP W4 KEY                         
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,GUXFID      CORP FID                                     
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NGUX140                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,IOADDR           R4=A(W4 RECORD)                              
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL            GET W4 DETAILS ELEMENT                       
         JNE   NGUX140                                                          
         MVC   GUXCRP,TAW4CRPN     ATTACHED CORPORATION NAME                    
         DROP  R4                                                               
                                                                                
         USING TLCMD,R3                                                         
NGUX140  MVI   GUXCMT,C'N'                                                      
         XC    TLCMKEY,TLCMKEY     PASS BACK ATTACHED COMMENT                   
         MVI   TLCMCD,TLCMCDQ      INDICATOR                                    
         MVI   TLCMTYP,TLCMTGUA                                                 
         MVC   TLCMSSN,RQGUSSSN                                                 
         MVC   TLCMGUA,GUXGRT                                                   
         XC    TLCMGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JNE   NGUXX                                                            
         MVI   GUXCMT,C'Y'                                                      
         DROP  R3                                                               
                                                                                
NGUXX    MVC   IOKEY,SVIOKEY        RESTORE GRT READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,GUXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT GUARANTEE SEARCH SUBSIDIARY AGENCY/CLIENT RECORDS     *         
***********************************************************************         
                                                                                
NXTGUU   J     *+12                                                             
         DC    C'*NXTGUU*'                                                      
         LR    RB,RF                                                            
         USING NXTGUU,RB                                                        
                                                                                
         CLI   GUSSTAT,GUSSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
                                                                                
         USING TAVAD,R4                                                         
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NGUU10                                                           
         XC    GUUVALS(GUUVALL),GUUVALS                                         
                                                                                
         L     R4,IOADDR                                                        
         MVI   ELCODE,TAVAELQ      READ FIRST/NEXT AGENCY/CLIENT                
         BRAS  RE,GETEL            ELEMENT                                      
         J     NGUU20                                                           
NGUU10   L     R4,SVADDR                                                        
         OC    SVCLIA,SVCLIA       ARE WE ALREADY IN THE ELEMENT?               
         JNZ   NGUU40                                                           
         BRAS  RE,NEXTEL                                                        
NGUU20   JNE   NOMORE                                                           
         ST    R4,SVADDR           FIRST TIME IN THE ELEMENT                    
                                                                                
         ZIC   RE,TAVALEN          ELEMENT LENGTH                               
         AR    RE,R4               RE --> END OF ELEMENT                        
         ST    RE,SVADDRE          SAVE END OF ELEMENT ADDRESSS                 
                                                                                
         LA    R6,TAVACLI          R5=A(CURRENT CLIENT IN ELEMENT)              
         J     *+8                                                              
NGUU40   L     R6,SVCLIA           AFTER 1ST TIME, USED SAVED ADDRESS           
         MVC   GUUAGY,TAVAAGY      AGENCY                                       
         XC    GUUCLI,GUUCLI       CLEAR CLIENT IN CASE THERE IS NONE           
         C     R6,SVADDRE          IF NO CLIENTS IN ELEMENT,                    
         JNL   NGUU50              DONE WITH THIS ELEMENT                       
         MVC   GUUCLI,0(R6)        CLIENT                                       
         LA    R6,L'TAVACLI(R6)    NEXT CLIENT                                  
         ST    R6,SVCLIA           SAVE ADDRESS OF NEXT CLIENT                  
         C     R6,SVADDRE          IF NO MORE CLIENTS IN ELEMENT,               
         JL    NGUUX                                                            
NGUU50   XC    SVCLIA,SVCLIA       CLEAR SAVED CLIENT ADDRESS                   
         XC    SVADDRE,SVADDRE     AND END OF ELEMENT ADDRESS                   
                                                                                
NGUUX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,GUUVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT GUARANTEE SEARCH PER CYCLE PERIOD DETAIL RECORDS      *         
***********************************************************************         
                                                                                
NXTGUP   J     *+12                                                             
         DC    C'*NXTGUP*'                                                      
         LR    RB,RF                                                            
         USING NXTGUP,RB                                                        
                                                                                
         CLI   GUSSTAT,GUSSSUC     EXIT IF INITIAL SEARCH WAS                   
         JNE   NOMORE              NOT SUCCESSFUL                               
         CLI   GUXTYP,C'P'         OR CURRENT GUARANTEE IS NOT                  
         JNE   NOMORE              PER CYCLE                                    
                                                                                
         CLI   LP_RMODE,LP_RFRST   INITIALIZE VARIABLES                         
         JNE   NGUP10                                                           
         XC    GUPVALS(GUPVALL),GUPVALS                                         
                                                                                
         USING TAGCD,R4                                                         
         L     R4,IOADDR                                                        
         MVI   ELCODE,TAGCELQ      READ FIRST/NEXT GUARANTEE CYCLE              
         BRAS  RE,GETEL            ELEMENT                                      
         J     NGUP20                                                           
NGUP10   L     R4,SVADDR                                                        
         BRAS  RE,NEXTEL                                                        
NGUP20   JNE   NOMORE                                                           
         ST    R4,SVADDR                                                        
                                                                                
         CLC   RQGUSPST,TAGCEND    FILTER START DATE MUST BE EARLIER            
         JH    NGUP10              OR EQUAL TO END DATE                         
         CLC   TAGCSTRT,RQGUSPEN   AND START DATE MUST BE EARLIER OR            
         JH    NGUP10              EQUAL TO FILTER END DATE                     
                                                                                
         GOTO1 VDATCON,DMCB,(1,TAGCSTRT),(8,GUPPST) PERIOD START DATE           
         GOTO1 (RF),(R1),(1,TAGCEND),(8,GUPPEN)     PERIOD END DATE             
         MVC   GUPAMT,TAGCAMT                       AMOUNT                      
         MVC   GUPBAL,TAGCBAL                       BALANCE                     
                                                                                
NGUPX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,GUPVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT GUARANTEE SEARCH COMMENT RECORDS                      *         
***********************************************************************         
                                                                                
NXTGUC   J     *+12                                                             
         DC    C'*NXTGUC*'                                                      
         LR    RB,RF                                                            
         USING NXTGUC,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST   ONLY EXECUTE FIRST TIME IN                   
         JNE   NOMORE                                                           
         OC    RQGUSGUA,RQGUSGUA   IF GUARANTEE CODE WAS PROVIDED               
         JZ    NOMORE              IN REQUEST                                   
         CLI   GUSSTAT,GUSSSUC     AND GUARANTEE WAS FOUND                      
         JNE   NOMORE                                                           
                                                                                
         USING TLCMD,R3                                                         
         LA    R3,IOKEY                                                         
         XC    0(L'TLCMKEY,R3),0(R3)                                            
         MVI   TLCMCD,TLCMCDQ                                                   
         MVI   TLCMTYP,TLCMTGUA                                                 
         MVC   TLCMSSN,RQGUSSSN                                                 
         MVC   TLCMGUA,RQGUSGUA                                                 
         XC    TLCMGUA,=4X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   NOMORE                                                           
         DROP  R3                                                               
                                                                                
         LA    R0,GUCVALS          CLEAR VARIABLES                              
         LHI   R1,GUCVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TAXCD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAXCELQ      READ FIRST EXTENDED COMMENT ELEMENT          
         BRAS  RE,GETEL                                                         
         JNE   NOMORE                                                           
                                                                                
NGUC10   LA    RF,GUCCM1           COPY EXTENDED COMMENT5 INTO                  
         CLI   TAXCSEQ,1           CORRESPONDING OUTPUT LINE                    
         JE    NGUC20                                                           
         ZIC   RE,TAXCSEQ                                                       
         SHI   RE,1                                                             
         MHI   RE,L'GUCCM1                                                      
         AR    RF,RE                                                            
NGUC20   ZIC   RE,TAXCLEN                                                       
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   0(0,RF),TAXCCMNT                                                 
         DROP  R4                                                               
                                                                                
         BRAS  RE,NEXTEL           GET NEXT EXTENDED COMMENT ELEMENT            
         JE    NGUC10                                                           
                                                                                
         MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,GUCVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT NEW MEDIA/INTERNET SEARCH STATUS RECORD               *         
***********************************************************************         
                                                                                
NXTNIS   J     *+12                                                             
         DC    C'*NXTNIS*'                                                      
         LR    RB,RF                                                            
         USING NXTNIS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
         MVI   NISSTAT,NISSUNS     INITIALIZE STATUS TO UNSUCCESSFUL            
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQNISSTF                                  
         JNE   NOMORE                                                           
                                                                                
         USING TLMDD,R3                                                         
         XC    TLMDKEY,TLMDKEY                                                  
         MVI   TLMDCD,TLMDCDQ                                                   
         MVI   TLMDTYPE,INTERNET                                                
         CLI   RQNISMED,C'I'                                                    
         JE    *+8                                                              
         MVI   TLMDTYPE,NEWMEDIA                                                
         LHI   R0,TLMDCODE-TLMDD-1                                              
         OC    RQNISCOD,RQNISCOD                                                
         JZ    NNIS10                                                           
         MVC   TLMDCODE,RQNISCOD                                                
         LHI   R0,L'TLMDKEY                                                     
         DROP  R3                                                               
                                                                                
NNIS10   STC   R0,LKEYCOMP         SAVE L' KEY COMPARE                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         BRAS  RE,FLTNI            IF NEW MEDIA/INTERNET MATCHES                
         JNE   NGUSX               ALL FILTERS                                  
         MVI   NISSTAT,NISSSUC     RETURN SUCCESSFUL STATUS                     
         TM    ERRSTAT,ESREVIW                                                  
         JZ    NNISX                                                            
         MVI   NISSTAT,NISSEMB                                                  
                                                                                
NNISX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,NISVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT NEW MEDIA/INTERNET EXPANDED DETAILS RECORDS           *         
***********************************************************************         
                                                                                
NXTNIX   J     *+12                                                             
         DC    C'*NXTNIX*'                                                      
         LR    RB,RF                                                            
         USING NXTNIX,RB                                                        
                                                                                
         CLI   NISSTAT,NISSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
                                                                                
         LA    R0,NIXVALS          CLEAR VARIABLES                              
         LHI   R1,NIXVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NNIX10              READ NEXT NEW MEDIA/INTERNET                 
         OC    RQNISCOD,RQNISCOD   RECORD                                       
         JNZ   NOMORE                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTNI                                                         
         JNE   NOMORE                                                           
                                                                                
         USING TLMDD,R4                                                         
NNIX10   L     R4,AIO3                                                          
         MVI   NIXMED,C'I'                                                      
         CLI   TLMDTYPE,INTERNET   PASS BACK MEDIA                              
         JE    *+8                                                              
         MVI   NIXMED,C'N'                                                      
         MVC   NIXCOD,TLMDCODE     AND CODE                                     
         DROP  R4                                                               
                                                                                
         USING TANAD,R4                                                         
         MVI   ELCODE,TANAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NNIXX                                                            
         ZIC   RF,TANALEN                                                       
         SHI   RF,3                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         MVC   NIXNAM(0),TANANAME  PASS BACK NAME                               
         DROP  R4                                                               
                                                                                
NNIXX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,NIXVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT TIMESHEET SEARCH STATUS RECORD                        *         
***********************************************************************         
                                                                                
NXTTMS   J     *+12                                                             
         DC    C'*NXTTMS*'                                                      
         LR    RB,RF                                                            
         USING NXTTMS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         MVI   TMSSTAT,TMSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,TMVALREQ         VALIDATE REQUEST                             
         JNE   NTMSX                                                            
                                                                                
         MVI   TMSSTAT,TMSSUNS     RESET STATUS TO UNSUCCESSFUL                 
                                                                                
         LA    R3,IOKEY            R3=A(KEY)                                    
                                                                                
         GOTOR (#VALSTF,AVALSTF),DMCB,RQTMSSTF                                  
         JNE   NOMORE                                                           
                                                                                
         USING TLTMD,R3                                                         
         XC    TLTMKEY,TLTMKEY     BUILD TIMESHEET KEY                          
         MVI   TLTMCD,TLTMCDQ      AND READ FOR IT                              
         CLI   RQTMSWEB,C'Y'                                                    
         JNE   *+8                                                              
         MVI   TLTMSTA,TLTMSWEB                                                 
         MVC   TLTMCOM,RQTMSCOM                                                 
         GOTOR (#CVTINV,ACVTINV),DMCB,RQTMSINV,TLTMINV                          
         XC    TLTMINV,=6X'FF'                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         MVI   LKEYCOMP,TLTMSSN-TLTMD-1                                         
                                                                                
         BRAS  RE,FLTTIM           IF TIMESHEET MATCHES ALL FILTERS             
         JNE   NTMSX               RETURN SUCCESSFUL STATUS                     
         MVI   TMSSTAT,TMSSSUC                                                  
         DROP  R3                                                               
                                                                                
NTMSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,TMSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
TMVALREQ NTR1                                                                   
         OC    RQTMSSTF,RQTMSSTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
         OC    RQTMSCOM,RQTMSCOM   ASSERT THAT INTERNAL COMMERCIAL              
         JZ    NO                  NUMBER IS PROVIDED                           
         OC    RQTMSINV,RQTMSINV   ASSERT THAT INVOICE NUMBER                   
         JNZ   YES                 IS PROVIDED                                  
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT TIMESHEET TOTALS DETAILS RECORDS                      *         
***********************************************************************         
                                                                                
NXTTMT   J     *+12                                                             
         DC    C'*NXTTMT*'                                                      
         LR    RB,RF                                                            
         USING NXTTMT,RB                                                        
                                                                                
         CLI   TMSSTAT,TMSSSUC     EXIT IF INITIAL SEARCH WAS                   
         JH    NOMORE              NOT SUCCESSFUL                               
                                                                                
         LA    R0,TMTVALS          CLEAR VARIABLES                              
         LHI   R1,TMTVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R3,IOKEY            R3=A(IOKEY)                                  
                                                                                
         CLI   LP_RMODE,LP_RFRST   IF THIS IS NOT THE FIRST TIME IN             
         JE    NTMT10              READ NEXT TIMESHEET RECORD                   
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         BRAS  RE,FLTTIM                                                        
         JNE   NOMORE                                                           
                                                                                
         USING TLTMD,R4                                                         
NTMT10   L     R4,AIO3                                                          
         MVI   TMTWCR,C'N'                                                      
         TM    TLTMSTA,TLTMSWEB                                                 
         JZ    *+8                                                              
         MVI   TMTWCR,C'Y'                                                      
         GOTO1 VHEXOUT,DMCB,TLTMCOM,TMTCOM,L'TLTMCOM,0                          
         GOTOR (#CVTINV,ACVTINV),DMCB,TLTMINV,TMTINV                            
         GOTO1 VHEXOUT,DMCB,TLTMSORT+4,TMTSEQ,2,0                               
         XC    ACURTATM,ACURTATM                                                
         DROP  R4                                                               
                                                                                
NTMTX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,TMTVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT TIMESHEET DAY DETAILS RECORDS                         *         
***********************************************************************         
                                                                                
NXTTMD   J     *+12                                                             
         DC    C'*NXTTMD*'                                                      
         LR    RB,RF                                                            
         USING NXTTMD,RB                                                        
                                                                                
         LA    R0,TMDVALS          CLEAR VARIABLES                              
         LHI   R1,TMDVALL                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING TATMD,R2                                                         
         L     R4,ACURTATM                                                      
         MVI   ELCODE,TATMELQ                                                   
         OC    ACURTATM,ACURTATM                                                
         JNZ   NTMD10                                                           
         L     R4,AIO3                                                          
         BRAS  RE,GETEL            R2=A(FIRST/NEXT TIMESHEET ELEMENT)           
         J     *+8                                                              
NTMD10   BRAS  RE,NEXTEL                                                        
         JNE   NOMORE                                                           
         LR    R2,R4                                                            
         ST    R2,ACURTATM                                                      
                                                                                
         USING TATTD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TATTELQ                                                   
         BRAS  RE,GETEL            R4=A(TIMESHEET TOTAL ELEMENT)                
         J     *+8                                                              
NTMD20   BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         CLC   TATTDATE,TATMDATE                                                
         JNE   NTMD20                                                           
                                                                                
         GOTO1 VDATCON,DMCB,(1,TATMDATE),(8,TMDDAT)                             
         GOTO1 VDATCON,DMCB,(1,TATMPWDT),(8,TMDPDD)                             
                                                                                
         GOTOR OUTTMTIM,DMCB,TATMWTST,TMDWST                                    
         GOTOR OUTTMTIM,DMCB,TATMM1ST,TMDM1S                                    
         GOTOR OUTTMTIM,DMCB,TATMM2ST,TMDM2S                                    
         GOTOR OUTTMTIM,DMCB,TATMM3ST,TMDM3S                                    
         GOTOR OUTTMTIM,DMCB,TATMPDST,TMDPDS                                    
         GOTOR OUTTMTIM,DMCB,TATMTTDP,TMDTDP                                    
         GOTOR OUTTMTIM,DMCB,TATMTFDP,TMDTID                                    
         GOTOR OUTTMTIM,DMCB,TATMTIDP,TMDTID                                    
         GOTOR OUTTMTIM,DMCB,TATMM3ST,TMDM3S                                    
                                                                                
         MVC   TMDOWA,TATMOTHR     PASS BACK OTHER AMOUNT                       
         MVC   TMDNWA,TATMWANE     WARDROBE ALLOTMENTS NON-EVENING              
         MVC   TMDEWA,TATMWAEV     WARDROBE ALLOTMENTS EVENING                  
         MVC   TMDO16,TATM16HR     AND 16HR RULE - # OF HOURS OVER              
                                                                                
         TM    TATTSTAT,TATTSART+TATTSSRT                                       
         JZ    NTMD30              IF ACTRA RATES OR SAG ON 2404A               
         MVC   TMDNP3,TATMNPAC     NUMBER OF ACTRA NGHT PREM HRS AT 25%         
         MVC   TMDNP4,TATMNPSG     NUMBER OF SAG NGHT PREM HRS AT 25%           
         MVC   TMDASH,TATTNPST     ACTRA NP STRAIGHT HRS                        
         MVC   TMDAOH,TATTNPOT     ACTRA NP OVERTIME HRS                        
         MVC   TMDAND,TATTNPDT     ACTRA NP DOUBLETIME HRS                      
         J     NTMD40                                                           
                                                                                
NTMD30   MVC   TMDNP1,TATMNP10     NUMBER OF NIGHT PREMIUM HRS AT 10%           
         MVC   TMDNP2,TATMNP20     NUMBER OF NIGHT PREMIUM HRS AT 20%           
         MVC   TMDESF,TATTXSAT     EXTRAS SATURDAY PAY ON FRIDAYS               
                                                                                
NTMD40   MVI   TMDNPR,C'N'                                                      
         TM    TATMSTAT,TATMSNPY   PASS BACK "NIGHT PREMIUM?" STATUS            
         JZ    *+8                                                              
         MVI   TMDNPR,C'Y'                                                      
                                                                                
         MVI   TMD16H,C'N'                                                      
         TM    TATMSTAT,TATMS16Y   PASS BACK "16 HOUR RULE?" STATUS             
         JZ    *+8                                                              
         MVI   TMD16H,C'Y'                                                      
                                                                                
         MVI   TMDMP1,C'N'                                                      
         TM    TATMSTAT,TATMSMPY   PASS BACK "MEAL PENALTY 1?" STATUS           
         JZ    *+8                                                              
         MVI   TMDMP1,C'Y'                                                      
                                                                                
         MVI   TMDMP2,C'N'                                                      
         TM    TATMSTAT,TATMSM2Y   PASS BACK "MEAL PENALTY 2?" STATUS           
         JZ    *+8                                                              
         MVI   TMDMP2,C'Y'                                                      
                                                                                
         MVI   TMDSMK,C'N'                                                      
         TM    TATMSTA2,TATMS2SP   PASS BACK "MEAL PENALTY 1?" STATUS           
         JZ    *+8                                                              
         MVI   TMDSMK,C'Y'                                                      
                                                                                
         MVI   TMDNDB,C'N'                                                      
         TM    TATMSTA2,TATMS2NM   PASS BACK "NON-DEDUCTIBLE MEAL?"             
         JZ    *+8                 STATUS                                       
         MVI   TMDNDB,C'Y'                                                      
                                                                                
         MVI   TMDREH,C'N'                                                      
         TM    TATMSTA2,TATMS2RD   PASS BACK "REHEARSAL DAY?"                   
         JZ    *+8                 STATUS                                       
         MVI   TMDREH,C'Y'                                                      
                                                                                
         MVI   TMDWCX,C'N'                                                      
         TM    TATMSTA2,TATMS2WC   PASS BACK "WEATHER CANCELLATION?"            
         JZ    *+8                 STATUS                                       
         MVI   TMDWCX,C'Y'                                                      
                                                                                
         MVI   TMDTDL,C'N'                                                      
         TM    TATMSTA2,TATMS2DL   PASS BACK "DISTANT LOCATION?"                
         JZ    *+8                 STATUS                                       
         MVI   TMDTDL,C'Y'                                                      
                                                                                
         MVI   TMDNCD,C'N'                                                      
         TM    TATMSTA2,TATMS2NC   PASS BACK "NON-CONSECUTIVE WORK              
         JZ    *+8                 DAY?" STATUS                                 
         MVI   TMDNCD,C'Y'                                                      
                                                                                
         MVI   TMDMP3,C'N'                                                      
         TM    TATMSTA3,TATMSM3Y   PASS BACK "MEAL PENALTY 3?" STATUS           
         JZ    *+8                                                              
         MVI   TMDMP3,C'Y'                                                      
                                                                                
         MVI   TMDRPV,C'N'                                                      
         TM    TATMSTA3,TATMSRPY   PASS BACK "REST PERIOD VIOLATION?"           
         JZ    *+8                 STATUS                                       
         MVI   TMDRPV,C'Y'                                                      
         DROP  R2                                                               
                                                                                
         MVC   TMDSPT,TATTSPOT     PASS BACK SPOTS                              
         MVC   TMDDYS,TATTDAYS     DAYS                                         
         MVC   TMDOTH,TATTOVTM     OVERTIME HOURS                               
         MVC   TMDDTH,TATTDBTM     DOUBLETIME HOURS                             
         MVC   TMDTRV,TATTTRVL     TRAVEL TIME                                  
         MVC   TMDPDW,TATTPDWD     PRIOR DAY WARDROBE                           
         MVC   TMDTAG,TATTTAG      TAGS                                         
         MVC   TMDINC,TATTINCL     INCLUDE CODE                                 
         MVC   TMDNSP,TATTNSPH     AMOUNT NOT SUBJECT TO P&H (MEAL PEN)         
         MVC   TMDAPY,TATTPYMT     ADDITION TO PAYMENT AMT. (SMOKE PAY)         
         MVC   TMDADJ,TATTADJ      ADJUSTMENT AMOUNT                            
                                                                                
         MVI   TMDHOL,C'N'                                                      
         TM    TATTSTAT,TATTSTHP   PASS BACK "HOLIDAY PAY?" STATUS              
         JZ    *+8                                                              
         MVI   TMDHOL,C'Y'                                                      
                                                                                
         MVI   TMDSAT,C'N'                                                      
         TM    TATTSTAT,TATTSTST   PASS BACK "SATURDAY PAY?" STATUS             
         JZ    *+8                                                              
         MVI   TMDSAT,C'Y'                                                      
                                                                                
         MVI   TMDSUN,C'N'                                                      
         TM    TATTSTAT,TATTSTSU   PASS BACK "SUNDAY PAY?" STATUS               
         JZ    *+8                                                              
         MVI   TMDSUN,C'Y'                                                      
                                                                                
         MVI   TMDACT,C'N'                                                      
         TM    TATTSTAT,TATTSART   PASS BACK "ACTRA RATES?" STATUS              
         JZ    *+8                                                              
         MVI   TMDACT,C'Y'                                                      
                                                                                
         MVI   TMDSAG,C'N'                                                      
         TM    TATTSTAT,TATTSSRT   PASS BACK "SAG RATES?" STATUS                
         JZ    *+8                                                              
         MVI   TMDSAG,C'Y'                                                      
                                                                                
         MVI   TMDW12,C'N'                                                      
         TM    TATTSTAT,TATTSWCH   PASS BACK "WEATHER CANCELLATION              
         JZ    *+8                 (1/2 PAYCHECK)?" STATUS                      
         MVI   TMDW12,C'Y'                                                      
                                                                                
         MVI   TMDW34,C'N'                                                      
         TM    TATTSTAT,TATTSW3Q   PASS BACK "WEATHER CANCELLATION              
         JZ    *+8                 (3/4 PAYCHECK)?" STATUS                      
         MVI   TMDW34,C'Y'                                                      
         DROP  R4                                                               
                                                                                
NTMDX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,TMDVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT TIMESHEET KEY/RECORD MATCHES SEARCH      *         
*        FILTERS AND RETURNS THE RECORD                               *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
         USING TLTMD,R3                                                         
FLTTIM   NTR1  BASE=*,LABEL=*                                                   
FTIM10   ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   READ TIMESHEET RECORD INTO AIO3              
         JNE   NO                                                               
                                                                                
         OC    RQTMSCSQ,RQTMSCSQ   IF FILTERING ON CAST SEQUENCE                
         JZ    FTIM20              NUMBER                                       
         CLC   RQTMSCSQ,TLTMSORT+4 DO SO NOW                                    
         JNE   FTIM30                                                           
         DROP  R3                                                               
                                                                                
FTIM20   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         J     YES                                                              
                                                                                
FTIM30   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FTIM10                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS TIME IN TIMESHEET FORMAT                     *         
*        ON ENTRY ... P1=A(TIME IN INTERNAL FORMAT)                   *         
*                     P2=A(OUTPUT FIELD)                              *         
***********************************************************************         
                                                                                
OUTTMTIM NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
         OC    0(4,R2),0(R2)                                                    
         JZ    XIT                                                              
         XR    R4,R4                                                            
         L     R3,4(R1)                                                         
                                                                                
OTT10    ZICM  RF,0(R2),2          RF=MILITARY TIME                             
         MVI   BYTE1,C'A'                                                       
         CHI   RF,100              AM, IF TIME < 100                            
         BNL   OTT20                                                            
         AHI   RF,1200             ADD 1200 TO THE TIME                         
         B     OTT30                                                            
OTT20    CHI   RF,1200             AM, IF TIME < 1200                           
         BL    OTT30                                                            
         MVI   BYTE1,C'N'          NOON, IF TIME = 1200                         
         BE    OTT30                                                            
                                                                                
         CHI   RF,1300                                                          
         BL    *+8                                                              
         SHI   RF,1200             SUBTRACT 1200 FROM TIME ABOVE 1300           
         MVI   BYTE1,C'P'          PM, IF TIME > 1200                           
         CHI   RF,1200                                                          
         BNE   OTT30                                                            
         MVI   BYTE1,C'M'          MIDNIGHT, IF TIME = 2400-1200=1200           
OTT30    EDIT  (RF),(4,0(R3)),ALIGN=LEFT                                        
         LA    RF,3(R3)            IF TIME IS 3 CHARS, PUT A,P,N,M HERE         
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         LA    RF,1(RF)            IF TIME IS 4 CHARS, PUT A,P,N,M HERE         
         MVC   0(1,RF),BYTE1       AM, PM, NOON, OR MIDNIGHT                    
                                                                                
         LTR   R4,R4                                                            
         JNZ   XIT                                                              
         LA    R2,2(R2)                                                         
         LA    R3,5(R3)                                                         
         AHI   R4,1                                                             
         J     OTT10                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT USE SEARCH STATUS RECORD                              *         
***********************************************************************         
                                                                                
NXTUSS   J     *+12                                                             
         DC    C'*NXTUSS*'                                                      
         LR    RB,RF                                                            
         USING NXTUSS,RB                                                        
                                                                                
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NOMORE                                                           
                                                                                
         MVI   USSSTAT,USSSINV     INITIALIZE STATUS TO INVALID                 
         BAS   RE,USVALREQ         REQUEST                                      
         JNE   NUSSX               IF REQUEST IS VALID                          
         MVI   USSSTAT,USSSUNS     RESET STATUS TO UNSUCCESSFUL                 
                                                                                
         LA    R3,IOKEY            VERIFY STAFF IS ON FILE                      
         GOTOR (#VALSTF,AVALSTF),DMCB,RQUSESTF                                  
         JNE   NUSSX                                                            
                                                                                
         OC    RQUSEPED,RQUSEPED   IF PERIOD END DATE IS NOT PROVIDED           
         JNZ   *+10                                                             
         MVC   RQUSEPED,RQUSEPSD   SET IT AS PERIOD START DATE                  
                                                                                
         CLC   RQUSEUSE,=C'LCB'    IF USE IS LOCAL CABLE                        
         JNE   *+10                                                             
         MVC   RQUSEUSE,=C'CBL'    FIRST SET TO PROCESS CABLE                   
                                                                                
         L     R2,AUDTAB                                                        
         MVI   0(R2),X'FF'         BUILD ASSET-LEVEL USE DETAILS TABLE          
         BAS   RE,BLDAUDET                                                      
                                                                                
         CLI   0(R2),X'FF'         IF ANY ASSET-LEVEL CYCLES MEET THE           
         JE    NUSSX               REQUEST CRITERIA                             
         MVI   USSSTAT,USSSSUC     SET STATUS TO SUCCESSFUL                     
                                                                                
NUSSX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,USSVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        ROUTINE ENSURES THAT REQUEST IS VALID                        *         
***********************************************************************         
                                                                                
USVALREQ NTR1                                                                   
         OC    RQUSESTF,RQUSESTF   ASSERT THAT STAFF ID IS PROVIDED             
         JZ    NO                                                               
                                                                                
         OC    RQUSECOM,RQUSECOM   ASSERT THAT INTERNAL COMMERCIAL              
         JZ    NO                  NUMBER IS PROVIDED                           
                                                                                
         OC    RQUSEUSE,RQUSEUSE   ASSERT THAT USE IS PROVIDED                  
         JNZ   USVR10              AND VALID                                    
         GOTOR (#VALFLD,ACHKFLD),DMCB,('VFUSE',RQUSEUSE)                        
         JNE   NO                                                               
                                                                                
USVR10   OC    RQUSEPSD,RQUSEPSD   ASSERT THAT PERIOD START DATE                
         JZ    NO                  IS PROVIDED                                  
                                                                                
         OC    RQUSEPED,RQUSEPED   IF PERIOD END DATE IS PROVIDED               
         JZ    YES                                                              
         CLC   RQUSEPSD,RQUSEPED   ASSERT THAT IT IS EQUAL TO OR                
         JNH   YES                 LATER THAN PERIOD START DATE                 
         J     NO                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS ASSET-LEVEL USE DETAILS TABLE                 *         
*        ON ENTRY ... R2 = A(USE DETAILS TABLE)                       *         
*                     R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
         USING TLUHD,R3                                                         
BLDAUDET NTR1                                                                   
         XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLUHCD,TLUHCDQ      READ ALL ASSET-LEVEL USAGE HISTORY           
         MVC   TLUHCOM,RQUSECOM    FOR REQUESTED USE                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     LAU20                                                            
LAU10    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
LAU20    CLC   IOKEY(TLUHUSE-TLUHD),IOKEYSAV                                    
         JNE   XIT                                                              
         CLC   TLUHUSE,RQUSEUSE                                                 
         JE    LAU30                                                            
                                                                                
         BRAS  RE,ISCBLUSE         IF REQUESTED USE IS CABLE/SPANISH            
         JNE   LAU10               CABLE                                        
         CLC   TLUHUSE,=C'LCB'     ALSO ACCEPT LOCAL CABLE                      
         JNE   LAU10                                                            
                                                                                
LAU30    GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
***********************************************************************         
                                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   LAU10                                                            
         CLC   RQUSEPSD,TAUHEND    IF USAGE HISTORY CYCLE FITS                  
         JH    LAU40               WITHIN REQUEST PERIOD                        
         CLC   TAUHSTRT,RQUSEPED                                                
         JH    LAU40                                                            
                                                                                
         USING UDTABD,R2                                                        
         LA    R2,UDTENT           INITIALIZE USE DETAILS TABLE                 
         XC    UDTENT,UDTENT       ENTRY ... THEN GO BUILD IT AND               
         BRAS  RE,BLDADDUH         POSSIBLY ADD IT TO USE DETAILS TABLE         
         DROP  R2,R3,R4                                                         
                                                                                
***********************************************************************         
                                                                                
*                                  BUILD USE DETAILS ENTRY FOR EACH             
LAU40    BRAS  RE,BLDADDMT         MARKET/NETWORK/SYSTEM AND POSSIBLY           
         J     LAU10               ADD THEM TO USE DETAILS TABLE                
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT EXISTING ASSET CYCLE RECORD                           *         
***********************************************************************         
                                                                                
NXTUAC   J     *+12                                                             
         DC    C'*NXTUAC*'                                                      
         LR    RB,RF                                                            
         USING NXTUAC,RB                                                        
                                                                                
         USING UDTABD,R2                                                        
         L     R2,SVADDR                                                        
         CLI   LP_RMODE,LP_RFRST   R2=A(NEXT USE DETAILS ENTRY)                 
         JNE   NUAC10                                                           
         MVI   TEMP,X'FF'                                                       
         L     R2,AUDTAB                                                        
                                                                                
NUAC10   CLI   0(R2),X'FF'         IF ANY MORE ASSET-LEVEL USE DETAILS          
         JE    NOMORE              TO OUTPUT ...                                
                                                                                
         LA    RE,TEMP                                                          
NUAC20   CLI   0(RE),X'FF'         ... IF WE'VE ALREADY ENCOUNTERED             
         JE    NUAC40              THIS CYCLE, BUMP TO NEXT ENTRY               
         CLC   UDTUSE(UDTALNQ),0(RE)                                            
         JE    NUAC30                                                           
         LA    RE,UDTALNQ(RE)                                                   
         J     NUAC20                                                           
NUAC30   LA    R2,UDTLNQ(R2)                                                    
         J     NUAC10                                                           
                                                                                
NUAC40   MVC   0(UDTALNQ,RE),UDTUSE                                             
         MVI   UDTALNQ(RE),X'FF'   ... IF NOT, OUTPUT USE DETAILS               
         GOTOR OUTUPL,DMCB,('OUPLCOM',0)                                        
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT ASSET LEVEL USE DETAILS RECORDS                       *         
***********************************************************************         
                                                                                
NXTUAL   J     *+12                                                             
         DC    C'*NXTUAL*'                                                      
         LR    RB,RF                                                            
         USING NXTUAL,RB                                                        
                                                                                
         BRAS  RE,ISBCSTLG         IF REQUESTED USE HAS BROADCAST               
         JNE   NOMORE              LAG CYCLES ...                               
                                                                                
         L     R2,SVADDR                                                        
         CLI   LP_RMODE,LP_RFRST   R2=A(NEXT USE DETAILS ENTRY)                 
         JNE   NUAL10                                                           
         L     R2,AUDTAB                                                        
                                                                                
NUAL10   GOTOR OUTUPL,DMCB,('OUPLMKT',0)  OUTPUT USE DETAILS FOR EACH           
         JNE   NOMORE                     MARKET/NETWORK/SYSTEM                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        OUTPUT PERFORMER LEVEL USE DETAILS RECORDS                   *         
***********************************************************************         
                                                                                
NXTUPL   J     *+12                                                             
         DC    C'*NXTUPL*'                                                      
         LR    RB,RF                                                            
         USING NXTUPL,RB                                                        
                                                                                
         BRAS  RE,ISBCSTLG         IF REQUESTED USE HAS BROADCAST               
         JNE   NOMORE              LAG CYCLES ...                               
                                                                                
         L     R2,SVADDR                                                        
         CLI   LP_RMODE,LP_RFRST                                                
         JNE   NUPL20                                                           
         XC    SVUHCSEQ,SVUHCSEQ                                                
                                                                                
NUPL10   ZIC   RE,SVUHCSEQ,2       SET TO READ USAGE HISTORY FOR 1ST/           
         AHI   RE,1                NEXT PERFORMER                               
         STCM  RE,3,SVUHCSEQ                                                    
                                                                                
         L     R2,AUDTAB           BUILD USE DETAILS TABLE                      
         MVI   0(R2),X'FF'         FOR PERFORMER                                
         ST    R2,SVADDR                                                        
         BAS   RE,LOOKPUSE                                                      
         JNE   NOMORE                                                           
                                                                                
NUPL20   GOTOR OUTUPL,DMCB,('OUPLMKT',0)  OTPUT USE DETAILS FOR EACH            
         JNE   NUPL10                     MARKET/NETWORK/SYSTEM                 
         J     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE LOOKS FOR ANY PERFORMER-LEVEL USE FOR REQUESTED USE  *         
*        ON ENTRY ... R3 = A(IOKEY)                                   *         
***********************************************************************         
                                                                                
LOOKPUSE NTR1                                                                   
         XR    R0,R0               INITIALIZE TO NO USE FOUND                   
                                                                                
         USING TLUHD,R3                                                         
         LA    R3,IOKEY                                                         
LPU10    XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLUHCD,TLUHCDQ      READ ALL PERFORMER-LEVEL USAGE               
         MVC   TLUHCOM,RQUSECOM    HISTORY FOR REQUESTED USE                    
         MVC   TLUHCSEQ,SVUHCSEQ                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         CLC   IOKEY(TLUHCSEQ-TLUHD),IOKEYSAV                                   
         JNE   NO                                                               
                                                                                
         MVC   SVUHCSEQ,TLUHCSEQ   SAVE CAST SEQUENCE NUMBER                    
                                                                                
LPU20    CLC   TLUHUSE,RQUSEUSE                                                 
         JE    LPU30                                                            
         BRAS  RE,ISCBLUSE         IF REQUESTED USE IS CABLE                    
         JNE   LPU40                                                            
         CLC   TLUHUSE,=C'LCB'     ALSO ACCEPT LOCAL CABLE                      
         JNE   LPU40                                                            
                                                                                
LPU30    GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         BRAS  RE,BLDADDMT         BUILD USE DETAILS ENTRY AND POSSIBLY         
         JNE   LPU40               ADD IT TO USE DETAILS TABLE                  
         AHI   R0,1                SET TO USE FOUND                             
                                                                                
LPU40    GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         CLC   IOKEY(TLUHUSE-TLUHD),IOKEYSAV                                    
         JE    LPU20                                                            
         DROP  R3                                                               
                                                                                
LPU50    LTR   R0,R0               IF NO USE WAS FOUND FOR PERFORMER            
         JNZ   YES                                                              
         ZIC   RE,SVUHCSEQ,2       TRY NEXT PERFORMER                           
         AHI   RE,1                                                             
         STCM  RE,3,SVUHCSEQ                                                    
         J     LPU10                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF REQUESTED USE IS CABLE                     *         
***********************************************************************         
                                                                                
ISCBLUSE NTR1  BASE=*,LABEL=*                                                   
         CLC   RQUSEUSE,=C'CBL'        IF CABLE                                 
         JE    YES                                                              
         CLC   RQUSEUSE,=C'SCB'        OR SPANISH CABLE                         
         JE    YES                                                              
         J     NO                      USE IS CABLE USE                         
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF REQUESTED USE HAS BROADCAST LAG CYCLES     *         
***********************************************************************         
                                                                                
ISBCSTLG NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ISCBLUSE             IF CABLE/SPANISH CABLE                   
         JE    YES                                                              
         CLC   RQUSEUSE,=C'WSP'        OR WILDSPOT                              
         JE    YES                                                              
         CLC   RQUSEUSE,=C'SWS'        OR SPANISH WILDSPOT                      
         JE    YES                                                              
         CLC   RQUSEUSE,=C'WSC'        OR CANADIAN WILDSPOT                     
         JE    YES                                                              
         CLC   RQUSEUSE,=C'ADW'        OR ADDENDUM WILDSPOT                     
         JE    YES                                                              
         CLC   RQUSEUSE,=C'LCB'        OR LOCAL CABLE                           
         JE    YES                     USE HAS BROADCAST LAG CYCLES             
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE GETS USAGE HISTORY RECORD AND ADDS ALL CNET/CSYS     *         
*        MARKET CYCLE TO USE DETAILS TABLE                            *         
***********************************************************************         
                                                                                
BLDADDMT NTR1  BASE=*,LABEL=*                                                   
         XR    R0,R0               INITIALIZE TO USE NOT ADDED                  
                                                                                
         USING TAMTD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAMTELQ      READ ALL MARKETS/NETWORKS/SYSTEMS            
         BRAS  RE,GETEL            PAID                                         
         J     *+8                                                              
BAMT10   BRAS  RE,NEXTEL                                                        
         JNE   BAMTX                                                            
                                                                                
         CLC   RQUSEPSD,TAMTCYCE   IF USAGE HISTORY CYCLE FOR                   
         JH    BAMT10              MARKET/NETWORK/SYSTEM FITS                   
         CLC   TAMTCYCS,RQUSEPED   WITHIN REQUEST PERIOD                        
         JH    BAMT10                                                           
                                                                                
         USING UDTABD,R2                                                        
         LA    R2,UDTENT           INITIALIZE USE DETAILS TABLE                 
         XC    UDTENT,UDTENT       ENTRY ...                                    
         MVC   UDTMNS,TAMTINUM                                                  
         MVC   UDTALP,TAMTCODE                                                  
         MVC   UDTMCS,TAMTCYCS                                                  
         MVC   UDTMCE,TAMTCYCE                                                  
                                                                                
         LR    R5,R4                                                            
         DROP  R4                                                               
                                                                                
         USING TAUHD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TAUHELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BAMT40                                                           
                                                                                
         USING TLUHD,R1                                                         
         L     R1,AIO3                                                          
         MVC   BYTE1,TAUHLCST                                                   
         CLC   TLUHUSE,=C'LCB'                                                  
         JE    BAMT20                                                           
         MVC   BYTE1,TAUHCSTA                                                   
         CLC   TLUHUSE,=C'CBL'                                                  
         JE    BAMT20                                                           
         CLC   TLUHUSE,=C'SCB'                                                  
         JE    BAMT20                                                           
         MVC   BYTE1,TAUHSTAT                                                   
BAMT20   TM    BYTE1,TAUHWCRD                                                   
         JZ    BAMT30                                                           
         OI    UDTSTAT,UDTSCRED                                                 
         DROP  R1                                                               
                                                                                
BAMT30   BRAS  RE,BLDADDUH         ... AND POSSIBLY ADD IT TO USE               
         JNE   BAMT40              DETAILS TABLE                                
         LHI   R0,1                SET TO USE ADDED                             
         DROP  R2,R4                                                            
                                                                                
BAMT40   MVI   ELCODE,TAMTELQ      GO READ NEXT MARKET/NETWORK/                 
         LR    R4,R5               SYSTEM                                       
         J     BAMT10                                                           
                                                                                
BAMTX    LTR   R0,R0               IF USE WAS ADDED RETURN POSITIVE             
         JNZ   YES                 CONDITION CODE                               
         J     NO                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE PUTS USE DETAILS INTO OUTPUT MAP                     *         
*        ON ENTRY ... R2 = A(USE DETAILS ENTRY TO OUTPUT)             *         
*                     P1 BYTE 0 = OUPLCOM, COMMERCIAL CYCLE LEVEL     *         
*                                 OUPLMKT, MKT/CNET/CSYS LEVEL        *         
***********************************************************************         
                                                                                
         USING UDTABD,R2                                                        
OUTUPL   NTR1  BASE=*,LABEL=*                                                   
         ZIC   R0,0(R1)                                                         
                                                                                
OUPL10   CLI   0(R2),X'FF'         R2=A(CURRENT USE DETAILS ENTRY)              
         JE    NO                                                               
                                                                                
         CHI   R0,OUPLMKT          IF OUTPUTTING MKT/CNET/CSYS LEVEL            
         JNE   OUPL20              HISTORY, SKIP COMMERCIAL LEVEL               
         OC    UDTMNS(UDTMLNQ),UDTMNS                                           
         JZ    OUPL40                                                           
         J     OUPL30                                                           
                                                                                
OUPL20   OC    UDTMNS(UDTMLNQ),UDTMNS IF OUTPUTTING COMMERCIAL LEVEL            
         JNZ   OUPL40                 HISTORY, SKIP MKT/CNET/CSYS LEVEL         
                                                                                
OUPL30   TM    UDTSTAT,UDTSCRED    SKIP CREDIT INVOICE ENTRIES                  
         JZ    OUPL50                                                           
                                                                                
OUPL40   LA    R2,UDTLNQ(R2)                                                    
         J     OUPL10                                                           
                                                                                
OUPL50   GOTO1 VHEXOUT,DMCB,SVUHCSEQ,UPLCSQ,L'SVUHCSEQ,0                        
         GOTO1 VHEXOUT,DMCB,UDTMNS,UPLMNS,L'UDTMNS,0                            
         MVC   UPLALP,UDTALP                                                    
         GOTO1 VDATCON,DMCB,(1,UDTMCS),(8,UPLMCS)                               
         GOTO1 (RF),(R1),(1,UDTMCE),(8,UPLMCE)                                  
         GOTOR (#CVTINV,ACVTINV),DMCB,UDTINV,UPLINV                             
         GOTO1 VDATCON,DMCB,(1,UDTICS),(8,UPLICS)                               
         GOTO1 (RF),(R1),(1,UDTICE),(8,UPLICE)                                  
         MVC   UPLUSE,UDTUSE                                                    
         LA    R2,UDTLNQ(R2)                                                    
         ST    R2,SVADDR                                                        
         DROP  R2                                                               
                                                                                
OUPLX    MVI   LP_RMODE,LP_RNEXT                                                
         LA    R0,UPLVALS                                                       
         STCM  R0,15,LP_ADATA                                                   
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
OUPLCOM  EQU   1                                                                
OUPLMKT  EQU   2                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS COMPLETE USE DETAIL ENTRY (ALREADY            *         
*        INITIALIZED IN UDTENT), LOOKS TO SEE IF ITS ALREADY IN USE   *         
*        DETAILS TABLE AND, IF NOT, ADDS IT TO TABLE                  *         
*        ON ENTRY ... R2 = A(UDTENT, IN PROGRESS USE DETAILS ENTRY)   *         
*                     R3 = A(USAGE HISTORY KEY)                       *         
*                     R4 = A(USAGE HISTORY ELEMENT)                   *         
***********************************************************************         
                                                                                
         USING UDTABD,R2                                                        
         USING TLUHD,R3                                                         
         USING TAUHD,R4                                                         
BLDADDUH NTR1  BASE=*,LABEL=*                                                   
         MVC   UDTICS,TAUHSTRT                                                  
         MVC   UDTICE,TAUHEND                                                   
         DROP  R4                                                               
                                                                                
         MVC   UDTINV,TLUHINV                                                   
         MVC   UDTUSE,TLUHUSE                                                   
         DROP  R3                                                               
                                                                                
         L     R2,AUDTAB                                                        
BAUH10   CLI   0(R2),X'FF'         ... AND CYCLE IS NOT ALREADY IN USE          
         JE    BAUH20              DETAILS TABLE, FIND 1ST EMPTY SLOT           
         CLC   UDTUSE(UDTSLNQ),UDTENT+UDTUSE-UDTABD                             
         JE    BAUH30                                                           
         LA    R2,UDTLNQ(R2)                                                    
         J     BAUH10                                                           
BAUH20   MVC   0(UDTLNQ,R2),UDTENT  ... AND ADD IT                              
         MVI   UDTLNQ(R2),X'FF'                                                 
         J     YES                                                              
                                                                                
BAUH30   CLC   UDTINV,UDTENT+UDTINV-UDTABD                                      
         JNH   BAUH40                                                           
         MVC   0(UDTLNQ,R2),UDTENT                                              
         J     YES                                                              
                                                                                
BAUH40   TM    UDTSTAT,UDTSCRED                                                 
         JO    NO                                                               
         MVC   0(UDTLNQ,R2),UDTENT                                              
         J     YES                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT GUARANTEE KEY/RECORD MATCHES SEARCH      *         
*        FILTERS AND RETURNS THE RECORD                               *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
FLTGRT   NTR1  BASE=*,LABEL=*                                                   
FGRT10   ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JNE   NO                                                               
         MVC   SVIOKEY,IOKEY                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLGUD,R4                                                         
         L     R4,IOADDR                                                        
         OC    RQGUSGUA,RQGUSGUA   IF GUARANTEE CODE IS PROVIDED                
         JZ    FGRT20                                                           
         MVC   FULL1,TLGUGUA                                                    
         XC    FULL1,=4X'FF'       COMPLEMENT GUARANTEE CODE                    
         CLC   RQGUSGUA,FULL1      ENSURE IT MATCHES GUAR                       
         JNE   FGRT90                                                           
         DROP  R4                                                               
                                                                                
         USING TAGUD,R4                                                         
FGRT20   L     R4,IOADDR           GET GUARANTEE DETAILS ELEMENT                
         MVI   ELCODE,TAGUELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLC   RQGUSCPN,TAGUCRP    ENSURE CORPORATION FILTER MATCHES            
         JNE   FGRT90              GUARANTEE                                    
                                                                                
         OC    RQGUSAGY,RQGUSAGY   IF AGENCY FILTER PROVIDED                    
         JZ    FGRT40                                                           
         OC    TAGUAGY,TAGUAGY                                                  
         JZ    FGRT40                                                           
         CLC   TAGUAGY,RQGUSAGY    ENSURE IT MATCHES GUARANTEE                  
         JNE   FGRT30                                                           
                                                                                
         OC    RQGUSCLI,RQGUSCLI   IF CLIENT FILTER PROVIDED                    
         JZ    FGRT40                                                           
         OC    TAGUCLI,TAGUCLI                                                  
         JZ    FGRT40                                                           
         CLC   TAGUCLI,RQGUSCLI    ENSURE IT MATCHES GUARANTEE                  
         JE    FGRT40                                                           
                                                                                
FGRT30   BRAS  RE,CKSUBS           CHECK FOR SUBSIDIARY AGY/CLI                 
         JNE   FGRT90                                                           
                                                                                
FGRT40   BRAS  RE,CKGUACC          ENSURE STAFF HAS ACCESS TO                   
         JNE   FGRT90              GUARANTEE                                    
                                                                                
         CLI   RQGUSTYP,C'P'       IF FILTERING ON PER CYCLE GUARANTEES         
         JNE   *+14                                                             
         OC    TAGUCOM,TAGUCOM     EXCLUDE IF INTERNAL COM IS NOT               
         JZ    FGRT90              PRESENT                                      
                                                                                
         CLI   RQGUSTYP,C'L'       IF FILTERING ON LARGE OVERSCALE GRT          
         JNE   *+14                                                             
         OC    TAGUCOM,TAGUCOM     EXCLUDE IF INTERNAL COM IS PRESENT           
         JNZ   FGRT90                                                           
                                                                                
         CLI   RQGUSLCK,C'Y'       IF FILTERING ON LOCKED GUARANTEES            
         JNE   *+12                                                             
         TM    TAGUSTAT,TAGUSLCK   EXCLUDE UNLOCKED GUARANTEES                  
         JZ    FGRT90                                                           
                                                                                
         CLI   RQGUSLCK,C'N'       IF FILTERING ON UNLOCKED GUARANTEES          
         JNE   *+12                                                             
         TM    TAGUSTAT,TAGUSLCK   EXCLUDE LOCKED GUARANTEES                    
         JO    FGRT90                                                           
                                                                                
         XC    ATAGCEL,ATAGCEL                                                  
                                                                                
         OC    RQGUSPST,RQGUSPST   IF NOT FILTERING ON PERIOD START             
         JNZ   FGRT50              OR PERIOD END DATES                          
         CLC   RQGUSPEN,=3X'FF'    DONE                                         
         JE    FGRT80                                                           
                                                                                
FGRT50   OC    TAGUCOM,TAGUCOM     IF LARGE OVERSCALE GUARANTEE                 
         JNZ   FGRT60                                                           
         CLC   RQGUSPST,TAGUEND    FILTER START DATE MUST BE EARLIER            
         JH    FGRT90              OR EQUAL TO END DATE                         
         CLC   TAGUSTRT,RQGUSPEN   AND START DATE MUST BE EARLIER OR            
         JH    FGRT90              EQUAL TO FILTER END DATE                     
         J     FGRT80                                                           
         DROP  R4                                                               
                                                                                
         USING TAGCD,R4                                                         
FGRT60   L     R4,IOADDR           GET GUARANTEE DETAILS ELEMENT                
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL            GET GUARANTEE CYCLE ELEMENTS                 
         J     *+8                 ELEMENT                                      
FGRT70   BRAS  RE,NEXTEL                                                        
         JNE   FGRT90                                                           
         CLC   RQGUSPST,TAGCEND    FILTER START DATE MUST BE EARLIER            
         JH    FGRT70              OR EQUAL TO END DATE                         
         CLC   TAGCSTRT,RQGUSPEN   AND START DATE MUST BE EARLIER OR            
         JH    FGRT70              EQUAL TO FILTER END DATE                     
         ST    R4,ATAGCEL                                                       
         DROP  R4                                                               
                                                                                
FGRT80   MVC   IOKEY,SVIOKEY       RESTORE GUAR READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         J     YES                 AND RETURN POSITIVE CONDITION CODE           
                                                                                
FGRT90   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FGRT10                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT COMMERCIAL KEY/RECORD MATCHES SEARCH     *         
*        FILTERS AND RETURNS THE PRIMARY COMMERCIAL RECORD IN AIO3    *         
*        AND VERSION RECORD IN AIO4                                   *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
FLTCOM   NTR1  BASE=*,LABEL=*                                                   
FCOM10   ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JE    FCOM20                                                           
         BAS   RE,ADJCOKEY                                                      
         JE    FCOM10                                                           
         J     NO                                                               
                                                                                
FCOM20   MVC   SVIOKEY,IOKEY                                                    
         MVI   AFMTAB,X'FF'                                                     
         MVI   EBD,C'N'                                                         
                                                                                
         USING TLCOPD,R3                                                        
         CLI   IOKEY,TLCOBCDQ      IF READING FOR NAME BREAKDOWN                
         JNE   FCOM30                                                           
         OC    RQCOSCID,RQCOSCID   AND COMMERCIAL ID WAS NOT PROVIDED           
         JNZ   FCOM30                                                           
         TM    TLCOBSTA,TLCOBSID   SKIP COMMERCIAL ID POINTERS                  
         JO    FCOMNXT                                                          
                                                                                
FCOM30   MVC   SVVER,RQCOSVER      INITIALIZE VERSION                           
                                                                                
         CLI   IOKEY,TLCOICDQ      IF READING FOR COMMERCIAL ID                 
         JNE   FCOM40                                                           
         CLI   TLCOIVER,1          SKIP VERSION 1                               
         JE    FCOMNXT             (ALREADY HANDLED VIA VERSION 0)              
                                                                                
         CLI   TLCOIVER,0          IF COMML ID BELONGS TO A VERSION             
         JE    FCOM60                                                           
         MVC   SVVER,TLCOIVER      SAVE VERSION NUMBER                          
                                                                                
         CLI   TLCOIVER,26         AND IF IT IS HIGHER THAN 26                  
         JNH   FCOM60              GO READ PRIMARY COMMERCIAL KEY               
         MVC   SVCOM,TLCOICOM                                                   
         J     FCOM50                                                           
                                                                                
FCOM40   CLI   IOKEY,TLCOVRDQ      IF READING VIA VERSION NUMBER KEY            
         JNE   FCOM60                                                           
         CLI   TLCOVVER,1          SKIP VERSION 1                               
         JE    FCOMNXT             (ALREADY HANDLED VIA VERSION 0)              
                                                                                
         CLI   TLCOVVER,0          IF COMML ID BELONGS TO A VERSION             
         JE    FCOM60                                                           
         MVC   SVVER,TLCOVVER      SAVE VERSION NUMBER                          
                                                                                
         CLI   TLCOVVER,26         AND IF IT IS HIGHER THAN 26                  
         JNH   FCOM60              GO READ PRIMARY COMMERCIAL KEY               
         MVC   SVCOM,TLCOVCOM                                                   
                                                                                
FCOM50   XC    0(L'TLDRKEY,R3),0(R3)                                            
         MVI   TLCOPCD,TLCOCCDQ    READ PRIMARY COMMERCIAL KEY                  
         MVC   TLCOCCOM,SVCOM                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
FCOM60   CLI   RQCOSIVR,C'Y'       IF NOT INCLUDING VERSIONS IN                 
         JE    *+12                SEARCH RESULTS                               
         CLI   SVVER,1             ENSURE THIS IS NOT A VERSION                 
         JH    FCOMNXT             HIGHER THAN 1                                
                                                                                
         CLI   IOKEY,TLCOBCDQ      IF USING SUPER SEARCH OPTION                 
         JNE   FCOM90                                                           
         L     RE,ACOMTAB          ENSURE THAT EACH COMMERCIAL IS               
FCOM70   CLI   0(RE),X'FF'         ONLY RETURNED ONCE                           
         JE    FCOM80                                                           
         CLC   TLCOBCOM,0(RE)                                                   
         JE    FCOMNXT                                                          
         LA    RE,L'TLCOBCOM(RE)                                                
         J     FCOM70                                                           
FCOM80   MVC   0(L'TLCOBCOM,RE),TLCOBCOM                                        
         MVI   L'TLCOBCOM(RE),X'FF'                                             
         DROP  R3                                                               
                                                                                
FCOM90   GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO3             R4=A(COMMERCIAL RECORD)                      
         ST    R4,SVADDR                                                        
                                                                                
         OC    RQCOSAGY,RQCOSAGY   IF AGENCY CODE IS PROVIDED                   
         JZ    *+14                                                             
         CLC   TLCOAGY,RQCOSAGY    ENSURE IT MATCHES COMMERCIAL                 
         JNE   FCOMNXT                                                          
                                                                                
         OC    RQCOSCLI,RQCOSCLI   IF CLIENT CODE IS PROVIDED                   
         JZ    *+14                                                             
         CLC   TLCOCLI,RQCOSCLI    ENSURE IT MATCHES COMMERCIAL                 
         JNE   FCOMNXT                                                          
                                                                                
         OC    RQCOSPRD,RQCOSPRD   IF PRODUCT CODE IS PROVIDED                  
         JZ    *+14                                                             
         CLC   TLCOPRD,RQCOSPRD    ENSURE IT MATCHES COMMERCIAL                 
         JNE   FCOMNXT                                                          
                                                                                
         CLI   RQCOSSKA,C'Y'       UNLESS SKIPPING AGENCY/CLIENT                
         JE    FCOM100             ACCESS CHECK                                 
         BRAS  RE,CKCOACC          ENSURE STAFF HAS ACCESS TO                   
         JNE   FCOMNXT             COMMERCIAL                                   
                                                                                
FCOM100  MVC   SVCOKEY,TLCOKEY     SAVE PRIMARY COMMERCIAL KEY                  
         DROP  R4                                                               
                                                                                
         CLI   SVVER,2             IF COMMERCIAL HAS VERSIONS ...               
         JNL   FCOM110                                                          
         L     R4,AIO3                                                          
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FCOM120                                                          
         MVI   SVVER,1                                                          
         J     FCOM120                                                          
                                                                                
         USING TLVRD,R3                                                         
FCOM110  XC    TLVRKEY,TLVRKEY     ... READ VERSION RECORD INTO AIO4            
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,SVCOKEY+TLCOCOM-TLCOD                                    
         MVC   TLVRVER,SVVER                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO4'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO4'                              
         DROP  R3                                                               
                                                                                
         MVC   SVADDR,AIO4         SAVE A(VERSION RECORD)                       
                                                                                
FCOM120  BAS   RE,FLTCOTIT         FILTER BASED ON TITLE                        
         JNE   FCOMNXT                                                          
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO3                                                          
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   RQCOSCIN,0          IF INTERNAL COMMERCIAL NUMBER                
         JE    FCOM150             FILTER IS DEFINED                            
         OC    RQCOSCID,RQCOSCID   AND COMMERCIAL ID IS PROVIDED                
         JZ    FCOM150                                                          
         LHI   RE,L'RQCOSCID-1     ENSURE IT MATCHES COMMERCIAL                 
         CLI   RQCOSEXT,C'Y'                                                    
         JE    FCOM140                                                          
         LA    RF,RQCOSCID+L'RQCOSCID-1                                         
FCOM130  CLI   0(RF),C' '                                                       
         JH    FCOM140                                                          
         BCTR  RF,0                                                             
         BCT   RE,FCOM130                                                       
FCOM140  EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   RQCOSCID(0),TACOCID                                              
         JNE   FCOMNXT                                                          
                                                                                
FCOM150  MVI   COSSPSST,COSSPSUC   SET PRIMARY SEARCH SUCCESSFUL                
                                                                                
         CLI   RQCOSTIN,0          IF COMMERCIAL TYPE FILTER IS                 
         JE    FCOM170             DEFINED                                      
         ZIC   RE,RQCOSTIN         RE=TYPE FILTER COUNTER                       
         ZICM  RF,ARQCOTYP,3       RF=A(TYPE FILTER LIST)                       
FCOM160  CLC   TACOTYPE,0(RF)      ENSURE COMMERCIAL TYPES MATCHES              
         JE    FCOM170             ONE OF THE FILTERS                           
         LA    RF,1(RF)                                                         
         BCT   RE,FCOM160                                                       
         J     FCOMNXT                                                          
                                                                                
FCOM170  CLI   RQCOSAIN,0          IF ACTRA TYPE FILTER IS DEFINED              
         JE    FCOM190                                                          
         ZIC   RE,RQCOSAIN         RE=ACTRA TYPE FILTER COUNTER                 
         ZICM  RF,ARQCOATY,3       RF=A(ACTRA TYPE FILTER LIST)                 
FCOM180  CLC   TACOCTYP,0(RF)      ENSURE ACTRA TYPE MATCHES                    
         JE    FCOM190             ONE OF THE FILTERS                           
         LA    RF,1(RF)                                                         
         BCT   RE,FCOM180                                                       
         J     FCOMNXT                                                          
                                                                                
FCOM190  CLI   RQCOSMIN,0          IF MEDIA FILTER IS DEFINED                   
         JE    FCOM210                                                          
         ZIC   RE,RQCOSMIN         RE=MEDIA FILTER COUNTER                      
         ZICM  RF,ARQCOMED,3       RF=A(MEDIA FILTER LIST)                      
FCOM200  CLC   TACOMED,0(RF)       ENSURE MEDIA MATCHES ONES OF                 
         JE    FCOM210             THE FILTERS                                  
         LA    RF,1(RF)                                                         
         BCT   RE,FCOM200                                                       
         J     FCOMNXT                                                          
                                                                                
FCOM210  OC    RQCOSPOL,RQCOSPOL   IF COMM POOL FILTER PROVIDED                 
         JZ    *+14                                                             
         CLC   TACOCGRP,RQCOSPOL   ENSURE IT MATCHES COMMERCIAL                 
         JNE   FCOMNXT                                                          
                                                                                
         CLI   RQCOSLCK,C'O'       IF ONLY RETURNING LOCKED                     
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSTLO   REJECT IF COMMERCIAL IS NOT                  
         JZ    FCOMNXT             LOCKED                                       
                                                                                
         CLI   RQCOSLCK,C'N'       IF ONLY RETURNING UNLOCKED                   
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSTLO   REJECT IF COMMERCIAL IS LOCKED               
         JO    FCOMNXT                                                          
                                                                                
         CLI   RQCOSREL,C'O'       IF ONLY RETURNING RELEASED                   
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSTRL   REJECT IF COMMERCIAL IS NOT                  
         JZ    FCOMNXT             RELEASED                                     
                                                                                
         CLI   RQCOSREL,C'N'       IF ONLY RETURNING UNRELEASED                 
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSTRL   REJECT IF COMMERCIAL IS RELEASED             
         JO    FCOMNXT                                                          
                                                                                
         CLI   RQCOSCDO,C'O'       IF ONLY RETURNING CANADIAN DOLLAR            
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSCAN   REJECT IF COMMERCIAL IS NOT                  
         JZ    FCOMNXT             CANADIAN DOLLARS                             
                                                                                
         CLI   RQCOSCDO,C'N'       IF ONLY RETURNING NON-CANADIAN               
         JNE   *+12                DOLLAR COMMERCIALS                           
         TM    TACOSTAT,TACOSCAN   REJECT IF CANADIAN DOLLARS                   
         JO    FCOMNXT                                                          
                                                                                
         CLI   RQCOSCRT,C'O'       IF ONLY RETURNING CANADIAN RATE              
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSCRT   REJECT IF COMMERCIAL IS NOT                  
         JZ    FCOMNXT             CANADIAN RATE                                
                                                                                
         CLI   RQCOSCRT,C'N'       IF ONLY RETURNING NON-CANADIAN               
         JNE   *+12                RATE COMMERCIALS                             
         TM    TACOSTAT,TACOSCRT   REJECT IF CANADIAN RATE                      
         JO    FCOMNXT                                                          
                                                                                
         CLI   RQCOSSOP,C'O'       IF ONLY RETURNING SOAP RESIDUAL              
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSRES   REJECT IF COMMERCIAL IS NOT                  
         JZ    FCOMNXT             SOAP RESIDUAL                                
                                                                                
         CLI   RQCOSSOP,C'N'       IF ONLY RETURNING NON-SOAP RESIDUAL          
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTAT,TACOSRES   REJECT IF SOAP RESIDUAL                      
         JO    FCOMNXT                                                          
                                                                                
         CLI   RQCOSPCY,C'O'       IF ONLY RETURNING PER CYCLE                  
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTA2,TACOPCYC   REJECT IF COMMERCIAL IS NOT                  
         JZ    FCOMNXT             PER CYCLE                                    
                                                                                
         CLI   RQCOSPCY,C'N'       IF ONLY RETURNING NON-PER CYCLE              
         JNE   *+12                COMMERCIALS                                  
         TM    TACOSTA2,TACOPCYC   REJECT IF PER CYCLE                          
         JO    FCOMNXT                                                          
                                                                                
         CLI   RQCOSEOM,C'Y'       IF EXCLUDING COMMERCIALS WITH                
         JNE   FCOM220             OLD-STYLE MUSIC CONTRACTS                    
         CLI   TACOTYPE,CTYMUS     AND COMMERCIAL IS NOT MUSIC                  
         JE    FCOM220             CONTRACT ...                                 
         DROP  R4                                                               
                                                                                
         L     R4,AIO3                                                          
         MVI   ELCODE,TAMCELQ      REJECT IF OLD-STYLE MUSIC CONTRACT           
         BRAS  RE,GETEL            ELEMENT ON COMMERCIAL ...                    
         JE    FCOMNXT                                                          
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO3             ... AND BUILD TABLE OF ATTACHED              
         MVI   ELCODE,TATRELQ      AFM CONTRACTS                                
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
FCOM211  BRAS  RE,NEXTEL                                                        
         JNE   FCOM220                                                          
         LA    RE,AFMTAB                                                        
FCOM212  CLC   TATRCOM,0(RE)                                                    
         JE    FCOM211                                                          
         CLI   0(RE),X'FF'                                                      
         JNE   FCOM213                                                          
         MVC   0(L'TATRCOM,RE),TATRCOM                                          
         MVI   L'TATRCOM(RE),X'FF'                                              
         J     FCOM211                                                          
FCOM213  LA    RE,L'TATRCOM(RE)                                                 
         J     FCOM212                                                          
         DROP  R4                                                               
                                                                                
FCOM220  CLI   RQCOSENV,C'Y'       IF ONLY RETURNING COMMERCIALS                
         JNE   FCOM230             WITH VERSIONS                                
         L     R4,AIO3                                                          
         MVI   ELCODE,TAVRELQ      REJECT IS COMMERCIAL DOES NOT                
         BRAS  RE,GETEL            HAVE VERSIONS                                
         JNE   FCOMNXT                                                          
                                                                                
         USING TLAYD,R3                                                         
FCOM230  OC    RQCOSEMP,RQCOSEMP   IF EMPLOYER FILTER PROVIDED                  
         JNZ   FCOM240                                                          
         CLI   RQCOSELA,C'Y'       OR EXCLUDING LOCKED AGENCIES                 
         JZ    FCOM260                                                          
FCOM240  XC    TLAYKEY,TLAYKEY     READ AGENCY RECORD INTO AIO5                 
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,SVCOKEY+TLCOAGY-TLCOD                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TAAYD,R4                                                         
         CLI   RQCOSELA,C'Y'       IF EXCLUDING LOCKED AGENCIES                 
         JNE   FCOM250                                                          
         L     R4,AIO5                                                          
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         TM    TAAYSTA3,TAAYSLCK   ENSURE AGENCY IS NOT LOCKED                  
         JO    FCOMNXT                                                          
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
FCOM250  OC    RQCOSEMP,RQCOSEMP   IF EMPLOYER FILTER PROVIDED                  
         JZ    FCOM260                                                          
         MVC   SVEMP,=CL3'TP '     SAVE DEFAULT EMPLOYER                        
         L     R4,AIO5                                                          
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FCOM260                                                          
         OC    TABROEOR,TABROEOR                                                
         JZ    FCOM260                                                          
         MVC   SVEMP,TABROEOR      OR OVERRIDE EMPLOYER                         
         DROP  R4                                                               
                                                                                
         USING TLCLD,R3                                                         
FCOM260  OC    RQCOSEMP,RQCOSEMP   IF EMPLOYER FILTER PROVIDED                  
         JNZ   FCOM270                                                          
         CLI   RQCOSELC,C'Y'       OR EXCLUDING LOCKED CLIENTS                  
         JNE   FCOM290                                                          
FCOM270  XC    TLCLKEY,TLCLKEY     READ CLIENT RECORD INTO AIO5                 
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,SVCOKEY+TLCOAGY-TLCOD                                    
         MVC   TLCLCLI,SVCOKEY+TLCOCLI-TLCOD                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JNE   FCOM290                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TACID,R4                                                         
         CLI   RQCOSELC,C'Y'       IF EXCLUDING LOCKED CLIENTS                  
         JNE   FCOM280                                                          
         L     R4,AIO5                                                          
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FCOM280                                                          
         TM    TACISTAT,TACISLCK   ENSURE CLIENT IS NOT LOCKED                  
         JO    FCOMNXT                                                          
         DROP  R4                                                               
                                                                                
         USING TABRD,R4                                                         
FCOM280  OC    RQCOSEMP,RQCOSEMP   IF EMPLOYER FILTER PROVIDED                  
         JZ    FCOM290                                                          
         L     R4,AIO5                                                          
         MVI   ELCODE,TABRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FCOM290                                                          
         OC    TABROEOR,TABROEOR                                                
         JZ    FCOM290                                                          
         MVC   SVEMP,TABROEOR      SAVE OVERRIDE EMPLOYER                       
         DROP  R4                                                               
                                                                                
FCOM290  OC    RQCOSEMP,RQCOSEMP   IF EMPLOYER FILTER PROVIDED                  
         JZ    FCOM300                                                          
         CLC   RQCOSEMP,SVEMP      ENSURE IT MATCHES COMMERCIAL                 
         JNE   FCOMNXT                                                          
                                                                                
         USING TLPRD,R3                                                         
FCOM300  CLI   RQCOSELP,C'Y'       IF EXCLUDING LOCKED PRODUCTS                 
         JNE   FCOM310                                                          
         OC    SVCOKEY+TLCOPRD-TLCOD(L'TLCOPRD),SVCOKEY+TLCOPRD-TLCOD           
         JZ    FCOM310                                                          
         XC    TLPRKEY,TLPRKEY     READ PRODUCT RECORD INTO AIO5                
         MVI   TLPRCD,TLPRCDQ                                                   
         MVC   TLPRAGY,SVCOKEY+TLCOAGY-TLCOD                                    
         MVC   TLPRCLI,SVCOKEY+TLCOCLI-TLCOD                                    
         MVC   TLPRPRD,SVCOKEY+TLCOPRD-TLCOD                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JNE   FCOM310                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TAPID,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAPIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FCOM310                                                          
         TM    TAPISTAT,TAPISLCK   ENSURE PRODUCT IS NOT LOCKED                 
         JO    FCOMNXT                                                          
         DROP  R4                                                               
                                                                                
         USING TLCAD,R3                                                         
FCOM310  CLI   RQCOSUIN,0          IF EXCLUDING UNIONS                          
         JNE   FCOM320                                                          
         CLI   RQCOSWIN,0          OR EXCLUDING W4 TYPES FOR                    
         JNE   FCOM320             NON-MUSICIANS                                
         CLI   RQCOSERL,C'Y'       OR EXCLUDING RELEASE LETTERS                 
         JNE   FCOM320                                                          
         CLI   RQCOSEP6,C'Y'       OR EXLUDING PRE-06 CONTRACTS                 
         JE    FCOM320                                                          
         CLI   RQCOSES9,C'Y'       OR EXCLUDING SSN THAT BEGIN                  
         JE    FCOM320             WITH 9                                       
         CLI   RQCOSELS,C'Y'       OR EXLUDING WHEN ALL PERFORMERS              
         JE    FCOM320             ARE LAST SERVICED                            
         OC    RQCOSPED,RQCOSPED   OR CHECKING PERFORMERS EXPIRATION            
         JZ    FCOM470             DATE AGAINST A PROVIDED DATE ...             
                                                                                
FCOM320  L     RE,ASSNTAB          INITIALIZE CAST SOCIAL SECURITY              
         MVI   0(RE),X'FF'         NUMBER TABLE                                 
         MVI   CASTSTAT,0          AND CAST STATUS BYTE                         
                                                                                
         XC    TLCAKEY,TLCAKEY     AND READ ALL CAST KEYS                       
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,SVCOKEY+TLCOCOM-TLCOD                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         J     FCOM340                                                          
FCOM330  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO5'                               
FCOM340  CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   FCOM420                                                          
                                                                                
         OI    CASTSTAT,CSHASCST   SET COMMERCIAL HAS CAST                      
                                                                                
         CLI   RQCOSES9,C'Y'       IF EXCLUDING SSNS THAT BEGIN                 
         JNE   *+12                WITH 9                                       
         CLI   TLCASSN,C'9'        ENSURE THAN SSN DOES NOT BEGIN               
         JE    FCOMNXT             WITH 9                                       
                                                                                
         CLI   RQCOSWIN,0          IF EXCLUDING W4 TYPES FOR                    
         JE    FCOM370             NON-MUSICIANS                                
         TM    TLCASORT,X'80'      AND PERFORMER IS NOT                         
         JO    FCOM370             A MUSICIAN                                   
         L     RE,ASSNTAB          SAVE SS# INTO TABLE                          
FCOM350  CLI   0(RE),X'FF'                                                      
         JE    FCOM360                                                          
         CLC   TLCASSN,0(RE)                                                    
         JE    FCOM370                                                          
         LA    RE,L'TLCASSN(RE)                                                 
         J     FCOM350                                                          
FCOM360  MVC   0(L'TLCASSN,RE),TLCASSN                                          
         MVI   L'TLCASSN(RE),X'FF'                                              
         DROP  R3                                                               
                                                                                
FCOM370  CLI   RQCOSERL,C'Y'       IF EXCLUDING RELEASE LETTERS                 
         JE    FCOM380                                                          
         CLI   RQCOSUIN,0          OR EXCLUDING UNIONS                          
         JNE   FCOM380                                                          
         CLI   RQCOSEP6,C'Y'       OR EXCLUDING PRE-06 CONTRACTS                
         JE    FCOM380                                                          
         CLI   RQCOSELS,C'Y'       OR EXCLUDING WHEN ALL PERFORMERS             
         JE    FCOM380             ARE LAST SERVICED                            
         OC    RQCOSPED,RQCOSPED   OR CHECKING PERFORMERS EXPIRATION            
         JZ    FCOM330             DATE AGAINST A PROVIDED DATE                 
FCOM380  GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5' GET CAST RECORD              
                                                                                
         USING TARLD,R4                                                         
         CLI   RQCOSERL,C'Y'       IF EXCLUDING RELEASE LETTERS                 
         JNE   FCOM390                                                          
         L     R4,AIO5                                                          
         MVI   ELCODE,TARLELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   FCOM390                                                          
         CLI   TARLSTAT,0          ENSURE CAST DOES NOT HAVE                    
         JNE   FCOMNXT             A RELEASE LETTER CODE                        
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
FCOM390  L     R4,AIO5                                                          
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         OC    TACALAST,TACALAST   IF CAST IS NOT LAST SERVICED                 
         JNZ   *+8                 TURN ON "ALL CAST NOT LAST                   
         OI    CASTSTAT,CSNOTLSV   SERVICED" FLAG                               
                                                                                
         CLI   RQCOSUIN,0          IF EXCLUDING UNIONS                          
         JE    FCOM410                                                          
         ZIC   RE,RQCOSUIN         RE=EXCLUDED UNION COUNTER                    
         ZICM  RF,ARQCOEUN,3       RF=A(EXCLUDED UNION LIST)                    
FCOM400  CLC   TACAUN,0(RF)        ENSURE UNION IS NOT IN THE LIST              
         JE    FCOMNXT                                                          
         LA    RF,L'TACAUN(RF)                                                  
         BCT   RE,FCOM400                                                       
                                                                                
FCOM410  CLI   RQCOSEP6,C'Y'       IF EXCLUDING PRE-06 CONTRACT                 
         JNE   FCOM415             YEARS                                        
         CLC   TACAYEAR(2),=C'06'  ENSURE CAST DOES NOT HAVE                    
         JL    FCOMNXT             CONTRACT LATER THAN 2006                     
         CLC   TACAYEAR(2),=C'70'  BUT NOT BETWEEN 1970 AND 1999                
         JH    FCOMNXT                                                          
                                                                                
FCOM415  OC    RQCOSPED,RQCOSPED   IF CHECKING PERFORMERS EXPIRATION            
         JZ    FCOM330             DATE AGAINST A PROVIDED DATE                 
         CLI   EBD,C'Y'            AND WE HAVEN'T ALREADY FOUND AN              
         JE    FCOM330             EXPIRED PERFORMER                            
         OC    TACAEXP,TACAEXP     COMPARE THIS PERFORMER'S EXPIRATION          
         JZ    FCOM330             DATE AGAINST PROVIDED DATE                   
         CLC   TACAEXP,RQCOSPED                                                 
         JNH   FCOM330                                                          
         MVI   EBD,C'Y'                                                         
         J     FCOM330                                                          
         DROP  R4                                                               
                                                                                
FCOM420  CLI   RQCOSELS,C'Y'       IF EXCLUDING WHEN ALL PERFORMERS             
         JNE   FCOM430             ARE LAST SERVICED                            
         TM    CASTSTAT,CSHASCST   AND COMMERCIAL HAS CAST                      
         JZ    FCOM430                                                          
         TM    CASTSTAT,CSNOTLSV   EXCLUDE IF ALL PERFORMERS ARE                
         JZ    FCOMNXT             LAST SERVICED                                
                                                                                
         USING TLW4D,R3                                                         
FCOM430  CLI   RQCOSWIN,0          IF EXCLUDING W4 TYPES FOR                    
         JE    FCOM470             NON-MUSICIANS                                
         L     R2,ASSNTAB                                                       
FCOM440  CLI   0(R2),X'FF'                                                      
         JE    FCOM470                                                          
         XC    TLW4KEY,TLW4KEY     READ ALL W4 RECORDS FOR                      
         MVI   TLW4CD,TLW4CDQ      NON-MUSICIANS                                
         MVC   TLW4SSN,0(R2)                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JNE   FCOM460                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,RQCOSWIN         RE=EXCLUDED W4 TYPE COUNTER                  
         ZICM  RF,ARQCOEWT,3       RF=A(EXCLUDED W4 TYPE LIST)                  
FCOM450  CLC   TAW4TYPE,0(RF)      ENSURE W4 TYPE IS NOT IN THE LIST            
         JE    FCOMNXT                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,FCOM450                                                       
         DROP  R4                                                               
                                                                                
FCOM460  LA    R2,L'TLCASSN(R2)                                                 
         J     FCOM440                                                          
                                                                                
FCOM470  CLI   AFMTAB,X'FF'        IF EXCLUDING COMMERCIALS WITH                
         JE    FCOM540             OLD-STYLE MUSIC CONTRACTS ...                
                                                                                
         LA    R2,AFMTAB                                                        
                                                                                
         USING TLCOPD,R3                                                        
FCOM480  XC    TLCOPKEY,TLCOPKEY   READ ALL ATTACHED AFM CONTRACT               
         MVI   TLCOPCD,TLCOCCDQ    RECORDS ...                                  
         MVC   TLCOCCOM,0(R2)                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO5'                               
         JNE   FCOM530                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         USING TAMCD,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAMCELQ      ... AND ENSURE ALL TRACKS CONFORM            
         BRAS  RE,GETEL            TO NEW MUSIC RULES                           
         JNE   FCOMNXT                                                          
FCOM490  TM    TAMCSTAT,TAMCSNEW                                                
         JZ    FCOMNXT                                                          
         BRAS  RE,NEXTEL                                                        
         JE    FCOM490                                                          
         DROP  R4                                                               
                                                                                
         USING TLCAD,R3                                                         
FCOM500  XC    TLCAKEY,TLCAKEY     READ ALL CAST RECORDS                        
         MVI   TLCACD,TLCACDQ      AND ENSURE THEY ALL HAVE                     
         MVC   TLCACOM,0(R2)       ASSOCIATED TRACKS                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         J     FCOM520                                                          
FCOM510  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO5'                               
FCOM520  CLC   IOKEY(TLCASORT-TLCAD),IOKEYSAV                                   
         JNE   FCOM530                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
         DROP  R3                                                               
                                                                                
         GOTOR (#GETELEM,AGETELEM),DMCB,('TAFNELQ',AIO5),('TAFNTTRK',0)         
         JE    FCOM510                                                          
         J     FCOMNXT                                                          
                                                                                
FCOM530  LA    R2,L'TATRCOM(R2)                                                 
         CLI   0(R2),X'FF'                                                      
         JNE   FCOM480                                                          
                                                                                
FCOM540  BAS   RE,BLDPVTAB         BUILD PAID VERSIONS TABLE                    
                                                                                
         MVC   IOKEY,SVIOKEY       RESTORE COMM READ SEQUENCE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     YES                 AND RETURN POSITIVE CONDITION CODE           
                                                                                
FCOMNXT  CLC   IOKEY(L'TLDRKEY),SVIOKEY                                         
         JE    FCOMNXTA                                                         
         MVC   IOKEY,SVIOKEY       READ NEXT COMMERCIAL KEY                     
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
FCOMNXTA GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         J     FCOM10                                                           
                                                                                
***********************************************************************         
*        ROUTINE RESETS IOKEY TO READ FOR ADDITONAL CRITERIA          *         
*        ON ENTRY ... R3=A(IOKEY)                                     *         
***********************************************************************         
                                                                                
ADJCOKEY NTR1                                                                   
         CLI   RQCOSCIN,2          IF MULTIPLE INTERNAL COMMERCIAL              
         JL    NO                  NUMBER FILTERS ARE PROVIDED                  
         ZIC   RE,RQCOSCIN         RE=INT COM NUMBER FILTER COUNTER             
         ZICM  RF,ARQCOCOM,3       RF=A(INT COM NUM FILTER LIST)                
                                                                                
         CLI   RQCOSIIN,0          IF INTERNAL COMMERCIAL NUMBER                
         JE    ACK20               TABLE WAS BUILT BY COMMERCIAL IDS            
         CLI   RQCOSEXT,C'C'                                                    
         JNE   ACK20                                                            
         ZICM  R1,ARQCOCID,3       BUMP TO FIRST COMMERCIAL ID                  
ACK10    OC    0(L'TLCOICID,R1),0(R1)                                           
         JNZ   ACK20                                                            
         LA    R1,L'TLCOICID(R1)                                                
         J     ACK10                                                            
                                                                                
ACK20    OC    0(L'RQCOSCOM,RF),0(RF)                                           
         JNZ   ACK50                                                            
         LA    RF,L'RQCOSCOM(RF)                                                
         CLI   RQCOSIIN,0                                                       
         JE    ACK40                                                            
         CLI   RQCOSEXT,C'C'                                                    
         JNE   ACK40                                                            
         LA    R1,L'TLCOICID(R1)                                                
ACK30    OC    0(L'TLCOICID,R1),0(R1)                                           
         JNZ   ACK40                                                            
         LA    R1,L'TLCOICID(R1)                                                
         J     ACK30                                                            
ACK40    BCT   RE,ACK20                                                         
         J     NO                                                               
                                                                                
ACK50    MVC   RQCOSCOM,0(RF)                                                   
         XC    0(L'RQCOSCOM,RF),0(RF)                                           
         XC    SRCHCID,SRCHCID                                                  
         CLI   RQCOSIIN,0                                                       
         JE    ACK60                                                            
         CLI   RQCOSEXT,C'C'                                                    
         JNE   ACK60                                                            
         MVC   SRCHCID,0(R1)                                                    
                                                                                
         USING TLCOPD,R3                                                        
ACK60    XC    TLCOPKEY,TLCOPKEY                                                
         MVI   TLCOPCD,TLCOCCDQ    READ FOR INTERNAL COMMERCIAL NUMBER          
         MVC   TLCOCCOM,RQCOSCOM                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         J     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS COMMERCIAL TITLE AGAINST FILTER               *         
*        ON ENTRY ... SVADDR=A(COMMERCIAL/VERSION RECORD)             *         
***********************************************************************         
                                                                                
FLTCOTIT NTR1                                                                   
         OC    RQCOSTIT,RQCOSTIT   IF COMMERCIAL TITLE IS PROVIDED              
         JZ    YES                                                              
                                                                                
         MVC   WORK,SPACES                                                      
                                                                                
         USING TANAD,R4                                                         
         L     R4,SVADDR                                                        
         MVI   ELCODE,TANAELQ      GET NAME ELEMENT                             
         BRAS  RE,GETEL            AND SAVE TITLE INTO WORK                     
         JNE   NO                                                               
         ZIC   RE,TANALEN                                                       
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         J     FCT10                                                            
         MVC   WORK(0),TANANAME                                                 
         DROP  R4                                                               
                                                                                
FCT10    LHI   R2,L'RQCOSTIT-1     R2=L'COMMERCIAL TITLE FILTER - 1             
         LA    R3,RQCOSTIT         R3=A(COMMERCIAL TITLE FILTER)                
         CLI   RQCOSTEX,C'Y'                                                    
         JE    FCT50                                                            
         LA    RF,RQCOSTIT+L'RQCOSTIT-1                                         
FCT20    CLI   0(RF),C' '                                                       
         JH    FCT30                                                            
         BCTR  RF,0                                                             
         BCT   R2,FCT20                                                         
                                                                                
FCT30    CLC   WORK,SPACES         DONE IF ALL WORDS IN NAME HAVE               
         JE    NO                  BEEN CHECKED                                 
                                                                                
FCT40    CLI   WORK,C' '           ELSE FIND NEXT WORD ...                      
         JNE   FCT50                                                            
         MVC   WORK,WORK+1                                                      
         MVI   WORK+L'WORK-1,C' '                                               
         J     FCT40                                                            
                                                                                
FCT50    EX    R2,*+8                                                           
         J     *+10                                                             
         CLC   0(0,R3),WORK        ... AND COMPARE IT TO FILTER                 
         JE    YES                                                              
         CLI   RQCOSTEX,C'Y'                                                    
         JE    NO                                                               
                                                                                
FCT60    MVC   WORK,WORK+1                                                      
         MVI   WORK+L'WORK-1,C' '                                               
         CLI   WORK,C' '                                                        
         JE    FCT30                                                            
         J     FCT60                                                            
                                                                                
***********************************************************************         
*        ROUTINE BUILDS PAID VERSIONS TABLE                           *         
*        ON ENTRY ... R3 = A(IOKEY)                                             
***********************************************************************         
                                                                                
BLDPVTAB NTR1                                                                   
         CLI   RQCOSPAD,C'Y'                                                    
         JNE   XIT                                                              
         CLC   LASTCOM,SVCOKEY+TLCOCOM-TLCOD                                    
         JE    XIT                                                              
                                                                                
         MVC   LASTCOM,SVCOKEY+TLCOCOM-TLCOD                                    
         MVI   PVRTAB,X'FF'                                                     
                                                                                
         USING TLINPD,R3                                                        
         XC    TLINPKEY,TLINPKEY                                                
         MVI   TLINPCD,TLINHCDQ                                                 
         MVC   TLINHCOM,RQCOSCOM                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO5'                               
         J     BPVT20                                                           
BPVT10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO5'                               
BPVT20   CLC   IOKEY(TLINHINV-TLINPD),IOKEYSAV                                  
         JNE   XIT                                                              
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO5'                              
                                                                                
         USING TAIND,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAINELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BPVT10                                                           
         TM    TAINSTAT,TAINSCIN+TAINSCAN                                       
         JNZ   BPVT10                                                           
         DROP  R4                                                               
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO5                                                          
         MVI   ELCODE,TAVRELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   BPVT10                                                           
                                                                                
         LA    R2,PVRTAB                                                        
BPVT30   CLI   0(R2),X'FF'                                                      
         JE    BPVT40                                                           
         CLC   TAVRVERS,0(R2)                                                   
         JE    BPVT10                                                           
         LA    R2,L'TAVRVERS(R2)                                                
         J     BPVT30                                                           
                                                                                
BPVT40   MVC   0(L'TAVRVERS,R2),TAVRVERS                                        
         MVI   L'TAVRVERS(R2),X'FF'                                             
         J     BPVT10                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS THAT NEW MEDIA/INTERNET KEY/RECORDS MATCHES   *         
*        SEARCH FILTERS AND RETURNS THE RECORD                        *         
*        ON ENTRY ... R3=A(KEY)                                       *         
***********************************************************************         
                                                                                
FLTNI    NTR1  BASE=*,LABEL=*                                                   
         ZIC   RF,LKEYCOMP                                                      
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         JNE   NO                                                               
         MVC   SVIOKEY,IOKEY                                                    
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE RETURNS COMMERCIAL RECORD INDEX FOR RQVRVER          *         
*        ON ENTRY ... P1=A(VERSION NUMBER)                            *         
*                     P2=A(FIELD TO STORE INDEX IN)                   *         
***********************************************************************         
                                                                                
SETINDEX NTR1  BASE=*,LABEL=*                                                   
         L     RF,0(R1)                                                         
                                                                                
         LA    RE,VERINDEX                                                      
SI10     CLC   0(1,RF),0(RE)     FIND PASSED IN VERSION NUMBER                  
         JNH   SI20              IN VERINDEX TABLE                              
         LA    RE,2(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         JNE   SI10                                                             
                                                                                
SI20     L     RF,4(R1)          SET COMMERCIAL INDEX EQUATE                    
         MVC   0(1,RF),1(RE)                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE VERSION       *         
*        CODE IS ON                                                   *         
***********************************************************************         
                                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK STAFF ACCESS TO COMMERCIAL RECORD           *         
*        ON ENTRY ... AIO3 = A(COMMERCIAL RECORD)                     *         
***********************************************************************         
                                                                                
CKCOACC  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACBLKMAX,0          IF AGY/CLI LIMITS WERE NOT SAVED,            
         JE    CCOAYES             STAFF HAS ACCESS TO ALL COMMERCIALS          
                                                                                
         CLI   ACLIMSTA,COSSINIT   IF THIS IS THE FIRST COMMERCIAL              
         JNE   *+8                 WE'RE CHECKING                               
         MVI   ACLIMSTA,COSSALIM   ADJUST AGY/CLI ACCESS STATUS                 
                                                                                
         USING TLCOD,R2                                                         
         L     R2,AIO3             R2=A(COMMERCIAL RECORD)                      
                                                                                
         ZIC   R3,ACBLKMAX         INIT # OF SAVED AGY/CLI LIMIT BLKS           
         XR    R0,R0               INIT CURRENT AGY/CLI LIMIT BLOCK             
                                                                                
CCOA10   CR    R0,R3               IF ALL AGY/CLI BLOCKS HAVE BEEN              
         JE    NO                  CHECKED, STAFF DOES NOT HAVE ACCESS          
         AHI   R0,1                                                             
                                                                                
         USING FAWSSVRD,R1                                                      
         LA    R1,WSSVRBLK                                                      
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R0,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF RECORD VIA WSSVR                
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,AIO1                                                     
         GOTO1 VWSSVR,(R1)                                                      
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         USING TAVAD,R4                                                         
CCOA20   L     R4,AIO1                                                          
CCOA30   CLI   0(R4),TAVAELQ       IF NO MORE TAVA ELEMENTS FOUND,              
         JNE   CCOA10              RECALL NEXT RECORD FROM WSSVR                
                                                                                
         CLC   TLCOAGY,TAVAAGY     IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   CCOA50                                                           
         CLI   TAVALEN,TAVALNQ     AND NO CLIENT LIMITS ARE DEFINED             
         JE    CCOAYES             ACCESS IS GRANTED                            
                                                                                
         CLI   ACLIMSTA,0          IF A COMMERCIAL HAS NOT ALREADY              
         JE    *+8                 PASSED AGY/CLI CHECK                         
         MVI   ACLIMSTA,COSSCLIM   ADJUST AGY/CLI ACCESS STATUS                 
                                                                                
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CCOA40   CLC   TLCOCLI,0(RF)       IF CLIENT IS FOUND IN STAFF LIMITS           
         JE    CCOAYES             ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CCOA40                                                           
                                                                                
CCOA50   ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         J     CCOA30                                                           
         DROP  R2,R4                                                            
                                                                                
CCOAYES  MVI   ACLIMSTA,0                                                       
         J     YES                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK STAFF ACCESS TO W4 RECORD                   *         
***********************************************************************         
                                                                                
CKW4ACC  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACBLKMAX,0          IF AGY/CLI LIMITS WERE NOT SAVED,            
         JE    YES                 STAFF HAS ACCESS TO ALL W4S                  
                                                                                
         MVC   SVIOKEY(L'TLW4KEY),IOKEY                                         
                                                                                
         USING TLCKPD,R2                                                        
         XC    TLCKPKEY,TLCKPKEY   READ ALL CHECK KEYS FOR PERFORMER            
         MVI   TLCKPCD,TLCKECDQ                                                 
         MVC   TLCKESSN,SVSSN                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IOCHKDIR+IO3'                            
         J     CW4A20                                                           
CW4A10   GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IOCHKDIR+IO3'                            
CW4A20   CLI   TLCKPCD,TLCKECDQ                                                 
         JNE   CW4AN                                                            
         CLC   TLCKESSN,SVSSN                                                   
         JNE   CW4AN                                                            
                                                                                
         ZIC   R3,ACBLKMAX         INIT # OF SAVED AGY/CLI LIMIT BLKS           
         XR    R0,R0               INIT CURRENT AGY/CLI LIMIT BLOCK             
                                                                                
CW4A30   CR    R0,R3               IF ALL AGY/CLI BLOCKS HAVE BEEN              
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
                                                                                
         USING TAVAD,R4                                                         
         L     R4,AIO5                                                          
CW4A40   CLI   0(R4),TAVAELQ       IF NO MORE TAVA ELEMENTS FOUND,              
         JNE   CW4A30              RECALL NEXT RECORD FROM WSSVR                
                                                                                
         CLC   TLCKEAGY,TAVAAGY    IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   CW4A80                                                           
         CLI   TAVALEN,TAVALNQ     AND NO CLIENT LIMITS ARE DEFINED             
         JE    CW4AY               ACCESS IS GRANTED                            
         DROP  R2                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOCHKFIL+IO3'                           
                                                                                
         USING TAPDD,R1                                                         
         L     R1,IOADDR                                                        
         LA    R1,TLRCELEM-TLRCD(R1)                                            
CW4A50   CLI   0(R1),TAPDELQ                                                    
         JE    CW4A60                                                           
         CLI   0(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     CW4A50                                                           
                                                                                
CW4A60   ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CW4A70   CLC   TAPDCLI,0(RF)       IF CLIENT IS FOUND IN STAFF LIMITS           
         JE    CW4AY               ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CW4A70                                                           
         DROP  R1                                                               
                                                                                
CW4A80   ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         J     CW4A40                                                           
         DROP  R4                                                               
                                                                                
CW4AY    MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    YES                                                              
         DC    H'00'                                                            
                                                                                
CW4AN    MVC   IOKEY,SVIOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    NO                                                               
         DC    H'00'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK FOR SUBSIDIARY AGENCIES/CLIENTS             *         
*        ON ENTRY ... IOADDR = A(GUARANTEE RECORD)                    *         
***********************************************************************         
                                                                                
CKSUBS   NTR1  BASE=*,LABEL=*                                                   
         USING TAVAD,R4                                                         
         L     R4,IOADDR           READ ALL AGENCY/CLIENT                       
         MVI   ELCODE,TAVAELQ      ELEMENTS                                     
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
CSUB30   BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
                                                                                
         CLC   RQGUSAGY,TAVAAGY    IF AGENCY IS MATCHED,                        
         JNE   CSUB30                                                           
         CLI   TAVALEN,TAVALNQ     AND NO CLIENTS IN ELEMENT,                   
         JE    YES                 ACCESS IS GRANTED                            
                                                                                
         OC    RQGUSCLI,RQGUSCLI   IF NOT FILTERING ON CLIENTS,                 
         JZ    YES                 ACCESS IS GRANTED                            
                                                                                
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CSUB40   CLC   RQGUSCLI,0(RF)      IF CLIENT IS MATCHED,                        
         JE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CSUB40                                                           
         J     CSUB30              GET NEXT ELEMENT                             
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK STAFF ACCESS TO GUARANTEE RECORD            *         
*        ON ENTRY ... IOADDR = A(GUARANTEE RECORD)                    *         
***********************************************************************         
                                                                                
CKGUACC  NTR1  BASE=*,LABEL=*                                                   
         CLI   ACBLKMAX,0          IF AGY/CLI LIMITS WERE NOT SAVED,            
         JE    YES                 STAFF HAS ACCESS TO ALL GUARANTEES           
                                                                                
         USING TAGUD,R4                                                         
         L     R4,IOADDR           A(GUARANTEE RECORD)                          
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         OC    TAGUAGY,TAGUAGY     STAFF WITH LIMITS DO NOT HAVE ACCESS         
         JZ    NO                  TO GUARANTEES WITHOUT LIMITS                 
         MVC   SVAGY,TAGUAGY       SAVE GUARANTEE'S PRIMARY AGENCY              
         MVC   SVCLI,TAGUCLI       AND PRIMARY CLIENT                           
         DROP  R4                                                               
                                                                                
         ZIC   R3,ACBLKMAX         INIT # OF SAVED AGY/CLI LIMIT BLKS           
         XR    R0,R0               INIT CURRENT AGY/CLI LIMIT BLOCK             
                                                                                
CGUA10   CR    R0,R3               IF ALL AGY/CLI BLOCKS HAVE BEEN              
         JE    CGUA20              CHECKED, STAFF DOES NOT HAVE                 
         AHI   R0,1                ACCESS TO PRIMARY AGY/CLI                    
                                                                                
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
                                                                                
         BAS   RE,CHKLIM           IF STAFF HAS ACCESS TO PRIMARY               
         JNE   CGUA10              AGENCY/CLIENT, ACCESS IS GRANTED             
         J     YES                                                              
                                                                                
         USING TAVAD,R4                                                         
CGUA20   L     R4,IOADDR           ELSE, READ ALL SUBSIDIARY AGENCY/            
         MVI   ELCODE,TAVAELQ      CLIENT ELEMENTS                              
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
CGUA30   BRAS  RE,NEXTEL                                                        
         JNE   NO                                                               
                                                                                
         XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         JNZ   CGUA40                                                           
         LHI   RF,TAVALNQ                                                       
CGUA40   D     RE,=A(L'TAVACLI)                                                 
                                                                                
         LR    R2,RF               R2=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R5,TAVACLI          RE=A(CURRENT CLIENT IN ELEMENT)              
                                                                                
         MVC   SVAGY,TAVAAGY       SAVE GUARANTEE'S CURRENT AGENCY              
         XC    SVCLI,SVCLI         AND CLIENT                                   
         CLI   TAVALEN,TAVALNQ                                                  
         JE    CGUA60                                                           
CGUA50   MVC   SVCLI,0(R5)                                                      
                                                                                
CGUA60   ZIC   R3,ACBLKMAX         INIT # OF SAVED AGY/CLI LIMIT BLKS           
         XR    R0,R0               INIT CURRENT AGY/CLI LIMIT BLOCK             
                                                                                
CGUA70   CR    R0,R3               IF ALL AGY/CLI BLOCKS HAVE BEEN              
         JE    CGUA70              CHECKED, STAFF DOES NOT HAVE                 
         AHI   R0,1                ACCESS TO PRIMARY AGY/CLI                    
                                                                                
         USING FAWSSVRD,R1                                                      
         CHI   R3,1                IF MORE THAN 1 STAFF RECORD                  
         JE    CGUA80              SAVED BY WSSVR ...                           
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
                                                                                
CGUA80   AHI   R0,1                                                             
                                                                                
         BAS   RE,CHKLIM           IF STAFF HAS ACCESS TO SUBSIDIARY            
         JE    YES                 AGENCY/CLIENT, ACCESS IS GRANTED             
                                                                                
         LA    R5,L'TAVACLI(R5)                                                 
         BCT   R2,CGUA50           BUMP TO NEXT CLIENT IN ELEMENT               
         J     CGUA30                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE CHECKS IF STAFF HAS ACCESS TO AGENCY/CLIENT          *         
*        ON ENTRY ... SVAGY=AGENCY, SVCLI=CLIENT                      *         
***********************************************************************         
CHKLIM   NTR1                                                                   
         USING TAVAD,R4                                                         
         L     R4,AIO5                                                          
CKLM30   CLI   0(R4),TAVAELQ       IF NO MORE TAVA ELEMENTS FOUND,              
         JNE   NO                  RECALL NEXT RECORD FROM WSSVR                
                                                                                
         CLC   SVAGY,TAVAAGY       IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   CKLM50                                                           
         CLI   TAVALEN,TAVALNQ     AND NO CLIENT LIMITS ARE DEFINED             
         JE    YES                 ACCESS IS GRANTED                            
                                                                                
         OC    SVCLI,SVCLI         IF GUARANTEE IS FOR ALL CLIENTS,             
         JZ    YES                 STAFF HAS ACCESS TO GUARANTEE                
                                                                                
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CKLM40   CLC   SVCLI,0(RF)         IF CLIENT IS FOUND IN STAFF LIMITS           
         JE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CKLM40                                                           
                                                                                
CKLM50   ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         J     CKLM30                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SAVE LENGTH OF INPUT IN NAME FIELDS                          *         
*        ON ENTRY ... 0(R1)=A(NAME FIELD)                             *         
*                     4(R1)=A(FIELD TO SAVE INPUT LENGTH IN)          *         
***********************************************************************         
                                                                                
SVLENGTH NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2=A(NAME FIELD)                             
         L     R3,4(R1)            R3=A(FIELD TO SAVE INPUT LENGTH IN)          
                                                                                
         XR    RF,RF               SAVE LENGTH OF LAST NAME INPUT               
         OC    0(L'RQW4LNM,R2),0(R2)                                            
         JZ    SVLEN20                                                          
         LA    RE,L'RQW4LNM-1(R2)                                               
         LHI   RF,L'RQW4LNM                                                     
SVLEN10  LTR   RF,RF                                                            
         JNZ   *+6                                                              
         DC    H'00'                                                            
         CLI   0(RE),C' '                                                       
         JNE   SVLEN20                                                          
         SHI   RE,1                                                             
         SHI   RF,1                                                             
         J     SVLEN10                                                          
SVLEN20  STC   RF,0(R3)                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK NAME INPUT AGAINST W4 KEY/RECORD            *         
*        ON ENTRY ... R2=A(FIRST READ W4 KEY)                         *         
***********************************************************************         
                                                                                
         USING TLW4PD,R2                                                        
CKNAME   NTR1  BASE=*,LABEL=*                                                   
         J     CKNAM20                                                          
                                                                                
CKNAM10  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
                                                                                
CKNAM20  ZIC   RE,LNAMELEN         ENSURE THAT IT MATCHES KEY                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   TLW4NLST(0),RQW4LNM                                              
         JNE   NO                                                               
         DROP  R2                                                               
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
                                                                                
         USING TLW4D,R1                                                         
         L     R1,IOADDR           R1=A(W4 RECORD)                              
         MVC   SVSSN,TLW4SSN       SAVE SS#                                     
                                                                                
         CLI   FNAMELEN,0          IF FIRST NAME WAS ENTERED                    
         JE    CKNAM50                                                          
         LA    R1,TLW4ELEM         R1=A(FIRST ELEMENT)                          
         DROP  R1                                                               
                                                                                
         USING TAW4D,R1                                                         
CKNAM30  CLI   0(R1),0                                                          
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R1),TAW4ELQ       FIND EMPLOYEE W4 DETAILS ELEMENT             
         JE    CKNAM40                                                          
         ZIC   RE,1(R1)                                                         
         AR    R1,RE                                                            
         J     CKNAM30                                                          
                                                                                
CKNAM40  ZIC   RE,FNAMELEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         CLC   TAW4NAM1(0),RQW4FNM IF FIRST NAME DOES NOT MATCH                 
         JNE   CKNAM10             GO READ NEXT W4 RECORD                       
         DROP  R1                                                               
                                                                                
CKNAM50  BRAS  RE,CKW4ACC          ENSURE STAFF HAS ACCESS                      
         JNE   CKNAM10                                                          
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* KEY DRIVER TABLES                                                   *         
***********************************************************************         
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
* REQUEST MAP FOR W4 SEARCH                                           *         
***********************************************************************         
                                                                                
REQW4S   LKREQ H,I#W4SDLO,OUTW4S                                                
RqStf    LKREQ F,1,(D,B#SAVED,RQW4STF),CHAR,TEXT=TA#STAFF,COL=*                 
RqSsn    LKREQ F,2,(D,B#SAVED,RQW4SSN),CHAR,TEXT=TA#SSN,COL=*                   
RqLNm    LKREQ F,3,(D,B#SAVED,RQW4LNM),CHAR,TEXT=TA#LNAME,COL=*                 
RqFNm    LKREQ F,4,(D,B#SAVED,RQW4FNM),CHAR,TEXT=TA#FNAME,COL=*                 
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR GUARANTEE SEARCH                                    *         
***********************************************************************         
                                                                                
REQGUS   LKREQ H,I#GUSDLD,OUTGUS                                                
RqStf    LKREQ F,D#GUSSTF,(D,B#SAVED,RQGUSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqSsn    LKREQ F,D#GUSSSN,(D,B#SAVED,RQGUSSSN),CHAR,TEXT=TA#SSN,COL=*           
RqGua    LKREQ F,D#GUSGUA,(D,B#SAVED,RQGUSGUA),CHAR,TEXT=TA#GRTCD,COL=*         
RqCrp    LKREQ F,D#GUSCRP,(D,B#SAVED,RQGUSCRP),CHAR,TEXT=TA#ATC,COL=*           
RqAgy    LKREQ F,D#GUSAGY,(D,B#SAVED,RQGUSAGY),CHAR,TEXT=TA#AGYCD,COL=*         
RqCli    LKREQ F,D#GUSCLI,(D,B#SAVED,RQGUSCLI),CHAR,TEXT=TA#CLICD,COL=*         
RqTyp    LKREQ F,D#GUSTYP,(D,B#SAVED,RQGUSTYP),CHAR,TEXT=TA#GRTTY,COL=*         
RqPst    LKREQ F,D#GUSPST,(D,B#SAVED,RQGUSPST),PDAT,TEXT=TA#PERST,COL=*         
RqPen    LKREQ F,D#GUSPEN,(D,B#SAVED,RQGUSPEN),PDAT,TEXT=TA#PEREN,COL=*         
RqLck    LKREQ F,D#GUSLCK,(D,B#SAVED,RQGUSLCK),CHAR,TEXT=TA#LOCKD,COL=*         
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR COMMERCIAL/VERSION SEARCH                           *         
***********************************************************************         
                                                                                
REQCOS   LKREQ H,I#COSDLD,OUTCOS                                                
RqStf    LKREQ F,D#COSSTF,(D,B#SAVED,RQCOSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqCom    LKREQ F,D#COSCOM,(I,B#SAVED,RQCOSCIN),HEXD,OLEN=L'RQCOSCOM,   *        
               LIST=F,TEXT=TA#COMCD,COL=*                                       
RqVer    LKREQ F,D#COSVER,(D,B#SAVED,RQCOSVER),UBIN,TEXT=TA#VER,COL=*           
RqAgy    LKREQ F,D#COSAGY,(D,B#SAVED,RQCOSAGY),CHAR,TEXT=TA#AGYCD,COL=*         
RqCid    LKREQ F,D#COSCID,(I,B#SAVED,RQCOSIIN),CHAR,OLEN=12,LIST=F,    *        
               TEXT=TA#CIDCD,COL=*                                              
RqCli    LKREQ F,D#COSCLI,(D,B#SAVED,RQCOSCLI),CHAR,TEXT=TA#CLICD,COL=*         
RqPrd    LKREQ F,D#COSPRD,(D,B#SAVED,RQCOSPRD),CHAR,TEXT=TA#PRDCD,COL=*         
RqTit    LKREQ F,D#COSTIT,(D,B#SAVED,RQCOSTIT),CHAR,TEXT=TA#COTIT,COL=*         
RqTyF    LKREQ F,D#COSTYP,(I,B#SAVED,RQCOSTIN),CHAR,OLEN=1,LIST=F,     *        
               TEXT=TA#COTYP,COL=*                                              
RqATF    LKREQ F,D#COSATY,(I,B#SAVED,RQCOSAIN),UBIN,OLEN=1,LIST=F,     *        
               TEXT=TA#ACTYP,COL=*                                              
RqMeF    LKREQ F,D#COSMED,(I,B#SAVED,RQCOSMIN),CHAR,OLEN=1,LIST=F,     *        
               TEXT=TA#MEDCD,COL=*                                              
RqPol    LKREQ F,D#COSPOL,(D,B#SAVED,RQCOSPOL),CHAR,TEXT=TA#COPOL,COL=*         
RqLck    LKREQ F,D#COSLCK,(D,B#SAVED,RQCOSLCK),CHAR,TEXT=TA#LOCKD,COL=*         
RqRel    LKREQ F,D#COSREL,(D,B#SAVED,RQCOSREL),CHAR,TEXT=TA#RELSD,COL=*         
RqCDo    LKREQ F,D#COSCDO,(D,B#SAVED,RQCOSCDO),CHAR,TEXT=TA#CANDO,COL=*         
RqCRt    LKREQ F,D#COSCRT,(D,B#SAVED,RQCOSCRT),CHAR,TEXT=TA#CANRT,COL=*         
RqSop    LKREQ F,D#COSSOP,(D,B#SAVED,RQCOSSOP),CHAR,TEXT=TA#SOAPR,COL=*         
RqPCy    LKREQ F,D#COSPCY,(D,B#SAVED,RQCOSPCY),CHAR,TEXT=TA#PCYCO,COL=*         
RqPad    LKREQ F,D#COSPAD,(D,B#SAVED,RQCOSPAD),CHAR,TEXT=TA#REPAD,COL=*         
RqIVr    LKREQ F,D#COSIVR,(D,B#SAVED,RQCOSIVR),CHAR,TEXT=TA#INCVR,COL=*         
RqIEx    LKREQ F,D#COSIEX,(D,B#SAVED,RQCOSEXT),CHAR,TEXT=TA#EXCID,COL=*         
RqAVr    LKREQ F,D#COSAVR,(D,B#SAVED,RQCOSAVR),CHAR,TEXT=TA#ATTVR,COL=*         
RqACo    LKREQ F,D#COSAAC,(D,B#SAVED,RQCOSAFM),CHAR,TEXT=TA#INCAC,COL=*         
RqSuS    LKREQ F,D#COSSUS,(D,B#SAVED,RQCOSSUS),CHAR,TEXT=TA#SUSRC,COL=*         
RqEmp    LKREQ F,D#COSEMP,(D,B#SAVED,RQCOSEMP),CHAR,TEXT=TA#EMPLY,COL=*         
RqELA    LKREQ F,D#COSELA,(D,B#SAVED,RQCOSELA),CHAR,TEXT=TA#EXLKA,COL=*         
RqELC    LKREQ F,D#COSELC,(D,B#SAVED,RQCOSELC),CHAR,TEXT=TA#EXLKC,COL=*         
RqELP    LKREQ F,D#COSELP,(D,B#SAVED,RQCOSELP),CHAR,TEXT=TA#EXLKP,COL=*         
RqENV    LKREQ F,D#COSENV,(D,B#SAVED,RQCOSENV),CHAR,TEXT=TA#EXCNV,COL=*         
RqEUn    LKREQ F,D#COSEUN,(I,B#SAVED,RQCOSUIN),CHAR,OLEN=L'TACAUN,     *        
               LIST=F,TEXT=TA#EXUNI,COL=*                                       
RqEWT    LKREQ F,D#COSEWT,(I,B#SAVED,RQCOSWIN),CHAR,OLEN=1,LIST=F,     *        
               TEXT=TA#EXW4T,COL=*                                              
RqERC    LKREQ F,D#COSERL,(D,B#SAVED,RQCOSERL),CHAR,TEXT=TA#EXRLT,COL=*         
RqEP6    LKREQ F,D#COSEP6,(D,B#SAVED,RQCOSEP6),CHAR,TEXT=TA#EXP06,COL=*         
RqTEx    LKREQ F,D#COSTEX,(D,B#SAVED,RQCOSTEX),CHAR,TEXT=TA#EXTIT,COL=*         
RqEOM    LKREQ F,D#COSEOM,(D,B#SAVED,RQCOSEOM),CHAR,TEXT=TA#EXOSM,COL=*         
RqTMA    LKREQ F,D#COSTMA,(D,B#SAVED,RQCOSTMA),CHAR,TEXT=TA#TMA,COL=*           
RqAMT    LKREQ F,D#COSAMT,(D,B#SAVED,RQCOSAMT),CHAR,TEXT=TA#AMOT,COL=*          
RqES9    LKREQ F,D#COSES9,(D,B#SAVED,RQCOSES9),CHAR,TEXT=TA#EXS9,COL=*          
RqSkA    LKREQ F,D#COSSKA,(D,B#SAVED,RQCOSSKA),CHAR,TEXT=TA#SKACC,COL=*         
RqELS    LKREQ F,D#COSELS,(D,B#SAVED,RQCOSELS),CHAR,TEXT=TA#ELS,COL=*           
RqPED    LKREQ F,D#COSPED,(D,B#SAVED,RQCOSPED),PDAT,TEXT=TA#PED,COL=*           
RqLim    LKREQ F,D#COSLIM,(D,B#SAVED,RQCOSLIM),UBIN,TEXT=TA#LIMIT,COL=*         
RqRWA    LKREQ F,D#COSRWA,(D,B#SAVED,RQCOSRWA),CHAR,TEXT=TA#REQWA,COL=*         
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR NEW MEDIA/INTERNET SEARCH                           *         
***********************************************************************         
                                                                                
REQNIS   LKREQ H,I#NISDLD,OUTNIS                                                
RqStf    LKREQ F,D#NISSTF,(D,B#SAVED,RQNISSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqMed    LKREQ F,D#NISMED,(D,B#SAVED,RQNISMED),CHAR,TEXT=TA#MEDCD,COL=*         
RqCod    LKREQ F,D#NISCOD,(D,B#SAVED,RQNISCOD),CHAR,TEXT=TA#CODE,COL=*          
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR TIMESHEET SEARCH                                    *         
***********************************************************************         
                                                                                
REQTMS   LKREQ H,I#TMSDLD,OUTTMS                                                
RqStf    LKREQ F,D#TMSSTF,(D,B#SAVED,RQTMSSTF),CHAR,TEXT=TA#STAFF,COL=*         
RqCom    LKREQ F,D#TMSCOM,(D,B#SAVED,RQTMSCOM),HEXD,TEXT=TA#COMCD,COL=*         
RqInv    LKREQ F,D#TMSINV,(D,B#SAVED,RQTMSINV),CHAR,TEXT=TA#INV,COL=*           
RqCSq    LKREQ F,D#TMSSEQ,(D,B#SAVED,RQTMSCSQ),HEXD,TEXT=TA#CSTSQ,COL=*         
RqWeb    LKREQ F,D#TMSWCR,(D,B#SAVED,RQTMSWEB),CHAR,TEXT=TA#WBCRE,COL=*         
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR USE SEARCH                                          *         
***********************************************************************         
                                                                                
REQUSE   LKREQ H,I#USEDLD,OUTUSE                                                
RqStf    LKREQ F,D#USESTF,(D,B#SAVED,RQUSESTF),CHAR,TEXT=TA#STAFF,COL=*         
RqCom    LKREQ F,D#USECOM,(D,B#SAVED,RQUSECOM),HEXD,TEXT=TA#COMCD,COL=*         
RqUse    LKREQ F,D#USEUSE,(D,B#SAVED,RQUSEUSE),CHAR,TEXT=TA#USECD,COL=*         
RqPSD    LKREQ F,D#USEPSD,(D,B#SAVED,RQUSEPSD),PDAT,TEXT=TA#PERST,COL=*         
RqPED    LKREQ F,D#USEPED,(D,B#SAVED,RQUSEPED),PDAT,TEXT=TA#PEREN,COL=*         
         LKREQ E                                                                
                                                                                
         LKREQ X                                                                
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
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - GUARANTEE SEARCH DOWNLOAD                              *         
***********************************************************************         
                                                                                
OUTGUS   LKOUT H                                                                
                                                                                
GUSREC   LKOUT R,O#GUSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#GUSSTA,(A,ARYGUS)                                            
         LKOUT E                                                                
                                                                                
GUXREC   LKOUT R,O#GUXDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#GUXDET,(A,ARYGUX)                                            
         LKOUT E                                                                
                                                                                
GUCREC   LKOUT R,O#GUCOMT                   ** COMMENT VALUES **                
Array    LKOUT C,O#GUCOMT,(A,ARYGUC)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - COMMERCIAL/VERSION SEARCH DOWNLOAD                     *         
***********************************************************************         
                                                                                
OUTCOS   LKOUT H                                                                
                                                                                
COSREC   LKOUT R,O#COSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#COSSTA,(A,ARYCOS)                                            
         LKOUT E                                                                
                                                                                
CODREC   LKOUT R,O#COSDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#COSDET,(A,ARYCOD)                                            
         LKOUT E                                                                
                                                                                
COXREC   LKOUT R,O#COXDET                   ** EXP DETAILS VALUES **            
Array    LKOUT C,O#COXDET,(A,ARYCOX)                                            
         LKOUT E                                                                
                                                                                
COLREC   LKOUT R,O#COSLIM                   ** LIMIT VALUES **                  
Array    LKOUT C,O#COSLIM,(A,ARYCOL)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - NEW MEDIA/INTERNET SEARCH DOWNLOAD                     *         
***********************************************************************         
                                                                                
OUTNIS   LKOUT H                                                                
                                                                                
NISREC   LKOUT R,O#NISSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#NISSTA,(A,ARYNIS)                                            
         LKOUT E                                                                
                                                                                
NIXREC   LKOUT R,O#NIXDET                   ** DETAILS VALUES **                
Array    LKOUT C,O#NIXDET,(A,ARYNIX)                                            
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - TIMESHEET SEARCH DOWNLOAD                              *         
***********************************************************************         
                                                                                
OUTTMS   LKOUT H                                                                
                                                                                
TMSREC   LKOUT R,O#TMSSTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#TMSSTA,(A,ARYTMS)                                            
         LKOUT E                                                                
                                                                                
TMTREC   LKOUT R,O#TMTOT                    ** TOTALS VALUES **                 
Array    LKOUT C,O#TMTOT,(A,ARYTMT)                                             
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP - USE SEARCH DOWNLOAD                                    *         
***********************************************************************         
                                                                                
OUTUSE   LKOUT H                                                                
                                                                                
USSREC   LKOUT R,O#USESTA                   ** STATUS VALUES **                 
Array    LKOUT C,O#USESTA,(A,ARYUSS)                                            
         LKOUT E                                                                
                                                                                
UACREC   LKOUT R,O#USEEAC                   ** EXISTING ASSET **                
Array    LKOUT C,O#USEEAC,(A,ARYUAC)        **  CYCLE VALUES  **                
         LKOUT E                                                                
                                                                                
UALREC   LKOUT R,O#USEALU                   ** ASSET LEVEL USE **               
Array    LKOUT C,O#USEALU,(A,ARYUAL)        ** DETAILS VALUES  **               
         LKOUT E                                                                
                                                                                
UPLREC   LKOUT R,O#USEPLU                   ** PERF LEVEL USE **                
Array    LKOUT C,O#USEPLU,(A,ARYUPL)        ** DETAILS VALUES **                
         LKOUT E                                                                
                                                                                
         LKOUT X                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR W4 SEARCH STATUS RECORDS                       *         
***********************************************************************         
                                                                                
ARYW4S   LKOUT A,(R,NXTW4S),ROWNAME=W4SVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,W4SSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR W4 SEARCH DETAILS RECORDS                      *         
***********************************************************************         
                                                                                
ARYW4D   LKOUT A,(R,NXTW4D),MULTIROW=Y,ROWNAME=W4DVALS                          
                                                                                
RtPid    LKOUT C,1,(D,,W4DPID),CHAR,ND=Y                                        
RtLNm    LKOUT C,2,(D,,W4DLNAM),CHAR,ND=Y                                       
RtFNm    LKOUT C,3,(D,,W4DFNAM),CHAR,ND=Y                                       
RtMNm    LKOUT C,4,(D,,W4DMNAM),CHAR,ND=Y                                       
RtAd1    LKOUT C,5,(D,,W4DADD1),CHAR,ND=Y                                       
RtAd2    LKOUT C,6,(D,,W4DADD2),CHAR,ND=Y                                       
RtAd3    LKOUT C,7,(D,,W4DADD3),CHAR,ND=Y                                       
RtCty    LKOUT C,8,(D,,W4DCITY),CHAR,ND=Y                                       
RtSta    LKOUT C,9,(D,,W4DSTAT),CHAR,ND=Y                                       
RtZip    LKOUT C,10,(D,,W4DZIPC),CHAR,ND=Y                                      
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL/VERSION SEARCH STATUS RECORDS       *         
***********************************************************************         
                                                                                
ARYCOS   LKOUT A,(R,NXTCOS),ROWNAME=COSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,COSSTAT),UBIN,ND=Y                                       
RtSDt    LKOUT C,2,(D,,COSSDATE),CHAR,ND=Y                                      
RtSTm    LKOUT C,3,(D,,COSSTIME),CHAR,ND=Y                                      
RtPSS    LKOUT C,4,(D,,COSSPSST),UBIN,ND=Y                                      
RtACS    LKOUT C,5,(D,,COSSACST),UBIN,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL/VERSION COMPRESSED DETAILS RECORDS *          
***********************************************************************         
                                                                                
ARYCOD   LKOUT A,(R,NXTCOD),MULTIROW=Y,ROWNAME=COXVALS                          
                                                                                
RtCom    LKOUT C,4,(D,,COXCOM),CHAR,ND=Y                                        
RtVer    LKOUT C,5,(D,,COXVER),UBIN,ND=Y                                        
RtAgy    LKOUT C,6,(D,,COXAGY),CHAR,ND=Y                                        
RtCid    LKOUT C,7,(D,,COXCID),CHAR,ND=Y                                        
RtCli    LKOUT C,8,(D,,COXCLI),CHAR,ND=Y                                        
RtPrd    LKOUT C,9,(D,,COXPRD),CHAR,ND=Y                                        
RtTit    LKOUT C,10,(D,,COXTIT),CHAR,ND=Y                                       
RtTyp    LKOUT C,11,(D,,COXTYP),CHAR,ND=Y                                       
RtMed    LKOUT C,18,(D,,COXMED),CHAR,ND=Y                                       
RtSec    LKOUT C,19,(D,,COXSEC),CHAR,ND=Y                                       
RtATY    LKOUT C,22,(D,,COXATY),UBIN,ND=Y                                       
RtFDt    LKOUT C,24,(D,,COXFDT),CHAR,ND=Y                                       
RtFSu    LKOUT C,25,(D,,COXFSU),CHAR,ND=Y                                       
RtFCy    LKOUT C,26,(D,,COXFCY),CHAR,ND=Y                                       
RtFSt    LKOUT C,27,(D,,COXFST),CHAR,ND=Y                                       
RtRDt    LKOUT C,28,(D,,COXRDT),CHAR,ND=Y                                       
RtRSu    LKOUT C,29,(D,,COXRSU),CHAR,ND=Y                                       
RtRCy    LKOUT C,30,(D,,COXRCY),CHAR,ND=Y                                       
RtRSt    LKOUT C,31,(D,,COXRST),CHAR,ND=Y                                       
RtMdt    LKOUT C,32,(D,,COXMDT),CHAR,ND=Y                                       
RtA1C    LKOUT C,50,(D,,COXA1C),CHAR,ND=Y                                       
RtA1T    LKOUT C,51,(D,,COXA1T),CHAR,ND=Y                                       
RtA1S    LKOUT C,53,(D,,COXA1S),CHAR,ND=Y                                       
RtA1I    LKOUT C,56,(D,,COXA1I),CHAR,ND=Y                                       
RtA2C    LKOUT C,58,(D,,COXA2C),CHAR,ND=Y                                       
RtA2T    LKOUT C,59,(D,,COXA2T),CHAR,ND=Y                                       
RtA2S    LKOUT C,61,(D,,COXA2S),CHAR,ND=Y                                       
RtA2I    LKOUT C,64,(D,,COXA2I),CHAR,ND=Y                                       
RtA3C    LKOUT C,66,(D,,COXA3C),CHAR,ND=Y                                       
RtA3T    LKOUT C,67,(D,,COXA3T),CHAR,ND=Y                                       
RtA3S    LKOUT C,69,(D,,COXA3S),CHAR,ND=Y                                       
RtA3I    LKOUT C,72,(D,,COXA3I),CHAR,ND=Y                                       
RtA4C    LKOUT C,74,(D,,COXA4C),CHAR,ND=Y                                       
RtA4T    LKOUT C,75,(D,,COXA4T),CHAR,ND=Y                                       
RtA4S    LKOUT C,77,(D,,COXA4S),CHAR,ND=Y                                       
RtA4I    LKOUT C,80,(D,,COXA4I),CHAR,ND=Y                                       
RtMid    LKOUT C,99,(D,,COXMID),CHAR,ND=Y                                       
RtPrN    LKOUT C,105,(D,,COXPRN),CHAR,ND=Y                                      
RtA5C    LKOUT C,120,(D,,COXA5C),CHAR,ND=Y                                      
RtA5T    LKOUT C,122,(D,,COXA5T),CHAR,ND=Y                                      
RtA5S    LKOUT C,124,(D,,COXA5S),CHAR,ND=Y                                      
RtA5I    LKOUT C,127,(D,,COXA5I),CHAR,ND=Y                                      
RtA6C    LKOUT C,130,(D,,COXA6C),CHAR,ND=Y                                      
RtA6T    LKOUT C,132,(D,,COXA6T),CHAR,ND=Y                                      
RtA6S    LKOUT C,134,(D,,COXA6S),CHAR,ND=Y                                      
RtA6I    LKOUT C,137,(D,,COXA6I),CHAR,ND=Y                                      
RtA7C    LKOUT C,140,(D,,COXA7C),CHAR,ND=Y                                      
RtA7T    LKOUT C,142,(D,,COXA7T),CHAR,ND=Y                                      
RtA7S    LKOUT C,144,(D,,COXA7S),CHAR,ND=Y                                      
RtA7I    LKOUT C,147,(D,,COXA7I),CHAR,ND=Y                                      
Rt1MA    LKOUT C,149,(D,,COX1MA),CHAR,ND=Y                                      
Rt2MA    LKOUT C,150,(D,,COX2MA),CHAR,ND=Y                                      
Rt3MA    LKOUT C,151,(D,,COX3MA),CHAR,ND=Y                                      
Rt4MA    LKOUT C,152,(D,,COX4MA),CHAR,ND=Y                                      
Rt5MA    LKOUT C,153,(D,,COX5MA),CHAR,ND=Y                                      
Rt6MA    LKOUT C,154,(D,,COX6MA),CHAR,ND=Y                                      
Rt7MA    LKOUT C,155,(D,,COX7MA),CHAR,ND=Y                                      
RtAMT    LKOUT C,167,(D,,COXAMT),CHAR,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL/VERSION EXPANDED DETAILS RECORDS *            
***********************************************************************         
                                                                                
ARYCOX   LKOUT A,(R,NXTCOX),MULTIROW=Y,ROWNAME=COXVALS                          
                                                                                
RtCom    LKOUT C,4,(D,,COXCOM),CHAR,ND=Y                                        
RtVer    LKOUT C,5,(D,,COXVER),UBIN,ND=Y                                        
RtAgy    LKOUT C,6,(D,,COXAGY),CHAR,ND=Y                                        
RtCid    LKOUT C,7,(D,,COXCID),CHAR,ND=Y                                        
RtCli    LKOUT C,8,(D,,COXCLI),CHAR,ND=Y                                        
RtPrd    LKOUT C,9,(D,,COXPRD),CHAR,ND=Y                                        
RtTit    LKOUT C,10,(D,,COXTIT),CHAR,ND=Y                                       
RtTyp    LKOUT C,11,(D,,COXTYP),CHAR,ND=Y                                       
RtATI    LKOUT C,12,(D,,COXATI),CHAR,ND=Y                                       
RtAdS    LKOUT C,13,(D,,COXADS),CHAR,ND=Y                                       
RtFFC    LKOUT C,14,(D,,COXFFC),CHAR,ND=Y                                       
RtFAr    LKOUT C,15,(D,,COXFAR),CHAR,ND=Y                                       
RtSAr    LKOUT C,16,(D,,COXSAR),CHAR,ND=Y                                       
RtExp    LKOUT C,17,(D,,COXEXP),CHAR,ND=Y                                       
RtMed    LKOUT C,18,(D,,COXMED),CHAR,ND=Y                                       
RtSec    LKOUT C,19,(D,,COXSEC),CHAR,ND=Y                                       
RtLid    LKOUT C,20,(D,,COXLID),CHAR,ND=Y                                       
RtLln    LKOUT C,21,(D,,COXLLN),CHAR,ND=Y                                       
RtATY    LKOUT C,22,(D,,COXATY),UBIN,ND=Y                                       
RtAex    LKOUT C,23,(D,,COXAEX),CHAR,ND=Y                                       
RtFDt    LKOUT C,24,(D,,COXFDT),CHAR,ND=Y                                       
RtFSu    LKOUT C,25,(D,,COXFSU),CHAR,ND=Y                                       
RtFCy    LKOUT C,26,(D,,COXFCY),CHAR,ND=Y                                       
RtFSt    LKOUT C,27,(D,,COXFST),CHAR,ND=Y                                       
RtRDt    LKOUT C,28,(D,,COXRDT),CHAR,ND=Y                                       
RtRSt    LKOUT C,29,(D,,COXRSU),CHAR,ND=Y                                       
RtRCy    LKOUT C,30,(D,,COXRCY),CHAR,ND=Y                                       
RtRSt    LKOUT C,31,(D,,COXRST),CHAR,ND=Y                                       
RtMdt    LKOUT C,32,(D,,COXMDT),CHAR,ND=Y                                       
RtMst    LKOUT C,33,(D,,COXMSU),CHAR,ND=Y                                       
RtMcy    LKOUT C,34,(D,,COXMCY),CHAR,ND=Y                                       
RtMSt    LKOUT C,35,(D,,COXMST),CHAR,ND=Y                                       
RtIN1    LKOUT C,36,(D,,COXIN1),CHAR,ND=Y                                       
RtIN2    LKOUT C,37,(D,,COXIN2),CHAR,ND=Y                                       
RtIN3    LKOUT C,38,(D,,COXIN3),CHAR,ND=Y                                       
RtIN4    LKOUT C,39,(D,,COXIN4),CHAR,ND=Y                                       
RtIN5    LKOUT C,40,(D,,COXIN5),CHAR,ND=Y                                       
RtIN6    LKOUT C,41,(D,,COXIN6),CHAR,ND=Y                                       
RtEYr    LKOUT C,42,(D,,COXEYR),CHAR,ND=Y                                       
RtETy    LKOUT C,43,(D,,COXETY),CHAR,ND=Y                                       
RtADt    LKOUT C,44,(D,,COXADT),CHAR,ND=Y                                       
RtIDt    LKOUT C,45,(D,,COXIDT),CHAR,ND=Y                                       
RtPol    LKOUT C,46,(D,,COXPOL),CHAR,ND=Y                                       
RtSCy    LKOUT C,47,(D,,COXSCY),CHAR,ND=Y                                       
RtAtt    LKOUT C,48,(D,,COXATT),CHAR,ND=Y                                       
RtCmT    LKOUT C,49,(D,,COXCMT),CHAR,ND=Y                                       
RtA1C    LKOUT C,50,(D,,COXA1C),CHAR,ND=Y                                       
RtA1T    LKOUT C,51,(D,,COXA1T),CHAR,ND=Y                                       
RtA1L    LKOUT C,52,(D,,COXA1L),CHAR,ND=Y                                       
RtA1S    LKOUT C,53,(D,,COXA1S),CHAR,ND=Y                                       
RtA1I    LKOUT C,56,(D,,COXA1I),CHAR,ND=Y                                       
RtA1Y    LKOUT C,57,(D,,COXA1Y),CHAR,ND=Y                                       
RtA2C    LKOUT C,58,(D,,COXA2C),CHAR,ND=Y                                       
RtA2T    LKOUT C,59,(D,,COXA2T),CHAR,ND=Y                                       
RtA2L    LKOUT C,60,(D,,COXA2L),CHAR,ND=Y                                       
RtA2S    LKOUT C,61,(D,,COXA2S),CHAR,ND=Y                                       
RtA2I    LKOUT C,64,(D,,COXA2I),CHAR,ND=Y                                       
RtA2Y    LKOUT C,65,(D,,COXA2Y),CHAR,ND=Y                                       
RtA3C    LKOUT C,66,(D,,COXA3C),CHAR,ND=Y                                       
RtA3T    LKOUT C,67,(D,,COXA3T),CHAR,ND=Y                                       
RtA3L    LKOUT C,68,(D,,COXA3L),CHAR,ND=Y                                       
RtA3S    LKOUT C,69,(D,,COXA3S),CHAR,ND=Y                                       
RtA3I    LKOUT C,72,(D,,COXA3I),CHAR,ND=Y                                       
RtA3Y    LKOUT C,73,(D,,COXA3Y),CHAR,ND=Y                                       
RtA4C    LKOUT C,74,(D,,COXA4C),CHAR,ND=Y                                       
RtA4T    LKOUT C,75,(D,,COXA4T),CHAR,ND=Y                                       
RtA4L    LKOUT C,76,(D,,COXA4L),CHAR,ND=Y                                       
RtA4S    LKOUT C,77,(D,,COXA4S),CHAR,ND=Y                                       
RtA4I    LKOUT C,80,(D,,COXA4I),CHAR,ND=Y                                       
RtA4Y    LKOUT C,81,(D,,COXA4Y),CHAR,ND=Y                                       
RtMu1    LKOUT C,82,(D,,COXMU1),CHAR,ND=Y                                       
RtMu2    LKOUT C,83,(D,,COXMU2),CHAR,ND=Y                                       
RtMu3    LKOUT C,84,(D,,COXMU3),CHAR,ND=Y                                       
RtMu4    LKOUT C,85,(D,,COXMU4),CHAR,ND=Y                                       
RtAus    LKOUT C,86,(D,,COXAUS),CHAR,ND=Y                                       
RtRus    LKOUT C,87,(D,,COXRUS),CHAR,ND=Y                                       
RtArt    LKOUT C,88,(D,,COXART),CHAR,ND=Y                                       
RtDub    LKOUT C,89,(D,,COXDUB),CHAR,ND=Y                                       
RtSty    LKOUT C,90,(D,,COXSTY),CHAR,ND=Y                                       
RtFl1    LKOUT C,91,(D,,COXFL1),CHAR,ND=Y                                       
RtLck    LKOUT C,92,(D,,COXLCK),CHAR,ND=Y                                       
RtRel    LKOUT C,93,(D,,COXREL),CHAR,ND=Y                                       
RtCdo    LKOUT C,94,(D,,COXCDO),CHAR,ND=Y                                       
RtCrt    LKOUT C,95,(D,,COXCRT),CHAR,ND=Y                                       
RtWrk    LKOUT C,96,(D,,COXWRK),CHAR,ND=Y                                       
RtSop    LKOUT C,97,(D,,COXSOP),CHAR,ND=Y                                       
RtPrt    LKOUT C,98,(D,,COXPRT),CHAR,ND=Y                                       
RtMid    LKOUT C,99,(D,,COXMID),CHAR,ND=Y                                       
RtAls    LKOUT C,100,(D,,COXALS),UBIN,ND=Y                                      
RtWid    LKOUT C,101,(D,,COXWID),CHAR,ND=Y                                      
RtCAy    LKOUT C,102,(D,,COXCAY),CHAR,ND=Y                                      
RtCIn    LKOUT C,103,(D,,COXCIN),CHAR,ND=Y                                      
RtNCS    LKOUT C,104,(D,,COXNCS),CHAR,ND=Y                                      
RtPrN    LKOUT C,105,(D,,COXPRN),CHAR,ND=Y                                      
RtVSI    LKOUT C,106,(D,,COXVSI),CHAR,ND=Y                                      
RtATC    LKOUT C,107,(D,,COXATC),CHAR,ND=Y                                      
RtACC    LKOUT C,108,(D,,COXACC),CHAR,ND=Y                                      
RtPCy    LKOUT C,109,(D,,COXPCY),CHAR,ND=Y                                      
RtPad    LKOUT C,110,(D,,COXPAD),CHAR,ND=Y                                      
RtA1A    LKOUT C,111,(D,,COXA1A),CHAR,ND=Y                                      
RtA1O    LKOUT C,112,(D,,COXA1O),CHAR,ND=Y                                      
RtA2A    LKOUT C,113,(D,,COXA2A),CHAR,ND=Y                                      
RtA2O    LKOUT C,114,(D,,COXA2O),CHAR,ND=Y                                      
RtA3A    LKOUT C,115,(D,,COXA3A),CHAR,ND=Y                                      
RtA3O    LKOUT C,116,(D,,COXA3O),CHAR,ND=Y                                      
RtA4A    LKOUT C,117,(D,,COXA4A),CHAR,ND=Y                                      
RtA4O    LKOUT C,118,(D,,COXA4O),CHAR,ND=Y                                      
RtA5A    LKOUT C,119,(D,,COXA5A),CHAR,ND=Y                                      
RtA5C    LKOUT C,120,(D,,COXA5C),CHAR,ND=Y                                      
RtA5O    LKOUT C,121,(D,,COXA5O),CHAR,ND=Y                                      
RtA5T    LKOUT C,122,(D,,COXA5T),CHAR,ND=Y                                      
RtA5L    LKOUT C,123,(D,,COXA5L),CHAR,ND=Y                                      
RtA5S    LKOUT C,124,(D,,COXA5S),CHAR,ND=Y                                      
RtA5I    LKOUT C,127,(D,,COXA5I),CHAR,ND=Y                                      
RtA5Y    LKOUT C,128,(D,,COXA5Y),CHAR,ND=Y                                      
RtA6A    LKOUT C,129,(D,,COXA6A),CHAR,ND=Y                                      
RtA6C    LKOUT C,130,(D,,COXA6C),CHAR,ND=Y                                      
RtA6O    LKOUT C,131,(D,,COXA6O),CHAR,ND=Y                                      
RtA6T    LKOUT C,132,(D,,COXA6T),CHAR,ND=Y                                      
RtA6L    LKOUT C,133,(D,,COXA6L),CHAR,ND=Y                                      
RtA6S    LKOUT C,134,(D,,COXA6S),CHAR,ND=Y                                      
RtA6I    LKOUT C,137,(D,,COXA6I),CHAR,ND=Y                                      
RtA6Y    LKOUT C,138,(D,,COXA6Y),CHAR,ND=Y                                      
RtA7A    LKOUT C,139,(D,,COXA7A),CHAR,ND=Y                                      
RtA7C    LKOUT C,140,(D,,COXA7C),CHAR,ND=Y                                      
RtA7O    LKOUT C,141,(D,,COXA7O),CHAR,ND=Y                                      
RtA7T    LKOUT C,142,(D,,COXA7T),CHAR,ND=Y                                      
RtA7L    LKOUT C,143,(D,,COXA7L),CHAR,ND=Y                                      
RtA7S    LKOUT C,144,(D,,COXA7S),CHAR,ND=Y                                      
RtA7I    LKOUT C,147,(D,,COXA7I),CHAR,ND=Y                                      
RtA7Y    LKOUT C,148,(D,,COXA7Y),CHAR,ND=Y                                      
Rt1MA    LKOUT C,149,(D,,COX1MA),CHAR,ND=Y                                      
Rt2MA    LKOUT C,150,(D,,COX2MA),CHAR,ND=Y                                      
Rt3MA    LKOUT C,151,(D,,COX3MA),CHAR,ND=Y                                      
Rt4MA    LKOUT C,152,(D,,COX4MA),CHAR,ND=Y                                      
Rt5MA    LKOUT C,153,(D,,COX5MA),CHAR,ND=Y                                      
Rt6MA    LKOUT C,154,(D,,COX6MA),CHAR,ND=Y                                      
Rt7MA    LKOUT C,155,(D,,COX7MA),CHAR,ND=Y                                      
RtHFN    LKOUT C,156,(D,,COXHFN),CHAR,ND=Y                                      
RtFl2    LKOUT C,157,(D,,COXFL2),CHAR,ND=Y                                      
RtFl3    LKOUT C,158,(D,,COXFL3),CHAR,ND=Y                                      
RtFl4    LKOUT C,159,(D,,COXFL4),CHAR,ND=Y                                      
RtAMT    LKOUT C,167,(D,,COXAMT),CHAR,ND=Y                                      
RtVDt    LKOUT C,168,(D,,COXVDT),CHAR,ND=Y                                      
RtVSt    LKOUT C,169,(D,,COXVST),CHAR,ND=Y                                      
RtSCi    LKOUT C,170,(D,,COXSCI),CHAR,ND=Y                                      
RtEBD    LKOUT C,171,(D,,COXEBD),CHAR,ND=Y                                      
RtCon    LKOUT C,172,(D,,COXCON),CHAR,ND=Y                                      
RtVTi    LKOUT C,173,(D,,COXVTI),CHAR,ND=Y                                      
Array    LKOUT C,O#COAVER,(A,ARYCOV)                                            
Array    LKOUT C,O#COAACN,(A,ARYCOC)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL ATTACHED VERSION RECORDS            *         
***********************************************************************         
                                                                                
ARYCOV   LKOUT A,(R,NXTCOV),MULTIROW=Y,ROWNAME=COXVALS                          
                                                                                
RtVer    LKOUT C,5,(D,,COXVER),UBIN,ND=Y                                        
RtCid    LKOUT C,7,(D,,COXCID),CHAR,ND=Y                                        
RtTit    LKOUT C,10,(D,,COXTIT),CHAR,ND=Y                                       
RtTyp    LKOUT C,11,(D,,COXTYP),CHAR,ND=Y                                       
RtAti    LKOUT C,12,(D,,COXATI),CHAR,ND=Y                                       
RtFar    LKOUT C,15,(D,,COXFAR),CHAR,ND=Y                                       
RtSec    LKOUT C,19,(D,,COXSEC),CHAR,ND=Y                                       
RtFDt    LKOUT C,24,(D,,COXFDT),CHAR,ND=Y                                       
RtFSt    LKOUT C,25,(D,,COXFSU),CHAR,ND=Y                                       
RtFCy    LKOUT C,26,(D,,COXFCY),CHAR,ND=Y                                       
RtFSt    LKOUT C,27,(D,,COXFST),CHAR,ND=Y                                       
RtRDt    LKOUT C,28,(D,,COXRDT),CHAR,ND=Y                                       
RtRSt    LKOUT C,29,(D,,COXRSU),CHAR,ND=Y                                       
RtRCy    LKOUT C,30,(D,,COXRCY),CHAR,ND=Y                                       
RtRSt    LKOUT C,31,(D,,COXRST),CHAR,ND=Y                                       
RtMdt    LKOUT C,32,(D,,COXMDT),CHAR,ND=Y                                       
RtMst    LKOUT C,33,(D,,COXMSU),CHAR,ND=Y                                       
RtMcy    LKOUT C,34,(D,,COXMCY),CHAR,ND=Y                                       
RtMSt    LKOUT C,35,(D,,COXMST),CHAR,ND=Y                                       
RtEyr    LKOUT C,42,(D,,COXEYR),CHAR,ND=Y                                       
RtEty    LKOUT C,43,(D,,COXETY),CHAR,ND=Y                                       
RtAdt    LKOUT C,44,(D,,COXADT),CHAR,ND=Y                                       
RtIdt    LKOUT C,45,(D,,COXIDT),CHAR,ND=Y                                       
RtCmt    LKOUT C,49,(D,,COXCMT),CHAR,ND=Y                                       
RtA1c    LKOUT C,50,(D,,COXA1C),CHAR,ND=Y                                       
RtA1t    LKOUT C,51,(D,,COXA1T),CHAR,ND=Y                                       
RtA1l    LKOUT C,52,(D,,COXA1L),CHAR,ND=Y                                       
RtA1s    LKOUT C,53,(D,,COXA1S),CHAR,ND=Y                                       
RtA1i    LKOUT C,56,(D,,COXA1I),CHAR,ND=Y                                       
RtA1y    LKOUT C,57,(D,,COXA1Y),CHAR,ND=Y                                       
RtA2c    LKOUT C,58,(D,,COXA2C),CHAR,ND=Y                                       
RtA2t    LKOUT C,59,(D,,COXA2T),CHAR,ND=Y                                       
RtA2l    LKOUT C,60,(D,,COXA2L),CHAR,ND=Y                                       
RtA2s    LKOUT C,61,(D,,COXA2S),CHAR,ND=Y                                       
RtA2i    LKOUT C,64,(D,,COXA2I),CHAR,ND=Y                                       
RtA2y    LKOUT C,65,(D,,COXA2Y),CHAR,ND=Y                                       
RtA3c    LKOUT C,66,(D,,COXA3C),CHAR,ND=Y                                       
RtA3t    LKOUT C,67,(D,,COXA3T),CHAR,ND=Y                                       
RtA3l    LKOUT C,68,(D,,COXA3L),CHAR,ND=Y                                       
RtA3s    LKOUT C,69,(D,,COXA3S),CHAR,ND=Y                                       
RtA3i    LKOUT C,72,(D,,COXA3I),CHAR,ND=Y                                       
RtA3y    LKOUT C,73,(D,,COXA3Y),CHAR,ND=Y                                       
RtA4c    LKOUT C,74,(D,,COXA4C),CHAR,ND=Y                                       
RtA4t    LKOUT C,75,(D,,COXA4T),CHAR,ND=Y                                       
RtA4l    LKOUT C,76,(D,,COXA4L),CHAR,ND=Y                                       
RtA4s    LKOUT C,77,(D,,COXA4S),CHAR,ND=Y                                       
RtA4i    LKOUT C,80,(D,,COXA4I),CHAR,ND=Y                                       
RtA4y    LKOUT C,81,(D,,COXA4Y),CHAR,ND=Y                                       
RtMu1    LKOUT C,82,(D,,COXMU1),CHAR,ND=Y                                       
RtMu2    LKOUT C,83,(D,,COXMU2),CHAR,ND=Y                                       
RtMu3    LKOUT C,84,(D,,COXMU3),CHAR,ND=Y                                       
RtMu4    LKOUT C,85,(D,,COXMU4),CHAR,ND=Y                                       
RtAus    LKOUT C,86,(D,,COXAUS),CHAR,ND=Y                                       
RtRus    LKOUT C,87,(D,,COXRUS),CHAR,ND=Y                                       
RtDub    LKOUT C,89,(D,,COXDUB),CHAR,ND=Y                                       
RtAls    LKOUT C,100,(D,,COXALS),UBIN,ND=Y                                      
RtWid    LKOUT C,101,(D,,COXWID),CHAR,ND=Y                                      
RtPad    LKOUT C,110,(D,,COXPAD),CHAR,ND=Y                                      
RtA1A    LKOUT C,111,(D,,COXA1A),CHAR,ND=Y                                      
RtA1O    LKOUT C,112,(D,,COXA1O),CHAR,ND=Y                                      
RtA2A    LKOUT C,113,(D,,COXA2A),CHAR,ND=Y                                      
RtA2O    LKOUT C,114,(D,,COXA2O),CHAR,ND=Y                                      
RtA3A    LKOUT C,115,(D,,COXA3A),CHAR,ND=Y                                      
RtA3O    LKOUT C,116,(D,,COXA3O),CHAR,ND=Y                                      
RtA4A    LKOUT C,117,(D,,COXA4A),CHAR,ND=Y                                      
RtA4O    LKOUT C,118,(D,,COXA4O),CHAR,ND=Y                                      
RtA5A    LKOUT C,119,(D,,COXA5A),CHAR,ND=Y                                      
RtA5C    LKOUT C,120,(D,,COXA5C),CHAR,ND=Y                                      
RtA5O    LKOUT C,121,(D,,COXA5O),CHAR,ND=Y                                      
RtA5T    LKOUT C,122,(D,,COXA5T),CHAR,ND=Y                                      
RtA5L    LKOUT C,123,(D,,COXA5L),CHAR,ND=Y                                      
RtA5S    LKOUT C,124,(D,,COXA5S),CHAR,ND=Y                                      
RtA5I    LKOUT C,127,(D,,COXA5I),CHAR,ND=Y                                      
RtA5Y    LKOUT C,128,(D,,COXA5Y),CHAR,ND=Y                                      
RtA6A    LKOUT C,129,(D,,COXA6A),CHAR,ND=Y                                      
RtA6C    LKOUT C,130,(D,,COXA6C),CHAR,ND=Y                                      
RtA6O    LKOUT C,131,(D,,COXA6O),CHAR,ND=Y                                      
RtA6T    LKOUT C,132,(D,,COXA6T),CHAR,ND=Y                                      
RtA6L    LKOUT C,133,(D,,COXA6L),CHAR,ND=Y                                      
RtA6S    LKOUT C,134,(D,,COXA6S),CHAR,ND=Y                                      
RtA6I    LKOUT C,137,(D,,COXA6I),CHAR,ND=Y                                      
RtA6Y    LKOUT C,138,(D,,COXA6Y),CHAR,ND=Y                                      
RtA7A    LKOUT C,139,(D,,COXA7A),CHAR,ND=Y                                      
RtA7C    LKOUT C,140,(D,,COXA7C),CHAR,ND=Y                                      
RtA7O    LKOUT C,141,(D,,COXA7O),CHAR,ND=Y                                      
RtA7T    LKOUT C,142,(D,,COXA7T),CHAR,ND=Y                                      
RtA7L    LKOUT C,143,(D,,COXA7L),CHAR,ND=Y                                      
RtA7S    LKOUT C,144,(D,,COXA7S),CHAR,ND=Y                                      
RtA7I    LKOUT C,147,(D,,COXA7I),CHAR,ND=Y                                      
RtA7Y    LKOUT C,148,(D,,COXA7Y),CHAR,ND=Y                                      
Rt1MA    LKOUT C,149,(D,,COX1MA),CHAR,ND=Y                                      
Rt2MA    LKOUT C,150,(D,,COX2MA),CHAR,ND=Y                                      
Rt3MA    LKOUT C,151,(D,,COX3MA),CHAR,ND=Y                                      
Rt4MA    LKOUT C,152,(D,,COX4MA),CHAR,ND=Y                                      
Rt5MA    LKOUT C,153,(D,,COX5MA),CHAR,ND=Y                                      
Rt6MA    LKOUT C,154,(D,,COX6MA),CHAR,ND=Y                                      
Rt7MA    LKOUT C,155,(D,,COX7MA),CHAR,ND=Y                                      
Rt26K    LKOUT C,170,(D,,COX26K),CHAR,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL ATTACHED AFM CONTRACT RECORDS       *         
***********************************************************************         
                                                                                
ARYCOC   LKOUT A,(R,NXTCOC),MULTIROW=Y,ROWNAME=COXVALS                          
                                                                                
RtCom    LKOUT C,4,(D,,COXCOM),CHAR,ND=Y                                        
RtVer    LKOUT C,5,(D,,COXVER),UBIN,ND=Y                                        
RtAgy    LKOUT C,6,(D,,COXAGY),CHAR,ND=Y                                        
RtCid    LKOUT C,7,(D,,COXCID),CHAR,ND=Y                                        
RtCli    LKOUT C,8,(D,,COXCLI),CHAR,ND=Y                                        
RtPrd    LKOUT C,9,(D,,COXPRD),CHAR,ND=Y                                        
RtTit    LKOUT C,10,(D,,COXTIT),CHAR,ND=Y                                       
RtTyp    LKOUT C,11,(D,,COXTYP),CHAR,ND=Y                                       
RtATI    LKOUT C,12,(D,,COXATI),CHAR,ND=Y                                       
RtAdS    LKOUT C,13,(D,,COXADS),CHAR,ND=Y                                       
RtFFC    LKOUT C,14,(D,,COXFFC),CHAR,ND=Y                                       
RtFAr    LKOUT C,15,(D,,COXFAR),CHAR,ND=Y                                       
RtSAr    LKOUT C,16,(D,,COXSAR),CHAR,ND=Y                                       
RtExp    LKOUT C,17,(D,,COXEXP),CHAR,ND=Y                                       
RtMed    LKOUT C,18,(D,,COXMED),CHAR,ND=Y                                       
RtSec    LKOUT C,19,(D,,COXSEC),CHAR,ND=Y                                       
RtLid    LKOUT C,20,(D,,COXLID),CHAR,ND=Y                                       
RtLln    LKOUT C,21,(D,,COXLLN),CHAR,ND=Y                                       
RtATY    LKOUT C,22,(D,,COXATY),UBIN,ND=Y                                       
RtAex    LKOUT C,23,(D,,COXAEX),CHAR,ND=Y                                       
RtFDt    LKOUT C,24,(D,,COXFDT),CHAR,ND=Y                                       
RtFSu    LKOUT C,25,(D,,COXFSU),CHAR,ND=Y                                       
RtFCy    LKOUT C,26,(D,,COXFCY),CHAR,ND=Y                                       
RtFSt    LKOUT C,27,(D,,COXFST),CHAR,ND=Y                                       
RtRDt    LKOUT C,28,(D,,COXRDT),CHAR,ND=Y                                       
RtRSt    LKOUT C,29,(D,,COXRSU),CHAR,ND=Y                                       
RtRCy    LKOUT C,30,(D,,COXRCY),CHAR,ND=Y                                       
RtRSt    LKOUT C,31,(D,,COXRST),CHAR,ND=Y                                       
RtMdt    LKOUT C,32,(D,,COXMDT),CHAR,ND=Y                                       
RtMst    LKOUT C,33,(D,,COXMSU),CHAR,ND=Y                                       
RtMcy    LKOUT C,34,(D,,COXMCY),CHAR,ND=Y                                       
RtMSt    LKOUT C,35,(D,,COXMST),CHAR,ND=Y                                       
RtIN1    LKOUT C,36,(D,,COXIN1),CHAR,ND=Y                                       
RtIN2    LKOUT C,37,(D,,COXIN2),CHAR,ND=Y                                       
RtIN3    LKOUT C,38,(D,,COXIN3),CHAR,ND=Y                                       
RtIN4    LKOUT C,39,(D,,COXIN4),CHAR,ND=Y                                       
RtIN5    LKOUT C,40,(D,,COXIN5),CHAR,ND=Y                                       
RtIN6    LKOUT C,41,(D,,COXIN6),CHAR,ND=Y                                       
RtEYr    LKOUT C,42,(D,,COXEYR),CHAR,ND=Y                                       
RtETy    LKOUT C,43,(D,,COXETY),CHAR,ND=Y                                       
RtADt    LKOUT C,44,(D,,COXADT),CHAR,ND=Y                                       
RtIDt    LKOUT C,45,(D,,COXIDT),CHAR,ND=Y                                       
RtPol    LKOUT C,46,(D,,COXPOL),CHAR,ND=Y                                       
RtSCy    LKOUT C,47,(D,,COXSCY),CHAR,ND=Y                                       
RtAtt    LKOUT C,48,(D,,COXATT),CHAR,ND=Y                                       
RtCmT    LKOUT C,49,(D,,COXCMT),CHAR,ND=Y                                       
RtA1C    LKOUT C,50,(D,,COXA1C),CHAR,ND=Y                                       
RtA1T    LKOUT C,51,(D,,COXA1T),CHAR,ND=Y                                       
RtA1L    LKOUT C,52,(D,,COXA1L),CHAR,ND=Y                                       
RtA1S    LKOUT C,53,(D,,COXA1S),CHAR,ND=Y                                       
RtA1I    LKOUT C,56,(D,,COXA1I),CHAR,ND=Y                                       
RtA1Y    LKOUT C,57,(D,,COXA1Y),CHAR,ND=Y                                       
RtA2C    LKOUT C,58,(D,,COXA2C),CHAR,ND=Y                                       
RtA2T    LKOUT C,59,(D,,COXA2T),CHAR,ND=Y                                       
RtA2L    LKOUT C,60,(D,,COXA2L),CHAR,ND=Y                                       
RtA2S    LKOUT C,61,(D,,COXA2S),CHAR,ND=Y                                       
RtA2I    LKOUT C,64,(D,,COXA2I),CHAR,ND=Y                                       
RtA2Y    LKOUT C,65,(D,,COXA2Y),CHAR,ND=Y                                       
RtA3C    LKOUT C,66,(D,,COXA3C),CHAR,ND=Y                                       
RtA3T    LKOUT C,67,(D,,COXA3T),CHAR,ND=Y                                       
RtA3L    LKOUT C,68,(D,,COXA3L),CHAR,ND=Y                                       
RtA3S    LKOUT C,69,(D,,COXA3S),CHAR,ND=Y                                       
RtA3I    LKOUT C,72,(D,,COXA3I),CHAR,ND=Y                                       
RtA3Y    LKOUT C,73,(D,,COXA3Y),CHAR,ND=Y                                       
RtA4C    LKOUT C,74,(D,,COXA4C),CHAR,ND=Y                                       
RtA4T    LKOUT C,75,(D,,COXA4T),CHAR,ND=Y                                       
RtA4L    LKOUT C,76,(D,,COXA4L),CHAR,ND=Y                                       
RtA4S    LKOUT C,77,(D,,COXA4S),CHAR,ND=Y                                       
RtA4I    LKOUT C,80,(D,,COXA4I),CHAR,ND=Y                                       
RtA4Y    LKOUT C,81,(D,,COXA4Y),CHAR,ND=Y                                       
RtMu1    LKOUT C,82,(D,,COXMU1),CHAR,ND=Y                                       
RtMu2    LKOUT C,83,(D,,COXMU2),CHAR,ND=Y                                       
RtMu3    LKOUT C,84,(D,,COXMU3),CHAR,ND=Y                                       
RtMu4    LKOUT C,85,(D,,COXMU4),CHAR,ND=Y                                       
RtAus    LKOUT C,86,(D,,COXAUS),CHAR,ND=Y                                       
RtRus    LKOUT C,87,(D,,COXRUS),CHAR,ND=Y                                       
RtArt    LKOUT C,88,(D,,COXART),CHAR,ND=Y                                       
RtDub    LKOUT C,89,(D,,COXDUB),CHAR,ND=Y                                       
RtSty    LKOUT C,90,(D,,COXSTY),CHAR,ND=Y                                       
RtFl1    LKOUT C,91,(D,,COXFL1),CHAR,ND=Y                                       
RtLck    LKOUT C,92,(D,,COXLCK),CHAR,ND=Y                                       
RtRel    LKOUT C,93,(D,,COXREL),CHAR,ND=Y                                       
RtCdo    LKOUT C,94,(D,,COXCDO),CHAR,ND=Y                                       
RtCrt    LKOUT C,95,(D,,COXCRT),CHAR,ND=Y                                       
RtWrk    LKOUT C,96,(D,,COXWRK),CHAR,ND=Y                                       
RtSop    LKOUT C,97,(D,,COXSOP),CHAR,ND=Y                                       
RtPrt    LKOUT C,98,(D,,COXPRT),CHAR,ND=Y                                       
RtMid    LKOUT C,99,(D,,COXMID),CHAR,ND=Y                                       
RtAls    LKOUT C,100,(D,,COXALS),UBIN,ND=Y                                      
RtWid    LKOUT C,101,(D,,COXWID),CHAR,ND=Y                                      
RtCAy    LKOUT C,102,(D,,COXCAY),CHAR,ND=Y                                      
RtCIn    LKOUT C,103,(D,,COXCIN),CHAR,ND=Y                                      
RtNCS    LKOUT C,104,(D,,COXNCS),CHAR,ND=Y                                      
RtPrN    LKOUT C,105,(D,,COXPRN),CHAR,ND=Y                                      
RtATC    LKOUT C,107,(D,,COXATC),CHAR,ND=Y                                      
RtACC    LKOUT C,108,(D,,COXACC),CHAR,ND=Y                                      
RtA5C    LKOUT C,120,(D,,COXA5C),CHAR,ND=Y                                      
RtA5T    LKOUT C,122,(D,,COXA5T),CHAR,ND=Y                                      
RtA5L    LKOUT C,123,(D,,COXA5L),CHAR,ND=Y                                      
RtA5S    LKOUT C,124,(D,,COXA5S),CHAR,ND=Y                                      
RtA5I    LKOUT C,127,(D,,COXA5I),CHAR,ND=Y                                      
RtA5Y    LKOUT C,128,(D,,COXA5Y),CHAR,ND=Y                                      
RtA6C    LKOUT C,130,(D,,COXA6C),CHAR,ND=Y                                      
RtA6T    LKOUT C,132,(D,,COXA6T),CHAR,ND=Y                                      
RtA6L    LKOUT C,133,(D,,COXA6L),CHAR,ND=Y                                      
RtA6S    LKOUT C,134,(D,,COXA6S),CHAR,ND=Y                                      
RtA6I    LKOUT C,137,(D,,COXA6I),CHAR,ND=Y                                      
RtA6Y    LKOUT C,138,(D,,COXA6Y),CHAR,ND=Y                                      
RtA7C    LKOUT C,140,(D,,COXA7C),CHAR,ND=Y                                      
RtA7T    LKOUT C,142,(D,,COXA7T),CHAR,ND=Y                                      
RtA7L    LKOUT C,143,(D,,COXA7L),CHAR,ND=Y                                      
RtA7S    LKOUT C,144,(D,,COXA7S),CHAR,ND=Y                                      
RtA7I    LKOUT C,147,(D,,COXA7I),CHAR,ND=Y                                      
RtA7Y    LKOUT C,148,(D,,COXA7Y),CHAR,ND=Y                                      
Rt1MA    LKOUT C,149,(D,,COX1MA),CHAR,ND=Y                                      
Rt2MA    LKOUT C,150,(D,,COX2MA),CHAR,ND=Y                                      
Rt3MA    LKOUT C,151,(D,,COX3MA),CHAR,ND=Y                                      
Rt4MA    LKOUT C,152,(D,,COX4MA),CHAR,ND=Y                                      
Rt5MA    LKOUT C,153,(D,,COX5MA),CHAR,ND=Y                                      
Rt6MA    LKOUT C,154,(D,,COX6MA),CHAR,ND=Y                                      
Rt7MA    LKOUT C,155,(D,,COX7MA),CHAR,ND=Y                                      
RtFl2    LKOUT C,157,(D,,COXFL2),CHAR,ND=Y                                      
RtFl3    LKOUT C,158,(D,,COXFL3),CHAR,ND=Y                                      
RtFl4    LKOUT C,159,(D,,COXFL4),CHAR,ND=Y                                      
RtAMT    LKOUT C,167,(D,,COXAMT),CHAR,ND=Y                                      
RtCon    LKOUT C,172,(D,,COXCON),CHAR,ND=Y                                      
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR COMMERCIAL LIMIT RECORDS                       *         
***********************************************************************         
                                                                                
ARYCOL   LKOUT A,(R,NXTCOL),MULTIROW=Y,ROWNAME=COLVALS                          
                                                                                
RtSta    LKOUT C,1,(D,,COLSTAT),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR GUARANTEE SEARCH STATUS RECORDS                *         
***********************************************************************         
                                                                                
ARYGUS   LKOUT A,(R,NXTGUS),ROWNAME=GUSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,GUSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR GUARANTEE SEARCH EXPANDED DETAILS RECORDS      *         
***********************************************************************         
                                                                                
ARYGUX   LKOUT A,(R,NXTGUX),MULTIROW=Y,ROWNAME=GUXVALS                          
                                                                                
RtSsn    LKOUT C,4,(D,,GUXSSN),CHAR,ND=Y                                        
RtGrt    LKOUT C,5,(D,,GUXGRT),CHAR,ND=Y                                        
RtFid    LKOUT C,6,(D,,GUXFID),CHAR,ND=Y                                        
RtCrp    LKOUT C,7,(D,,GUXCRP),CHAR,ND=Y                                        
RtAgy    LKOUT C,8,(D,,GUXAGY),CHAR,ND=Y                                        
RtCli    LKOUT C,9,(D,,GUXCLI),CHAR,ND=Y                                        
RtTyp    LKOUT C,10,(D,,GUXTYP),CHAR,ND=Y                                       
RtPst    LKOUT C,11,(D,,GUXPST),CHAR,ND=Y                                       
RtPen    LKOUT C,12,(D,,GUXPEN),CHAR,ND=Y                                       
RtAmt    LKOUT C,13,(D,,GUXAMT),CBIN,ND=Y                                       
RtBal    LKOUT C,14,(D,,GUXBAL),CBIN,ND=Y                                       
RtBDr    LKOUT C,15,(D,,GUXBDR),CHAR,ND=Y                                       
RtGag    LKOUT C,16,(D,,GUXGAG),CHAR,ND=Y                                       
RtInv    LKOUT C,17,(D,,GUXINV),CHAR,ND=Y                                       
RtPov    LKOUT C,18,(D,,GUXPOV),CHAR,ND=Y                                       
RtIgp    LKOUT C,19,(D,,GUXIGP),CHAR,ND=Y                                       
RtPnh    LKOUT C,20,(D,,GUXPNH),CHAR,ND=Y                                       
RtInp    LKOUT C,21,(D,,GUXINP),CHAR,ND=Y                                       
RtPay    LKOUT C,22,(D,,GUXPAY),CBIN,ND=Y                                       
RtCom    LKOUT C,23,(D,,GUXCOM),CHAR,ND=Y                                       
RtIsc    LKOUT C,24,(D,,GUXISC),CHAR,ND=Y                                       
RtLck    LKOUT C,25,(D,,GUXLCK),CHAR,ND=Y                                       
RtEx1    LKOUT C,26,(D,,GUXEX1),CHAR,ND=Y                                       
RtEx2    LKOUT C,27,(D,,GUXEX2),CHAR,ND=Y                                       
RtEx3    LKOUT C,28,(D,,GUXEX3),CHAR,ND=Y                                       
RtEx4    LKOUT C,29,(D,,GUXEX4),CHAR,ND=Y                                       
RtEx5    LKOUT C,30,(D,,GUXEX5),CHAR,ND=Y                                       
RtEx6    LKOUT C,31,(D,,GUXEX6),CHAR,ND=Y                                       
RtEx7    LKOUT C,32,(D,,GUXEX7),CHAR,ND=Y                                       
RtEx8    LKOUT C,33,(D,,GUXEX8),CHAR,ND=Y                                       
RtEx9    LKOUT C,34,(D,,GUXEX9),CHAR,ND=Y                                       
RtX10    LKOUT C,35,(D,,GUXX10),CHAR,ND=Y                                       
RtX11    LKOUT C,36,(D,,GUXX11),CHAR,ND=Y                                       
RtX12    LKOUT C,37,(D,,GUXX12),CHAR,ND=Y                                       
RtX13    LKOUT C,38,(D,,GUXX13),CHAR,ND=Y                                       
RtX14    LKOUT C,39,(D,,GUXCMT),CHAR,ND=Y                                       
RtWID    LKOUT C,40,(D,,GUXWID),CHAR,ND=Y                                       
Array    LKOUT C,O#GUUBAC,(A,ARYGUU)                                            
Array    LKOUT C,O#GUPCYC,(A,ARYGUP)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR GUARANTEE SEARCH SUBSIDIARY AGY/CLI RECORDS    *         
***********************************************************************         
                                                                                
ARYGUU   LKOUT A,(R,NXTGUU),MULTIROW=Y,ROWNAME=GUUVALS                          
                                                                                
RtAgy    LKOUT C,8,(D,,GUUAGY),CHAR,ND=Y                                        
RtCli    LKOUT C,9,(D,,GUUCLI),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR GUARANTEE SEARCH PERIOD DETAIL RECORDS         *         
***********************************************************************         
                                                                                
ARYGUP   LKOUT A,(R,NXTGUP),MULTIROW=Y,ROWNAME=GUPVALS                          
                                                                                
RtPst    LKOUT C,11,(D,,GUPPST),CHAR,ND=Y                                       
RtPen    LKOUT C,12,(D,,GUPPEN),CHAR,ND=Y                                       
RtAmt    LKOUT C,13,(D,,GUPAMT),CBIN,ND=Y                                       
RtBal    LKOUT C,14,(D,,GUPBAL),CBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR GUARANTEE COMMENT RECORDS                      *         
***********************************************************************         
                                                                                
ARYGUC   LKOUT A,(R,NXTGUC),MULTIROW=Y,ROWNAME=GUCVALS                          
                                                                                
RtGC1    LKOUT C,1,(D,,GUCCM1),CHAR,ND=Y                                        
RtGC2    LKOUT C,2,(D,,GUCCM2),CHAR,ND=Y                                        
RtGC3    LKOUT C,3,(D,,GUCCM3),CHAR,ND=Y                                        
RtGC4    LKOUT C,4,(D,,GUCCM4),CHAR,ND=Y                                        
RtGC5    LKOUT C,5,(D,,GUCCM5),CHAR,ND=Y                                        
RtGC6    LKOUT C,6,(D,,GUCCM6),CHAR,ND=Y                                        
RtGC7    LKOUT C,7,(D,,GUCCM7),CHAR,ND=Y                                        
RtGC8    LKOUT C,8,(D,,GUCCM8),CHAR,ND=Y                                        
RtGC9    LKOUT C,9,(D,,GUCCM9),CHAR,ND=Y                                        
RtGCA    LKOUT C,10,(D,,GUCC10),CHAR,ND=Y                                       
RtGCB    LKOUT C,11,(D,,GUCC11),CHAR,ND=Y                                       
RtGCC    LKOUT C,12,(D,,GUCC12),CHAR,ND=Y                                       
RtGCD    LKOUT C,13,(D,,GUCC13),CHAR,ND=Y                                       
RtGCE    LKOUT C,14,(D,,GUCC14),CHAR,ND=Y                                       
RtGCF    LKOUT C,15,(D,,GUCC15),CHAR,ND=Y                                       
RtGCG    LKOUT C,16,(D,,GUCC16),CHAR,ND=Y                                       
                                                                                
         LKOUT E                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NEW MEDIA/INTERNET SEARCH STATUS RECORDS       *         
***********************************************************************         
                                                                                
ARYNIS   LKOUT A,(R,NXTNIS),ROWNAME=NISVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,NISSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR NEW MEDIA/INTERNET EXPANDED DETAILS RECORDS    *         
***********************************************************************         
                                                                                
ARYNIX   LKOUT A,(R,NXTNIX),MULTIROW=Y,ROWNAME=NIXVALS                          
                                                                                
RtTyp    LKOUT C,4,(D,,NIXMED),CHAR,ND=Y                                        
RtCod    LKOUT C,5,(D,,NIXCOD),CHAR,ND=Y                                        
RtNam    LKOUT C,6,(D,,NIXNAM),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR TIMESHEET SEARCH STATUS RECORDS                *         
***********************************************************************         
                                                                                
ARYTMS   LKOUT A,(R,NXTTMS),ROWNAME=TMSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,TMSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR TIMESHEET TOTALS RECORDS                       *         
***********************************************************************         
                                                                                
ARYTMT   LKOUT A,(R,NXTTMT),MULTIROW=Y,ROWNAME=TMTVALS                          
                                                                                
RtCom    LKOUT C,4,(D,,TMTCOM),CHAR,ND=Y                                        
RtInv    LKOUT C,5,(D,,TMTINV),CHAR,ND=Y                                        
RtSeq    LKOUT C,6,(D,,TMTSEQ),CHAR,ND=Y                                        
RtWCr    LKOUT C,70,(D,,TMTWCR),CHAR,ND=Y                                       
Array    LKOUT C,O#CAVERS,(A,ARYTMD)                                            
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR TIMESHEET DAY DETAILS RECORDS                  *         
***********************************************************************         
                                                                                
ARYTMD   LKOUT A,(R,NXTTMD),MULTIROW=Y,ROWNAME=TMDVALS                          
                                                                                
RtDat    LKOUT C,7,(D,,TMDDAT),CHAR,ND=Y                                        
RtSpt    LKOUT C,8,(D,,TMDSPT),CBIN,ND=Y                                        
RtTag    LKOUT C,9,(D,,TMDTAG),CBIN,ND=Y                                        
RtWST    LKOUT C,10,(D,,TMDWST),CHAR,ND=Y                                       
RtWET    LKOUT C,11,(D,,TMDWET),CHAR,ND=Y                                       
RtNDB    LKOUT C,12,(D,,TMDNDB),CHAR,ND=Y                                       
RtM1S    LKOUT C,13,(D,,TMDM1S),CHAR,ND=Y                                       
RtM1E    LKOUT C,14,(D,,TMDM1E),CHAR,ND=Y                                       
RtM2S    LKOUT C,15,(D,,TMDM2S),CHAR,ND=Y                                       
RtM2E    LKOUT C,16,(D,,TMDM2E),CHAR,ND=Y                                       
RtM3S    LKOUT C,17,(D,,TMDM3S),CHAR,ND=Y                                       
RtM3E    LKOUT C,18,(D,,TMDM3E),CHAR,ND=Y                                       
RtPDD    LKOUT C,19,(D,,TMDPDD),CHAR,ND=Y                                       
RtPDS    LKOUT C,20,(D,,TMDPDS),CHAR,ND=Y                                       
RtPDE    LKOUT C,21,(D,,TMDPDE),CHAR,ND=Y                                       
RtNPR    LKOUT C,22,(D,,TMDNPR),CHAR,ND=Y                                       
RtMP1    LKOUT C,23,(D,,TMDMP1),CHAR,ND=Y                                       
RtMP2    LKOUT C,24,(D,,TMDMP2),CHAR,ND=Y                                       
RtMP3    LKOUT C,25,(D,,TMDMP3),CHAR,ND=Y                                       
RtSmk    LKOUT C,26,(D,,TMDSMK),CHAR,ND=Y                                       
Rt16H    LKOUT C,27,(D,,TMD16H),CHAR,ND=Y                                       
RtReh    LKOUT C,29,(D,,TMDREH),CHAR,ND=Y                                       
RtWCx    LKOUT C,30,(D,,TMDWCX),CHAR,ND=Y                                       
RtRPV    LKOUT C,31,(D,,TMDRPV),CHAR,ND=Y                                       
RtNCD    LKOUT C,32,(D,,TMDNCD),CHAR,ND=Y                                       
RtTDL    LKOUT C,33,(D,,TMDTDL),CHAR,ND=Y                                       
RtTDP    LKOUT C,34,(D,,TMDTDP),CHAR,ND=Y                                       
RtTAR    LKOUT C,35,(D,,TMDTAR),CHAR,ND=Y                                       
RtFDP    LKOUT C,36,(D,,TMDFDP),CHAR,ND=Y                                       
RtFAR    LKOUT C,37,(D,,TMDFAR),CHAR,ND=Y                                       
RtTID    LKOUT C,38,(D,,TMDTID),CHAR,ND=Y                                       
RtTIA    LKOUT C,39,(D,,TMDTIA),CHAR,ND=Y                                       
RtNWA    LKOUT C,40,(D,,TMDNWA),CBIN,ND=Y                                       
RtEWA    LKOUT C,41,(D,,TMDEWA),CBIN,ND=Y                                       
RtOWA    LKOUT C,42,(D,,TMDOWA),CBIN,ND=Y                                       
RtNP1    LKOUT C,45,(D,,TMDNP1),CBIN,ND=Y                                       
RtNP2    LKOUT C,46,(D,,TMDNP2),CBIN,ND=Y                                       
RtNP3    LKOUT C,47,(D,,TMDNP3),CBIN,ND=Y                                       
RtNP4    LKOUT C,48,(D,,TMDNP4),CBIN,ND=Y                                       
RtO16    LKOUT C,49,(D,,TMDO16),CBIN,ND=Y                                       
RtHol    LKOUT C,50,(D,,TMDHOL),CHAR,ND=Y                                       
RtSat    LKOUT C,51,(D,,TMDSAT),CHAR,ND=Y                                       
RtSun    LKOUT C,52,(D,,TMDSUN),CHAR,ND=Y                                       
RtACT    LKOUT C,53,(D,,TMDACT),CHAR,ND=Y                                       
RtSAG    LKOUT C,54,(D,,TMDSAG),CHAR,ND=Y                                       
RtW12    LKOUT C,55,(D,,TMDW12),CHAR,ND=Y                                       
RtW34    LKOUT C,56,(D,,TMDW34),CHAR,ND=Y                                       
RtDys    LKOUT C,57,(D,,TMDDYS),CBIN,ND=Y                                       
RtOTH    LKOUT C,58,(D,,TMDOTH),CBIN,ND=Y                                       
RtDTH    LKOUT C,59,(D,,TMDDTH),CBIN,ND=Y                                       
RtTrv    LKOUT C,60,(D,,TMDTRV),CBIN,ND=Y                                       
RtPDW    LKOUT C,61,(D,,TMDPDW),CBIN,ND=Y                                       
RtASH    LKOUT C,62,(D,,TMDASH),CBIN,ND=Y                                       
RtAOH    LKOUT C,63,(D,,TMDAOH),CBIN,ND=Y                                       
RtInc    LKOUT C,64,(D,,TMDINC),CHAR,ND=Y                                       
RtNSP    LKOUT C,65,(D,,TMDNSP),CBIN,ND=Y                                       
RtAPy    LKOUT C,66,(D,,TMDAPY),CBIN,ND=Y                                       
RtAdj    LKOUT C,67,(D,,TMDADJ),CBIN,ND=Y                                       
RtESF    LKOUT C,68,(D,,TMDESF),CBIN,ND=Y                                       
RtAND    LKOUT C,69,(D,,TMDAND),CBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
         EJECT                                                                  
***********************************************************************         
* ARRAY DEFINITION FOR USE SEARCH STATUS RECORDS                      *         
***********************************************************************         
                                                                                
ARYUSS   LKOUT A,(R,NXTUSS),ROWNAME=USSVALS                                     
                                                                                
RtSta    LKOUT C,1,(D,,USSSTAT),UBIN,ND=Y                                       
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR EXISTING ASSET CYCLE RECORDS                   *         
***********************************************************************         
                                                                                
ARYUAC   LKOUT A,(R,NXTUAC),,MULTIROW=Y,ROWNAME=UPLVALS                         
                                                                                
RtUse    LKOUT C,1,(D,,UPLUSE),CHAR,ND=Y                                        
RtCyS    LKOUT C,2,(D,,UPLICS),CHAR,ND=Y                                        
RtCyE    LKOUT C,3,(D,,UPLICE),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR ASSET LEVEL USE DETAILS RECORDS                *         
***********************************************************************         
                                                                                
ARYUAL   LKOUT A,(R,NXTUAL),MULTIROW=Y,ROWNAME=UPLVALS                          
                                                                                
RtUse    LKOUT C,1,(D,,UPLUSE),CHAR,ND=Y                                        
RtInv    LKOUT C,2,(D,,UPLINV),CHAR,ND=Y                                        
RtICS    LKOUT C,3,(D,,UPLICS),CHAR,ND=Y                                        
RtICE    LKOUT C,4,(D,,UPLICE),CHAR,ND=Y                                        
RtNMS    LKOUT C,5,(D,,UPLMNS),CHAR,ND=Y                                        
RtAlp    LKOUT C,6,(D,,UPLALP),CHAR,ND=Y                                        
RtNCS    LKOUT C,7,(D,,UPLMCS),CHAR,ND=Y                                        
RtNCE    LKOUT C,8,(D,,UPLMCE),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* ARRAY DEFINITION FOR PERFORMER LEVEL USE DETAILS RECORDS            *         
***********************************************************************         
                                                                                
ARYUPL   LKOUT A,(R,NXTUPL),MULTIROW=Y,ROWNAME=UPLVALS                          
                                                                                
RtCSq    LKOUT C,1,(D,,UPLCSQ),CHAR,ND=Y                                        
RtUse    LKOUT C,2,(D,,UPLUSE),CHAR,ND=Y                                        
RtInv    LKOUT C,3,(D,,UPLINV),CHAR,ND=Y                                        
RtICS    LKOUT C,4,(D,,UPLICS),CHAR,ND=Y                                        
RtICE    LKOUT C,5,(D,,UPLICE),CHAR,ND=Y                                        
RtNMS    LKOUT C,6,(D,,UPLMNS),CHAR,ND=Y                                        
RtAlp    LKOUT C,7,(D,,UPLALP),CHAR,ND=Y                                        
RtNCS    LKOUT C,8,(D,,UPLMCS),CHAR,ND=Y                                        
RtNCE    LKOUT C,9,(D,,UPLMCE),CHAR,ND=Y                                        
                                                                                
         LKOUT E                                                                
                                                                                
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
                                                                                
SAVED    DSECT                     ** DSECT TO COVER SAVED STORAGE **           
                                                                                
WVALUES  DS    0X                  ** LITERAL VALUES **                         
FFFS     DS    XL4                                                              
WVALUEL  EQU   *-WVALUES                                                        
                                                                                
*** REQUEST VALUES ****                                                         
                                                                                
* W4 SEARCH DOWNLOAD                                                            
                                                                                
REQVALS  DS    0F                                                               
RQW4STF  DS    CL8                 STAFF                                        
RQW4SSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
RQW4LNM  DS    CL16                FIRST NAME                                   
RQW4FNM  DS    CL16                LAST NAME                                    
                                                                                
* GUARANTEE SEARCH DOWNLOAD                                                     
                                                                                
         ORG   REQVALS                                                          
RQGUSSTF DS    CL8                 STAFF                                        
RQGUSSSN DS    CL9                 SSN                                          
RQGUSGUA DS    CL4                 GUARANTEE CODE                               
RQGUSCPN DS    CL1                 ATTACHED CORPORATION NUMBER                  
RQGUSCRP DS    CL9                 ATTACHED CORPORATION FID                     
RQGUSAGY DS    CL6                 AGENCY CODE                                  
RQGUSCLI DS    CL6                 CLIENT CODE                                  
RQGUSTYP DS    CL1                 TYPE                                         
RQGUSPST DS    XL3                 PERIOD START DATE                            
RQGUSPEN DS    XL3                 PERIOD END DATE                              
RQGUSLCK DS    CL1                 LOCKED?                                      
                                                                                
* COMMERCIAL/VERSION SEARCH DOWNLOAD                                            
                                                                                
         ORG   REQVALS                                                          
RQCOSSTF DS    CL8                 STAFF                                        
RQCOSCIN DS    XL1                 INTERNAL COMMERCIAL NUMBER COUNT             
ARQCOCOM DS    AL3                 A(INT COM NUM FILTER LIST)                   
RQCOSCOM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQCOSVER DS    XL1                 VERSION NUMBER                               
RQCOSAGY DS    CL6                 AGENCY CODE                                  
RQCOSIIN DS    XL1                 COMMERCIAL ID COUNT                          
ARQCOCID DS    AL3                 A(COMMERCIAL ID FILTER LIST)                 
RQCOSCID DS    CL12                COMMERCIAL ID                                
RQCOSCLI DS    CL6                 CLIENT CODE                                  
RQCOSPRD DS    CL6                 PRODUCT CODE                                 
RQCOSTIT DS    CL36                TITLE                                        
RQCOSTIN DS    XL1                 TYPE FILTER COUNT                            
ARQCOTYP DS    AL3                 A(COMMERCIAL TYPE FILTER LIST)               
RQCOSMIN DS    XL1                 MEDIA FILTER COUNT                           
ARQCOMED DS    AL3                 A(COMMERCIAL MEDIA FILTER LIST)              
RQCOSAIN DS    XL1                 ACTRA TYPE FILTER COUNT                      
ARQCOATY DS    AL3                 A(ACTRA TYPE FILTER LIST)                    
RQCOSPOL DS    CL6                 COMMERCIAL POOL CODE                         
RQCOSLCK DS    CL1                 LOCKED?                                      
RQCOSREL DS    CL1                 RELEASED?                                    
RQCOSCDO DS    CL1                 CANADIAN DOLLARS?                            
RQCOSCRT DS    CL1                 CANADIAN RATES?                              
RQCOSSOP DS    CL1                 SOAP RESIDUALS?                              
RQCOSPCY DS    CL1                 PER CYCLE COMMERCIALS?                       
RQCOSPAD DS    CL1                 RETURN PAID STATUS?                          
RQCOSIVR DS    CL1                 INCLUDE VERSIONS IN SEARCH?                  
RQCOSEXT DS    CL1                 SEARCH FOR COMML ID EXACTLY?                 
RQCOSAVR DS    CL1                 RETURN ATTACHED VERSIONS?                    
RQCOSAFM DS    CL1                 RETURN ATTACHED AFM CONTRACTS?               
RQCOSSUS DS    CL1                 SUPER SEARCH?                                
RQCOSEMP DS    CL3                 EMPLOYER                                     
RQCOSELA DS    CL1                 EXCLUDE LOCKED AGENCIES?                     
RQCOSELC DS    CL1                 EXCLUDE LOCKED CLIENTS?                      
RQCOSELP DS    CL1                 EXCLUDE LOCKED PRODUCTS?                     
RQCOSENV DS    CL1                 EXCLUDE NON-VERSION COMMERCIALS?             
RQCOSUIN DS    XL1                 EXCLUDED UNION FILTER COUNT                  
ARQCOEUN DS    AL3                 A(EXCLUDED UNION FILTER LIST)                
RQCOSWIN DS    XL1                 EXCLUDED W4 TYPE COUNT                       
ARQCOEWT DS    AL3                 A(EXCLUDED W4 TYPE FILTER LIST)              
RQCOSERL DS    CL1                 EXCLUDE RELEASE LETTERS?                     
RQCOSEP6 DS    CL1                 EXCLUDE PRE-06 CONTRACTS?                    
RQCOSTEX DS    CL1                 SEARCH FOR COMML TITLE EXACTLY?              
RQCOSEOM DS    CL1                 EXCLUDE COMM'LS W/ OLD STYLE MUSIC?          
RQCOSTMA DS    CL1                 RETURN TRACK HAS MUSICIANS?                  
RQCOSAMT DS    CL1                 RETURN ALL MUSICIANS ON TRACKS?              
RQCOSES9 DS    CL1                 EXCLUDE SSNS THAT BEGIN WITH 9?              
RQCOSSKA DS    CL1                 SKIP ACCESS CHECK?                           
RQCOSELS DS    CL1                 EXCLUDE WHEN ALL PERFS LAST SERV'D?          
RQCOSPED DS    XL3                 ANY PERFORMERS EXPIRE BEFORE DATE            
RQCOSLIM DS    H                   LIMIT RESULTS TO ...                         
RQCOSRWA DS    CL18                REQUESTING WEB APPLICATION                   
         ORG                                                                    
                                                                                
* GUARANTEE SEARCH DOWNLOAD                                                     
                                                                                
         ORG   REQVALS                                                          
RQNISSTF DS    CL8                 STAFF                                        
RQNISMED DS    CL1                 MEDIA                                        
RQNISCOD DS    CL4                 CODE                                         
         ORG                                                                    
                                                                                
* TIMESHEET SEARCH DOWNLOAD                                                     
                                                                                
         ORG   REQVALS                                                          
RQTMSSTF DS    CL8                 STAFF                                        
RQTMSWEB DS    CL1                 WEB CREATED?                                 
RQTMSCOM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQTMSINV DS    XL6                 INVOICE NUMBER                               
RQTMSCSQ DS    XL2                 CAST SEQUENCE NUMBER                         
         ORG                                                                    
                                                                                
* USE SEARCH DOWNLOAD                                                           
                                                                                
         ORG   REQVALS                                                          
RQUSESTF DS    CL8                 STAFF                                        
RQUSECOM DS    XL4                 INTERNAL COMMERCIAL NUMBER                   
RQUSEUSE DS    CL3                 USE                                          
RQUSEPSD DS    XL3                 PERIOD START DATE                            
RQUSEPED DS    XL3                 PERIOD END DATE                              
         ORG                                                                    
                                                                                
REQVALL  EQU   *-REQVALS                                                        
                                                                                
*** OUTPUT VALUES ****                                                          
                                                                                
OUTVALS  DS    0X                  ** OUTPUT VALUES **                          
                                                                                
* ** W4 SEARCH STATUS **                                                        
                                                                                
W4SVALS  DS    0X                                                               
W4SSTAT  DS    CL1                 STATUS                                       
W4SSTSUC EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
W4SSTUNS EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
W4SVALL  EQU   *-W4SVALS                                                        
                                                                                
* ** W4 SEARCH DETAILS **                                                       
                                                                                
W4DVALS  DS    0X                                                               
W4DPID   DS    CL6                 PID                                          
W4DLNAM  DS    CL16                LAST NAME                                    
W4DFNAM  DS    CL16                FIRST NAME                                   
W4DMNAM  DS    CL16                MIDDLE NAME                                  
W4DADD1  DS    CL30                ADDRESS 1                                    
W4DADD2  DS    CL30                ADDRESS 2                                    
W4DADD3  DS    CL30                ADDRESS 3                                    
W4DCITY  DS    CL25                CITY                                         
W4DSTAT  DS    CL2                 STATE                                        
W4DZIPC  DS    CL10                ZIP                                          
W4DVALL  EQU   *-W4DVALS                                                        
                                                                                
* ** COMMERCIAL SEARCH STATUS *                                                 
                                                                                
         ORG   OUTVALS             RESET DISPLACEMENT                           
COSVALS  DS    0X                                                               
COSSTAT  DS    XL1                 STATUS                                       
COSSTEMB EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
COSSTSUC EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
COSSTUNS EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
COSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
COSSDATE DS    CL8                 SEARCH DATE                                  
COSSTIME DS    CL8                 SEARCH TIME                                  
COSSPSST DS    XL1                 PRIMARY SEARCH STATUS                        
COSSPSUC EQU   1                   PRIMARY SEARCH SUCCESSFUL                    
COSSPUNS EQU   2                   PRIMARY SEARCH UNSUCCESSFUL                  
COSSACST DS    XL1                 AGENCY/CLIENT ACCESS STATUS                  
COSSALIM EQU   1                   UNSUCCESSFUL DUE TO AGENCY LIMIT             
COSSCLIM EQU   2                   UNSUCCESSFUL DUE TO CLIENT LIMIT             
COSSINIT EQU   X'FF'               INITIALIZED STATUS                           
COSVALL  EQU   *-COSVALS                                                        
                                                                                
* ** COMMERCIAL EXPANDED DETAILS *                                              
                                                                                
COXVALS  DS    0X                                                               
COXCOM   DS    CL8                 INTERNAL COMMERCIAL NUMBER                   
COXAGY   DS    CL6                 AGENCY CODE                                  
COXCLI   DS    CL6                 CLIENT CODE                                  
COXPRD   DS    CL6                 PRODUCT CODE                                 
COXADS   DS    CL2                 ADDENDUM STATE                               
COXFFC   DS    CL8                 FIRST FIXED CYCLE DATE                       
COXSAR   DS    CL8                 SECOND SEASON FIRST AIR DATE                 
COXEXP   DS    CL8                 EXPIRATION DATE                              
COXMED   DS    CL1                 MEDIA CODE                                   
COXLID   DS    CL12                LIFT ID                                      
COXLLN   DS    CL3                 LIFT LENGTH                                  
COXATY   DS    XL1                 ACTRA TYPE                                   
COXAEX   DS    CL8                 ACTRA EXPIRATION DATE                        
COXPOL   DS    CL6                 COMMERCIAL POOL CODE                         
COXSCY   DS    CL1                 HAS SPLIT CYCLES?                            
COXATT   DS    CL2                 ATTENTION CODE (BILL TO)                     
COXART   DS    CL1                 AFM RATE                                     
COXSTY   DS    CL1                 SESSION TYPE                                 
COXFL1   DS    CL1                 FILTER 1                                     
COXLCK   DS    CL1                 COMMERCIAL LOCKED?                           
COXREL   DS    CL1                 COMMERCIAL RELEASED?                         
COXCDO   DS    CL1                 CANADIAN DOLLARS?                            
COXCRT   DS    CL1                 CANADIAN RATES?                              
COXWRK   DS    CL1                 WORKDATE ON CHECKS?                          
COXSOP   DS    CL1                 SOAP RESIDUALS?                              
COXPRT   DS    CL1                 DISPLAY AS PRINT?                            
COXMID   DS    CL12                MASTER COMMERCIAL ID                         
COXCAY   DS    CL6                 CSF INVOICE AGENCY                           
COXCIN   DS    CL6                 CSF INVOICE NUMBER                           
COXNCS   DS    CL1                 CHARGE CSF?                                  
COXPRN   DS    CL16                PRODUCT NAME                                 
COXVSI   DS    CL18                VITA SESSION ID                              
COXATC   DS    CL1                 ATTACHED TP COMMENT?                         
COXACC   DS    CL1                 ATTACHED CLIENT COMMENT?                     
COXPCY   DS    CL1                 PER CYCLE COMMERCIAL?                        
COXHFN   DS    CL1                 GENERATE HOLDING FEE NOTICES?                
COXFL2   DS    CL3                 FILTER 2                                     
COXFL3   DS    CL1                 FILTER 3                                     
COXFL4   DS    CL1                 FILTER 4                                     
COXVDT   DS    CL8                 VERIFICATION DATE                            
COXVST   DS    CL8                 VERIFICATION STAFF ID                        
COXSCI   DS    CL12                SEARCHED FOR COMMERCIAL ID                   
COXEBD   DS    CL1                 PERFORMERS HAVE EXPIRED BEFORE DATE          
COXCON   DS    CL1                 CONTRACT TYPE                                
COXVTI   DS    CL8                 VERIFICATION TIME                            
COX1LNQ  EQU   *-COXADS                                                         
                                                                                
COXVER   DS    XL1                 VERSION NUMBER                               
COXCID   DS    CL12                COMMERCIAL ID                                
COXTIT   DS    CL36                TITLE                                        
COXTYP   DS    CL1                 COMMERCIAL TYPE                              
COXATI   DS    CL36                TITLE FOR AFM                                
COXFAR   DS    CL8                 FIRST AIR DATE                               
COXSEC   DS    CL3                 LENGTH (SECONDS)                             
COXFDT   DS    CL8                 FILM DATE                                    
COXFSU   DS    CL12                FILM STUDIO                                  
COXFCY   DS    CL12                FILM CITY                                    
COXFST   DS    CL2                 FILM STATE                                   
COXRDT   DS    CL8                 RECORDING DATE                               
COXRSU   DS    CL12                RECORDING STUDIO                             
COXRCY   DS    CL12                RECORDING CITY                               
COXRST   DS    CL2                 RECORDING STATE                              
COXMDT   DS    CL8                 MUSIC DATE                                   
COXMSU   DS    CL12                MUSIC STUDIO                                 
COXMCY   DS    CL12                MUSIC CITY                                   
COXMST   DS    CL2                 MUSIC STATE                                  
COXIN1   DS    CL4                 INTERNET/NEW MEDIA CODE 1                    
COXIN2   DS    CL4                 INTERNET/NEW MEDIA CODE 2                    
COXIN3   DS    CL4                 INTERNET/NEW MEDIA CODE 3                    
COXIN4   DS    CL4                 INTERNET/NEW MEDIA CODE 4                    
COXIN5   DS    CL4                 INTERNET/NEW MEDIA CODE 5                    
COXIN6   DS    CL4                 INTERNET/NEW MEDIA CODE 6                    
COXEYR   DS    CL4                 EDIT TYPE YEAR                               
COXETY   DS    CL6                 EDIT TYPE                                    
COXADT   DS    CL8                 ACTIVE DATE                                  
COXIDT   DS    CL8                 INACTIVE DATE                                
COXCMT   DS    CL60                COMMENT                                      
COXA1A   DS    CL6                 AFM 1 AGENCY                                 
COXA1C   DS    CL12                AFM 1 CODE                                   
COXA1O   DS    CL8                 AFM 1 INTERNAL COMMERCIAL NUMBER             
COXA1T   DS    CL1                 AFM 1 TRACK                                  
COXA1L   DS    CL1                 AFM 1 LIFT                                   
COXA1S   DS    CL3                 AFM 1 LENGTH (SECONDS)                       
COXA1I   DS    CL36                AFM 1 TRACK TITLE                            
COXA1Y   DS    CL1                 AFM 1 MUSIC TYPE                             
COXA1LNQ EQU   *-COXA1A                                                         
COXA2A   DS    CL6                 AFM 2 AGENCY                                 
COXA2C   DS    CL12                AFM 2 CODE                                   
COXA2O   DS    CL8                 AFM 2 INTERNAL COMMERCIAL NUMBER             
COXA2T   DS    CL1                 AFM 2 TRACK                                  
COXA2L   DS    CL1                 AFM 2 LIFT                                   
COXA2S   DS    CL3                 AFM 2 LENGTH (SECONDS)                       
COXA2I   DS    CL36                AFM 2 TRACK TITLE                            
COXA2Y   DS    CL1                 AFM 2 MUSIC TYPE                             
COXA2LNQ EQU   *-COXA2A                                                         
COXA3A   DS    CL6                 AFM 3 AGENCY                                 
COXA3C   DS    CL12                AFM 3 CODE                                   
COXA3O   DS    CL8                 AFM 3 INTERNAL COMMERCIAL NUMBER             
COXA3T   DS    CL1                 AFM 3 TRACK                                  
COXA3L   DS    CL1                 AFM 3 LIFT                                   
COXA3S   DS    CL3                 AFM 3 LENGTH (SECONDS)                       
COXA3I   DS    CL36                AFM 3 TRACK TITLE                            
COXA3Y   DS    CL1                 AFM 3 MUSIC TYPE                             
COXA3LNQ EQU   *-COXA3A                                                         
COXA4A   DS    CL6                 AFM 4 AGENCY                                 
COXA4C   DS    CL12                AFM 4 CODE                                   
COXA4O   DS    CL8                 AFM 4 INTERNAL COMMERCIAL NUMBER             
COXA4T   DS    CL1                 AFM 4 TRACK                                  
COXA4L   DS    CL1                 AFM 4 LIFT                                   
COXA4S   DS    CL3                 AFM 4 LENGTH (SECONDS)                       
COXA4I   DS    CL36                AFM 4 TRACK TITLE                            
COXA4Y   DS    CL1                 AFM 4 MUSIC TYPE                             
COXA4LNQ EQU   *-COXA4A                                                         
COXA5A   DS    CL6                 AFM 5 AGENCY                                 
COXA5C   DS    CL12                AFM 5 CODE                                   
COXA5O   DS    CL8                 AFM 5 INTERNAL COMMERCIAL NUMBER             
COXA5T   DS    CL1                 AFM 5 TRACK                                  
COXA5L   DS    CL1                 AFM 5 LIFT                                   
COXA5S   DS    CL3                 AFM 5 LENGTH (SECONDS)                       
COXA5I   DS    CL36                AFM 5 TRACK TITLE                            
COXA5Y   DS    CL1                 AFM 5 MUSIC TYPE                             
COXA5LNQ EQU   *-COXA5A                                                         
COXA6A   DS    CL6                 AFM 6 AGENCY                                 
COXA6C   DS    CL12                AFM 6 CODE                                   
COXA6O   DS    CL8                 AFM 6 INTERNAL COMMERCIAL NUMBER             
COXA6T   DS    CL1                 AFM 6 TRACK                                  
COXA6L   DS    CL1                 AFM 6 LIFT                                   
COXA6S   DS    CL3                 AFM 6 LENGTH (SECONDS)                       
COXA6I   DS    CL36                AFM 6 TRACK TITLE                            
COXA6Y   DS    CL1                 AFM 6 MUSIC TYPE                             
COXA6LNQ EQU   *-COXA6A                                                         
COXA7A   DS    CL6                 AFM 7 AGENCY                                 
COXA7C   DS    CL12                AFM 7 CODE                                   
COXA7O   DS    CL8                 AFM 7 INTERNAL COMMERCIAL NUMBER             
COXA7T   DS    CL1                 AFM 7 TRACK                                  
COXA7L   DS    CL1                 AFM 7 LIFT                                   
COXA7S   DS    CL3                 AFM 7 LENGTH (SECONDS)                       
COXA7I   DS    CL36                AFM 7 TRACK TITLE                            
COXA7Y   DS    CL1                 AFM 7 MUSIC TYPE                             
COXA7LNQ EQU   *-COXA7A                                                         
COXMU1   DS    CL8                 MUSIC CODE 1                                 
COXMU2   DS    CL8                 MUSIC CODE 2                                 
COXMU3   DS    CL8                 MUSIC CODE 3                                 
COXMU4   DS    CL8                 MUSIC CODE 4                                 
COXAUS   DS    CL1                 ALLOWABLE USES                               
COXRUS   DS    CL1                 REMAINING USES                               
COXDUB   DS    CL8                 DUB DATE                                     
COXALS   DS    XL1                 VERSION ALIAS                                
COXWID   DS    CL18                WEB APPLICATION ID                           
COXPAD   DS    CL1                 PAID?                                        
COX1MA   DS    CL1                 TRACK 1 HAS MUSICIANS ASSOCIATED?            
COX2MA   DS    CL1                 TRACK 2 HAS MUSICIANS ASSOCIATED?            
COX3MA   DS    CL1                 TRACK 3 HAS MUSICIANS ASSOCIATED?            
COX4MA   DS    CL1                 TRACK 4 HAS MUSICIANS ASSOCIATED?            
COX5MA   DS    CL1                 TRACK 5 HAS MUSICIANS ASSOCIATED?            
COX6MA   DS    CL1                 TRACK 6 HAS MUSICIANS ASSOCIATED?            
COX7MA   DS    CL1                 TRACK 7 HAS MUSICIANS ASSOCIATED?            
COXAMT   DS    CL1                 ALL MUSICIANS ON TRACKS?                     
COX26K   DS    CL1                 26K?                                         
COXMALNQ EQU   *-COX1MA                                                         
COX2LNQ  EQU   *-COXVER                                                         
COXVALL  EQU   *-COXVALS                                                        
                                                                                
* ** COMMERCIAL SEARCH LIMIT STATUS *                                           
                                                                                
COLVALS  DS    0X                                                               
COLSTAT  DS    CL1                 STATUS                                       
COLVALL  EQU   *-COLVALS                                                        
                                                                                
* ** GUARANTEE SEARCH STATUS **                                                 
                                                                                
         ORG   OUTVALS                                                          
GUSVALS  DS    0X                                                               
GUSSTAT  DS    CL1                 STATUS                                       
GUSSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
GUSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
GUSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
GUSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
GUSVALL  EQU   *-GUSVALS                                                        
                                                                                
* ** GUARANTEE EXPANDED DETAILS **                                              
                                                                                
GUXVALS  DS    0X                                                               
GUXSSN   DS    CL9                 SOCIAL SECURITY NUMBER                       
GUXGRT   DS    CL4                 GUARANTEE CODE                               
GUXFID   DS    CL9                 ATTACHED CORPORATION FID                     
GUXCRP   DS    CL32                ATTACHED CORPORATION NAME                    
GUXAGY   DS    CL6                 PRIMARY AGENCY CODE                          
GUXCLI   DS    CL6                 PRIMARY CLIENT CODE                          
GUXTYP   DS    CL1                 TYPE                                         
GUXPST   DS    CL8                 PERIOD START DATE                            
GUXPEN   DS    CL8                 PERIOD END DATE                              
GUXAMT   DS    F                   AMOUNT                                       
GUXBAL   DS    F                   BALANCE                                      
GUXBDR   DS    CL1                 BALANCE DIRECTION                            
GUXGAG   DS    CL6                 AGENCY THAT ADDED GUARANTEE                  
GUXINV   DS    CL6                 INVOICE THAT ADDED GUARANTEE                 
GUXPOV   DS    CL1                 PAY OVERAGE?                                 
GUXIGP   DS    CL1                 IGNORE PAY OVERAGE NO ON ESTIMATING?         
GUXPNH   DS    CL1                 PAY P&H ON USE?                              
GUXINP   DS    CL1                 INSTALLMENT PAYMENT PAID?                    
GUXPAY   DS    F                   PAYMENT (INSTALLMENT) AMOUNT                 
GUXCOM   DS    CL8                 PRIMARY INTERNAL COMMERCIAL NUMBER           
GUXISC   DS    CL12                PRIMARY COMMERCIAL ID                        
GUXLCK   DS    CL1                 LOCKED?                                      
GUXEX1   DS    CL3                 EXCLUDED USE #1                              
GUXEX2   DS    CL3                 EXCLUDED USE #2                              
GUXEX3   DS    CL3                 EXCLUDED USE #3                              
GUXEX4   DS    CL3                 EXCLUDED USE #4                              
GUXEX5   DS    CL3                 EXCLUDED USE #5                              
GUXEX6   DS    CL3                 EXCLUDED USE #6                              
GUXEX7   DS    CL3                 EXCLUDED USE #7                              
GUXEX8   DS    CL3                 EXCLUDED USE #8                              
GUXEX9   DS    CL3                 EXCLUDED USE #8                              
GUXX10   DS    CL3                 EXCLUDED USE #10                             
GUXX11   DS    CL3                 EXCLUDED USE #11                             
GUXX12   DS    CL3                 EXCLUDED USE #12                             
GUXX13   DS    CL3                 EXCLUDED USE #13                             
GUXCMT   DS    CL1                 COMMENT INDICATOR                            
GUXWID   DS    CL18                WEB APPLICATION ID                           
GUXVALL  EQU   *-GUXVALS                                                        
                                                                                
* ** GUARANTEE SUBSIDIARY AGENCY/CLIENTS **                                     
                                                                                
GUUVALS  DS    0X                                                               
GUUAGY   DS    CL6                 PRIMARY AGENCY CODE                          
GUUCLI   DS    CL6                 PRIMARY CLIENT CODE                          
GUUVALL  EQU   *-GUUVALS                                                        
                                                                                
* ** PER CYCLE GUARANTEE'S PERIOD DETAILS **                                    
                                                                                
GUPVALS  DS    0X                                                               
GUPPST   DS    CL8                 PERIOD START DATE                            
GUPPEN   DS    CL8                 PERIOD END DATE                              
GUPAMT   DS    F                   AMOUNT                                       
GUPBAL   DS    F                   BALANCE                                      
GUPVALL  EQU   *-GUPVALS                                                        
                                                                                
* ** GUARANTEE COMMENTS **                                                      
                                                                                
GUCVALS  DS    0X                                                               
GUCCM1   DS    CL78                COMMENT #1                                   
GUCCM2   DS    CL78                COMMENT #2                                   
GUCCM3   DS    CL78                COMMENT #3                                   
GUCCM4   DS    CL78                COMMENT #4                                   
GUCCM5   DS    CL78                COMMENT #5                                   
GUCCM6   DS    CL78                COMMENT #6                                   
GUCCM7   DS    CL78                COMMENT #7                                   
GUCCM8   DS    CL78                COMMENT #8                                   
GUCCM9   DS    CL78                COMMENT #9                                   
GUCC10   DS    CL78                COMMENT #10                                  
GUCC11   DS    CL78                COMMENT #11                                  
GUCC12   DS    CL78                COMMENT #12                                  
GUCC13   DS    CL78                COMMENT #13                                  
GUCC14   DS    CL78                COMMENT #14                                  
GUCC15   DS    CL78                COMMENT #15                                  
GUCC16   DS    CL78                COMMENT #16                                  
GUCVALL  EQU   *-GUCVALS                                                        
         ORG                                                                    
                                                                                
* ** NEW MEDIA/INTERNET SEARCH STATUS **                                        
                                                                                
         ORG   OUTVALS                                                          
NISVALS  DS    0X                                                               
NISSTAT  DS    CL1                 STATUS                                       
NISSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
NISSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
NISSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
NISVALL  EQU   *-NISVALS                                                        
                                                                                
* ** NEW MEDIA/INTERNET EXPANDED DETAILS **                                     
                                                                                
NIXVALS  DS    0X                                                               
NIXMED   DS    CL1                 MEDIA                                        
NIXCOD   DS    CL4                 CODE                                         
NIXNAM   DS    CL30                NAME                                         
NIXVALL  EQU   *-NIXVALS                                                        
         ORG                                                                    
                                                                                
* ** TIMESHEET SEARCH STATUS **                                                 
                                                                                
         ORG   OUTVALS                                                          
TMSVALS  DS    0X                                                               
TMSSTAT  DS    CL1                 STATUS                                       
TMSSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
TMSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
TMSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
TMSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
TMSVALL  EQU   *-TMSVALS                                                        
                                                                                
* ** TIMESHEET TOTALS DETAILS **                                                
                                                                                
TMTVALS  DS    0X                                                               
TMTCOM   DS    CL8                 INTERNAL COMMERCIAL NUMBER                   
TMTINV   DS    CL6                 INVOICE                                      
TMTSEQ   DS    CL4                 SEQUENCE NUMBER                              
TMTWCR   DS    CL1                 WEB-CREATED?                                 
TMTVALL  EQU   *-NIXVALS                                                        
                                                                                
* ** TIMESHEET DAY DETAILS **                                                   
                                                                                
TMDVALS  DS    0X                                                               
TMDDAT   DS    CL8                 TIMESHEET DATE                               
TMDSPT   DS    XL1                 SPOTS                                        
TMDTAG   DS    XL1                 TAGS                                         
TMDWST   DS    CL5                 WORK TIME START TIME                         
TMDWET   DS    CL5                 WORK TIME END TIME                           
TMDNDB   DS    CL8                 NON-DEDUCTIBLE MEAL?                         
TMDM1S   DS    CL5                 MEAL 1 START TIME                            
TMDM1E   DS    CL5                 MEAL 1 END TIME                              
TMDM2S   DS    CL5                 MEAL 2 START TIME                            
TMDM2E   DS    CL5                 MEAL 2 END TIME                              
TMDM3S   DS    CL5                 MEAL 3 START TIME                            
TMDM3E   DS    CL5                 MEAL 3 END TIME                              
TMDPDD   DS    CL8                 PRIOR DAY WARDROBE DATE                      
TMDPDS   DS    CL5                 PRIOR DAY WARDROBE START TIME                
TMDPDE   DS    CL5                 PRIOR DAY WARDROBE END TIME                  
TMDNPR   DS    CL1                 NIGHT PREMIUM?                               
TMDMP1   DS    CL1                 MEAL PENALTY 1?                              
TMDMP2   DS    CL1                 MEAL PENALTY 2?                              
TMDMP3   DS    CL1                 MEAL PENALTY 3?                              
TMDSMK   DS    CL1                 SMOKE PAY?                                   
TMD16H   DS    CL1                 16 HOUR RULE?                                
TMDREH   DS    CL1                 REHEARSAL DAY?                               
TMDWCX   DS    CL1                 WEATHER CANCELLATION?                        
TMDRPV   DS    CL1                 REST PERIOD VIOLATION?                       
TMDNCD   DS    CL1                 NON-CONSECUTIVE WORK DAY?                    
TMDTDL   DS    CL1                 DISTANT LOCATION?                            
TMDTDP   DS    CL5                 TRAVEL TO DEPART TIME                        
TMDTAR   DS    CL5                 TRAVEL TO ARRIVE TIME                        
TMDFDP   DS    CL5                 TRAVEL FROM DEPART TIME                      
TMDFAR   DS    CL5                 TRAVEL FROM ARRIVE TIME                      
TMDTID   DS    CL5                 TRAVEL INTERVENING DEPART TIME               
TMDTIA   DS    CL5                 TRAVEL INTERVENING ARRIVE TIME               
TMDNWA   DS    XL1                 WARDROBE ALLOTMENTS NON-EVENING              
TMDEWA   DS    XL1                 WARDROBE ALLOTMENTS EVENING                  
TMDOWA   DS    F                   OTHER AMOUNT                                 
TMDNP1   DS    XL2                 NUMBER OF NIGHT PREMIUM HRS AT 10%           
TMDNP2   DS    XL2                 NUMBER OF NIGHT PREMIUM HRS AT 20%           
TMDNP3   DS    XL2                 NUMBER OF ACTRA NGHT PREM HRS AT 25%         
TMDNP4   DS    XL2                 NUMBER OF SAG NGHT PREM HRS AT 25%           
TMDO16   DS    XL1                 16HR RULE - # OF HOURS OVER                  
TMDHOL   DS    CL1                 HOLIDAY PAY?                                 
TMDSAT   DS    CL1                 SATURDAY PAY?                                
TMDSUN   DS    CL1                 SUNDAY PAY?                                  
TMDACT   DS    CL1                 ACTRA RATES?                                 
TMDSAG   DS    CL1                 SAG RATES?                                   
TMDW12   DS    CL1                 WEATHER CANCELLATION (1/2 PAYCHECK)?         
TMDW34   DS    CL1                 WEATHER CANCELLATION (3/4 PAYCHECK)?         
TMDDYS   DS    XL1                 DAYS                                         
TMDOTH   DS    XL1                 OVERTIME HOURS                               
TMDDTH   DS    XL1                 DOUBLETIME HOURS                             
TMDTRV   DS    XL2                 TRAVEL TIME                                  
TMDPDW   DS    XL2                 PRIOR DAY WARDROBE                           
TMDASH   DS    XL2                 ACTRA NP STRAIGHT HRS                        
TMDAOH   DS    XL2                 ACTRA NP OVERTIME HRS                        
TMDINC   DS    CL1                 INCLUDE CODE                                 
TMDNSP   DS    XL4                 AMOUNT NOT SUBJECT TO P&H (MEAL PEN)         
TMDAPY   DS    XL4                 ADDITION TO PAYMENT AMT. (SMOKE PAY)         
TMDADJ   DS    XL4                 ADJUSTMENT AMOUNT                            
TMDESF   DS    XL2                 EXTRAS SATURDAY PAY ON FRIDAYS               
TMDAND   DS    XL2                 ACTRA NP DOUBLETIME HRS                      
TMDVALL  EQU   *-TMDVALS                                                        
         ORG                                                                    
                                                                                
* ** USE SEARCH STATUS **                                                       
                                                                                
         ORG   OUTVALS                                                          
USSVALS  DS    0X                                                               
USSSTAT  DS    CL1                 STATUS                                       
USSSEMB  EQU   0                   SUCCESSFUL   - EMBEDDED DOWNLOAD             
USSSSUC  EQU   1                   SUCCESSFUL   - MATCHES FOUND                 
USSSUNS  EQU   2                   UNSUCCESSFUL - NO MATCHES FOUND              
USSSINV  EQU   3                   UNSUCCESSFUL - INVALID REQUEST               
USSVALL  EQU   *-USSVALS                                                        
                                                                                
* ** ASSET/PERFORMER LEVEL USE DETAILS **                                       
                                                                                
UPLVALS  DS    0X                                                               
UPLCSQ   DS    CL4                 CAST SEQUENCE NUMBER                         
UPLUSE   DS    CL3                 USE                                          
UPLINV   DS    CL6                 INVOICE NUMBER                               
UPLICS   DS    CL8                 INVOICE CYCLE START DATE                     
UPLICE   DS    CL8                 INVOICE CYCLE END DATE                       
UPLMNS   DS    CL8                 MARKET/NETWORK/SYSTEM PRIMARY KEY            
UPLALP   DS    CL6                 MARKET/NETWORK/SYSTEM ALPHA CODE             
UPLMCS   DS    CL8                 MARKET/NETWORK/SYSTEM CYC START DT           
UPLMCE   DS    CL8                 MARKET/NETWORK/SYSTEM CYC END DATE           
UPLVALL  EQU   *-UPLVALS                                                        
         ORG                                                                    
                                                                                
OUTVALL  EQU   *-OUTVALS                                                        
                                                                                
*** REGULAR STORAGE ***                                                         
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      RUNNER/DDLINK MODE                           
USERID   DS    XL2                 HEX USER ID                                  
COMPANY  DS    XL(L'LP_AGYB)       COMPANY CODE                                 
CPYALPH  DS    CL(L'LP_AGY)        COMPANY ALPHA ID                             
                                                                                
MAP      DS    XL2                                                              
                                                                                
PROSTAT  DS    XL1                 PROGRAM STATUS                               
PSSUS    EQU   X'80'               SUPER SEARCH                                 
                                                                                
ACOMTAB  DS    A                   A(COMMERCIAL TABLE)                          
         ORG   ACOMTAB                                                          
AUDTAB   DS    A                   A(USE DETAILS TABLE)                         
ASSNTAB  DS    A                   A(CAST SS# TABLE)                            
                                                                                
SVADDR   DS    A                   SAVED AREA FOR NEEDED ADDRESSES              
         ORG   SVADDR                                                           
ACURTAVA DS    A                   A(CURRENT AGY/CLI LIMIT ELEMENT)             
         ORG   SVADDR                                                           
ACURTATM DS    A                   A(CURRENT TIMESHEET DETAILS ELEMENT)         
SVCLIA   DS    A                   A(CURRENT CLIENT IN AGY/CLI ELEM)            
SVADDRE  DS    A                   A(END OF AGY/CLI ELEMENT)                    
ATAGCEL  DS    A                   A(GUARANTEE CYCLE ELEMENT)                   
                                                                                
SVSSN    DS    CL(L'TLW4SSN)       SAVED AREA FOR W4 SS#                        
SVAGY    DS    CL6                 SAVED AGENCY                                 
SVCLI    DS    CL6                 SAVED CLIENT                                 
SVEMP    DS    CL3                 SAVED EMPLOYER                               
SVCOM    DS    XL4                 SAVED INTERNAL CID                           
SVVER    DS    XL1                 SAVED VERSION NUMBER                         
SVCOSTA2 DS    XL1                 SAVED SECOND COMMERCIAL STATUS               
SVCORP   DS    XL1                 SAVED CORPORATION CODE                       
SVAACNS  DS    XL(50*L'TATRCOM)    SAVED AFM CONTRACTS                          
         DS    X                   END OF AFM CONTRACTS LIST                    
SVRECEQU DS    X                   SAVED RECORD EQUATE                          
SRCHCID  DS    CL(L'TLCOICID)      CURRENT SEARCHED COMMERCIAL ID               
SVUHCSEQ DS    XL(L'TLUHCSEQ)      SAVED CAST SEQUENCE NUMBER                   
EBD      DS    C                   PERFORMERS HAVE EXPIRED BEFORE DATE          
                                                                                
SVIOKEY  DS    XL(L'IOKEY)         SAVED KEY FOR NESTED ROUTINES                
SVCOKEY  DS    XL(L'TLCOKEY)       SAVED COMMERCIAL KEY                         
SVVRKEY  DS    XL(L'TLVRKEY)       SAVED VERSION KEY                            
                                                                                
CASTSTAT DS    XL1                 CAST STATUS BYTE                             
CSHASCST EQU   X'80'               COMM'L HAS CAST                              
CSNOTLSV EQU   X'40'               COMM'L HAS CAST NOT LAST SERVICED            
                                                                                
PVRTAB   DS    XL251               PAID VERSIONS TABLE                          
                                                                                
FRSTCOM  DS    XL4                 FIRST INTERNAL COMMERCIAL NUMBER             
LASTCOM  DS    XL4                 LAST INTERNAL COMMERCIAL NUMBER              
ABOVELIM DS    H                   RECORD LEFT BEFORE LIMIT                     
                                                                                
LNAMELEN DS    X                   LENGTH OF LAST NAME INPUT                    
FNAMELEN DS    X                   LENGTH OF FIRST NAME INPUT                   
                                                                                
LKEYCOMP DS    X                   LENGTH OF KEY COMPARE                        
                                                                                
ACLIMSTA DS    X                   AGENCY/CLIENT LIMIT STATUS                   
                                                                                
UDTENT   DS    XL(UDTLNQ)          USE DETAILS TABLE ENTRY                      
                                                                                
AFMTAB   DS    XL(7*L'TATRCOM+1)   TABLE OF AFM CONTRACTS                       
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
***********************************************************************         
*        DSECT TO COVER ASSOCIATED TRACK TABLE FOR TATRNTR            *         
*        INCLUDE BOOK                                                 *         
***********************************************************************         
                                                                                
TATRTABD DSECT                                                                  
TTCOM    DS    XL(L'TATRCOM)     INTERNAL COMMERCIAL NUMBER                     
TTTRK    DS    XL(L'TATRTRK)     TRACK                                          
TTLNQ    EQU   *-TATRTABD                                                       
                                                                                
***********************************************************************         
*        DSECT TO COVER USE DETAILS TABLE ENTRY                       *         
***********************************************************************         
                                                                                
UDTABD   DSECT                                                                  
UDTSTAT  DS    XL1                 STATUS                                       
UDTSCRED EQU   X'80'               CREDIT PAYMENT OR CANCELLED                  
UDTINV   DS    CL6                 INVOICE NUMBER                               
                                                                                
UDTUSE   DS    CL3                 USE                                          
UDTICS   DS    XL3                 INVOICE CYCLE START DATE                     
UDTICE   DS    XL3                 INVOICE CYCLE END DATE                       
UDTALNQ  EQU   *-UDTUSE                                                         
                                                                                
UDTMNS   DS    CL4                 MARKET/NETWORK/SYSTEM PRIMARY KEY            
UDTALP   DS    CL6                 MARKET/NETWORK/SYSTEM ALPHA CODE             
UDTSLNQ  EQU   *-UDTUSE                                                         
                                                                                
UDTMCS   DS    XL3                 MARKET/NETWORK/SYSTEM CYC START DT           
UDTMCE   DS    XL3                 MARKET/NETWORK/SYSTEM CYC END DATE           
UDTMLNQ  EQU   *-UDTMNS                                                         
                                                                                
UDTLNQ   EQU   *-UDTABD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TALNK13   06/17/15'                                      
         END                                                                    
